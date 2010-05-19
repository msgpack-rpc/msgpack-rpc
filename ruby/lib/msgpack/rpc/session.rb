#
# MessagePack-RPC for Ruby
#
# Copyright (C) 2010 FURUHASHI Sadayuki
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
#
module MessagePack
module RPC


class Session
	def initialize(builder, address, loop)
		@address = address
		@loop = loop
		@reqtable = {}
		@timeout = 10    # FIXME default timeout time
		@seqid = 0
		@transport = builder.build_transport(self, address)
	end
	attr_reader :loop, :address
	attr_accessor :timeout

	# backward compatibility
	def port; @address.port; end
	def host; @address.host; end

	def call(method, *args)
		call_async(method, *args).get
	end

	def call_async(method, *args)
		future = send_request(method, args)
	end
	alias send call_async

	def callback(method, *args, &block)
		future = send_request(method, args)
		future.attach_callback(block)
		future
	end

	def notify(method, *args)
		send_notify(method, args)
		nil
	end

	def close
		@transport.close
		@reqtable = {}
		@seqid = 0
		self
	end

	# from ClientTransport
	def on_response(sock, msgid, error, result)
		if future = @reqtable.delete(msgid)
			future.set_result(error, result)
		end
	end

	# from ClientTransport
	def on_connect_failed
		@reqtable.reject! {|msgid, future|
			begin
				future.set_result ConnectError.new, nil
			rescue
			end
			true
		}
		nil
	end

	# from Client, SessionPool
	def step_timeout
		timedout = []
		@reqtable.reject! {|msgid, future|
			if future.step_timeout
				timedout.push(future)
				true
			end
		}
		timedout.each {|future|
			begin
				future.set_result TimeoutError.new, nil
			rescue
			end
		}
		!@reqtable.empty?
	end

	private
	def send_request(method, param)
		method = method.to_s
		msgid = @seqid
		@seqid += 1; if @seqid >= 1<<31 then @seqid = 0 end
		data = [REQUEST, msgid, method, param].to_msgpack
		@transport.send_data(data)
		@reqtable[msgid] = Future.new(self, @loop)
	end

	def send_notify(method, param)
		method = method.to_s
		data = [NOTIFY, method, param].to_msgpack
		@transport.send_data(data)
		nil
	end
end


end
end
