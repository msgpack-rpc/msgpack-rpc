#
# MessagePack-RPC for Ruby
#
# Copyright (C) 2010-2011 FURUHASHI Sadayuki
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


# Session is a abstract class corresponds with a remote host.
# You can call remote method using call, call_async, callback or notify method.
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

	# Sets and gets timeout in seconds.
	attr_accessor :timeout

	# backward compatibility
	def port  #:nodoc:
		@address.port
	end

	# backward compatibility
	def host  #:nodoc:
		@address.host
	end

	# call-seq:
	#   call(symbol, *args) -> result of remote method
	#
	# Calls remote method.
	# This method is same as call_async(method, *args).get
	def call(method, *args)
		send_request(method, args).get
	end

	# call-seq:
	#   call_apply(symbol, params) -> result of remote method
	#
	# Calls remote method.
	# This method is same as call(method, *args) excepting that
	# the arugment is a array.
	def call_apply(method, params)
		send_request(method, params).get
	end

	# call-seq:
	#   call_async(symbol, *args) -> Future
	#
	# Calls remote method asynchronously.
	# This method is non-blocking and returns Future.
	def call_async(method, *args)
		future = send_request(method, args)
	end

	# call-seq:
	#   call_async_apply(symbol, params) -> Future
	#
	# Calls remote method asynchronously.
	# This method is same as call_async(method, *args) excepting that
	# the arugment is a array.
	def call_async_apply(method, params)
		future = send_request(method, params)
	end

	# backward compatibility
	alias_method :send_without_call_async, :send
	def send(method, *args)
	    if caller.first =~ /.*_test.rb/ || caller.first =~ /.*_spec.rb/ then
	    	    warn "\n Don't use send method. Use 'call_async' method."
            end
	    call_async(method, *args)
	end


	# call-seq:
	#   callback(symbol, *args) {|future| }
	#
	# Calls remote method asynchronously.
	# The callback method is called with Future when the result is reached.
	# This method is same as call_async(method, *args).attach_callback {|future|  }
	def callback(method, *args, &block)
		future = send_request(method, args)
		future.attach_callback(block)
		future
	end

	# call-seq:
	#   callback_apply(symbol, params) {|future| }
	#
	# Calls remote method asynchronously.
	# The callback method is called with Future when the result is reached.
	# This method is same as callback(method, *args).attach_callback {|future|  }
	# excepting that the argument is a array.
	def callback_apply(method, params, &block)
		future = send_request(method, params)
		future.attach_callback(block)
		future
	end

	# call-seq:
	#   notify(symbol, *args) -> nil
	#
	# Calls remote method with NOTIFY protocol.
	# It doesn't require server to return results.
	# This method is non-blocking and returns nil.
	def notify(method, *args)
		send_notify(method, args)
		nil
	end

	# call-seq:
	#   notify_apply(symbol, params) -> nil
	#
	# Calls remote method with NOTIFY protocol.
	# It doesn't require server to return results.
	# This method is non-blocking and returns nil.
	# This method is same as notify(method, *args) excepting that
	# the argument is a array.
	def notify_apply(method, params)
		send_notify(method, params)
		nil
	end

	# Closes underlaying Transport and destroy resources.
	def close
		@transport.close
		@reqtable = {}
		@seqid = 0
		self
	end

	# from ClientTransport
	def on_response(sock, msgid, error, result)  #:nodoc:
		if future = @reqtable.delete(msgid)
			future.set_result(error, result)
		end
	end

	# from ClientTransport
	def on_connect_failed  #:nodoc:
		@reqtable.reject! {|msgid, future|
			future.set_result ConnectionTimeoutError::CODE, ["connection timed out"]
			true
		}
		nil
	end

	# from Client, SessionPool
	def step_timeout  #:nodoc:
		timedout = []
		@reqtable.reject! {|msgid, future|
			if future.step_timeout
				timedout.push(future)
				true
			end
		}
		timedout.each {|future|
			future.set_result TimeoutError::CODE, ["request timed out"]
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
