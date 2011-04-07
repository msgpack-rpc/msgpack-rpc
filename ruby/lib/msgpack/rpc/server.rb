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


# Server is usable for RPC server.
# Note that Server is a SessionPool.
class Server < SessionPool
	# 1. initialize(builder, loop = Loop.new)
	# 2. initialize(loop = Loop.new)
	def initialize(arg1=nil, arg2=nil)
		super(arg1, arg2)
		@dispatcher = nil
		@listeners = []
	end

	# 1. serve(dispatcher)
	# 2. serve(obj, accept = obj.public_methods)
	def serve(obj, accept = obj.public_methods)
		if obj.is_a?(Dispatcher)
			# 2.
			@dispatcher = obj
		else
			# 1.
			@dispatcher = ObjectDispatcher.new(obj, accept)
		end
		self
	end

	# 1. listen(listener, obj = nil, accept = obj.public_methods)
	# 2. listen(host, port, obj = nil, accept = obj.public_methods)
	def listen(arg1, arg2 = nil, arg3 = nil, arg4 = nil)
		if arg1.respond_to?(:listen)
			# 1.
			listener = arg1
			obj      = arg2
			accept   = arg3 || obj.public_methods
		else
			# 2.
			listener = TCPServerTransport.new(Address.new(arg1,arg2))
			obj      = arg3
			accept   = arg4 || obj.public_methods
		end

		unless obj.nil?
			serve(obj, accept)
		end

		listener.listen(self)
		@listeners.push(listener)
		nil
	end

	def close
		@listeners.reject! {|listener|
			listener.close
			true
		}
		super
	end

	# from ServerTransport
	def on_request(sendable, msgid, method, param)  #:nodoc:
		responder = Responder.new(sendable, msgid)
		dispatch_method(method, param, responder)
	end

	# from ServerTransport
	def on_notify(method, param)  #:nodoc:
		responder = NullResponder.new
		dispatch_method(method, param, responder)
	end

	private
	def dispatch_method(method, param, responder)  #:nodoc:
		begin
			sent = false
			early_result = nil
			result = @dispatcher.dispatch(method, param) do |result_|
				unless result_.is_a?(AsyncResult)
					responder.result(result_)
					sent = true
				end
				early_result = result_
			end

		#FIXME on NoMethodError
		# res.error(NO_METHOD_ERROR); return

		#FIXME on ArgumentError
		# res.error(ArgumentError); return

		rescue
			responder.error($!.to_s)
			return
		end

		if early_result.is_a?(AsyncResult)
			early_result.set_responder(responder)
		elsif sent
			return
		elsif result.is_a?(AsyncResult)
			result.set_responder(responder)
		else
			responder.result(result)
		end
	end
end


class AsyncResult
	def initialize
		@responder = nil
		@sent = false
	end

	def result(retval, err = nil)
		unless @sent
			if @responder
				@responder.result(retval, err)
			else
				@result = [retval, err]
			end
			@sent = true
		end
		nil
	end

	def error(err)
		result(nil, err)
		nil
	end

	def set_responder(res)  #:nodoc:
		@responder = res
		if @sent && @result
			@responder.result(*@result)
			@result = nil
		end
	end
end


class Responder
	def initialize(sendable, msgid)
		@sendable = sendable  # send_message method is required
		@msgid = msgid
		@sent = false
	end

	def sent?
		@sent
	end

	def result(retval, err = nil)
		unless @sent
			data = [RESPONSE, @msgid, err, retval].to_msgpack
			@sendable.send_data(data)
			@sent = true
		end
		nil
	end

	def error(err, retval = nil)
		result(retval, err)
	end
end


class NullResponder
	def sent?
		true
	end

	def result(retval, err = nil)
		nil
	end

	def error(err, retval = nil)
		nil
	end
end


#:nodoc:
class Server::Base
	def initialize(*args)
		@base = Server.new(*args)
		@base.serve(self)
	end
	attr_reader :base
end


end
end
