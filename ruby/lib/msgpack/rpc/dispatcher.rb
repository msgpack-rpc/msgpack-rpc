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


class Responder
	def initialize(sendable, msgid)
		@sendable = sendable  # send_message method is required
		@msgid = msgid
	end

	def result(retval, err = nil)
		data = [RESPONSE, @msgid, err, retval].to_msgpack
		@sendable.send_data(data)
	end

	def error(err, retval = nil)
		result(retval, err)
	end
end


class ObjectDispatcher
	def initialize(obj, accept = obj.public_methods)
		@obj = obj
		@accept = accept.map {|m| m.is_a?(Integer) ? m : m.to_s}
	end

	def dispatch_request(session, method, param, responder)
		begin
			sent = false
			early_result = nil
			result = forward_method(session, method, param) do |result_|
				unless result_.is_a?(AsyncResult)
					responder.result(result_)
					sent = true
				end
				early_result = result_
			end
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

	def dispatch_notify(session, method, param)
		forward_method(session, method, param)
	rescue
	end

	private
	def forward_method(session, method, param, &block)
		unless @accept.include?(method)
			raise NoMethodError, "method `#{method}' is not accepted"
		end
		@obj.send(method, *param, &block)
	end
end


end
end
