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


module MessageReceiver
	def on_message(msg, *ctx)
		case msg[0]
		when REQUEST
			on_request(msg[1], msg[2], msg[3], *ctx)
		when RESPONSE
			on_response(msg[1], msg[2], msg[3], *ctx)
		when NOTIFY
			on_notify(msg[1], msg[2], *ctx)
		else
			raise RPCError.new("unknown message type #{msg[0]}")
		end
	end

	#def on_request(msgid, method, param)
	#end

	#def on_notify(method, param)
	#end

	#def on_response(msgid, error, result)
	#end
end


end
end
