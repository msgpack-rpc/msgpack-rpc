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


module Dispatcher
end


class ObjectDispatcher
	include Dispatcher

	def initialize(obj, accept = obj.public_methods)
		@obj = obj
		@accept = accept.map {|m| m.is_a?(Integer) ? m : m.to_s }
	end

	def dispatch(method, param, &block)
		unless @accept.include?(method)
			raise NoMethodError, "method `#{method}' is not accepted"
		end
		@obj.send(method, *param, &block)
	end
end


#:nodoc:
class MethodForwarder
end


end
end
