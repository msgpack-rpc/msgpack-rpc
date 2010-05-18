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


class Future
	def initialize(session, loop, callback = nil)
		@timeout = session.timeout
		@loop = loop
		@callback_handler = callback
		@error_handler = nil
		@result_handler = nil
		@set = false
		@error = nil
		@result = nil
	end
	attr_reader :loop
	attr_accessor :result, :error

	def get
		join
		if error.nil?
			if @result_handler
				return @result_handler.call(@result)
			else
				return @result
			end
		end
		if @error_handler
			return @error_handler.call(self)
		else
			raise @error if @error.is_a?(Error)
			raise RemoteError.new(@error, @result)
		end
	end

	def join
		until @set
			@loop.run_once
		end
		self
	end

	def attach_callback(proc = nil, &block)
		@callback_handler = proc || block
	end

	def attach_error_handler(proc = nil, &block)
		@error_handler = proc || block
	end

	def attach_result_handler(proc = nil, &block)
		@result_handler = proc || block
	end

	def set_result(err, res)
		@error  = err
		@result = res
		if @callback_handler
			@callback_handler.call(err, res)
		end
		@set = true
	end

	def step_timeout
		if @timeout < 1
			true
		else
			@timeout -= 1
			false
		end
	end
end


end
end
