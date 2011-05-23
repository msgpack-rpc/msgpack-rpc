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


# MultiFuture bunldes up multiple Future objects.
class MultiFuture
	def initialize
		@all = []

		@not_joined = []
		@joined = []
		@error = []
		@success = []

		@on_all = nil
		@on_error = nil
		@on_success = nil
		@on_num = {}  # {num => callback}
	end

	# Gets all registered Future objects.
	attr_reader :all

	# Gets Future objects which are not joined yet.
	attr_reader :not_joined

	# Gets Future objects which are already joined.
	attr_reader :joined

	# Gets Future objects which are joined as error.
	attr_reader :error

	# Gets Future objects which are joined as success.
	attr_reader :success

	# Clears all Future objects and all callback methods.
	def clear
		@all = []

		@not_joined = []
		@joined = []
		@error = []
		@success = []

		clear_callback
	end

	# Clears all callback methods registered by on_xxx methods.
	def clear_callback
		@on_all = nil
		@on_error = nil
		@on_success = nil
		@on_num = {}
		self
	end

	# Registeres new Future object.
	# Returns self.
	def add(future)
		future.attach_callback(&method(:callback))
		@all << future
		@not_joined << future
		self
	end

	# Attaches a callback method that is called when
	# all Future objects are joined.
	# Returns self.
	def on_all(&block)
		@on_all = block
		self
	end

	# Attaches a callback method that is called when
	# Future objects are joined as success.
	# Returns self.
	def on_success(&block)
		@on_success = block
		self
	end

	# Attaches a callback method that is called when
	# Future objects are joined as error.
	# Returns self.
	def on_error(&block)
		@on_error = block
		self
	end

	# Attaches a callback method that is called when
	# specified number of Future objects are joined.
	# Returns self.
	def on_num(n, &block)
		@on_num[n.to_i] = block
		self
	end

	# Waits until all Future objects join.
	def join_all
		@not_joined.each {|future|
			future.join
		}
		@all
	end

	# Waits until at least one Future object joins as success.
	# Returns the joined Future object or nil.
	def join_success
		until @not_joined.empty?
			unless @success.empty?
				break
			end
			@not_joined.first.loop.run_once
		end
		@success.last
	end

	# Waits until at least one Future object joins as error.
	# Returns the joined Future object or nil.
	def join_error
		until @not_joined.empty?
			unless @error.empty?
				break
			end
			@not_joined.first.loop.run_once
		end
		@error.last
	end

	# Waits until specified number of Future objects join.
	# Returns the joined Future objects or nil.
	def join_num(n)
		until @not_joined.empty?
			unless @joined.size >= n
				return @joined
			end
			@not_joined.first.loop.run_once
		end
		nil
	end

	private

	def callback(future)
		if @not_joined.delete(future)
			@joined << future

			if future.error == nil
				@success << future
				if @on_success
					@on_success.call(future) rescue nil
				end
			else
				@error << future
				if @on_error
					@on_error.call(future) rescue nil
				end
			end

			if callback = @on_num[@joined.size]
				callback.call(@joined) rescue nil
			end

			if @on_all && @not_joined.empty?
				@on_all.call(@all) rescue nil
			end
		end
	end
end


end
end
