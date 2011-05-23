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


Loop = Rev::Loop


module LoopUtil
	attr_reader :loop

	class Timer < Rev::TimerWatcher
		def initialize(interval, repeating, &block)
			@block = block
			super(interval, repeating)
		end
		def on_timer
			@block.call
		end
	end

	def start_timer(interval, repeating, &block)
		@loop.attach Timer.new(interval, repeating, &block)
	end

	class TaskQueue < Rev::AsyncWatcher
		def initialize
			@queue = []
			super
		end

		def push(task)
			@queue.push(task)
			signal
		end

		def on_signal
			while task = @queue.shift
				begin
					task.call
				rescue
				end
			end
		end
	end

	def submit(task = nil, &block)
		task ||= block
		unless @queue
			@queue = TaskQueue.new
			@loop.attach(@queue)
		end
		@queue.push(task)
	end

	def run
		@loop.run
	end

	def stop
		@queue.detach if @queue && @queue.attached?
		@loop.stop
		# attach dummy timer
		@loop.attach Rev::TimerWatcher.new(0, false)
		nil
	end
end


end
end
