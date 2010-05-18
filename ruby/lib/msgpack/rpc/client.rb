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


class Client < Session
	def initialize(arg1, arg2, arg3=nil)
		if arg1.respond_to?(:build_transport)
			# initialize(builder, address, loop = Loop.new)
			builder = arg1
			address = arg2
			loop    = arg3 || Loop.new
		else
			# initialize(host, port, loop = Loop.new)
			builder = TCPTransport.new
			address = Address.new(arg1, arg2)
			loop    = arg3 || Loop.new
		end

		super(builder, address, loop)

		@timer = Timer.new(1, true, &method(:step_timeout))
		loop.attach(@timer)
	end

	def self.open(*args, &block)
		c = new(*args)
		begin
			block.call(c)
		ensure
			c.close
		end
	end

	def close
		@timer.detach if @timer.attached?
		super
	end

	include LoopUtil
end


class Client::Base
	def initialize(*args)
		@base = Client.new(*args)
	end
	attr_reader :base
end


end
end
