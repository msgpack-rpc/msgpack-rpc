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


# Client is usable for RPC client.
# Note that Client includes LoopUtil.
class Client < Session
	# 1. initialize(builder, address, loop = Loop.new)
	# 2. initialize(host, port, loop = Loop.new)
	#
	# Creates a client.
	def initialize(arg1, arg2, arg3=nil)
		if arg1.respond_to?(:build_transport)
			# 1.
			builder = arg1
			address = arg2
			loop    = arg3 || Loop.new
		else
			# 2.
			builder = TCPTransport.new
			address = Address.new(arg1, arg2)
			loop    = arg3 || Loop.new
		end

		super(builder, address, loop)

		@timer = Timer.new(1, true, &method(:step_timeout))
		loop.attach(@timer)
	end

	# call-seq:
	#   Client.open(arg1, arg2, arg3=nil) {|client|  }
	#
	# 1. open(builder, address, loop = Loop.new) {|client }
	# 2. open(host, port, loop = Loop.new) {|client }
	# Creates a client, calls the block and closes the client.
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


#:nodoc:
class Client::Base
	def initialize(*args)
		@base = Client.new(*args)
	end
	attr_reader :base
end


end
end
