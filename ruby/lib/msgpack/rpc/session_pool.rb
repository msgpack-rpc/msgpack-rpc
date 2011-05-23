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


# SessionPool is usable for connection pooling.
# You can get pooled Session using get_session method.
# Note that SessionPool includes LoopUtil.
class SessionPool
	# 1. initialize(builder, loop = Loop.new)
	# 2. initialize(loop = Loop.new)
	#
	# Creates an SessionPool.
	def initialize(arg1=nil, arg2=nil)
		if arg1.respond_to?(:build_transport)
			# 1.
			builder = arg1
			loop    = arg2 || Loop.new
		else
			# 2.
			builder = TCPTransport.new
			loop    = arg1 || Loop.new
		end

		@builder = builder
		@loop = loop
		@pool = {}

		@timer = Timer.new(1, true, &method(:step_timeout))
		loop.attach(@timer)
	end

	# 1. get_session(address)
	# 2. get_session(host, port)
	#
	# Returns pooled Session.
	# If there are no pooled Session for the specified address,
	# this method creates the Session and pools it.
	def get_session(arg1, arg2=nil)
		if arg2.nil?
			# 1.
			addr = arg1
		else
			# 2.
			host = arg1
			port = arg2
			addr = Address.new(host, port)
		end

		@pool[addr] ||= Session.new(@builder, addr, @loop)
	end

	# backward compatibility
	alias_method :get_session_addr,:get_session   #:nodoc:

	def close
		@pool.reject! {|addr, s|
			s.close
			true
		}
		@timer.detach if @timer.attached?
		nil
	end

	include LoopUtil

	private
	def step_timeout
		@pool.each_pair {|addr,s| s.step_timeout }
	end
end


end
end
