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


class Error < StandardError
end

class RPCError < Error
	def initialize(msg)
		super(msg)
	end
end

class RemoteError < RPCError
	def initialize(msg, result = nil)
		super(msg)
		@result = result
	end
	attr_reader :result
end

class TimeoutError < Error
	def initialize(msg = "request timed out")
		super
	end
end

class ConnectError < TimeoutError
	def initialize(msg = "connect failed")
		super
	end
end


=begin
#TODO

class Error < StandardError
end

class RPCError < Error
	def initialize(msg, code)
		super(msg)
		@code = code
	end
	attr_reader :code
end


class TimeoutError < RPCError
	CODE = -60
	def initialize(msg = "request timed out")
		super(msg, CODE)
	end
end


class ClientError < RPCError
	def initialize(msg, code)
		super(msg, code)
	end
end


class TransportError < RPCError
	CODE = -50
	def initialize(msg, code = CODE)
		super(msg, CODE)
	end
end

class NetworkUnreachableError < TransportError
	CODE = -51
	def initialize(msg = "network unreachable")
		super(msg, CODE)
	end
end

class ConnectionRefusedError < TransportError
	CODE = -52
	def initialize(msg = "connection refused")
		super(msg, CODE)
	end
end

class ConnectionTimeoutError < TransportError
	CODE = -53
	def initialize(msg = "connection timed out")
		super(msg, CODE)
	end
end


class MessageRefusedError < ClientError
	CODE = -40
	def initialize(msg = "broken message", code = CODE)
		super(msg, code)
	end
end

class MessageTooLargeError < MessageRefusedError
	CODE = -41
	def initialize(msg = "messge too large")
		super(msg, CODE)
	end
end


class CallError < ClientError
	CODE = -20
	def initialize(msg, code = CODE)
		super(msg, code)
	end
end

class NoMethodError < CallError
	CODE = -21
	def initialize(msg = "method not found")
		super(msg, CODE)
	end
end

class ArgumentError < NoMethodError
	CODE = -22
	def initialize(msg = "invalid argument")
		super(msg, CODE)
	end
end


class ServerError < RPCError
	CODE = -30
	def initialize(msg, code = CODE)
		super(msg, code)
	end
end

class ServerBusyError < ServerError
	CODE = -31
	def initialize(msg = "server temporally busy")
		super(msg, CODE)
	end
end


class RemoteError < RPCError
	def initialize(msg, code, result = nil)
		super(msg, code)
		@result = result
	end
	attr_reader :result
end

class RemoteRuntimeError < RemoteRuntimeError
	CODE = -10
	def initialize(msg, result = nil)
		super(msg, CODE, result)
	end
end
=end


end
end
