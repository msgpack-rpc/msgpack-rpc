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


##
## MessagePack-RPC Exception
##
#
#  RPCError
#  |
#  +-- TimeoutError
#  |
#  +-- TransportError
#  |   |
#  |   +-- NetworkUnreachableError
#  |   |
#  |   +-- ConnectionRefusedError
#  |   |
#  |   +-- ConnectionTimeoutError
#  |   |
#  |   +-- MalformedMessageError
#  |   |
#  |   +-- StreamClosedError
#  |
#  +-- CallError
#  |   |
#  |   +-- NoMethodError
#  |   |
#  |   +-- ArgumentError
#  |
#  +-- ServerError
#  |   |
#  |   +-- ServerBusyError
#  |
#  +-- RemoteError
#      |
#      +-- RuntimeError
#      |
#      +-- (user-defined errors)

class RPCError < Error
	def initialize(code, *data)
		@code = code.to_s
		@data = data
		super(@data.shift || @code)
	end
	attr_reader :code
	attr_reader :data

	def is?(code)
		if code.is_a?(Class) && code < RPCError
			if code == RemoteError
				return @code[0] != ?.
			end
			code = code::CODE
		end
		@code == code || @code[0,code.length+1] == "#{code}."
	end
end


##
# Top Level Errors
#
class TimeoutError < RPCError
	CODE = ".TimeoutError"
	def initialize(msg)
		super(self.class::CODE, msg)
	end
end

class TransportError < RPCError
	CODE = ".TransportError"
	def initialize(msg)
		super(self.class::CODE, msg)
	end
end

class CallError < RPCError
	CODE = ".CallError"
	def initialize(msg)
		super(self.class::CODE, msg)
	end
end

class ServerError < RPCError
	CODE = ".ServerError"
	def initialize(msg)
		super(self.class::CODE, msg)
	end
end

class RemoteError < RPCError
	CODE = ""
	def initialize(code, *data)
		super(code, *data)
	end
end


##
# TransportError
#
class NetworkUnreachableError < TransportError
	CODE = ".TransportError.NetworkUnreachableError"
end

class ConnectionRefusedError < TransportError
	CODE = ".TransportError.ConnectionRefusedError"
end

class ConnectionTimeoutError < TransportError
	CODE = ".TransportError.ConnectionTimeoutError"
end

class MalformedMessageError < TransportError
	CODE = ".TransportError.ConnectionRefusedError"
end

class StreamClosedError < TransportError
	CODE = ".TransportError.StreamClosedError"
end

##
# CallError
#
class NoMethodError < CallError
	CODE = ".CallError.NoMethodError"
end

class ArgumentError < CallError
	CODE = ".CallError.ArgumentError"
end

##
# ServerError
#
class ServerBusyError < ServerError
	CODE = ".ServerError.ServerBusyError"
end

##
# RuntimeError
#
class RuntimeError < RemoteError
	CODE = ".RuntimeError"
	def initialize(msg, *data)
		super("RuntimeError", msg, *data)
	end
end


class RPCError
	def self.create(code, data)
		if code[0] == ?.
			code = code.dup
			while true
				if klass = SYSTEM_ERROR_TABLE[code]
					return klass.new(*data)
				end
				if code.slice!(/\.[^\.]*$/) == nil || code.empty?
					return RPCError.new(code, *data)
				end
			end

		elsif code == RuntimeError::CODE
			RuntimeError.new(*data)

		else
			RemoteError.new(code, *data)
		end
	end

	private
	SYSTEM_ERROR_TABLE = {
		TimeoutError::CODE            => TimeoutError,
		TransportError::CODE          => TransportError,
		CallError::CODE               => CallError,
		ServerError::CODE             => ServerError,
		NetworkUnreachableError::CODE => NetworkUnreachableError,
		ConnectionRefusedError::CODE  => ConnectionRefusedError,
		ConnectionTimeoutError::CODE  => ConnectionTimeoutError,
		MalformedMessageError::CODE   => MalformedMessageError,
		StreamClosedError::CODE       => StreamClosedError,
		NoMethodError::CODE           => NoMethodError,
		ArgumentError::CODE           => ArgumentError,
		ServerBusyError::CODE         => ServerBusyError,
	}
end


end
end
