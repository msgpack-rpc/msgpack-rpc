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

# Marshal mode contributed by moe@morepanda.net

module MessagePack
module RPC
module Marshal

#
# == Marshal Mode
#
# In Marshal mode any object that can be marshalled may be used
# as a method parameter or return value. The objects are transparently
# marshalled on the sender side and unmarshalled on the receiver side.
#
# This frees you from having to manually serialize/deserialize your
# objects when both of your endpoints are running in this mode.
#
# === Usage
#
# Usage is identical to regular msgpack-rpc.
# 
# Just use MessagePack::RPC::Marshal::Client, MessagePack::RPC::Marshal::SessionPool
# and MessagePack::RPC::Marshal::Server instead of the normal RPC::Client/Server/SessionPool.
#

class Client < RPC::Client
  def send_request(method, param)
    super method, ::Marshal.dump(param)
  end

  def on_response(sock, msgid, error, result)  #:nodoc:
    result = ::Marshal.load(result) unless result.nil?
    result = super(sock, msgid, error, result)
  end
end

class SessionPool < RPC::SessionPool
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

    # Create Marshal::Client instead of plain Session
		@pool[addr] ||= Marshal::Client.new(@builder, addr, @loop)
	end
end

class MarshalledResponder < Responder
  def initialize(proxy_me)
    @proxied = proxy_me
  end

	def result(retval, err = nil)
    @proxied.result ::Marshal.dump(retval), err
  end
end

class Server < RPC::Server
	# from ServerTransport
	def on_request(sendable, msgid, method, param)  #:nodoc:
		responder = MarshalledResponder.new( Responder.new(sendable, msgid) )
		dispatch_method(method, ::Marshal.load(param), responder)
	end
end

end
end
end


