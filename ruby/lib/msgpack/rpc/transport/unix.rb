#
# MessagePack-RPC for Ruby UNIX transport
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


class UNIXTransport
	def initialize
	end

	# Transport interface
	def build_transport(session, address)
		UNIXClientTransport.new(session, address)
	end

	class BasicSocket < Rev::UNIXSocket
		def initialize(io)
			super(io)
			@pac = MessagePack::Unpacker.new
		end

		# from Rev::TCPSocket
		def on_readable
			super
		rescue
			close
		end

		# from Rev::UNIXSocket
		def on_read(data)
			@pac.feed(data)
			@pac.each {|obj|
				on_message(obj)
			}
		end

		include MessageReceiver
	end
end


class UNIXClientTransport
	def initialize(session, address)
		io = UNIXSocket.new(address)

		begin
			@sock = ClientSocket.new(io, session)
		rescue
			io.close
			raise
		end

		begin
			session.loop.attach(@sock)
		rescue
			@sock.close
			raise
		end
	end

	# ClientTransport interface
	def send_data(data)
		@sock.send_data(data)
	end

	# ClientTransport interface
	def close
		@sock.detach if @sock.attached?
		@sock.close
	end

	class ClientSocket < UNIXTransport::BasicSocket
		def initialize(io, session)
			super(io)
			@s = session
		end

		# MessageSendable interface
		def send_data(data)
			write data
		end

		# MessageReceiver interface
		def on_request(msgid, method, param)
			raise RPCError.new("request message on client session")
		end

		# MessageReceiver interface
		def on_notify(method, param)
			raise RPCError.new("notify message on client session")
		end

		# MessageReceiver interface
		def on_response(msgid, error, result)
			@s.on_response(self, msgid, error, result)
		end
	end
end


class UNIXServerTransport
	def initialize(address)
		@address = address
		@sock = nil
	end

	# ServerTransport interface
	def listen(server)
		@server = server
		@lsock  = Rev::UNIXServer.new(@address, ServerSocket, @server)
		begin
			@server.loop.attach(@lsock)
		rescue
			@lsock.close
			raise
		end
	end

	# ServerTransport interface
	def close
		return unless @lsock
		@lsock.detach if @lsock.attached?
		@lsock.close
	end

	private
	class ServerSocket < UNIXTransport::BasicSocket
		def initialize(io, server)
			super(io)
			@server = server
		end

		# MessageSendable interface
		def send_data(data)
			write data
		end

		# MessageReceiver interface
		def on_request(msgid, method, param)
			@server.on_request(self, msgid, method, param)
		end

		# MessageReceiver interface
		def on_notify(method, param)
			@server.on_notify(method, param)
		end

		# MessageReceiver interface
		def on_response(msgid, error, result)
			raise RPCError.new("response message on server session")
		end
	end
end


end
end
