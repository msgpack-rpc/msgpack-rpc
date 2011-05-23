#
# MessagePack-RPC for Ruby UDP transport
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


class UDPTransport
	def initialize
	end

	# Transport interface
	def build_transport(session, address)
		UDPClientTransport.new(session, address)
	end

	class BasicSocket < Rev::IOWatcher
		HAVE_DNRL = UDPSocket.public_instance_methods.include?(:do_not_reverse_lookup)

		def initialize(io)
			io.do_not_reverse_lookup = true if HAVE_DNRL
			super(io)
			@io = io
		end

		attr_reader :io

		def on_readable
			begin
				data, addr = @io.recvfrom(64*1024) # FIXME buffer size
			rescue Errno::EAGAIN
				return
			end

			# FIXME multiple objects in one message
			obj = MessagePack.unpack(data)
			on_message(obj, addr)
		rescue
			# FIXME log
			return
		end

		include MessageReceiver
	end
end


class UDPClientTransport
	def initialize(session, address)
		io = UDPSocket.new
		io.connect(*address)

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

	private
	class ClientSocket < UDPTransport::BasicSocket
		def initialize(io, session)
			super(io)
			@s = session
		end

		# MessageSendable interface
		def send_data(data)
			@io.send(data, 0)
		end

		# MessageReceiver interface
		def on_request(msgid, method, param, addr)
			raise Error.new("request message on client session")
		end

		# MessageReceiver interface
		def on_notify(method, param, addr)
			raise Error.new("notify message on client session")
		end

		# MessageReceiver interface
		def on_response(msgid, error, result, addr)
			@s.on_response(self, msgid, error, result)
		end
	end
end


class UDPServerTransport
	def initialize(address)
		@address = address
		@sock = nil
	end

	# ServerTransport interface
	def listen(server)
		@server = server
		host, port = *@address.unpack
		io = UDPSocket.new
		io.bind(*@address)

		begin
			@sock = ServerSocket.new(io, @server)
		rescue
			io.close
			raise
		end

		begin
			@server.loop.attach(@sock)
		rescue
			@sock.close
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
	class ServerSocket < UDPTransport::BasicSocket
		def initialize(io, server)
			super(io)
			@server = server
		end

		# MessageReceiver interface
		def on_request(msgid, method, param, addr)
			sender = ResponseSender.new(@io, addr[3], addr[1])
			@server.on_request(sender, msgid, method, param)
		end

		# MessageReceiver interface
		def on_notify(method, param, addr)
			@server.on_notify(method, param)
		end

		# MessageReceiver interface
		def on_response(msgid, error, result, addr)
			raise Error.new("response message on server session")
		end
	end

	class ResponseSender
		def initialize(io, host, port)
			@io = io
			@host = host
			@port = port
		end

		# MessageSendable interface
		def send_data(data)
			@io.send(data, 0, @host, @port)
		end
	end
end


end
end
