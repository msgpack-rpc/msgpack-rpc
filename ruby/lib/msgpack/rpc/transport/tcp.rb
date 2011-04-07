#
# MessagePack-RPC for Ruby TCP transport
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


class TCPTransport
	def initialize
		@reconnect_limit = 5   # FIXME default reconnect_limit
	end

	attr_accessor :reconnect_limit

	# Transport interface
	def build_transport(session, address)
		TCPClientTransport.new(session, address, @reconnect_limit)
	end

	class BasicSocket < Rev::TCPSocket
		def initialize(io)
			super(io)
			@pac = MessagePack::Unpacker.new
		end

		# from Rev::TCPSocket
		def on_readable
			super
		rescue
			# FIXME send Connection Close message
			# FIXME log
			close
		end

		# from Rev::TCPSocket
		def on_read(data)
			@pac.feed_each(data) {|obj|
				on_message(obj)
			}
		end

		include MessageReceiver
	end
end


class TCPClientTransport
	def initialize(session, address, reconnect_limit)
		@session = session
		@address = address

		@pending = ""
		@sockpool = []
		@connecting = 0
		@reconnect_limit = reconnect_limit
	end

	# ClientTransport interface
	def send_data(data)
		if @sockpool.empty?
			if @connecting == 0
				try_connect
				@connecting = 1
			end
			@pending << data
		else
			# FIXME pesudo connection load balance
			# sock = @sockpool.choice
			sock = @sockpool.first
			sock.send_data(data)
		end
	end

	# ClientTransport interface
	def close
		@sockpool.reject! {|sock|
			sock.detach if sock.attached?
			sock.close
			true
		}
		@sockpool = []
		@connecting = 0
		@pending = ""
		self
	end

	# from TCPClientTransport::ClientSocket::on_connect
	def on_connect(sock)
		@sockpool.push(sock)
		sock.send_pending(@pending)
		@pending = ""
		@connecting = 0
	end

	# from TCPClientTransport::ClientSocket::on_connect_failed
	def on_connect_failed(sock)
		if @connecting < @reconnect_limit
			try_connect
			@connecting += 1
		else
			@connecting = 0
			@pending = ""
			@session.on_connect_failed
		end
	end

	# from TCPClientTransport::ClientSocket::on_close
	def on_close(sock)
		@sockpool.delete(sock)
	end

	private
	def try_connect
		host, port = *@address
		sock = ClientSocket.connect(host, port, self, @session)  # async connect
		@session.loop.attach(sock)
	end

	class ClientSocket < TCPTransport::BasicSocket
		def initialize(io, transport, session)
			super(io)
			@t = transport
			@s = session
		end

		# MessageSendable interface
		def send_data(data)
			write data
		end

		# from TCPClientTransport::on_connect
		def send_pending(data)
			write data
		end

		# MessageReceiver interface
		def on_request(msgid, method, param)
			raise Error.new("request message on client session")
		end

		# MessageReceiver interface
		def on_notify(method, param)
			raise Error.new("notify message on client session")
		end

		# MessageReceiver interface
		def on_response(msgid, error, result)
			@s.on_response(self, msgid, error, result)
		end

		# from Rev::TCPSocket
		def on_connect
			return unless @t
			@t.on_connect(self)
		end

		# from Rev::TCPSocket
		def on_connect_failed
			return unless @t
			@t.on_connect_failed(self)
		rescue
			nil
		end

		# from Rev::TCPSocket
		def on_close
			return unless @t
			@t.on_close(self)
			@t = nil
			@s = nil
		rescue
			nil
		end
	end
end


class TCPServerTransport
	def initialize(address)
		@address = address
		@lsock = nil
	end

	# ServerTransport interface
	def listen(server)
		@server = server
		host, port = *@address.unpack
		@lsock  = Rev::TCPServer.new(host, port, ServerSocket, @server)
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
	class ServerSocket < TCPTransport::BasicSocket
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
			raise Error.new("response message on server session")
		end
	end
end


end
end
