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
require 'msgpack'
require 'rev'
require 'zlib'

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


REQUEST  = 0    # [0, msgid, method, param]
RESPONSE = 1    # [1, msgid, error, result]
NOTIFY   = 2    # [2, method, param]
SESSION  = 3    # [3, addr]
OPTION   = 4    # [4, txopt, rxopt] #[, rxproto, rxaddr]
OPT_DEFLATE = 0b00000001
#PROTO_TCP   = 0
#PROTO_UDP   = 1


class Address
	# +--+----+
	# | 2|  4 |
	# +--+----+
	# port network byte order
	#    IPv4 address
	#
	# +--+----------------+
	# | 2|       16       |
	# +--+----------------+
	# port network byte order
	#    IPv6 address
	#

	test = Socket.pack_sockaddr_in(0,'0.0.0.0')
	if test[0] == "\0"[0] || test[1] == "\0"[0]
		# Linux
		def initialize(host, port)
			raw = Socket.pack_sockaddr_in(port, host)
			family = raw.unpack('S')[0]
			if family == Socket::AF_INET
				@serial = raw[2,6]
			elsif family == Socket::AF_INET6
				@serial = raw[2,2] + raw[8,16]
			else
				raise "Unknown address family: #{family}"
			end
		end
	else
		# BSD
		def initialize(host, port)
			raw = Socket.pack_sockaddr_in(port, host)
			family = raw.unpack('CC')[1]
			if family == Socket::AF_INET
				@serial = raw[2,6]
			elsif family == Socket::AF_INET6
				@serial = raw[2,2] + raw[8,16]
			else
				raise "Unknown address family: #{family}"
			end
		end
	end

	def host
		unpack[0]
	end

	def port
		unpack[1]
	end

	def connectable?
		port != 0
	end

	def sockaddr
		Address.parse_sockaddr(@serial)
	end

	def unpack
		Address.parse(@serial)
	end

	def self.parse_sockaddr(raw)
		if raw.length == 6
			addr = Socket.pack_sockaddr_in(0, '0.0.0.0')
			addr[2,6] = raw[0,6]
		else
			addr = Socket.pack_sockaddr_in(0, '::')
			addr[2,2]  = raw[0,2]
			addr[8,16] = raw[2,16]
		end
		addr
	end

	def self.parse(raw)
		Socket.unpack_sockaddr_in(parse_sockaddr(raw)).reverse
	end

	def self.load(raw)
		Address.new *parse(raw)
	end

	def dump
		@serial
	end

	def to_msgpack(out = '')
		@serial.to_msgpack(out)
	end

	def to_s
		unpack.join(':')
	end

	def to_a
		unpack
	end

	def inspect
		"#<#{self.class} #{to_s} @serial=#{@serial.inspect}>"
	end

	def eql?(o)
		o.class == Address && dump.eql?(o.dump)
	end

	def hash
		dump.hash
	end

	def ==(o)
		eql?(o)
	end
end


class Future
	def initialize(s, loop, block = nil)
		@s = s
		@timeout = s.timeout
		@loop = loop
		@block = block
		@error  = nil
		@result = nil
	end
	attr_reader :loop
	attr_accessor :result, :error

	def attach_callback(proc = nil, &block)
		@block = proc || block
	end

	def call(err, res)
		@error  = err
		@result = res
		if @block
			@block.call(err, res)
		end
		@s = nil
	end

	def join
		while @s
			@loop.run_once
		end
		self
	end

	def step_timeout
		if @timeout < 1
			true
		else
			@timeout -= 1
			false
		end
	end
end


class Responder
	def initialize(tran, msgid)
		@tran = tran  # send_message method is required
		@msgid = msgid
	end

	def result(retval, err = nil)
		@tran.send_message [RESPONSE, @msgid, err, retval]
	end

	def error(err, retval = nil)
		result(retval, err)
	end
end


class ExchangeOption
	def initialize(flags = 0)
		@flags = flags
	end

	def get
		@flags
	end

	def deflate=(val)
		val ? (@flags |= OPT_DEFLATE) : (@flags &= ~OPT_DEFLATE)
	end

	def deflate?
		@flags & OPT_DEFLATE != 0
	end

	def reset
		@flags = 0
		self
	end

	def ==(o)
		@flags == o.get
	end

	def to_msgpack(out = '')
		@flags.to_msgpack(out)
	end

	def self.from_msgpack(obj)
		@flags = obj
	end
end

class StreamOption
	def initialize(txopt = ExchangeOption.new, rxopt = ExchangeOption.new) #, rxproto = nil, rxaddr = nil)
		@txopt = txopt
		@rxopt = rxopt
		#@rxproto = rxproto  # nil or PROTO_TCP or PROTO_UDP
		#@rxaddr  = rxaddr   # nil or address
	end
	attr_accessor :txopt, :rxopt  #, :rxproto, :rxaddr

	def default?
		@txopt.get == 0 && @rxopt.get == 0 && @rxproto.nil?
	end

	def reset
		@txopt.reset
		@rxopt.reset
		#@rxproto = nil
		#@rxaddr = nil
		self
	end

	def ==(o)
		@txopt == o.txopt && @rxopt == o.rxopt  # &&
			#@rxproto == o.rxproto && @rxaddr == o.rxaddr
	end

	def to_msgpack(out = '')
		array = [OPTION, @txopt, @rxopt]
		#if @rxproto
		#	array << @rxproto
		#	if @rxaddr
		#		array << @rxaddr
		#	end
		#end
		array.to_msgpack(out)
	end

	def self.from_msgpack(obj)
		txopt = ExchangeOption.new(obj[1] || 0)
		rxopt = ExchangeOption.new(obj[2] || 0)
		#if obj[3]
		#	rxproto = obj[3]
		#	if obj[4]
		#		rxaddr = Address.load(obj[4])
		#	end
		#end
		StreamOption.new(txopt, rxopt)  #, rxproto, rxaddr)
	end
end

class TransportOption < StreamOption
	def initialize(*args)
		super()
		#@proto = PROTO_TCP
		#@address = nil
		args.each do |x|
			case x
			#when :tx_tcp, :tcp
			#	@proto = PROTO_TCP
			#when :tx_udp, :udp
			#	@proto = PROTO_UDP
			#when :rx_tcp
			#	@rxproto = PROTO_TCP
			#when :rx_udp
			#	@rxproto = PROTO_UDP
			when :deflate
				@txopt.deflate = true
				@rxopt.deflate = true
			when :tx_deflate
				@txopt.deflate = true
			when :rx_deflate
				@rxopt.deflate = true
			#when PROTO_TCP
			#	@proto = PROTO_TCP
			#when PROTO_UDP
			#	@proto = PROTO_UDP
			#when Address
			#	@rxaddr = x
			else
				raise "unknown option #{x.inspect}"
			end
		end
	end
	#attr_accessor :proto, :address

	def reset
		#@proto = PROTO_TCP
		@address = nil
		super
	end

	def ==(o)
		super(o)  # && @proto = o.proto && @address = o.address
	end

	#def deflate(val = true) txopt.deflate = va; rxopt.deflate = va; self end
	#def rx_deflate(val = true) @rxopt.deflate = val; self; end
	#def tx_deflate(val = true) @txopt.deflate = val; self; end

	#def tcp; @proto = PROTO_TCP; self; end
	#def tx_tcp; @proto = PROTO_TCP; self; end
	#def rx_tcp; @rxproto = PROTO_TCP; self; end

	#def udp; @proto = PROTO_UDP; self; end
	#def rx_udp; @proto = PROTO_UDP; self; end
	#def rx_udp; @rxproto = PROTO_UDP; self; end
end


class Session
	def initialize(to_addr, dtopt, dispatcher, self_addr, loop)
		@address = to_addr  # destination address
		@self_address = self_addr  # self session identifier
		@dispatcher = dispatcher || NullDispatcher.new
		@dtopt = dtopt   # default transport option
		@dtran = create_transport(@dtopt)  # default transport
		@loop = loop
		@trans = []
		@reqtable = {}
		@timeout = 10    # FIXME default timeout time
		reset
	end
	attr_reader :address, :self_address, :loop
	attr_accessor :timeout

	def send(method, *args)
		send_over(nil, method, *args)
	end

	def callback(method, *args, &block)
		callback_over(nil, method, *args, &block)
	end

	def call(method, *args)
		call_over(nil, method, *args)
	end

	def notify(method, *args)
		notify_over(nil, method, *args)
	end

	def over(*args)
		Over.new(self, TransportOption.new(*args))
	end

	def default_option
		@dtopt.dup
	end

	def send_over(topt, method, *args)
		msgid = send_request(topt, method, args)
		@reqtable[msgid] = Future.new(self, @loop)
	end

	def callback_over(topt, method, *args, &block)
		msgid = send_request(topt, method, args)
		@reqtable[msgid] = Future.new(self, @loop, block)
	end

	def call_over(topt, method, *args)
		# FIXME if @reqtable.empty? optimize
		req = send_over(topt, method, *args)
		req.join
		if req.error
			raise req.error if req.error.is_a?(Error)
			raise RemoteError.new(req.error, req.result)
		end
		req.result
	end

	def notify_over(topt, method, *args)
		send_notify(topt, method, args)
	end

	def send_message_over(topt, msg)  # for Over#send_message
		get_transport(topt).send_message(msg)
	end

	def close
		@dtran.close
		@trans.each {|tran| tran.close }
		reset
		self
	end

	def step_timeout
		reqs = []
		@reqtable.reject! {|msgid, req|
			if req.step_timeout
				reqs.push(req)
			end
		}
		reqs.each {|req|
			begin
				req.call TimeoutError.new, nil
			rescue
			end
		}
		!@reqtable.empty?
	end

	def on_message(sock, msg)   # Session interface
		case msg[0]
		when REQUEST
			on_request(sock, msg[1], msg[2], msg[3])
		when RESPONSE
			on_response(sock, msg[1], msg[2], msg[3])
		when NOTIFY
			on_notify(sock, msg[1], msg[2])
		when SESSION
			# ignore because session is already bound
		else
			raise RPCError.new("unknown message type #{msg[0]}")
		end
	end

	def on_connect_failed
		@reqtable.reject! {|msgid, req|
			begin
				req.call ConnectError.new, nil
			rescue
			end
			true
		}
		# FIXME reset?
	end

	private
	def create_transport(topt)
		TCPTransport.new(self, topt)
	end

	def get_transport(topt)
		if topt.nil? || @dtran.match?(topt)
			return @dtran
		end
		if tran = @trans.find {|f| f.match?(topt) }
			return tran
		end
		tran = create_transport(topt)
		@trans.push(tran)
		tran
	end

	class Over
		def initialize(session, topt)
			@session = session
			@topt = topt
		end

		def send(method, *args)
			@session.send_over(@topt, method, *args)
		end

		def callback(method, *args, &block)
			@session.callback_over(@topt, method, *args, &block)
		end

		def call(method, *args)
			@session.call_over(@topt, method, *args)
		end

		def notify(method, *args)
			@session.notify_over(@topt, method, *args)
		end

		def send_message(msg)  # Transport interface for Responder
			@session.send_message_over(@topt, msg)
		end
	end

	def send_notify(topt, method, param)
		unless @address
			raise RPCError.new("unexpected send request on server session")
		end
		method = method.to_s unless method.is_a?(Integer)
		tran = get_transport(topt)
		tran.send_message([NOTIFY, method, param])
	end

	def send_request(topt, method, param)
		unless @address
			raise RPCError.new("unexpected send request on server session")
		end
		method = method.to_s unless method.is_a?(Integer)
		tran = get_transport(topt)
		msgid = @seqid
		@seqid += 1; if @seqid >= 1<<31 then @seqid = 0 end
		@reqtable[msgid] = Future.new(self, @loop)
		tran.send_message([REQUEST, msgid, method, param])
		msgid
	end

	def on_response(sock, msgid, error, result)
		if req = @reqtable.delete(msgid)
			req.call(error, result)
		end
	end

	def on_request(sock, msgid, method, param)
		#if sock.option.rxproto
		#	#if @address
		#	tran = Over.new(self, sock.option)
		#else
			tran = sock
		#end
		res = Responder.new(tran, msgid)
		@dispatcher.dispatch_request(self, method, param, res)
	end

	def on_notify(sock, method, param)
		@dispatcher.dispatch_notify(self, method, param)
	end

	def on_close(sock)
		@sockpool.delete(sock)
	end

	def reset
		@reqtable = {}
		@seqid = 0
	end
end


class BasicTransport
	def initialize(session, topt)
		@session = session
		@option = topt
	end

	def match?(topt)
		@option == topt
	end

	#def close; end

	protected
	def get_address
		#@option.address || @session.address
		@session.address
	end
end

class TCPTransport < BasicTransport
	def initialize(session, topt)
		super(session, topt)

		@pending = ""
		@sockpool = []
		@connecting = 0
		@reconnect = 5   # FIXME default reconnect limit

		@initmsg = ""
		if session.self_address
			@initmsg << [SESSION, session.self_address].to_msgpack
		end
		unless topt.default?
			@initmsg << topt.to_msgpack
		end
		@initmsg = nil if @initmsg.empty?

		if topt.txopt.deflate?
			@deflate = Zlib::Deflate.new
		else
			@deflate = nil
		end
	end

	def send_message(msg)   # Transport interface
		if @sockpool.empty?
			if @connecting == 0
				try_connect
				@connecting = 1
			end
			if @deflate
				@pending << @deflate.deflate(msg.to_msgpack, Zlib::SYNC_FLUSH)
			else
				@pending << msg.to_msgpack
			end
		else
			# FIXME pesudo connection load balance
			sock = @sockpool.first
			sock.send_message(msg)
		end
	end

	def close   # Transport interface
		@sockpool.reject! {|sock|
			sock.detach if sock.attached?
			sock.close
			true
		}
		@sockpool = []
		@connecting = 0
		@pending = ""
		@deflate.reset if @deflate
		self
	end

	def on_connect(sock)
		@sockpool.push(sock)
		sock.send_pending(@pending, @deflate)
		@pending = ""
		@deflate = Zlib::Deflate.new if @deflate
		@connecting = 0
	end

	def on_connect_failed(sock)
		if @connecting < @reconnect
			try_connect
			@connecting += 1
		else
			@connecting = 0
			@pending = ""
			@session.on_connect_failed
		end
	end

	def on_close(sock)
		@sockpool.delete(sock)
	end

	private
	def try_connect
		addr = get_address
		if addr.nil?
			return  # FIXME raise?
		end
		host, port = *addr
		sock = ActiveSocket.connect(host, port, self, @option, @session)  # async connect
		if @initmsg
			sock.write @initmsg
		end
		@session.loop.attach(sock)
	end
end

class TCPTransport::Socket < Rev::TCPSocket
	def initialize(sock, session)
		@buffer = ''
		@nread = 0
		@mpac = MessagePack::Unpacker.new
		@deflate = nil
		@inflate = nil
		@s = session
		super(sock)
	end

	def session
		@s
	end

	def on_read(data)
		if @inflate
			data = @inflate.inflate(data)
			return if data.empty?
		end
		@buffer << data

		while true
			@nread = @mpac.execute(@buffer, @nread)
			if @mpac.finished?
				msg = @mpac.data
				@mpac.reset
				@buffer.slice!(0, @nread)
				@nread = 0
				on_message(msg)
				next unless @buffer.empty?
			end
			break
		end
	end

	def on_message(msg)
		return unless @s
		@s.on_message(self, msg)
	end

	def on_close
		@deflate.close if @deflate
		@inflate.close if @inflate
	end

	def send_message(msg)  # Transport interface
		if @deflate
			data = @deflate.deflate(msg.to_msgpack, Zlib::SYNC_FLUSH)
		else
			data = msg.to_msgpack
		end
		write data
	end
end

class TCPTransport::ActiveSocket < TCPTransport::Socket
	def initialize(sock, tran, topt, session)
		super(sock, session)
		@tran = tran
		set_option(topt)
	end

	def on_connect
		return unless @tran
		@tran.on_connect(self)
	end

	def on_connect_failed
		return unless @tran
		@tran.on_connect_failed(self)
	end

	def on_close
		return unless @tran
		@tran.on_close(self)
		@tran = nil
		@s = nil
	rescue
		nil
	ensure
		super
	end

	def send_pending(data, z)
		write data
		@deflate = z
	end

	private
	def set_option(sopt)  # stream option
		if sopt.txopt.deflate?
			@deflate = Zlib::Deflate.new
		end
		if sopt.rxopt.deflate?
			@inflate = Zlib::Inflate.new
			#@buffer = @inflate.inflate(@buffer) unless @buffer.empty?
		end
	end
end

class TCPTransport::PassiveSocket < TCPTransport::Socket
	def initialize(sock, create_session)
		super(sock, create_session.call)
		@option = TransportOption.new  # active option (reversed)
	end

	attr_reader :option  # for Session#on_request

	def rebind(session)
		@s = session
	end

	def on_message(msg)
		if msg[0] == OPTION
			sopt = StreamOption.from_msgpack(msg)
			set_option(sopt)
			return
		end
		super(msg)
	end

	private
	def set_option(sopt)
		if sopt.txopt.deflate?
			@inflate = Zlib::Inflate.new
			@buffer = @inflate.inflate(@buffer) unless @buffer.empty?
		end
		if sopt.rxopt.deflate?
			@deflate = Zlib::Deflate.new
		end
		# rx-tx reverse
		@option = TransportOption.new
		@option.txopt = sopt.rxopt
		@option.rxopt = sopt.txopt
		#@option.proto = sopt.rxproto || PROTO_TCP
		#@option.address = sopt.rxaddr
	end
end

class TCPTransport::Listener
	def initialize(host, port, &create_session)
		@lsock = Rev::TCPServer.new(host, port, TCPTransport::PassiveSocket, create_session)
	end

	def activate(loop)
		loop.attach(@lsock)
	end

	def close
		@lsock.detach if @lsock.attached?
		@lsock.close
	end
end


=begin
class UDPTransport < BasicTransport
	def initialize(session, topt)
		super(session, topt)

		initmsg = ""
		if session.self_address
			initmsg << [SESSION, session.self_address].to_msgpack
		end
		unless topt.default?
			initmsg << topt.to_msgpack
		end
		initmsg = nil if initmsg.empty?

		sock = UDPSocket.new
		@addr = get_address
		@asock = ActiveSocket.new(sock, initmsg, topt, session)
	end

	def send_message(msg)   # Transport interface
		@asock.send_message_to(msg, @addr)
	end

	def close
		@sock.close
		@deflate.close if @deflate
	end
end

class UDPTransport::Socket < Rev::IO
	def initialize(sock, create_session)
		@sock = sock
		@mpac = MessagePack::Unpacker.new
		@create_session = create_session
		super(sock)
	end

	def on_readable
		begin
			buffer, from = @sock.recvfrom(65536)
		rescue Errno::EAGAIN
			return
		rescue Errno::EINTR
			return
		end
		# addr = Address.from_sockaddr(from) FIXME
		on_recv(buffer, addr)
	end

	def on_message(rc, msg)
		return unless @s
		@s.on_message(rc, msg)
	end

	def send_message_with(initmsg, msg, deflate, addr)
		if deflate
			d2 = deflate.deflate(msg.to_msgpack, Zlib::SYNC_FLUSH)
		else
			d2 = msg.to_msgpack
		end
		if initmsg
			data = d2
		else
			data = initmsg.dup
			data << d2
		end
		@sock.sendto(data, 0, addr.sockaddr)
	end
end

class UDPTransport::ActiveSocket < UDPTransport::Socket
	def initialize(sock, initmsg, topt, session)
		@sock = sock
		super(sock)
		if topt.txopt.deflate?
			@deflate = Zlib::Deflate.new
		else
			@deflate = nil
		end
		@initmsg = initmsg
		@s = session
	end

	def on_recv(data, addr)
		rc = ReturnContext.new(self, addr)
		if @inflate
			data = @inflate.inflate(data)
			@inflate.finish
			return if data.empty?
		end
		nread = 0
		while true
			nread = @mpac.execute(data, nread)
			if @mpac.finished?
				msg = @mpac.data
				@mpac.reset
				data.slice!(0, nread)
				nread = 0
				on_message(rc, msg)
				next unless data.empty?
			else
				@mpac.reset
			end
			break
		end
	end

	def send_message_to(msg, addr)
		send_message_with(@initmsg, msg, @deflate, addr)
	end

	private
	class ReturnContext
		def initialize(asock, addr)
			@asock = asock
			@addr = addr
		end

		def send_message(msg)  # Transport interface
			@asock.send_message_to(msg, @addr)
		end
	end
end

class UDPTransport::PassiveSocket < UDPTransport::Socket
	def initialize(sock, create_session)
		super(sock)
		@create_session = create_session
		@inflate = LazyInflate.new
		@deflate = LazyInflate.new
	end

	def on_recv(data, addr)
		rc = PassiveReturnContext.new(self, addr)
		nread = 0
		while true
			nread = @mpac.execute(data, nread)
			if @mpac.finished?
				msg = @mpac.data
				@mpac.reset
				data.slice!(0, nread)
				nread = 0
				data = on_message(data, rc, msg)
				next unless data.empty?
			else
				@mpac.reset
			end
			break
		end
	end

	def on_message(data, rc, msg)
		if msg[0] == OPTION
			sopt = StreamOption.from_msgpack(msg)
			return set_option(data, rc, sopt)
		end
		super(rc, msg)
	end

	attr_reader :deflate   # for ReturnContext#send_message

	private
	class ReturnContext
		def initialize(psock, addr)
			@psock = psock
			@addr = addr
			@option = TransportOption.new
		end
		attr_accessor :option

		def send_message(msg)  # Transport interface
			if @option.txopt.deflate?
				deflate = @psock.deflate.get
			else
				deflate = nil
			end
			@psock.send_message_with(nil, msg, deflate, @addr)
		end
	end

	class LazyDeflate
		def initialize
			@deflate = nil
		end
		def get
			@deflate ||= Zlib::Deflate.new
		end
	end

	class LazyInflate
		def initialize
			@inflate = nil
		end
		def get
			@inflate ||= Zlib::Inflate.new
		end
	end

	def set_option(data, rc, sopt)
		if sopt.txopt.deflate?
			inflate = @inflate.get
			data = inflate.inflate(data)
			@inflate.finish
		end
		# rx-tx reverse
		rc.option.reset
		rc.option.txopt = sopt.rxopt
		rc.option.proto = sopt.rxproto || PROTO_UDP
		rc.option.address = sopt.rxaddr
		data
	end
end

class UDPTransport::Listener
	def initialize(host, port, &create_session)
		@sock = UDPSocket.new
		@sock.bind(host, port)
		@psock = PassiveSocket.new(@sock, &create_session)
	end

	def activate(loop)
		loop.attach(@psock)
	end

	def close
		@psock.detach if @psock.attached?
		@psock.close
	end
end
=end


class AsyncResult
	def initialize
		@responder = nil
		@sent = false
	end

	def result(retval, err = nil)
		unless @sent
			if @responder
				@responder.result(retval, err)
			else
				@result = [retval, err]
			end
			@sent = true
		end
		nil
	end

	def error(err)
		result(nil, err)
		nil
	end

	def responder=(res)
		@responder = res
		if @sent && @result
			@responder.result(*@result)
			@result = nil
		end
	end
end


class NullDispatcher
	def dispatch_request(session, method, param, res)
		raise RPCError.new("unexpected request message")
	end

	def dispatch_notify(session, method, param)
		raise RPCError.new("unexpected notify message")
	end
end

class ObjectDispatcher
	def initialize(obj, accept = obj.public_methods)
		@obj = obj
		@accept = accept.map {|m| m.is_a?(Integer) ? m : m.to_s}
	end

	def dispatch_request(session, method, param, res)
		begin
			result = forward_method(session, method, param)
		rescue
			res.error($!.to_s)
			return
		end
		if result.is_a?(AsyncResult)
			result.responder = res
		else
			res.result(result)
		end
	end

	def dispatch_notify(session, method, param)
		forward_method(session, method, param)
	rescue
	end

	private
	def forward_method(session, method, param)
		unless @accept.include?(method)
			raise NoMethodError, "method `#{method}' is not accepted"
		end
		@obj.send(method, *param) { session }
	end
end


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


class Client < Session
	def initialize(host, port, loop = Loop.new, dtopt = TransportOption.new)
		@loop = loop
		@host = host
		@port = port

		addr = Address.new(host, port)
		super(addr, dtopt, NullDispatcher.new, nil, loop)

		@timer = Timer.new(1, true) {
			step_timeout
		}
		loop.attach(@timer)
	end
	attr_reader :host, :port

	def self.open(host, port, loop = Loop.new, dtopt = TransportOption.new, &block)
		cli = new(host, port, loop, dtopt)
		begin
			block.call(cli)
		ensure
			cli.close
		end
	end

	def close
		@timer.detach if @timer.attached?
		super
	end

	include LoopUtil
end


class SessionPool
	def initialize(loop = Loop.new, dtopt = TransportOption.new)
		@loop = loop
		@spool = {}
		@stimer = Timer.new(1, true, &method(:step_timeout))
		@dtopt = dtopt
		loop.attach(@stimer)
	end

	def get_session(host, port)
		addr = Address.new(host, port)
		get_session_addr(addr)
	end

	def get_session_addr(addr)
		@spool[addr] ||= create_session(addr)
	end

	def close
		@spool.reject! {|addr, s|
			s.close
			true
		}
		@stimer.detach if @stimer.attached?
		nil
	end

	include LoopUtil

	protected
	def create_session(addr)
		Session.new(addr, @dtopt, nil, nil, @loop)
	end

	private
	def step_timeout
		@spool.each_pair {|addr, s|
			s.step_timeout
		}
	end
end


class Server < SessionPool
	def initialize(loop = Loop.new)
		super(loop, TransportOption.new)
		@dispatcher = nil
		@listeners = []
	end

	def serve(obj, accept = obj.public_methods)
		@dispatcher = ObjectDispatcher.new(obj, accept)
	end

	def listen(host, port, obj = nil, accept = obj.public_methods)
		unless obj.nil?
			serve(obj, accept)
		end
		listen_real(host, port)
		self
	end

	def listen_real(host, port)  #, proto = PROTO_TCP)
		creator = Proc.new { create_session(nil) }
		#case proto
		#when PROTO_TCP, :tcp
		#	listener = TCPTransport::Listener.new(host, port, &creator)
		#when PROTO_UDP, :udp
		#	listener = UDPTransport::Listener.new(host, port, &creator)
		#else
		#	raise "unknown option: #{proto.inspect}"
		#end
		listener = TCPTransport::Listener.new(host, port, &creator)
		@listeners.push(listener)
		listener.activate(@loop)
		self
	end

	def close
		@listeners.reject! {|listener|
			listener.close
			true
		}
		super
	end

	protected
	def create_session(addr)
		Session.new(addr, @dtopt, @dispatcher, nil, @loop)
	end
end


class Cluster < SessionPool
	def initialize(host, port, loop = Loop.new, dtopt = TransportOption.new)
		super(loop, dtopt)
		@host = host
		@port = port
		@dispatcher = nil
		@self_addr = Address.new(host, port)
		@listeners = []
		listen(host, port)  # FIXME obsolete?
	end
	attr_reader :host, :port

	def serve(obj, accept = obj.public_methods)
		@dispatcher = ObjectDispatcher.new(obj, accept)
		self
	end

	def listen(host, port)  #, proto = PROTO_TCP)
		lz = LazyBinder.new(self)
		creator = Proc.new { lz }
		#case proto
		#when PROTO_TCP, :tcp
		#	listener = TCPTransport::Listener.new(host, port, &creator)
		#when PROTO_UDP, :udp
		#	listener = UDPTransport::Listener.new(host, port, &creator)
		#else
		#	raise "unknown option: #{proto.inspect}"
		#end
		listener = TCPTransport::Listener.new(host, port, &creator)
		@listeners.push(listener)
		listener.activate(@loop)
		self
	end

	def close
		if @lsock
			@lsock.detach if @lsock.attached?
			@lsock.close
		end
		super
	end

	include LoopUtil

	protected
	def create_session(addr)
		Session.new(addr, @dtopt, @dispatcher, @self_addr, @loop)
	end

	public
	def create_server_session
		# FIXME
		Session.new(nil, @dtopt, @dispatcher, @self_addr, @loop)
	end

	private
	class LazyBinder < Session
		def initialize(cluster)
			@cluster = cluster
		end

		def on_message(sock, msg)   # Session interface
			if msg[0] == SESSION
				# cluster
				addr = Address.load(msg[1])
				session = @cluster.get_session_addr(addr)
				sock.rebind(session)
			else
				# subsys
				session = @cluster.create_server_session
				sock.rebind(session)
				sock.on_message(msg)
			end
		end
	end
end


end
end

