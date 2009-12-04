#
# MessagePack-RPC for Ruby
#
# Copyright (C) 2009 FURUHASHI Sadayuki
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
	def initialize
		super("request timed out")
	end
end

class Responder
	def initialize(socket, msgid)
		@socket = socket
		@msgid = msgid
	end

	def result(retval, err = nil)
		@socket.write RPCSocket.pack_response(@msgid, retval, err)
	end

	def error(err)
		result(nil, err)
	end
end


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

	def initialize(host, port)
		raw = Socket.pack_sockaddr_in(port, host)
		if raw[0] == 0 || raw[1] == 0
			# Linux
			family = raw.unpack('S')[0]
		else
			# BSD
			family = raw[1]
		end
		if family == Socket::AF_INET
			@serial = raw[2,6]
		elsif family == Socket::AF_INET6
			@serial = raw[2,2] + raw[8,16]
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


REQUEST  = 0
RESPONSE = 1
NOTIFY   = 2
INIT     = 3


class RPCSocket < Rev::TCPSocket
	def initialize(sock, session)
		@buffer = ''
		@nread = 0
		@mpac = MessagePack::Unpacker.new
		@s = session
		super(sock)
	end

	def bind_session(s)
		@s = s
	end

	def bound?
		!@s.nil?
	end

	def on_read(data)
		@buffer << data

		while true
			@nread = @mpac.execute(@buffer, @nread)

			if @mpac.finished?
				msg = @mpac.data

				@mpac.reset
				@buffer.slice!(0, @nread)
				@nread = 0

				on_message(msg)  # RPCSocket#on_message

				next unless @buffer.empty?
			end

			break
		end
	end

	def on_message(msg)
		case msg[0]
		when REQUEST
			on_request(msg[1], msg[2], msg[3])
		when RESPONSE
			on_response(msg[1], msg[2], msg[3])
		when NOTIFY
			on_notify(msg[1], msg[2])
		when INIT
			# ignore
		else
			raise RPCError.new("unknown message type #{msg[0]}")
		end
	end

	def on_connect
		return unless @s
		@s.on_connect(self)
	end

	def on_connect_failed
		return unless @s
		@s.on_connect_failed(self)
	end

	def on_close
		return unless @s
		@s.on_close(self)
		@s = nil
	rescue
		nil
	end

	def on_request(msgid, method, param)
		return unless @s
		@s.on_request(method, param, Responder.new(self,msgid))
	end

	def on_notify(method, param)
		return unless @s
		@s.on_notify(method, param)
	end

	def on_response(msgid, error, result)
		return unless @s
		@s.on_response(msgid, error, result)
	end

	def send_message(msg)
		write msg.to_msgpack
	end

	def self.pack_request(msgid, method, param)
		[REQUEST, msgid, method, param].to_msgpack
	end

	def self.pack_response(msgid, result, error)
		[RESPONSE, msgid, error, result].to_msgpack
	end

	def self.pack_notify(method, param)
		[NOTIFY, method, param].to_msgpack
	end

	def self.pack_init(msg)
		[INIT, msg].to_msgpack
	end
end


class Session

	class BasicRequest
		def initialize(s, loop)
			@s = s
			@timeout = s.timeout
			@loop = loop
		end
		attr_reader :loop

		def call(err, res)
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

	class AsyncRequest < BasicRequest
		def initialize(s, loop)
			super(s, loop)
			@error  = nil
			@result = nil
		end
		attr_accessor :result, :error

		def call(err, res)
			@error  = err
			@result = res
			@s = nil
		end
	end

	class CallbackRequest < BasicRequest
		def initialize(s, loop, block)
			super(s, loop)
			@block = block
		end

		def call(err, res)
			@block.call(err, res)
		end
	end


	def initialize(initmsg, target_addr, dispatcher, loop)
		@initmsg = initmsg
		@target_addr = target_addr
		@dispatcher = dispatcher || NullDispatcher.new
		@loop = loop
		@timeout = 10    # FIXME default timeout time
		@reconnect = 5   # FIXME default reconnect limit
		reset
	end
	attr_accessor :timeout, :reconnect

	def address
		@target_addr
	end

	def on_connect(sock)
		@sockpool.push(sock)
		sock.write @pending
		@pending = ""
		@connecting = 0
	end

	def on_connect_failed(sock)
		if @connecting < @reconnect
			try_connect
			@connecting += 1
		else
			@connecting = 0
		end
	end


	def send(method, *args)
		msgid = send_request(method, args)
		@reqtable[msgid] = AsyncRequest.new(self, @loop)
	end

	def callback(method, *args, &block)
		msgid = send_request(method, args)
		@reqtable[msgid] = CallbackRequest.new(self, @loop, block)
	end

	def call(method, *args)
		# FIXME if @reqtable.empty? optimize
		req = send(method, *args)
		req.join
		if req.error
			raise TimeoutError.new if req.error == :TimeoutError
			raise RemoteError.new(req.error, req.result)
		end
		req.result
	end

	def notify(method, *args)
		send_notify(method, args)
	end

	def close
		@sockpool.reject! {|sock|
			sock.detach if sock.attached?
			sock.close
			true
		}
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
		reqs.each {|req| req.call :TimeoutError, nil }
		!@reqtable.empty?
	end


	def on_response(msgid, error, result)
		if req = @reqtable.delete(msgid)
			req.call(error, result)
		end
	end

	def on_request(method, param, res)
		@dispatcher.dispatch_request(self, method, param, res)
	end

	def on_notify(method, param)
		@dispatcher.dispatch_notify(self, method, param)
	end

	def on_close(sock)
		@sockpool.delete(sock)
	end


	private
	def reset
		@sockpool = []
		@reqtable = {}
		@seqid = 0
		@pending = ""
		@connecting = 0
	end

	def send_request(method, param)
		method = method.to_s unless method.is_a?(Integer)
		msgid = @seqid
		@seqid += 1; if @seqid >= 1<<31 then @seqid = 0 end
		send_data RPCSocket.pack_request(msgid, method, param)
		msgid
	end

	def send_notify(method, param)
		method = method.to_s unless method.is_a?(Integer)
		send_data RPCSocket.pack_notify(method, param)
		nil
	end

	def send_data(msg)
		unless @target_addr
			raise RPCError.new("unexpected send request on server session")
		end
		if @sockpool.empty?
			if @connecting == 0
				try_connect
				@connecting = 1
			end
			@pending << msg
		else
			# FIXME pesudo connection load balance
			sock = @sockpool.first
			sock.write msg
		end
	end

	def try_connect
		port, host = ::Socket.unpack_sockaddr_in(@target_addr.sockaddr)
		sock = RPCSocket.connect(host, port, self)  # async connect
		if @initmsg
			sock.write @initmsg
		end
		@loop.attach(sock)
	end
end


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
	def initialize(host, port, loop = Loop.new)
		@loop = loop
		@host = host
		@port = port

		target_addr = Address.new(host, port)
		super(nil, target_addr, NullDispatcher.new, loop)

		@timer = Timer.new(1, true) {
			step_timeout
		}
		loop.attach(@timer)
	end
	attr_reader :host, :port

	def self.open(host, port, loop = Loop.new, &block)
		cli = new(host, port, loop)
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
	def initialize(loop = Loop.new)
		@loop = loop
		@spool = {}
		@stimer = Timer.new(1, true, &method(:step_timeout))
		loop.attach(@stimer)
	end

	def get_session(host, port)
		target_addr = Address.new(host, port)
		@spool[target_addr] ||= create_session(target_addr)
	end

	def close
		@spool.reject! {|target_addr, s|
			s.close
			true
		}
		@stimer.detach if @stimer.attached?
		nil
	end

	include LoopUtil

	protected
	def create_session(target_addr)
		Session.new(nil, target_addr, nil, @loop)
	end

	private
	def step_timeout
		@spool.each_pair {|target_addr, s|
			s.step_timeout
		}
	end
end


class Server < SessionPool
	class Socket < RPCSocket
		def initialize(sock, dispatcher, loop)
			s = Session.new(nil, nil, dispatcher, loop)
			super(sock, s)
		end
	end

	def initialize(loop = Loop.new)
		super(loop)
		@lsocks = []
	end

	def listen(host, port, obj, accept = obj.public_methods)
		dispatcher = ObjectDispatcher.new(obj, accept)
		lsock = Rev::TCPServer.new(host, port, Server::Socket, dispatcher, @loop)
		@lsocks.push(lsock)
		@loop.attach(lsock)
	end

	def close
		@lsocks.reject! {|lsock|
			lsock.detach if lsock.attached?
			lsock.close
			true
		}
		super
	end
end


end  # module RPC
end  # module MessagePack

