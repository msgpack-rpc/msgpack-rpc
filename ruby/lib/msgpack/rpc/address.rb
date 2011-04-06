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

	def <=>(o)
		dump <=> o.dump
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


end
end
