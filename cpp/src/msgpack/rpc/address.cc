//
// msgpack::rpc::address - MessagePack-RPC for C++
//
// Copyright (C) 2009-2010 FURUHASHI Sadayuki
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
#include "address.h"
#include <netdb.h>
#include <stdexcept>
#include <string.h>

namespace msgpack {
namespace rpc {


address::address(const struct sockaddr_in& addr)
{
#ifdef MSGPACK_RPC_IPV6
	m_serial_length = 6;
	memcpy(&m_serial_address[0], &addr.sin_port, 2);
	memcpy(&m_serial_address[2], &addr.sin_addr.s_addr, 4);
#else
	memcpy(&m_serial_address[0], &addr.sin_port, 2);
	memcpy(&m_serial_address[2], &addr.sin_addr.s_addr, 4);
	memset(&m_serial_address[6], 0, 2);
#endif
}

#ifdef MSGPACK_RPC_IPV6
address::address(const struct sockaddr_in6& addr)
{
	m_serial_length = 22;
	memcpy(&m_serial_address[0], &addr.sin6_port, 2);
	memcpy(&m_serial_address[2], addr.sin6_addr.s6_addr, 16);
}
#endif

address::address(const std::string& host, uint16_t port)
{
	addrinfo hints = {};
#ifdef MSGPACK_RPC_IPV6
	hints.ai_family = AF_UNSPEC;
#else
	hints.ai_family = AF_INET;
#endif
	hints.ai_socktype = SOCK_STREAM;  // FIXME
	hints.ai_flags = AI_ADDRCONFIG;

	addrinfo *res = NULL;
	if(getaddrinfo(host.c_str(), NULL, &hints, &res) != 0) {
		throw std::runtime_error("failed to resolve host name");
	}

	for(addrinfo* rp=res; rp; rp = rp->ai_next) {
		if(rp->ai_family == AF_INET &&
				rp->ai_addrlen >= sizeof(struct sockaddr_in)) {
			struct sockaddr_in* addr = (struct sockaddr_in*)rp->ai_addr;
			addr->sin_port = htons(port);
			*this = address(*addr);
			freeaddrinfo(res);
			return;
		}
	}

#ifdef MSGPACK_RPC_IPV6
	// prefer IPv4 address
	for(addrinfo* rp=res; rp; rp = rp->ai_next) {
		if(rp->ai_family == AF_INET6 &&
				rp->ai_addrlen >= sizeof(struct sockaddr_in6)) {
			struct sockaddr_in6* addr = (struct sockaddr_in6*)rp->ai_addr;
			addr->sin6_port = htons(port);
			*this = address(*addr);
			freeaddrinfo(res);
			return;
		}
	}
#endif

	freeaddrinfo(res);
	throw std::runtime_error("failed to resolve host name");
}

void address::load(address* to, const char* data, size_t len)
{
#ifdef MSGPACK_RPC_IPV6
	if(len != 6 && len != 22) {
		throw std::runtime_error("unknown address type");
	}

	memcpy(to->m_serial_address, data, len);
	to->m_serial_length = len;

#else
	if(len != 6) {
		throw std::runtime_error("unknown address type");
	}

	memcpy(to->m_serial_address, data, 6);
	memset(&to->m_serial_address[6], 0, 2);
#endif
}

void address::getaddr(sockaddr* addrbuf) const
{
#ifdef MSGPACK_RPC_IPV6
	if(m_serial_length == 6) {
		sockaddr_in* addr = reinterpret_cast<sockaddr_in*>(addrbuf);

		memset(addr, 0, sizeof(sockaddr_in));
		addr->sin_family = AF_INET;
		addr->sin_port = raw_port();
		addr->sin_addr.s_addr = *((uint32_t*)&m_serial_address[2]);

	} else {
		sockaddr_in6* addr = reinterpret_cast<sockaddr_in6*>(addrbuf);

		memset(addr, 0, sizeof(sockaddr_in6));
		addr->sin6_family = AF_INET6;
		addr->sin6_port = raw_port();
		memcpy(addr->sin6_addr.s6_addr, &m_serial_address[2], 16);
		addr->sin6_scope_id = 0;  // FIXME *((uint32_t*)&m_serial_address[18]);
	}

#else
	sockaddr_in* addr = reinterpret_cast<sockaddr_in*>(addrbuf);

	memset(addr, 0, sizeof(sockaddr_in));
	addr->sin_family = AF_INET;
	addr->sin_port = raw_port();
	addr->sin_addr.s_addr = *((uint32_t*)&m_serial_address[2]);
#endif
}


std::ostream& operator<< (std::ostream& stream, const address& addr)
{
#ifdef MSGPACK_RPC_IPV6
	if(addr.m_serial_length == 6) {
		uint32_t sa = *(uint32_t*)&addr.m_serial_address[2];
		char buf[16];
		return stream << ::inet_ntop(AF_INET, &sa, buf, sizeof(buf)) << ':' << ntohs(addr.raw_port());
	} else {
		unsigned char sa[16];
		char buf[41];
		memcpy(sa, &addr.m_serial_address[2], sizeof(sa));
		return stream << '[' << ::inet_ntop(AF_INET6, sa, buf, sizeof(buf)) << "]:" << ntohs(addr.raw_port());
	}
#else
	uint32_t sa = *(uint32_t*)&addr.m_serial_address[2];
	char buf[16];
	return stream << ::inet_ntop(AF_INET, &sa, buf, sizeof(buf)) << ':' << ntohs(addr.raw_port());
#endif
}


}  // namespace rpc
}  // namespace msgpack

