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


//void address::copy_n46(const address& addr)
//{
//	char* tmp;
//
//	switch(m_type) {
//	case IPV4:
//	case IPV6:
//		break;
//
//	case PATH:
//		tmp = strdup(addr.m.path.str);
//		if(tmp == NULL) {
//			throw std::bad_alloc();
//		}
//		m.path.str = tmp;
//		break;
//
//	default:
//		m.ext.size = addr.m.ext.size;
//		if(m.ext.size > sizeof(m.ext.buffer.stack)) {
//			m.ext.buffer.heap = (char*)malloc(m.ext.size);
//			if(m.ext.buffer.heap == NULL) {
//				throw std::bad_alloc();
//			}
//			memcpy(m.ext.buffer.heap, addr.m.ext.buffer.heap, m.ext.size);
//		} else {
//			memcpy(m.ext.buffer.stack, addr.m.ext.buffer.stack, m.ext.size);
//		}
//		break;
//	}
//}

//void address::copy_op_n46(const address& addr)
//{
//	uint16_t heap_size = 0;
//	char* heap = NULL;
//
//	switch(m_type) {
//	case IPV4:
//	case IPV6:
//		break;
//
//	case PATH:
//		heap_size = strlen(m.path.str)+1;
//		heap = m.path.str;
//		break;
//
//	default:
//		if(m.ext.size > sizeof(m.ext.buffer.stack)) {
//			heap_size = m.ext.size;
//			heap = m.ext.buffer.heap;
//		}
//		break;
//	}
//
//	switch(addr.m_type) {
//	case IPV4:
//	case IPV6:
//		break;
//
//	case PATH: {
//			uint16_t size = strlen(addr.m.path.str)+1;
//			if(size <= heap_size) {
//				m.path.str = heap;
//				memcpy(m.path.str, addr.m.path.str, size);
//			} else {
//				char* tmp = strdup(addr.m.path.str);
//				if(tmp == NULL) {
//					throw std::bad_alloc();
//				}
//				m.path.str = tmp;
//				if(heap != NULL) { free(heap); }
//			}
//			break;
//		}
//
//	default:
//		if(addr.m.ext.size <= sizeof(m.ext.buffer.stack)) {
//			m.ext.size = addr.m.ext.size;
//			memcpy(m.ext.buffer.stack, addr.m.ext.buffer.stack, m.ext.size);
//			if(heap != NULL) { free(heap); }
//
//		} else if(addr.m.ext.size <= heap_size) {
//			m.ext.size = addr.m.ext.size;
//			m.ext.buffer.heap = heap;
//			memcpy(m.ext.buffer.heap, addr.m.ext.buffer.heap, m.ext.size);
//
//		} else {
//			char* tmp = (char*)malloc(addr.m.ext.size);
//			if(tmp == NULL) {
//				throw std::bad_alloc();
//			}
//			m.ext.size = addr.m.ext.size;
//			m.ext.buffer.heap = tmp;
//			memcpy(m.ext.buffer.heap, addr.m.ext.buffer.heap, m.ext.size);
//			if(heap != NULL) { free(heap); }
//		}
//	}
//}


void ip_address::resolve(const char* host, uint16_t port, int v6) {
	addrinfo hints = {};
	if(v6 == 0) {
		hints.ai_family = AF_INET;
	} else if(v6 == 1) {
		hints.ai_family = AF_UNSPEC;
	} else { // v6 == 2
		hints.ai_family = AF_INET6;
	}
	hints.ai_socktype = SOCK_STREAM;  // FIXME
	hints.ai_flags = AI_ADDRCONFIG;

	addrinfo *res = NULL;
	if(getaddrinfo(host, NULL, &hints, &res) != 0) {
		throw std::runtime_error("failed to resolve host name");
	}

	if(v6 != 2) {
		for(addrinfo* rp=res; rp; rp = rp->ai_next) {
			if(rp->ai_family != AF_INET || rp->ai_addrlen < sizeof(struct sockaddr_in)) {
				continue;
			}
			struct sockaddr_in* addr = (struct sockaddr_in*)rp->ai_addr;

			m_type = IPV4;
			set_port(port);
			memcpy(&m.ipv4.buffer[2], &addr->sin_addr.s_addr, 4);

			freeaddrinfo(res);
			return;
		}
	}

	if(v6 != 0) {
		for(addrinfo* rp=res; rp; rp = rp->ai_next) {
			if(rp->ai_family != AF_INET6 || rp->ai_addrlen < sizeof(struct sockaddr_in6)) {
				continue;
			}
			struct sockaddr_in6* addr = (struct sockaddr_in6*)rp->ai_addr;

			m_type = IPV6;
			set_port(port);
			memcpy(&m.ipv6.buffer[2], addr->sin6_addr.s6_addr, 16);

			freeaddrinfo(res);
			return;
		}
	}

	freeaddrinfo(res);
	throw std::runtime_error("failed to resolve host name");
}

ip_address::ip_address(const std::string& host, uint16_t port) :
	address(IPV4)
{
	resolve(host.c_str(), port, 1);
}

ipv4_address::ipv4_address(const std::string& host, uint16_t port) :
	ip_address(IPV4)
{
	resolve(host.c_str(), port, 0);
}

ipv6_address::ipv6_address(const std::string& host, uint16_t port) :
	ip_address(IPV6)
{
	resolve(host.c_str(), port, 2);
}


void address::get_addr(sockaddr* addrbuf) const
{
	switch(m_type) {
	case IPV4: {
			sockaddr_in* addr = (sockaddr_in*)addrbuf;
			memset(addr, 0, sizeof(sockaddr_in));
			addr->sin_family = AF_INET;
			addr->sin_port = htons(*(uint16_t*)m.ipv4.buffer);
			memcpy(&addr->sin_addr.s_addr, m.ipv4.buffer+2, 4);
			break;
		}

	case IPV6: {
			sockaddr_in6* addr = (sockaddr_in6*)addrbuf;
			memset(addr, 0, sizeof(sockaddr_in6));
			addr->sin6_family = AF_INET6;
			addr->sin6_port = htons(*(uint16_t*)m.ipv6.buffer);
			memcpy(addr->sin6_addr.s6_addr, m.ipv6.buffer+2, 16);
			break;
		}

	case PATH: {
			sockaddr_un* addr = (sockaddr_un*)addrbuf;
			memset(addr, 0, sizeof(sockaddr_un));
			addr->sun_family = AF_LOCAL;
			// FIXME check path length
			strcpy(addr->sun_path, m.path.str);
			break;
		}

	default:
		// FIXME
		break;
	}
}


std::ostream& operator<< (std::ostream& stream, const address& a)
{
	switch(a.m_type) {
	case address::IPV4: {
			uint32_t sa = *(uint32_t*)(a.m.ipv4.buffer+2);
			char buf[16];
			return stream << inet_ntop(AF_INET, &sa, buf, sizeof(buf)) << ':' << ntohs(*(uint16_t*)a.m.ipv4.buffer);
		}

	case address::IPV6: {
			unsigned char sa[16];
			char buf[41];
			memcpy(sa, a.m.ipv6.buffer+2, sizeof(sa));
			return stream << '[' << ::inet_ntop(AF_INET6, sa, buf, sizeof(buf)) << "]:" << ntohs(*(uint16_t*)a.m.ipv6.buffer);
		}

	case address::PATH:
		return stream << a.m.path.str;

	default:
		// FIXME
		return stream << "<unknown address>" << std::endl;
	}
}


}  // namespace rpc
}  // namespace msgpack

