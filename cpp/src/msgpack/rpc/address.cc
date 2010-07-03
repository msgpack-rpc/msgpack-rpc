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


void address::copy_ex(const address& a)
{
	m.ex.family = a.m.ex.family;
	m.ex.addrlen = a.m.ex.addrlen;
	m.ex.addr = (struct sockaddr*)malloc(a.m.ex.addrlen);
	if(m.ex.addr == NULL) {
		throw std::bad_alloc();
	}
	memcpy(m.ex.addr, a.m.ex.addr, a.m.ex.addrlen);
}

void address::assign_ex(const address& o)
{
	if(is_ex() && m.ex.addrlen >= o.m.ex.addrlen) {
		m.ex.family = o.m.ex.family;
		m.ex.addrlen = o.m.ex.addrlen;
		memcpy(m.ex.addr, o.m.ex.addr, o.m.ex.addrlen);
		return;
	}

	struct sockaddr* addr = (struct sockaddr*)malloc(o.m.ex.addrlen);
	if(addr == NULL) {
		throw std::bad_alloc();
	}

	if(is_ex()) {
		free(m.ex.addr);
	}
	memcpy(addr, o.m.ex.addr, o.m.ex.addrlen);
	m.ex.family = o.m.ex.family;
	m.ex.addrlen = o.m.ex.addrlen;
	m.ex.addr = addr;
}


void ip_address::resolve(const char* host, uint16_t port, int v6)
{
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
			memcpy(&m.ipv4, rp->ai_addr, sizeof(struct sockaddr_in));
			set_port(port);
			freeaddrinfo(res);
			return;
		}
	}

	if(v6 != 0) {
		for(addrinfo* rp=res; rp; rp = rp->ai_next) {
			if(rp->ai_family != AF_INET6 || rp->ai_addrlen < sizeof(struct sockaddr_in6)) {
				continue;
			}
			memcpy(&m.ipv6, rp->ai_addr, sizeof(struct sockaddr_in6));
			set_port(port);
			freeaddrinfo(res);
			return;
		}
	}

	freeaddrinfo(res);
	throw std::runtime_error("failed to resolve host name");
}

ip_address::ip_address(const std::string& host, uint16_t port) :
	address(AF_INET)
{
	resolve(host.c_str(), port, 1);
}

ipv4_address::ipv4_address(const std::string& host, uint16_t port) :
	ip_address(AF_INET)
{
	resolve(host.c_str(), port, 0);
}

ipv6_address::ipv6_address(const std::string& host, uint16_t port) :
	ip_address(AF_INET6)
{
	resolve(host.c_str(), port, 2);
}


path_address::path_address(const std::string& path) :
	address(AF_LOCAL)
{
	// FIXME check path length
	struct sockaddr_un* addr = (struct sockaddr_un*)malloc(sizeof(struct sockaddr_un));
	if(addr == NULL) {
		throw std::bad_alloc();
	}
	memset(addr, 0, sizeof(struct sockaddr_un));
	addr->sun_family = AF_LOCAL;
	memcpy(addr->sun_path, path.c_str(), path.size()+1);
	m.ex.addrlen = sizeof(struct sockaddr_un);
	m.ex.addr = (struct sockaddr*)addr;
}


std::ostream& operator<< (std::ostream& stream, const address& a)
{
	if(a.family() == AF_INET) {
		char buf[16];
		return stream << inet_ntop(AF_INET, &a.m.ipv4.sin_addr, buf, sizeof(buf)) << ':' << ntohs(a.m.ipv4.sin_port);

	} else if(a.family() == AF_INET6) {
		char buf[41];
		return stream << '[' << ::inet_ntop(AF_INET6, &a.m.ipv6.sin6_addr, buf, sizeof(buf)) << "]:" << ntohs(a.m.ipv6.sin6_port);

	} else if(a.family() == AF_LOCAL) {
		return stream << ((struct sockaddr_un*)a.m.ex.addr)->sun_path;

	} else {
		// FIXME
		return stream << "<unknown address>";
	}
}


}  // namespace rpc
}  // namespace msgpack

