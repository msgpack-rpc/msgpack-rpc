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
#ifndef MSGPACK_RPC_ADDRESS_H__
#define MSGPACK_RPC_ADDRESS_H__

#include <iostream>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <msgpack.hpp>
#include <mp/unordered_map.h>

namespace msgpack {
namespace rpc {


class address {
public:
	address();
	address(const address&);
	const address& operator= (const address& o);
	~address();

public:
	sa_family_t family() const { return m.ipv4.sin_family; }

	socklen_t get_addrlen() const;
	void get_addr(sockaddr* addrbuf) const;

	bool operator== (const address& o) const;
	bool operator!= (const address& o) const;
	bool operator<  (const address& o) const;
	bool operator>  (const address& o) const;

	struct hash {
		size_t operator() (const msgpack::rpc::address& a) const;
	};

	friend std::ostream& operator<< (std::ostream& stream, const address& a);

protected:
	union {
		struct sockaddr_in  ipv4;
		struct sockaddr_in6 ipv6;
		struct {
			uint16_t family;  // don't reference. use family()
			socklen_t addrlen;
			struct sockaddr* addr;
		} ex;
	} m;

	bool is_ex() const {
		return m.ipv4.sin_family != AF_INET && m.ipv4.sin_family != AF_INET6 && m.ipv4.sin_family != 0;
	}

	void set_family(sa_family_t f) {
		m.ipv4.sin_family = f;
	}

protected:
	address(sa_family_t f) { m.ipv4.sin_family = f; }

private:
	void copy_ex(const address& a);
	void assign_ex(const address& a);
};

std::ostream& operator<< (std::ostream& stream, const address& a);


class ip_address : public address {
public:
	ip_address(const std::string& host, uint16_t port);
	ip_address(const struct sockaddr_in& a);
	ip_address(const struct sockaddr_in6& a);

	uint16_t get_port() const;
	void set_port(uint16_t port);

protected:
	ip_address(sa_family_t f) : address(f) { }

	void resolve(const char* host, uint16_t port, int v6);
};


class ipv4_address : public ip_address {
public:
	ipv4_address(const std::string& host, uint16_t port);
	ipv4_address(const struct sockaddr_in& a);
};


class ipv6_address : public ip_address {
public:
	ipv6_address(const std::string& host, uint16_t port);
	ipv6_address(const struct sockaddr_in6& a);

	uint32_t get_flowinfo() const { return m.ipv6.sin6_flowinfo; }
	void set_flowinfo(uint32_t v) { m.ipv6.sin6_flowinfo = v; }

	uint32_t get_scope_id() const { return m.ipv6.sin6_scope_id; }
	void set_scope_id(uint32_t v) { m.ipv6.sin6_scope_id = v; }
};


class path_address : public address {
public:
	path_address(const std::string& path);

	const char* get_path() const;
};


inline address::address()
{
	memset(&m, 0, sizeof(m));
}

inline address::~address()
{
	if(is_ex()) {
		free(m.ex.addr);
	}
}

inline address::address(const address& a)
{
	if(a.is_ex()) {
		copy_ex(a);
	} else {
		memcpy(&m, &a.m, sizeof(m));
	}
}

inline const address& address::operator=(const address& o)
{
	if(o.is_ex()) {
		assign_ex(o);
		return *this;
	}

	if(is_ex()) {
		free(m.ex.addr);
	}
	memcpy(&m, &o.m, sizeof(m));

	return *this;
}

inline socklen_t address::get_addrlen() const
{
	if(family() == AF_INET) {
		return sizeof(struct sockaddr_in);
	} else if(family() == AF_INET6) {
		return sizeof(struct sockaddr_in6);
	} else {
		return m.ex.addrlen;
	}
}

inline void address::get_addr(sockaddr* addrbuf) const
{
	if(family() == AF_INET) {
		memcpy(addrbuf, &m.ipv4, sizeof(struct sockaddr_in));
	} else if(family() == AF_INET6) {
		memcpy(addrbuf, &m.ipv6, sizeof(struct sockaddr_in6));
	} else {
		memcpy(addrbuf, m.ex.addr, m.ex.addrlen);
	}
}


inline ip_address::ip_address(const struct sockaddr_in& a)
{
	m.ipv4 = a;
	m.ipv4.sin_family = AF_INET;
}

inline ip_address::ip_address(const struct sockaddr_in6& a)
{
	m.ipv6 = a;
	m.ipv6.sin6_family = AF_INET6;
}

inline ipv4_address::ipv4_address(const struct sockaddr_in& a) :
	ip_address(a) { }

inline ipv6_address::ipv6_address(const struct sockaddr_in6& a) :
	ip_address(a) { }

inline uint16_t ip_address::get_port() const
{
	return ntohs(m.ipv4.sin_port);
}

inline void ip_address::set_port(uint16_t port)
{
	m.ipv4.sin_port = htons(port);
}


inline const char* path_address::get_path() const
{
	return ((struct sockaddr_un*)m.ex.addr)->sun_path;
}


inline bool address::operator== (const address& o) const
{
	if(family() != o.family()) {
		return false;
	}
	if(family() == AF_INET) {
		return memcmp(&m.ipv4, &o.m.ipv4, sizeof(m.ipv4)) == 0;
	} else if(family() == AF_INET6) {
		return memcmp(&m.ipv6, &o.m.ipv6, sizeof(m.ipv6)) == 0;
	} else {
		return m.ex.addrlen == o.m.ex.addrlen &&
			memcmp(m.ex.addr, o.m.ex.addr, m.ex.addrlen) == 0;
	}
}

inline bool address::operator!= (const address& o) const
{
	return !(*this == o);
}

inline bool address::operator<  (const address& o) const
{
	if(family() != o.family()) {
		// FIXME platform dependent
		return family() < o.family();
	}
	if(family() == AF_INET) {
		return memcmp(&m.ipv4, &o.m.ipv4, sizeof(struct sockaddr_in)) < 0;
	} else if(family() == AF_INET6) {
		return memcmp(&m.ipv6, &o.m.ipv6, sizeof(struct sockaddr_in6)) < 0;
	}
	if(m.ex.addrlen != o.m.ex.addrlen) {
		return m.ex.addrlen < o.m.ex.addrlen;
	}
	return memcmp(&m.ex.addr, &o.m.ex.addr, m.ex.addrlen) < 0;
}

inline bool address::operator>  (const address& o) const
{
	if(family() != o.family()) {
		// FIXME platform dependent
		return family() > o.family();
	}
	if(family() == AF_INET) {
		return memcmp(&m.ipv4, &o.m.ipv4, sizeof(struct sockaddr_in)) > 0;
	} else if(family() == AF_INET6) {
		return memcmp(&m.ipv6, &o.m.ipv6, sizeof(struct sockaddr_in6)) > 0;
	}
	if(m.ex.addrlen != o.m.ex.addrlen) {
		return m.ex.addrlen > o.m.ex.addrlen;
	}
	return memcmp(&m.ex.addr, &o.m.ex.addr, m.ex.addrlen) > 0;
}

inline size_t address::hash::operator() (const msgpack::rpc::address& a) const
{
	if(a.family() == AF_INET) {
		return mp::hash<std::string>()(std::string((const char*)&a.m.ipv4, sizeof(struct sockaddr_in)));
	} else if(a.family() == AF_INET6) {
		return mp::hash<std::string>()(std::string((const char*)&a.m.ipv6, sizeof(struct sockaddr_in6)));
	} else {
		return mp::hash<std::string>()(std::string((const char*)&a.m.ex.addr, a.m.ex.addrlen));
	}
}


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/address.h */

