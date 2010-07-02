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
	socklen_t get_addrlen() const;
	void get_addr(sockaddr* addrbuf) const;

	typedef enum type_t {
		IPV4 = 0,
		IPV6 = 1,
		PATH = 2,
		UNKNOWN = 255,
	} type_t;

	type_t type() const { return m_type; }

public:
	bool operator== (const address& o) const;
	bool operator!= (const address& o) const;
	bool operator<  (const address& o) const;
	bool operator>  (const address& o) const;

	struct hash {
		size_t operator() (const msgpack::rpc::address& a) const;
	};

	friend std::ostream& operator<< (std::ostream& stream, const address& a);

protected:
	type_t m_type;

	union {
		struct {
			// +--+----+
			// | 2|  4 |
			// +--+----+
			// port network byte order
			//    IPv4 address
			char buffer[6];
		} ipv4;

		struct {
			// +--+----------------+
			// | 2|       16       |
			// +--+----------------+
			// port network byte order
			//    IPv6 address
			char buffer[18];
		} ipv6;

		struct {
			char* str;
		} path;
	} m;

protected:
	address(type_t t) : m_type(t) { }

public:
	//// FIXME
	//template <typename Packer>
	//void msgpack_pack(Packer& pk) const;

	//void msgpack_unpack(msgpack::object o);

	//void msgpack_object(msgpack::object* o, msgpack::zone* z);
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
	ip_address(type_t t) : address(t) { }

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
};

class path_address : public address {
public:
	path_address(const std::string& path);

	const char* get_path() const;
};


inline address::address()
{
	m_type = UNKNOWN;
	memset(&m, 0, sizeof(m));
}

inline address::~address()
{
	switch(m_type) {
	case IPV4:
	case IPV6:
		break;

	case PATH:
		free(m.path.str);
		break;

	default:
		break;
	}
}

inline address::address(const address& a)
{
	m_type = a.m_type;

	memcpy(&m, &a.m, sizeof(m));

	if(m_type == PATH) {
		m.path.str = strdup(a.m.path.str);
		if(m.path.str == NULL) {
			throw std::bad_alloc();
		}
	}
}

inline const address& address::operator=(const address& o)
{
	if(m_type == PATH && o.m_type == PATH) {
		size_t msize = strlen(m.path.str);
		size_t asize = strlen(o.m.path.str);
		if(msize >= asize) {
			memcpy(m.path.str, o.m.path.str, asize+1);
			return *this;
		}
	}

	if(o.m_type == PATH) {
		char* tmp = strdup(o.m.path.str);
		if(tmp == NULL) {
			throw std::bad_alloc();
		}
		if(m_type == PATH) {
			free(m.path.str);
		}
		m.path.str = tmp;
	} else {
		memcpy(&m, &o.m, sizeof(m));
	}

	m_type = o.m_type;
	return *this;
}

inline socklen_t address::get_addrlen() const
{
	switch(m_type) {
	case IPV4:
		return sizeof(sockaddr_in);
	case IPV6:
		return sizeof(sockaddr_in6);
	case PATH:
		return sizeof(sockaddr_un);
	default:
		return 0;
	}
}

inline ip_address::ip_address(const struct sockaddr_in& a) :
	address(IPV4)
{
	memcpy(&m.ipv4.buffer[0], &a.sin_port, 2);
	memcpy(&m.ipv4.buffer[2], &a.sin_addr.s_addr, 4);
}

inline ip_address::ip_address(const struct sockaddr_in6& a) :
	address(IPV6)
{
	memcpy(&m.ipv6.buffer[0], &a.sin6_port, 2);
	memcpy(&m.ipv6.buffer[2], a.sin6_addr.s6_addr, 16);
}

inline ipv4_address::ipv4_address(const struct sockaddr_in& a) :
	ip_address(a) { }

inline ipv6_address::ipv6_address(const struct sockaddr_in6& a) :
	ip_address(a) { }

inline uint16_t ip_address::get_port() const
{
	return ntohs(*(uint16_t*)m.ipv4.buffer);
}

inline void ip_address::set_port(uint16_t port)
{
	*(uint16_t*)m.ipv4.buffer = htons(port);
}

inline path_address::path_address(const std::string& path) :
	address(PATH)
{
	// FIXME check path length
	m.path.str = strdup(path.c_str());
}

inline const char* path_address::get_path() const
{
	return m.path.str;
}


inline bool address::operator== (const address& o) const
{
	switch(m_type) {
	case IPV4:
		return memcmp(m.ipv4.buffer, o.m.ipv4.buffer, sizeof(m.ipv4.buffer)) == 0;
	case IPV6:
		return memcmp(m.ipv6.buffer, o.m.ipv6.buffer, sizeof(m.ipv6.buffer)) == 0;
	case PATH:
		return strcmp(m.path.str, o.m.path.str) == 0;
	default:
		return false;
	}
}

inline bool address::operator!= (const address& o) const
{
	return !(*this == o);
}

inline bool address::operator<  (const address& o) const
{
	switch(m_type) {
	case IPV4:
		return memcmp(m.ipv4.buffer, o.m.ipv4.buffer, sizeof(m.ipv4.buffer)) < 0;
	case IPV6:
		return memcmp(m.ipv6.buffer, o.m.ipv6.buffer, sizeof(m.ipv6.buffer)) < 0;
	case PATH:
		return strcmp(m.path.str, o.m.path.str) < 0;
	default:
		return 0;
	}
}

inline bool address::operator>  (const address& o) const
{
	switch(m_type) {
	case IPV4:
		return memcmp(m.ipv4.buffer, o.m.ipv4.buffer, sizeof(m.ipv4.buffer)) > 0;
	case IPV6:
		return memcmp(m.ipv6.buffer, o.m.ipv6.buffer, sizeof(m.ipv6.buffer)) > 0;
	case PATH:
		return strcmp(m.path.str, o.m.path.str) > 0;
	default:
		return 0;
	}
}

inline size_t address::hash::operator() (const msgpack::rpc::address& a) const
{
	size_t h = mp::hash<int>()(a.m_type);
	switch(a.m_type) {
	case IPV4:
		return h ^ mp::hash<std::string>()(std::string(a.m.ipv4.buffer, sizeof(a.m.ipv4.buffer)));
	case IPV6:
		return h ^ mp::hash<std::string>()(std::string(a.m.ipv6.buffer, sizeof(a.m.ipv6.buffer)));
	case PATH:
		return h ^ mp::hash<std::string>()(std::string(a.m.path.str));
	default:
		return h;
	}
}

// FIXME
//
//template <typename Packer>
//void address::msgpack_pack(Packer& pk) const
//{
//	pk.pack_array(2);
//
//	pk.pack_int(m_type);
//
//	switch(m_type) {
//	case IPV4:
//		pk.pack_raw(sizeof(m.ipv4.buffer));
//		pk.pack_raw_body(m.ipv4.buffer, sizeof(m.ipv4.buffer));
//		break;
//
//	case IPV6:
//		pk.pack_raw(sizeof(m.ipv6.buffer));
//		pk.pack_raw_body(m.ipv6.buffer, sizeof(m.ipv6.buffer));
//		break;
//
//	case PATH: {
//			size_t size = strlen(m.path.str);
//			pk.pack_raw(size);
//			pk.pack_raw_body(m.path.str, size);
//			break;
//		}
//
//	default:
//		// FIXME
//		throw std::runtime_error("unknown address type");
//	}
//}
//
//void address::msgpack_unpack(msgpack::object o)
//{
//
//	if(o.type != type::ARRAY) { throw type_error(); }
//	if(o.via.array.size < 2) { throw type_error(); }
//
//	int type;
//	o.via.array.ptr[0].convert(&type);
//
//	msgpack::object p = o.via.array.ptr[1];
//
//	// FIXME free(m.path.str);
//
//	switch(type) {
//	case IPV4:
//		if(p.type != type::RAW) { throw type_error(); }
//		if(p.via.raw.size != sizeof(m.ipv4.buffer)) { throw type_error(); }
//		memcpy(m.ipv4.buffer, p.via.raw.ptr, sizeof(m.ipv4.buffer));
//		break;
//
//	case IPV6:
//		if(p.type != type::RAW) { throw type_error(); }
//		if(p.via.raw.size != sizeof(m.ipv6.buffer)) { throw type_error(); }
//		memcpy(m.ipv6.buffer, p.via.raw.ptr, sizeof(m.ipv6.buffer));
//		break;
//
//	case PATH:
//		if(p.type != type::RAW) { throw type_error(); }
//		{
//			char* tmp = malloc(p.via.raw.size+1);
//			if(tmp == NULL) {
//				throw std::bad_alloc();
//			}
//			memcpy(tmp, p.via.raw.ptr, p.via.raw.size);
//			tmp[p.via.raw.size] = '\0';
//			m.path.str = tmp;
//		}
//		break;
//
//	default:
//		throw msgpack::type_error("unknown address type");
//	}
//}
//
//void address::msgpack_object(msgpack::object* o, msgpack::zone* z)
//{
//}


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/address.h */

