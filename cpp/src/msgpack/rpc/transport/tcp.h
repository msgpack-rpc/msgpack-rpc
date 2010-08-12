//
// msgpack::rpc::transport::tcp - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_TCP_H__
#define MSGPACK_RPC_TRANSPORT_TCP_H__

#include "../transport.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>

namespace msgpack {
namespace rpc {


class tcp_builder : public builder::base<tcp_builder> {
public:
	tcp_builder();
	~tcp_builder();

	std::auto_ptr<client_transport> build(session_impl* s, const address& addr) const;

	tcp_builder& connect_timeout(double sec)
		{ m_connect_timeout = sec; return *this; }

	double connect_timeout() const
		{ return m_connect_timeout; }

	tcp_builder& reconnect_limit(unsigned int num)
		{ m_reconnect_limit = num; return *this; }

	unsigned int reconnect_limit() const
		{ return m_reconnect_limit; }

public:
	double m_connect_timeout;
	unsigned int m_reconnect_limit;
};


class tcp_listener : public listener::base<tcp_listener> {
public:
	tcp_listener(const std::string& host, uint16_t port);
	tcp_listener(const address& addr);

	~tcp_listener();

	std::auto_ptr<server_transport> listen(server_impl* svr) const;

private:
	address m_addr;

private:
	tcp_listener();
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport/tcp.h */

