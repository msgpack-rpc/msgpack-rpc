//
// msgpack::rpc::server - MessagePack-RPC for C++
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
#include "server.h"
#include "transport/listener.h"

#include "transport/tcp.h"

namespace msgpack {
namespace rpc {


MP_UTIL_DEF(server) {
	shared_session create_server_session();
};

server::server(loop lo) :
	session_pool(lo)
{ }

server::~server()
{
	close();
}

shared_session server::create_session(const address& addr)
{
	return shared_session(new session_impl(
				addr, m_default_opt, address(), m_dp, m_loop));
}

shared_session MP_UTIL_IMPL(server)::create_server_session()
{
	return create_session(address());
}

void server::serve(dispatcher* dp)
{
	m_dp = dp;
}

void server::close()
{
	m_listener.reset();
}

void server::listen(address addr)
{
	char addrbuf[addr.addrlen()];
	addr.getaddr((struct sockaddr*)addrbuf);

	m_listener.reset( new transport::tcp::listener(
			AF_INET, SOCK_STREAM, 0,
			(struct sockaddr*)addrbuf, addr.addrlen(),
			m_loop,
			mp::bind(&MP_UTIL_IMPL(server)::create_server_session, &MP_UTIL)) );
}


}  // namespace rpc
}  // namespace msgpack

