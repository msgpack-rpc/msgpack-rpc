//
// msgpack::rpc::client - MessagePack-RPC for C++
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
#include "client.h"
#include "session_impl.h"
#include "transport/tcp.h"
#include "cclog/cclog.h"

namespace msgpack {
namespace rpc {


MP_UTIL_DEF(client) {
	void start_timeout();
	static bool step_timeout(weak_session ws);
};

void MP_UTIL_IMPL(client)::start_timeout()
{
	m_pimpl->get_loop_ref()->add_timer(1.0, 1.0, mp::bind(
				&MP_UTIL_IMPL(client)::step_timeout,
				weak_session(session::m_pimpl)
				));
}

bool MP_UTIL_IMPL(client)::step_timeout(weak_session ws)
{
	shared_session s = ws.lock();
	if(!s) {
		return false;
	}
	s->step_timeout();
	return true;
}


client::client(const std::string& host, uint16_t port, loop lo) :
	session(session_impl::create(tcp_builder(), ip_address(host,port), lo))
{
	MP_UTIL.start_timeout();
}

client::client(const address& addr, loop lo) :
	session(session_impl::create(tcp_builder(), addr, lo))
{
	MP_UTIL.start_timeout();
}

client::client(const builder& b, const address& addr, loop lo) :
	session(session_impl::create(b, addr, lo))
{
	MP_UTIL.start_timeout();
}

client::~client() { }


}  // namespace rpc
}  // namespace msgpack

