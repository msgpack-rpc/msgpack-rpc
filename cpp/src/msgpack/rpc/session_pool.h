//
// msgpack::rpc::session_pool - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_SESSION_POOL_H__
#define MSGPACK_RPC_SESSION_POOL_H__

#include "session.h"
#include "loop_util.h"
#include "address.h"
#include "impl_fwd.h"
#include "types.h"
#include <mp/utilize.h>
#include <mp/sync.h>
#include <mp/unordered_map.h>

namespace msgpack {
namespace rpc {


class session_pool : public loop_util {
public:
	session_pool(loop lo = loop());

	~session_pool();

	session get_session(const address& addr);

	session get_session(const std::string& host, uint16_t port)
		{ return get_session(address(host, port)); }

private:
	MP_UTILIZE;

private:
	typedef mp::unordered_map<address, weak_session, address::hash> table_t;
	typedef mp::sync<table_t>::ref table_ref;
	mp::sync<table_t> m_table;

	void start_timeout_timer(double timeout_step_interval_sec);
	void timeout_callback();

protected:
	virtual shared_session create_session(const address& addr);

	option m_default_opt;

private:
	session_pool(const session_pool&);
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/session_pool.h */

