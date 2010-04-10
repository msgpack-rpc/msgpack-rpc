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
#include "session_pool.h"
#include "session_impl.h"

namespace msgpack {
namespace rpc {


MP_UTIL_DEF(session_pool) {
	bool step_timeout();
};


session_pool::session_pool(loop lo) :
	loop_util(lo)
{
	m_loop->add_timer(1.0, 1.0,
			mp::bind(&MP_UTIL_IMPL(session_pool)::step_timeout, &MP_UTIL));
	// FIXME thisの寿命: weak_ptrのlock()が失敗したらタイマーを終了？
	//start_timer(&t, &t,
	//		mp::bind(&session_pool::step_timeout, this,
	//			mp::weak_ptr<session_pool>(shared_from_this()) ));
}

session_pool::~session_pool()
{
}

session session_pool::get_session(const address& addr)
{
	table_ref ref(m_table);

	table_t::iterator found = ref->find(addr);
	if(found != ref->end()) {
		shared_session s = found->second.lock();
		if(s) {
			return session(s);
		}
	}

	shared_session s = create_session(addr);
	ref->insert( table_t::value_type(addr, weak_session(s)) );
	return session(s);
}

shared_session session_pool::create_session(const address& addr)
{
	return shared_session(new session_impl(
				addr, m_default_opt, address(), NULL, m_loop));
}

bool MP_UTIL_IMPL(session_pool)::step_timeout()
{
	table_ref ref(m_table);
	for(table_t::iterator it(ref->begin());
			it != ref->end(); ) {
		shared_session s(it->second.lock());
		if(s) {
			s->step_timeout();
			++it;
		} else {
			ref->erase(it++);
		}
	}
	return true;
}


}  // namespace rpc
}  // namespace msgpack

