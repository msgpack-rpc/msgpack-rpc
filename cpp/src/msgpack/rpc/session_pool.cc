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
#include "session_pool_impl.h"
#include "session_impl.h"
#include "transport/tcp.h"
#include "cclog/cclog.h"

namespace msgpack {
namespace rpc {


MP_UTIL_DEF(session_pool) {
	void start_timeout();
	static bool step_timeout(weak_session_pool wsp);
};

void MP_UTIL_IMPL(session_pool)::start_timeout()
{
	get_loop()->add_timer(1.0, 1.0, mp::bind(
				&MP_UTIL_IMPL(session_pool)::step_timeout,
				weak_session_pool(m_pimpl)
				));
}

bool MP_UTIL_IMPL(session_pool)::step_timeout(weak_session_pool wsp)
{
	shared_session_pool sp = wsp.lock();
	if(!sp) {
		return false;
	}
	sp->step_timeout();
	return true;
}


session_pool_impl::session_pool_impl(const builder& b, loop lo) :
	m_loop(lo), m_builder(b.copy()) { }

session_pool_impl::~session_pool_impl() { }

session session_pool_impl::get_session(const address& addr)
{
	table_ref ref(m_table);

	table_t::iterator found = ref->find(addr);
	if(found != ref->end()) {
		shared_session s = found->second.lock();
		if(s) {
			return session(s);
		}
		ref->erase(found);
	}

	shared_session s(session_impl::create(*m_builder, addr, m_loop));
	ref->insert( table_t::value_type(addr, weak_session(s)) );

	return session(s);
}

void session_pool_impl::step_timeout()
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
}


session_pool::session_pool(loop lo) :
	m_pimpl(new session_pool_impl(tcp_builder(), lo))
{
	MP_UTIL.start_timeout();
}

session_pool::session_pool(const builder& b, loop lo) :
	m_pimpl(new session_pool_impl(b, lo))
{
	MP_UTIL.start_timeout();
}

session_pool::session_pool(shared_session_pool pimpl) :
	m_pimpl(pimpl)
{
	MP_UTIL.start_timeout();
}

session_pool::~session_pool() { }

session session_pool::get_session(const address& addr)
	{ return m_pimpl->get_session(addr); }

const loop& session_pool::get_loop() const
	{ return const_cast<const session_pool_impl*>(m_pimpl.get())->get_loop(); }

loop session_pool::get_loop()
	{ return m_pimpl->get_loop(); }


}  // namespace rpc
}  // namespace msgpack

