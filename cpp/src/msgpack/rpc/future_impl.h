//
// msgpack::rpc::future - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_FUTURE_IMPL_H__
#define MSGPACK_RPC_FUTURE_IMPL_H__

#include "future.h"
#include "session_impl.h"
#include <mp/pthread.h>
#include <mp/memory.h>

namespace msgpack {
namespace rpc {


class future_impl : public mp::enable_shared_from_this<future_impl> {
public:
	future_impl(shared_session s, loop lo) :
		m_session(s),
		m_loop(lo),
		m_timeout(s->get_timeout())  // FIXME
	{ }

	~future_impl() { }

	object get_impl();

	void join();
	void wait();
	void recv();

	object result() const
	{
		return m_result;
	}

	object error() const
	{
		return m_error;
	}

	auto_zone& zone() { return m_zone; }

	void attach_callback(callback_t func);

	void set_result(object result, object error, auto_zone z);

	bool step_timeout()
	{
		if(m_timeout == 0) {
			return true;
		} else {
			m_timeout--;
			return false;
		}
	}

private:
	shared_session m_session;
	loop m_loop;

	unsigned int m_timeout;
	callback_t m_callback;

	object m_result;
	object m_error;
	auto_zone m_zone;

	mp::pthread_mutex m_mutex;
	mp::pthread_cond m_cond;

private:
	future_impl();
	future_impl(const future_impl&);
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/future_impl.h */

