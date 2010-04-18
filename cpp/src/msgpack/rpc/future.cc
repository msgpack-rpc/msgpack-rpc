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
#include "future_impl.h"
#include <cclog/cclog.h>

namespace msgpack {
namespace rpc {


void future_impl::wait()
{
	mp::pthread_scoped_lock lk(m_mutex);
	while(m_session) {
		m_cond.wait(m_mutex);
	}
}

void future_impl::recv()
{
	while(m_session) {
		m_loop->run_once();
	}
}

void future_impl::join()
{
	if(m_loop->is_running()) {
		wait();
	} else {
		recv();
	}
}


object future_impl::get_impl()
{
	join();
	if(!m_error.is_nil()) {
		// FIXME throw msgpack::rpc::remote_error
		throw std::runtime_error("remote error");
	}
	return m_result;
}

object future::get_impl()
{
	if(!m_pimpl) {
		// FIXME
		throw std::runtime_error("null future reference");
	}
	return m_pimpl->get_impl();
}


static void callback_real(
		callback_t callback, future f)
try {
	callback(f);
} catch (std::exception& e) {
	LOG_ERROR("response callback error: ",e.what());
} catch (...) {
	LOG_ERROR("response callback error: unknown error");
}

void future_impl::attach_callback(callback_t func)
{
	mp::pthread_scoped_lock lk(m_mutex);
	m_callback = func;
	if(!m_session) {
		lk.unlock();
		m_loop->submit(&callback_real, m_callback,
				future(shared_from_this()));
	}
}


void future_impl::set_result(object result, object error, auto_zone z)
{
	mp::pthread_scoped_lock lk(m_mutex);
	m_result = result;
	m_error = error;
	m_z = z;
	m_session.reset();

	m_cond.broadcast();

	if(m_callback) {
		lk.unlock();
		// FIXME submit?
		callback_real(m_callback, future(shared_from_this()));
	}
}


future& future::join()
{
	m_pimpl->join();
	return *this;
}

future& future::wait()
{
	m_pimpl->wait();
	return *this;
}

future& future::recv()
{
	m_pimpl->recv();
	return *this;
}

future& future::attach_callback(
		mp::function<void (future)> func)
{
	m_pimpl->attach_callback(func);
	return *this;
}


}  // namespace rpc
}  // namespace msgpack

