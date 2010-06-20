//
// msgpack::rpc::request_impl - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_REQUEST_IMPL_H__
#define MSGPACK_RPC_REQUEST_IMPL_H__

#include "message_sendable.h"
#include "request.h"

namespace msgpack {
namespace rpc {


class request_impl {
public:
	request_impl(shared_message_sendable ms, msgid_t msgid,
			object method, object params, auto_zone z) :
		m_ms(ms), m_msgid(msgid),
		m_method(method), m_params(params), m_zone(z) { }

	~request_impl() { }

	object method() { return m_method; }
	object params() { return m_params; }
	auto_zone& zone() { return m_zone; }
	msgid_t get_msgid() const { return m_msgid; }

public:
	bool is_sent()
	{
		return !m_ms;
	}

	void send_data(vrefbuffer* vbuf, shared_zone z)
	{
		shared_message_sendable ms = m_ms;
		if(!ms) { return; }
		ms->send_data(vbuf, z);
		m_ms.reset();
	}

	void send_data(sbuffer* sbuf)
	{
		shared_message_sendable ms = m_ms;
		if(!ms) { return; }
		ms->send_data(sbuf);
		m_ms.reset();
	}

private:
	shared_message_sendable m_ms;
	msgid_t m_msgid;

	object m_method;
	object m_params;
	auto_zone m_zone;

private:
	request_impl();
	request_impl(const request_impl&);
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/request_impl.h */

