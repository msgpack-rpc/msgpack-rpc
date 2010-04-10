//
// msgpack::rpc::session_impl - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_SESSION_IMPL_H__
#define MSGPACK_RPC_SESSION_IMPL_H__

#include "session.h"
#include "reqtable.h"
#include "message.h"
#include "dispatcher.h"
#include "message_sendable.h"
#include "option.h"
#include "impl_fwd.h"

namespace msgpack {
namespace rpc {


class session_impl : public mp::enable_shared_from_this<session_impl> {
public:
	session_impl(const address& to_address,
			const transport_option& topt,
			const address& self_address,
			dispatcher* dp, loop lo);

	~session_impl();

	const address& get_address() const
	{
		return m_addr;
	}

	const address& get_self_address() const
	{
		return m_self_addr;
	}

	loop get_loop()
	{
		return m_loop;
	}

	loop& get_loop_ref()
	{
		return m_loop;
	}

	void set_timeout(unsigned int sec)
	{
		m_timeout = sec;
	}

	unsigned int get_timeout() const
	{
		return m_timeout;
	}

public:
	future send_request_impl(msgid_t msgid, vrefbuffer* vbuf, auto_zone life, option opt);

	future send_request_impl(msgid_t msgid, sbuffer* sbuf, option opt);

public:
	void on_message(
			message_sendable* ms,
			object msg, auto_zone z);

	void on_response(
			message_sendable* ms, msgid_t msgid,
			object result, object error, auto_zone z);

	void on_request(
			message_sendable* ms, msgid_t msgid,
			object method, object param, auto_zone z);

	void on_notify(
			message_sendable* ms,
			object method, object param, auto_zone z);

	void step_timeout();

private:
	address m_addr;
	address m_self_addr;

	transport_option m_dtopt;
	shared_transport m_dtran;
	// FIXME std::vector<shared_transport>

	dispatcher* m_dp;

	loop m_loop;

	reqtable m_reqtable;

	unsigned int m_timeout;

private:
	transport* get_transport(option opt);

private:
	session_impl();
	session_impl(const session_impl&);
};


inline loop& session::get_loop_ref()
{
	return m_pimpl->get_loop_ref();
}


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/session_impl.h */

