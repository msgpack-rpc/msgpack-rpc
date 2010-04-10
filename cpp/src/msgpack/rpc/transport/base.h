//
// msgpack::rpc::transport::base - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_BASE_H__
#define MSGPACK_RPC_TRANSPORT_BASE_H__

#include "message_sendable.h"
#include "loop.h"
#include "session_impl.h"
#include "impl_fwd.h"
#include "option.h"

namespace msgpack {
namespace rpc {
namespace transport {


class base : public message_sendable {
public:
	base(session_impl* s, const transport_option& topt) :
		m_session(s), m_option(topt) { }

	virtual ~base() { }

	//// message_sendable interface
	//void send_data(msgpack::vrefbuffer* vbuf, auto_zone z);
	//void send_data(msgpack::sbuffer* sbuf);
	//shared_message_sendable shared_from_this();

protected:
	const address& get_address() const
	{
		return m_session->get_address();
	}

	loop get_loop()
	{
		return m_session->get_loop();
	}

	session_impl& get_session()
	{
		return *m_session;
	}

private:
	session_impl* m_session;
	transport_option m_option;
};


}  // namespace transport
}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport/base.h */

