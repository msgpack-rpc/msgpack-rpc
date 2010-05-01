//
// msgpack::rpc::request - MessagePack-RPC for C++
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
#include "request_impl.h"

namespace msgpack {
namespace rpc {


session request::from()
{
	return m_pimpl->from();
}

object request::method()
{
	return m_pimpl->method();
}

object request::params()
{
	return m_pimpl->params();
}

bool request::is_active() const
{
	return m_pimpl->is_active();
}

uint32_t request::get_msgid() const
{
	return m_pimpl->msgid();
}

void request::send_data(sbuffer* sbuf)
{
	m_pimpl->send_data(sbuf);
}

void request::send_data(vrefbuffer* vbuf, shared_zone life)
{
	m_pimpl->send_data(vbuf, life);
}

auto_zone& request::zone()
{
	return m_pimpl->zone();
}


}  // namespace rpc
}  // namespace msgpack

