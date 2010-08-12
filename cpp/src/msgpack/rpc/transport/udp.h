//
// msgpack::rpc::transport::udp - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_UDP_H__
#define MSGPACK_RPC_TRANSPORT_UDP_H__

#include "../transport.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>

namespace msgpack {
namespace rpc {


class udp_builder : public builder::base<udp_builder> {
public:
	udp_builder();
	~udp_builder();

	std::auto_ptr<client_transport> build(session_impl* s, const address& addr) const;
};


class udp_listener : public listener::base<udp_listener> {
public:
	udp_listener(const std::string& host, uint16_t port);
	udp_listener(const address& addr);

	~udp_listener();

	std::auto_ptr<server_transport> listen(server_impl* svr) const;

private:
	address m_addr;

private:
	udp_listener();
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport/udp.h */

