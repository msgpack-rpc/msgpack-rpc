//
// msgpack::rpc::transport::unix - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_UNIX_H__
#define MSGPACK_RPC_TRANSPORT_UNIX_H__

#include "../transport.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>

namespace msgpack {
namespace rpc {


class unix_builder : public builder::base<unix_builder> {
public:
	unix_builder();
	~unix_builder();

	std::auto_ptr<client_transport> build(session_impl* s, const address& addr) const;
};


class unix_listener : public listener::base<unix_listener> {
public:
	unix_listener(const std::string& path);
	unix_listener(const address& addr);

	~unix_listener();

	std::auto_ptr<server_transport> listen(server_impl* svr) const;

private:
	address m_addr;

private:
	unix_listener();
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport/unix.h */

