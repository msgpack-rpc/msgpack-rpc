//
// msgpack::rpc::transport - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_IMPL_H__
#define MSGPACK_RPC_TRANSPORT_IMPL_H__

#include "transport.h"
#include "message_sendable.h"

namespace msgpack {
namespace rpc {


class client_transport : public message_sendable {
public:
	client_transport() { }
	// FIXME close
};


class server_transport {
public:
	server_transport() { }
	virtual ~server_transport() { }
	virtual void close() = 0;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport_impl.h */

