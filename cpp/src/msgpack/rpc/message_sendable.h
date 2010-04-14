//
// msgpack::rpc::message_sendable - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_MESSAGE_SENDABLE_H__
#define MSGPACK_RPC_MESSAGE_SENDABLE_H__

#include "types.h"

namespace msgpack {
namespace rpc {


class message_sendable;
typedef mp::shared_ptr<message_sendable> shared_message_sendable;

class message_sendable {
public:
	message_sendable() { }
	virtual ~message_sendable() { }
	virtual void send_data(sbuffer* sbuf) = 0;
	virtual void send_data(vrefbuffer* vbuf, shared_zone life) = 0;
	virtual shared_message_sendable shared_from_this() = 0;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/message_sendable.h */

