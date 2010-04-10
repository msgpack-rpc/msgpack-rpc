//
// msgpack::rpc::dispatcher - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_DISPATCHER_H__
#define MSGPACK_RPC_DISPATCHER_H__

#include "request.h"
#include "types.h"

namespace msgpack {
namespace rpc {


class dispatcher {
public:
	virtual void dispatch(request req, auto_zone z) = 0;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/dispatcher.h */

