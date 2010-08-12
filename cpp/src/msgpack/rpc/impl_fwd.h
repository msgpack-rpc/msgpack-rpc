//
// msgpack::rpc::impl - Cluster Communication Framework
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
#ifndef MSGPACK_RPC_IMPL_H__
#define MSGPACK_RPC_IMPL_H__

#include <mp/memory.h>

namespace msgpack {
namespace rpc {


class future;
class future_impl;
typedef mp::shared_ptr<future_impl> shared_future;

class session;
class session_impl;
typedef mp::shared_ptr<session_impl> shared_session;
typedef mp::weak_ptr<session_impl> weak_session;

class session_pool;
class session_pool_impl;
typedef mp::shared_ptr<session_pool_impl> shared_session_pool;

class server;
class server_impl;
typedef mp::shared_ptr<server_impl> shared_server;
typedef mp::weak_ptr<server_impl> weak_server;

class request;
class request_impl;
typedef mp::shared_ptr<request_impl> shared_request;


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/impl.h */

