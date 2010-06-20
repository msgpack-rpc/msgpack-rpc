//
// msgpack::rpc::reqtable - Cluster Communication Framework
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
#ifndef MSGPACK_RPC_REQTABLE_H__
#define MSGPACK_RPC_REQTABLE_H__

#include "protocol.h"
#include "impl_fwd.h"
#include <vector>
#include <mp/unordered_map.h>
#include <mp/pthread.h>

namespace msgpack {
namespace rpc {


class reqtable {
public:
	reqtable() { }
	~reqtable() { }

public:
	void insert(msgid_t msgid, shared_future f);

	shared_future take(msgid_t msgid);

	void take_all(std::vector<shared_future>* all);

	void step_timeout(std::vector<shared_future>* timedout);

private:
	mp::pthread_mutex m_mutex;
	typedef mp::unordered_map<msgid_t, shared_future> map_t;
	map_t m_map;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/reqtable.h */

