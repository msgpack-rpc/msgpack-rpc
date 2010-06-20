//
// msgpack::rpc::loop_util - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_LOOP_UTIL_H__
#define MSGPACK_RPC_LOOP_UTIL_H__

#include "loop.h"

namespace msgpack {
namespace rpc {


template <typename MixIn>
class loop_util {
public:
	void start(size_t num)
		{ static_cast<MixIn*>(this)->get_loop()->start(num); }

	void run(size_t num)
		{ static_cast<MixIn*>(this)->get_loop()->run(num); }

	void run_once()
		{ static_cast<MixIn*>(this)->get_loop()->run_once(); }

	void end()
		{ static_cast<MixIn*>(this)->get_loop()->end(); }

	void join()
		{ static_cast<MixIn*>(this)->get_loop()->join(); }

	bool is_running() const
		{ return static_cast<MixIn*>(this)->get_loop()->is_running(); }

	bool is_end() const
		{ return static_cast<MixIn*>(this)->get_loop()->is_end(); }
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/loop_util.h */

