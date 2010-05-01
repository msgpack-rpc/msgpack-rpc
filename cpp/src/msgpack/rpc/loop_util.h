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
#include <mp/functional.h>

namespace msgpack {
namespace rpc {


class loop_util {
public:
	loop_util(loop lo) :
		m_loop(lo) { }

	~loop_util() { }

	loop get_loop() { return m_loop; }

	void start(size_t num)  { m_loop->start(num); }
	void run(size_t num)    { m_loop->run(num);   }
	void run_once()         { m_loop->run_once(); }
	void end()              { m_loop->end();      }
	void join()             { m_loop->join();     }
	bool is_running() const { return m_loop->is_running(); }
	bool is_end()     const { return m_loop->is_end();     }

protected:
	loop m_loop;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/loop_util.h */

