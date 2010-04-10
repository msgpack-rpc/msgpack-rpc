//
// msgpack::rpc::server - MessagePack-RPC for C++
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
#ifndef MSGPACK_RPC_SERVER_H__
#define MSGPACK_RPC_SERVER_H__

#include "session_pool.h"
#include "dispatcher.h"
#include <mp/utilize.h>
#include <string>

namespace msgpack {
namespace rpc {


class server : public session_pool {
public:
	server(loop lo = loop());
	~server();

	void serve(dispatcher* dp);

	void listen(address addr);

	void listen(const std::string& host, uint16_t port)
		{ listen(address(host, port)); }

	void close();

private:
	dispatcher* m_dp;
	std::auto_ptr<transport::listener> m_listener;

	MP_UTILIZE;

protected:
	shared_session create_session(const address& addr);

private:
	server(const server&);
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/server.h */

