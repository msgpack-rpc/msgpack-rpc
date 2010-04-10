//
// msgpack::rpc::transport::tcp - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_TCP_H__
#define MSGPACK_RPC_TRANSPORT_TCP_H__

#include "transport/base.h"
#include "transport/listener.h"
#include "session_impl.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>

#include <mp/wavy.h>

namespace msgpack {
namespace rpc {
namespace transport {


class tcp : public transport::base, public mp::enable_shared_from_this<tcp> {
public:
	tcp(session_impl* s, const transport_option& topt);
	~tcp();

	// message_sendable interface
	void send_data(msgpack::vrefbuffer* vbuf, auto_zone z);
	void send_data(msgpack::sbuffer* sbuf);
	shared_message_sendable shared_from_this();

public:
	class listener;

private:
	class socket;
	class active_socket;
	class passive_socket;

	typedef mp::shared_ptr<tcp> shared_tcp_transport;

	typedef std::vector<socket*> sockpool_t;

	struct sync_t {
		sync_t() : sockpool_rr(0), connecting(0) { }
		sockpool_t sockpool;
		size_t sockpool_rr;
		unsigned int connecting;
		mp::wavy::xfer pending_xf;
	};

	typedef mp::sync<sync_t>::ref sync_ref;
	mp::sync<sync_t> m_sync;

	MP_UTILIZE;

private:
	void on_close(socket* sock);
	friend class active_socket;

protected:
	double m_connect_timeout;
	unsigned int m_reconnect_limit;

private:
	tcp();
	tcp(const tcp&);
};


class tcp::listener : public transport::listener {
public:
	listener(
			int socket_family, int socket_type, int protocol,
			const sockaddr* addr, socklen_t addrlen,
			loop lo,
			mp::function<shared_session ()> create_session);

	~listener();

	void close();

private:
	int m_lsock;
	loop m_loop;
	mp::function<shared_session ()> m_create_session;

	MP_UTILIZE;

private:
	listener();
	listener(const listener&);
};


}  // namespace transport
}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport/tcp.h */

