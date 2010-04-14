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
#include "transport/tcp.h"
#include "cclog/cclog.h"

namespace msgpack {
namespace rpc {
namespace transport {


class tcp::socket : public mp::wavy::handler, public message_sendable {
public:
	socket(int fd, loop lo, shared_session s);
	virtual ~socket();

public:
	// message_sendable interface
	void send_data(msgpack::vrefbuffer* vbuf, shared_zone z);
	void send_data(msgpack::sbuffer* sbuf);
	shared_message_sendable shared_from_this();

public:
	// from wavy: readable notification
	void on_read(mp::wavy::event& e);
	virtual void on_message(object msg, auto_zone z, mp::wavy::event& e);

protected:
	msgpack::unpacker m_pac;
	loop m_loop;
	shared_session m_session;

private:
	socket();
	socket(const socket&);
};


class tcp::active_socket : public socket {
public:
	active_socket(int fd, shared_tcp_transport tran, shared_session s);
	~active_socket();  // on_close

	// FIXME deflate send_pending

private:
	MP_UTILIZE;

private:
	shared_tcp_transport m_tran;  // weak?

private:
	active_socket();
	active_socket(const active_socket&);
};


class tcp::passive_socket : public socket {
public:
	passive_socket(int fd, shared_session s);

	passive_socket(int fd, loop lo);
	void rebind(shared_session s);

	~passive_socket();

public:
	// FIXME
	//void on_message(object msg, auto_zone z, mp::wavy::event& e);

private:
	stream_option m_option;   // transport option?

	MP_UTILIZE;

private:
	passive_socket();
	passive_socket(const passive_socket&);
};


using namespace mp::placeholders;


MP_UTIL_DEF(tcp) {
	void try_connect(sync_ref& lk_ref);
	void connect_callback(int fd, int err, shared_session session_life);
};


tcp::tcp(session_impl* s, const transport_option& topt) :
	base(s, topt),
	m_connect_timeout(5.0),  // FIXME default connect timeout
	m_reconnect_limit(3)     // FIXME default connect reconnect limit
{
	// FIXME initmsg
	// FIXME deflate
}

tcp::~tcp()
{
	// FIXME
}


void MP_UTIL_IMPL(tcp)::try_connect(sync_ref& lk_ref)
{
	address addr = get_address();
	if(!addr.connectable()) {
		return;  // FIXME throw?
	}

	LOG_INFO("connecting to ",addr);

	char addrbuf[addr.addrlen()];
	addr.getaddr((sockaddr*)addrbuf);

	get_loop()->connect(
			AF_INET, SOCK_STREAM, 0,
			(sockaddr*)addrbuf, addr.addrlen(),
			m_connect_timeout,
			mp::bind(
				&MP_UTIL_IMPL(tcp)::connect_callback, &MP_UTIL,
				_1, _2, get_session().shared_from_this()));
}

void MP_UTIL_IMPL(tcp)::connect_callback(
		int fd, int err, shared_session session_life)
{
	sync_ref ref(m_sync);

	if(fd >= 0) {
		// success
		try {
			LOG_DEBUG("connect success to ",get_address()," fd=",fd);

			mp::shared_ptr<active_socket> as =
				get_loop()->add_handler<active_socket>(
						fd, mp::enable_shared_from_this<tcp>::shared_from_this(), session_life);

			ref->sockpool.push_back(as.get());

			// FIXME initmsg

			get_loop()->commit(fd, &ref->pending_xf);
			ref->pending_xf.clear();
			// FIXME deflate

			ref->connecting = 0;

			return;

		} catch (...) {
			::close(fd);
			LOG_WARN("attach failed or send pending failed");
		}
	}

	if(ref->connecting < m_reconnect_limit) {
		LOG_WARN("connect to ",get_address()," failed, retrying: ",strerror(err));
		try_connect(ref);
		++ref->connecting;

	} else {
		LOG_WARN("connect to ",get_address()," failed, abort: ",strerror(err));
		ref->connecting = 0;
		ref->pending_xf.clear();
		//get_session().transport_lost(); / connect_failed  // FIXME
	}
}

void tcp::on_close(socket* sock)
{
	sync_ref ref(m_sync);
	sockpool_t::iterator found = std::find(
			ref->sockpool.begin(), ref->sockpool.end(), sock);
	if(found != ref->sockpool.end()) {
		ref->sockpool.erase(found);
	}
}

void tcp::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	sync_ref ref(m_sync);
	if(ref->sockpool.empty()) {
		if(ref->connecting == 0) {
			MP_UTIL.try_connect(ref);
			ref->connecting = 1;
		}
		// FIXME deflate
		ref->pending_xf.push_writev(vbuf->vector(), vbuf->vector_size());
		ref->pending_xf.push_finalize(z);
	} else {
		// FIXME pesudo connecting load balance
		socket* sock = ref->sockpool[0];
		sock->send_data(vbuf, z);
	}
}

void tcp::send_data(msgpack::sbuffer* sbuf)
{
	sync_ref ref(m_sync);
	if(ref->sockpool.empty()) {
		if(ref->connecting == 0) {
			MP_UTIL.try_connect(ref);
			ref->connecting = 1;
		}
		// FIXME deflate
		ref->pending_xf.push_write(sbuf->data(), sbuf->size());
		ref->pending_xf.push_finalize(&::free, sbuf->data());
		sbuf->release();
	} else {
		// FIXME pesudo connecting load balance
		socket* sock = ref->sockpool[0];
		sock->send_data(sbuf);
	}
}

shared_message_sendable tcp::shared_from_this()
{
	return mp::static_pointer_cast<message_sendable>(
			mp::enable_shared_from_this<tcp>::shared_from_this());
}

shared_message_sendable tcp::socket::shared_from_this()
{
	return mp::static_pointer_cast<message_sendable>(
			mp::wavy::handler::shared_self<socket>());
}


#ifndef MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE
#define MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE (64*1024)
#endif

#ifndef MSGPACK_RPC_TCP_SOCKET_RESERVE_SIZE
#define MSGPACK_RPC_TCP_SOCKET_RESERVE_SIZE (8*1024)
#endif

tcp::socket::socket(int fd, loop lo, shared_session s) :
	mp::wavy::handler(fd),
	m_pac(MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE),
	m_loop(lo),
	m_session(s)
{ }

tcp::socket::~socket() { }

void tcp::socket::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	m_loop->writev(fd(), vbuf->vector(), vbuf->vector_size(), z);
}

void tcp::socket::send_data(msgpack::sbuffer* sbuf)
{
	m_loop->write(fd(), sbuf->data(), sbuf->size(), &::free, sbuf->data());
	sbuf->release();
}


void tcp::socket::on_message(object msg, auto_zone z, mp::wavy::event& e)
{
	if(!m_session) { return; }
	e.more();   // FIXME m_pacにデータが残っているときだけmore()。そうでなければnext()
	m_session->on_message(this, msg, z);
}

void tcp::socket::on_read(mp::wavy::event& e)
try {
	while(true) {
		if(m_pac.execute()) {
			object msg = m_pac.data();
			LOG_TRACE("obj received: ",msg);
			auto_zone z( m_pac.release_zone() );
			m_pac.reset();
			on_message(msg, z, e);
			return;
		}

		m_pac.reserve_buffer(MSGPACK_RPC_TCP_SOCKET_RESERVE_SIZE);

		ssize_t rl = ::read(ident(), m_pac.buffer(), m_pac.buffer_capacity());
		if(rl <= 0) {
			if(rl == 0) { throw mp::system_error(errno, "connection closed"); }
			if(errno == EAGAIN || errno == EINTR) { return; }
			else { throw mp::system_error(errno, "read error"); }
		}

		m_pac.buffer_consumed(rl);
	}

} catch(msgpack::type_error& e) {
	LOG_ERROR("connection: type error");
	throw;
} catch(std::exception& e) {
	LOG_WARN("connection: ", e.what());
	throw;
} catch(...) {
	LOG_ERROR("connection: unknown error");
	throw;
}


tcp::active_socket::active_socket(int fd, shared_tcp_transport tran, shared_session s) :
	socket(fd, s->get_loop(), s),
	m_tran(tran)
{
	// FIXME deflate
}

tcp::active_socket::~active_socket()
{
	// FIXME
	m_tran->on_close(this);
}


tcp::passive_socket::passive_socket(int fd, shared_session s) :
	socket(fd, s->get_loop(), s)
{
	// FIXME deflate
}

tcp::passive_socket::passive_socket(int fd, loop lo) :
	socket(fd, lo, shared_session())
{ }

void tcp::passive_socket::rebind(shared_session s)
{
	m_session = s;
}

tcp::passive_socket::~passive_socket()
{
	// FIXME
}


MP_UTIL_DEF(tcp::listener) {
	void accept_callback(int fd, int err);
};

tcp::listener::listener(
		int socket_family, int socket_type, int protocol,
		const sockaddr* addr, socklen_t addrlen,
		loop lo,
		mp::function<shared_session ()> create_session) :
	m_lsock(-1),
	m_loop(lo),
	m_create_session(create_session)
{
	m_lsock = m_loop->listen(
			socket_family, socket_type, protocol,
			addr, addrlen,
			mp::bind(&MP_UTIL_IMPL(listener)::accept_callback, &MP_UTIL, _1, _2));
}

tcp::listener::~listener()
{
	close();
}

void tcp::listener::close()
{
	if(m_lsock >= 0) {
		::close(m_lsock);
		m_lsock = -1;
	}
}

void MP_UTIL_IMPL(tcp::listener)::accept_callback(int fd, int err)
{
	// FIXME
	if(fd < 0) {
		LOG_ERROR("accept failed");
		return;
	}
	LOG_TRACE("accepted fd=",fd);

	try {
		m_loop->add_handler<passive_socket>(
				fd, m_create_session());
	} catch (...) {
		::close(fd);
		throw;
	}
}


}  // namespace transport
}  // namespace rpc
}  // namespace msgpack

