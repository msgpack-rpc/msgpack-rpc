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
#include "../types.h"
#include "../transport/base.h"
#include "../transport/tcp.h"
#include "cclog/cclog.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>
#include <vector>

namespace msgpack {
namespace rpc {
namespace transport {
namespace tcp {

namespace {

using namespace mp::placeholders;


template <typename MixIn>
class basic_socket : public mp::wavy::handler, public message_sendable, public protocol_handler<MixIn> {
public:
	basic_socket(int fd, loop lo);
	~basic_socket();

	void on_read(mp::wavy::event& e);

	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

protected:
	unpacker m_pac;
	loop m_loop;
};


class client_transport;
class server_transport;


class client_socket : public basic_socket<client_socket> {
public:
	client_socket(int sock, client_transport* tran, shared_session s);
	~client_socket();

	void on_response(msgid_t msgid,
			object result, object error, auto_zone z);

private:
	client_transport* m_tran;
	shared_session m_session;  // has auto_ptr<client_transport>

private:
	client_socket();
	client_socket(const client_socket&);
};

class server_socket : public basic_socket<server_socket> {
public:
	server_socket(int sock, shared_server svr);
	~server_socket();

	void on_request(
			msgid_t msgid,
			object method, object params, auto_zone z);

	void on_notify(
			object method, object params, auto_zone z);

private:
	shared_server m_svr;

private:
	server_socket();
	server_socket(const server_socket&);
};


class client_transport : public rpc::client_transport {
public:
	client_transport(shared_session s, const address& addr, const tcp_builder& b);
	~client_transport();

public:
	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

private:
	typedef std::vector<client_socket*> sockpool_t;

	struct sync_t {
		sync_t() : sockpool_rr(0), connecting(0) { }
		sockpool_t sockpool;
		size_t sockpool_rr;
		unsigned int connecting;
		mp::wavy::xfer pending_xf;
	};

	typedef mp::sync<sync_t>::ref sync_ref;
	mp::sync<sync_t> m_sync;

	shared_session m_session;

	double m_connect_timeout;
	unsigned int m_reconnect_limit;

private:
	void try_connect(sync_ref& lk_ref);
	void connect_callback(int fd, int err, shared_session session_life);
	void on_connect(int fd, sync_ref& ref);
	void on_connect_failed(int fd, sync_ref& ref);

	friend class client_socket;
	void on_close(client_socket* sock);

private:
	client_transport();
	client_transport(const client_transport&);
};


class server_transport : public rpc::server_transport {
public:
	server_transport(const address& addr, shared_server svr);
	~server_transport();

public:
	void close();

	static void on_accept(loop lo, shared_server svr, int fd, int err);

private:
	int m_lsock;

private:
	server_transport();
	server_transport(const server_transport&);
};


#ifndef MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE
#define MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE (64*1024)
#endif

#ifndef MSGPACK_RPC_TCP_SOCKET_RESERVE_SIZE
#define MSGPACK_RPC_TCP_SOCKET_RESERVE_SIZE (8*1024)
#endif

template <typename MixIn>
basic_socket<MixIn>::basic_socket(int fd, loop lo) :
	mp::wavy::handler(fd),
	m_pac(MSGPACK_RPC_TCP_SOCKET_BUFFER_SIZE),
	m_loop(lo) { }

template <typename MixIn>
basic_socket<MixIn>::~basic_socket() { }

template <typename MixIn>
void basic_socket<MixIn>::on_read(mp::wavy::event& e)
try {
	while(true) {
		if(m_pac.execute()) {
			object msg = m_pac.data();
			LOG_TRACE("obj received: ",msg);
			auto_zone z( m_pac.release_zone() );
			m_pac.reset();

			e.more();  // FIXME
			protocol_handler<MixIn>::on_message(msg, z);
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

template <typename MixIn>
void basic_socket<MixIn>::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	m_loop->writev(fd(), vbuf->vector(), vbuf->vector_size(), z);
}

template <typename MixIn>
void basic_socket<MixIn>::send_data(msgpack::sbuffer* sbuf)
{
	m_loop->write(fd(), sbuf->data(), sbuf->size(), &::free, sbuf->data());
	sbuf->release();
}


client_socket::client_socket(int sock, client_transport* tran, shared_session s) :
	basic_socket<client_socket>(sock, s->get_loop()),
	m_tran(tran), m_session(s)
{
	// FIXME
}

client_socket::~client_socket()
{
	// FIXME
	m_tran->on_close(this);
}

void client_socket::on_response(msgid_t msgid,
			object result, object error, auto_zone z)
{
	m_session->on_response(
			msgid, result, error, z);
}


client_transport::client_transport(shared_session s, const address& addr, const tcp_builder& b) :
	m_session(s),
	m_connect_timeout(b.connect_timeout()),
	m_reconnect_limit(b.reconnect_limit())
{ }

client_transport::~client_transport() { }

void client_transport::on_connect(int fd, sync_ref& ref)
{
	LOG_DEBUG("connect success to ",m_session->get_address()," fd=",fd);

	mp::shared_ptr<client_socket> cs =
		m_session->get_loop()->add_handler<client_socket>(
				fd, this, m_session);

	ref->sockpool.push_back(cs.get());

	m_session->get_loop()->commit(fd, &ref->pending_xf);
	ref->pending_xf.clear();

	ref->connecting = 0;
}

void client_transport::on_connect_failed(int err, sync_ref& ref)
{
	if(ref->connecting < m_reconnect_limit) {
		LOG_WARN("connect to ",m_session->get_address()," failed, retrying: ",strerror(err));
		try_connect(ref);
		++ref->connecting;
		return;
	}

	LOG_WARN("connect to ",m_session->get_address()," failed, abort: ",strerror(err));
	ref->connecting = 0;
	ref->pending_xf.clear();

	ref.reset();
	m_session->on_connect_failed();
}

void client_transport::connect_callback(int fd, int err, shared_session session_life)
{
	sync_ref ref(m_sync);

	if(fd >= 0) {
		// success
		try {
			on_connect(fd, ref);
			return;
		} catch (...) {
			::close(fd);
			LOG_WARN("attach failed or send pending failed");
		}
	}

	on_connect_failed(err, ref);
}

void client_transport::try_connect(sync_ref& lk_ref)
{
	address addr = m_session->get_address();
	//// FIXME
	//if(addr.get_port() == 0) {
	//	return;  // FIXME throw?
	//}

	LOG_INFO("connecting to ",addr);

	char addrbuf[addr.get_addrlen()];
	addr.get_addr((sockaddr*)addrbuf);

	m_session->get_loop()->connect(
			PF_INET, SOCK_STREAM, 0,
			(sockaddr*)addrbuf, sizeof(addrbuf),
			m_connect_timeout,
			mp::bind(
				&client_transport::connect_callback, this,
				_1, _2, m_session));
}

void client_transport::on_close(client_socket* sock)
{
	sync_ref ref(m_sync);
	sockpool_t::iterator found = std::find(
			ref->sockpool.begin(), ref->sockpool.end(), sock);
	if(found != ref->sockpool.end()) {
		ref->sockpool.erase(found);
	}
}

void client_transport::send_data(sbuffer* sbuf)
{
	sync_ref ref(m_sync);
	if(ref->sockpool.empty()) {
		if(ref->connecting == 0) {
			try_connect(ref);
			ref->connecting = 1;
		}
		ref->pending_xf.push_write(sbuf->data(), sbuf->size());
		ref->pending_xf.push_finalize(&::free, sbuf->data());
		sbuf->release();
	} else {
		// FIXME pesudo connecting load balance
		client_socket* sock = ref->sockpool[0];
		sock->send_data(sbuf);
	}
}

void client_transport::send_data(vrefbuffer* vbuf, shared_zone life)
{
	sync_ref ref(m_sync);
	if(ref->sockpool.empty()) {
		if(ref->connecting == 0) {
			try_connect(ref);
			ref->connecting = 1;
		}
		ref->pending_xf.push_writev(vbuf->vector(), vbuf->vector_size());
		ref->pending_xf.push_finalize(life);
	} else {
		// FIXME pesudo connecting load balance
		client_socket* sock = ref->sockpool[0];
		sock->send_data(vbuf, life);
	}
}


server_socket::server_socket(int sock, shared_server svr) :
	basic_socket<server_socket>(sock, svr->get_loop()),
	m_svr(svr) { }

server_socket::~server_socket() { }

void server_socket::on_request(
		msgid_t msgid,
		object method, object params, auto_zone z)
{
	m_svr->on_request(shared_self<server_socket>(),
			msgid, method, params, z);
}

void server_socket::on_notify(
		object method, object params, auto_zone z)
{
	m_svr->on_notify(method, params, z);
}


server_transport::server_transport(const address& addr, shared_server svr) :
	m_lsock(-1)
{
	char addrbuf[addr.get_addrlen()];
	addr.get_addr((sockaddr*)addrbuf);

	loop lo = svr->get_loop();

	m_lsock = lo->listen(
			PF_INET, SOCK_STREAM, 0,
			(sockaddr*)addrbuf, sizeof(addrbuf),
			mp::bind(&server_transport::on_accept, lo, svr, _1, _2));
}

server_transport::~server_transport()
{
	close();
}

void server_transport::close()
{
	if(m_lsock >= 0) {
		::close(m_lsock);  // FIXME shutdown? invalidate fd without releasing fd number
		m_lsock = -1;
	}
}

void server_transport::on_accept(loop lo, shared_server svr, int fd, int err)
{
	// FIXME
	if(fd < 0) {
		LOG_ERROR("accept failed");
		return;
	}
	LOG_TRACE("accepted fd=",fd);

	try {
		lo->add_handler<server_socket>(fd, svr);
	} catch (...) {
		::close(fd);
		throw;
	}
}


}  // noname namespace

}  // namespace tcp
}  // namespace transport


tcp_builder::tcp_builder() :
	m_connect_timeout(5.0),  // FIXME default connect timeout
	m_reconnect_limit(3)     // FIXME default connect reconnect limit
{ }

tcp_builder::~tcp_builder() { }

std::auto_ptr<client_transport> tcp_builder::build(shared_session s, const address& addr) const
{
	return std::auto_ptr<client_transport>(new transport::tcp::client_transport(s, addr, *this));
}


tcp_listener::tcp_listener(const std::string& host, uint16_t port) :
	m_addr(ip_address(host, port)) { }

tcp_listener::tcp_listener(const address& addr) :
	m_addr(addr) { }

tcp_listener::~tcp_listener() { }

std::auto_ptr<server_transport> tcp_listener::listen(shared_server svr) const
{
	return std::auto_ptr<server_transport>(new transport::tcp::server_transport(m_addr, svr));
}


}  // namespace rpc
}  // namespace msgpack

