//
// msgpack::rpc::transport::unix - MessagePack-RPC for C++
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
#include "../transport/unix.h"
#include "cclog/cclog.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>
#include <vector>

#include <sys/un.h>

namespace msgpack {
namespace rpc {
namespace transport {
namespace unix {

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
	client_socket(int sock, shared_session s);
	~client_socket();

	void on_response(msgid_t msgid,
			object result, object error, auto_zone z);

private:
	shared_session m_session;

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
	client_transport(shared_session s, const address& addr, const unix_builder& b);
	~client_transport();

public:
	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

private:
	mp::shared_ptr<client_socket> m_sock;

private:
	client_transport();
	client_transport(const client_transport&);
};


class server_transport : public rpc::server_transport {
public:
	server_transport(const std::string& path, shared_server svr);
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


#ifndef MSGPACK_RPC_UNIX_SOCKET_BUFFER_SIZE
#define MSGPACK_RPC_UNIX_SOCKET_BUFFER_SIZE (64*1024)
#endif

#ifndef MSGPACK_RPC_UNIX_SOCKET_RESERVE_SIZE
#define MSGPACK_RPC_UNIX_SOCKET_RESERVE_SIZE (8*1024)
#endif

template <typename MixIn>
basic_socket<MixIn>::basic_socket(int fd, loop lo) :
	mp::wavy::handler(fd),
	m_pac(MSGPACK_RPC_UNIX_SOCKET_BUFFER_SIZE),
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

		m_pac.reserve_buffer(MSGPACK_RPC_UNIX_SOCKET_RESERVE_SIZE);

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


client_socket::client_socket(int sock, shared_session s) :
	basic_socket<client_socket>(sock, s->get_loop()),
	m_session(s) { }

client_socket::~client_socket() { }

void client_socket::on_response(msgid_t msgid,
			object result, object error, auto_zone z)
{
	m_session->on_response(
			msgid, result, error, z);
}


client_transport::client_transport(shared_session s, const address& addr, const unix_builder& b)
{
	int sock = ::socket(PF_LOCAL, SOCK_STREAM, 0);
	if(sock < 0) {
		throw mp::system_error(errno, "failed to open UNIX socket");
	}

	try {
		// FIXME UNIX addr
		char addrbuf[addr.addrlen()];
		addr.getaddr((sockaddr*)addrbuf);

		if(::connect(sock, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to connect UNIX socket");
		}

		m_sock = s->get_loop()->add_handler<client_socket>(sock, s);

	} catch(...) {
		::close(sock);
		throw;
	}
}

client_transport::~client_transport() { }

void client_transport::send_data(sbuffer* sbuf)
{
	m_sock->send_data(sbuf);
}

void client_transport::send_data(vrefbuffer* vbuf, shared_zone life)
{
	m_sock->send_data(vbuf, life);
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


server_transport::server_transport(const std::string& path, shared_server svr) :
	m_lsock(-1)
{
	// FIXME UNIX_MAX_PATH?
	//if(path.size() >= UNIX_MAX_PATH) {  // FIXME?
	//	throw std::runtime_error("path name too long"); // FIXME message
	//}

	struct sockaddr_un addrbuf;
	memset(&addrbuf, 0, sizeof(addrbuf));
	addrbuf.sun_family = AF_UNIX;
	//addrbuf.sun_len = path.size()+1; FIXME
	memcpy(addrbuf.sun_path, path.c_str(), path.size()+1);

	loop lo = svr->get_loop();

	m_lsock = lo->listen(
			PF_LOCAL, SOCK_STREAM, 0,
			(sockaddr*)&addrbuf, sizeof(addrbuf),
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

}  // namespace unix
}  // namespace transport


unix_builder::unix_builder() { }

unix_builder::~unix_builder() { }

std::auto_ptr<client_transport> unix_builder::build(shared_session s, const address& addr) const
{
	return std::auto_ptr<client_transport>(new transport::unix::client_transport(s, addr, *this));
}


unix_listener::unix_listener(const std::string& path) :
	m_path(path) { }

unix_listener::~unix_listener() { }

std::auto_ptr<server_transport> unix_listener::listen(shared_server svr) const
{
	return std::auto_ptr<server_transport>(new transport::unix::server_transport(m_path, svr));
}


}  // namespace rpc
}  // namespace msgpack

