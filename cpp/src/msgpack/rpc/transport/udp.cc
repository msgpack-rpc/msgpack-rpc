//
// msgpack::rpc::transport::udp - MessagePack-RPC for C++
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
#include "../transport/udp.h"
#include "cclog/cclog.h"
#include <mp/functional.h>
#include <mp/sync.h>
#include <mp/utilize.h>
#include <vector>

namespace msgpack {
namespace rpc {
namespace transport {
namespace udp {

namespace {

using namespace mp::placeholders;


#ifndef MSGPACK_RPC_UDP_SOCKET_BUFFER_SIZE
#define MSGPACK_RPC_UDP_SOCKET_BUFFER_SIZE (64*1024)
#endif


template <typename MixIn>
class basic_socket : public mp::wavy::handler {
public:
	basic_socket(int fd, loop lo);
	~basic_socket();

	void on_read(mp::wavy::event& e);

	void on_message(object msg, auto_zone z,
			const sockaddr* addrbuf, socklen_t addrlen);

	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);


	void on_request(
			msgid_t msgid,
			object method, object params, auto_zone z,
			const sockaddr* addrbuf, socklen_t addrlen)
	{
		throw msgpack::type_error();  // FIXME
	}

	void on_notify(
			object method, object params, auto_zone z)
	{
		throw msgpack::type_error();  // FIXME
	}

	void on_response(msgid_t msgid,
			object result, object error, auto_zone z)
	{
		throw msgpack::type_error();  // FIXME
	}

protected:
	loop m_loop;
	char m_buffer[MSGPACK_RPC_UDP_SOCKET_BUFFER_SIZE];
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

	void close();

	void on_request(
			msgid_t msgid,
			object method, object params, auto_zone z,
			const sockaddr* addrbuf, socklen_t addrlen);

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
	client_transport(shared_session s, const address& addr, const udp_builder& b);
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
	server_transport(const address& addr, shared_server svr);
	~server_transport();

public:
	void close();

private:
	mp::shared_ptr<server_socket> m_sock;

private:
	server_transport();
	server_transport(const server_transport&);
};


class response_sender : public message_sendable {
public:
	response_sender(int sock, const sockaddr* addrbuf, socklen_t addrlen);
	~response_sender();

	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

private:
	int m_sock;
	struct sockaddr_storage m_addrbuf;
	size_t m_addrlen;

private:
	response_sender();
	response_sender(const response_sender&);
};


template <typename MixIn>
basic_socket<MixIn>::basic_socket(int fd, loop lo) :
	mp::wavy::handler(fd),
	m_loop(lo) { }

template <typename MixIn>
basic_socket<MixIn>::~basic_socket() { }

template <typename MixIn>
void basic_socket<MixIn>::on_read(mp::wavy::event& e)
try {
	msgpack::unpacked result;
	struct sockaddr_storage addrbuf;
	socklen_t addrlen;

	while(true) {
		addrlen = sizeof(addrbuf);

		ssize_t rl = ::recvfrom(ident(), m_buffer, MSGPACK_RPC_UDP_SOCKET_BUFFER_SIZE,
				0, (sockaddr*)&addrbuf, &addrlen);
		if(rl <= 0) {
			if(rl == 0) { throw mp::system_error(errno, "connection closed"); }
			if(errno == EAGAIN || errno == EINTR) { return; }
			else { throw mp::system_error(errno, "read error"); }
		}

		e.next();

		msgpack::unpack(&result, m_buffer, rl);

		on_message(result.get(), result.zone(), (struct sockaddr*)&addrbuf, addrlen);
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
void basic_socket<MixIn>::on_message(object msg, auto_zone z,
		const sockaddr* addrbuf, socklen_t addrlen)
{
	msg_rpc rpc;
	msg.convert(&rpc);

	switch(rpc.type) {
	case REQUEST: {
			msg_request<object, object> req;
			msg.convert(&req);
			static_cast<MixIn*>(this)->on_request(
					req.msgid, req.method, req.param, z,
					addrbuf, addrlen);
		}
		break;

	case RESPONSE: {
			msg_response<object, object> res;
			msg.convert(&res);
			static_cast<MixIn*>(this)->on_response(
					res.msgid, res.result, res.error, z);
		}
		break;

	case NOTIFY: {
			msg_notify<object, object> req;
			msg.convert(&req);
			static_cast<MixIn*>(this)->on_notify(
					req.method, req.param, z);
		}
		break;

	default:
		throw msgpack::type_error();
	}
}


template <typename MixIn>
void basic_socket<MixIn>::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	// FIXME?
	m_loop->writev(fd(), vbuf->vector(), vbuf->vector_size(), z);
}

template <typename MixIn>
void basic_socket<MixIn>::send_data(msgpack::sbuffer* sbuf)
{
	// FIXME?
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


client_transport::client_transport(shared_session s, const address& addr, const udp_builder& b)
{
	int sock = ::socket(PF_INET, SOCK_DGRAM, 0);
	if(sock < 0) {
		throw mp::system_error(errno, "failed to open UDP socket");
	}

	try {
		char addrbuf[addr.addrlen()];
		addr.getaddr((sockaddr*)addrbuf);

		if(::connect(sock, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to connect UDP socket");
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


response_sender::response_sender(int sock, const sockaddr* addrbuf, socklen_t addrlen) :
	m_sock(sock)
{
	if(addrlen > sizeof(m_addrbuf)) {
		throw std::runtime_error("invalid sizeof address");
	}
	memcpy((void*)&m_addrbuf, (const void*)addrbuf, addrlen);
	m_addrlen = addrlen;
}

response_sender::~response_sender() { }

void response_sender::send_data(sbuffer* sbuf)
{
	// FIXME m_sock is non-blocking mode
	sendto(m_sock, sbuf->data(), sbuf->size(), 0,
			(struct sockaddr*)&m_addrbuf, m_addrlen);
	// FIXME check errno == EAGAIN
}

void response_sender::send_data(vrefbuffer* vbuf, shared_zone life)
{
	// FIXME m_sock is non-blocking mode
	sendto(m_sock, vbuf->vector(), vbuf->vector_size(), 0,
			(struct sockaddr*)&m_addrbuf, m_addrlen);
	// FIXME check errno == EAGAIN
}


server_socket::server_socket(int sock, shared_server svr) :
	basic_socket<server_socket>(sock, svr->get_loop()),
	m_svr(svr) { }

server_socket::~server_socket() { }

void server_socket::on_request(
		msgid_t msgid,
		object method, object params, auto_zone z,
		const sockaddr* addrbuf, socklen_t addrlen)
{
	mp::shared_ptr<message_sendable> ms(new response_sender(fd(), addrbuf, addrlen));
	m_svr->on_request(ms, msgid, method, params, z);
}

void server_socket::on_notify(
		object method, object params, auto_zone z)
{
	m_svr->on_notify(method, params, z);
}

void server_socket::close()
{
	::close(fd());  // FIXME shutdown? invalidate fd without releasing fd number
}


server_transport::server_transport(const address& addr, shared_server svr)
{
	char addrbuf[addr.addrlen()];
	addr.getaddr((sockaddr*)addrbuf);

	int sock = ::socket(PF_INET, SOCK_DGRAM, 0);
	if(sock < 0) {
		throw mp::system_error(errno, "failed to open UDP socket");
	}

	try {
		char addrbuf[addr.addrlen()];
		addr.getaddr((sockaddr*)addrbuf);

		if(::bind(sock, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to bind UDP socket");
		}

		m_sock = svr->get_loop()->add_handler<server_socket>(sock, svr);

	} catch(...) {
		::close(sock);
		throw;
	}
}

server_transport::~server_transport()
{
	close();
}

void server_transport::close()
{
	m_sock->close();  // FIXME
}

}  // noname namespace

}  // namespace udp
}  // namespace transport


udp_builder::udp_builder() { }

udp_builder::~udp_builder() { }

std::auto_ptr<client_transport> udp_builder::build(shared_session s, const address& addr) const
{
	return std::auto_ptr<client_transport>(new transport::udp::client_transport(s, addr, *this));
}


udp_listener::udp_listener(const std::string& host, uint16_t port) :
	m_addr(host, port) { }

udp_listener::udp_listener(const address& addr) :
	m_addr(addr) { }

udp_listener::~udp_listener() { }

std::auto_ptr<server_transport> udp_listener::listen(shared_server svr) const
{
	return std::auto_ptr<server_transport>(new transport::udp::server_transport(m_addr, svr));
}


}  // namespace rpc
}  // namespace msgpack

