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

class client_transport;
class server_transport;


class client_socket : public dgram_handler<client_socket> {
public:
	client_socket(int sock, session_impl* s);
	~client_socket();

	void on_response(msgid_t msgid,
			object result, object error, auto_zone z);

private:
	weak_session m_session;

private:
	client_socket();
	client_socket(const client_socket&);
};


class client_transport : public rpc::client_transport {
public:
	client_transport(session_impl* s, const address& addr, const udp_builder& b);
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


client_socket::client_socket(int sock, session_impl* s) :
	dgram_handler<client_socket>(sock, s->get_loop()),
	m_session(s->shared_from_this()) { }

client_socket::~client_socket() { }

void client_socket::on_response(msgid_t msgid,
			object result, object error, auto_zone z)
{
	shared_session s = m_session.lock();
	if(!s) {
		throw closed_exception();
	}
	s->on_response(
			msgid, result, error, z);
}


client_transport::client_transport(session_impl* s, const address& addr, const udp_builder& b)
{
	int sock = ::socket(PF_INET, SOCK_DGRAM, 0);
	if(sock < 0) {
		throw mp::system_error(errno, "failed to open UDP socket");
	}

	try {
		char addrbuf[addr.get_addrlen()];
		addr.get_addr((sockaddr*)addrbuf);

		if(::connect(sock, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to connect UDP socket");
		}

		m_sock = s->get_loop()->add_handler<client_socket>(sock, s);

	} catch(...) {
		::close(sock);
		throw;
	}
}

client_transport::~client_transport()
{
	m_sock->remove_handler();
}

void client_transport::send_data(sbuffer* sbuf)
{
	m_sock->send_data(sbuf);
}

void client_transport::send_data(vrefbuffer* vbuf, shared_zone life)
{
	m_sock->send_data(vbuf, life);
}


class server_socket : public dgram_handler<server_socket> {
public:
	server_socket(int sock, shared_server svr);
	~server_socket();

	void on_request(
			msgid_t msgid,
			object method, object params, auto_zone z,
			const sockaddr* addrbuf, socklen_t addrlen);

	void on_notify(
			object method, object params, auto_zone z);

private:
	weak_server m_svr;

private:
	server_socket();
	server_socket(const server_socket&);
};


class server_transport : public rpc::server_transport {
public:
	server_transport(server_impl* svr, const address& addr);
	~server_transport();

	void close();

private:
	mp::shared_ptr<server_socket> m_sock;

private:
	server_transport();
	server_transport(const server_transport&);
};


server_socket::server_socket(int sock, shared_server svr) :
	dgram_handler<server_socket>(sock, svr->get_loop()),
	m_svr(svr) { }

server_socket::~server_socket() { }

void server_socket::on_request(
		msgid_t msgid,
		object method, object params, auto_zone z,
		const sockaddr* addrbuf, socklen_t addrlen)
{
	shared_server svr = m_svr.lock();
	if(!svr) {
		throw closed_exception();
	}
	svr->on_request(get_response_sender(addrbuf, addrlen),
			msgid, method, params, z);
}

void server_socket::on_notify(
		object method, object params, auto_zone z)
{
	shared_server svr = m_svr.lock();
	if(!svr) {
		throw closed_exception();
	}
	svr->on_notify(method, params, z);
}


server_transport::server_transport(server_impl* svr, const address& addr)
{
	int sock = ::socket(PF_INET, SOCK_DGRAM, 0);
	if(sock < 0) {
		throw mp::system_error(errno, "failed to open UDP socket");
	}

	try {
		char addrbuf[addr.get_addrlen()];
		addr.get_addr((sockaddr*)addrbuf);

		if(::bind(sock, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to bind UDP socket");
		}

		m_sock = svr->get_loop()->add_handler<server_socket>(
				sock,
				mp::static_pointer_cast<server_impl>(svr->shared_from_this())
				);

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
	m_sock->remove_handler();
}


}  // noname namespace

}  // namespace udp
}  // namespace transport


udp_builder::udp_builder() { }

udp_builder::~udp_builder() { }

std::auto_ptr<client_transport> udp_builder::build(session_impl* s, const address& addr) const
{
	return std::auto_ptr<client_transport>(new transport::udp::client_transport(s, addr, *this));
}


udp_listener::udp_listener(const std::string& host, uint16_t port) :
	m_addr(ip_address(host, port)) { }

udp_listener::udp_listener(const address& addr) :
	m_addr(addr) { }

udp_listener::~udp_listener() { }

std::auto_ptr<server_transport> udp_listener::listen(server_impl* svr) const
{
	return std::auto_ptr<server_transport>(
			new transport::udp::server_transport(svr, m_addr));
}


}  // namespace rpc
}  // namespace msgpack

