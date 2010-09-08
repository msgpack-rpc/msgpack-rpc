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

#ifdef unix
#undef unix
#endif

namespace msgpack {
namespace rpc {
namespace transport {
namespace unix {

namespace {

using namespace mp::placeholders;


class client_transport;
class server_transport;


class client_socket : public stream_handler<client_socket> {
public:
	client_socket(int fd, session_impl* s);
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
	client_transport(session_impl* s, const address& addr, const unix_builder& b);
	~client_transport();

public:
	void send_data(sbuffer* sbuf);
	void send_data(auto_vreflife vbuf);

private:
	session_impl* m_session;
	mp::shared_ptr<client_socket> m_sock;

private:
	client_transport();
	client_transport(const client_transport&);
};


client_socket::client_socket(int fd, session_impl* s) :
	stream_handler<client_socket>(fd, s->get_loop_ref()),
	m_session(s->shared_from_this()) { }

client_socket::~client_socket() { }

void client_socket::on_response(msgid_t msgid,
			object result, object error, auto_zone z)
{
	shared_session s = m_session.lock();
	if(!s) {
		throw closed_exception();
	}
	s->on_response(msgid, result, error, z);
}


client_transport::client_transport(session_impl* s, const address& addr, const unix_builder& b) :
	m_session(s)
{
	int fd = ::socket(PF_LOCAL, SOCK_STREAM, 0);
	if(fd < 0) {
		throw mp::system_error(errno, "failed to open UNIX socket");
	}

	try {
		// FIXME UNIX addr
		char addrbuf[addr.get_addrlen()];
		addr.get_addr((sockaddr*)addrbuf);

		if(::connect(fd, (sockaddr*)addrbuf, sizeof(addrbuf)) < 0) {
			throw mp::system_error(errno, "failed to connect UNIX socket");
		}

		m_sock = m_session->get_loop_ref()->add_handler<client_socket>(fd, m_session);

	} catch(...) {
		::close(fd);
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

void client_transport::send_data(auto_vreflife vbuf)
{
	m_sock->send_data(vbuf);
}


class server_socket : public stream_handler<server_socket> {
public:
	server_socket(int fd, shared_server svr);
	~server_socket();

	void on_request(
			msgid_t msgid,
			object method, object params, auto_zone z);

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

	static void on_accept(int fd, int err, weak_server wsvr);

private:
	int m_lsock;
	loop m_loop;

private:
	server_transport();
	server_transport(const server_transport&);
};


server_socket::server_socket(int fd, shared_server svr) :
	stream_handler<server_socket>(fd, svr->get_loop_ref()),
	m_svr(svr) { }

server_socket::~server_socket() { }

void server_socket::on_request(
		msgid_t msgid,
		object method, object params, auto_zone z)
{
	shared_server svr = m_svr.lock();
	if(!svr) {
		throw closed_exception();
	}
	svr->on_request(get_response_sender(), msgid, method, params, z);
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


server_transport::server_transport(server_impl* svr, const address& addr) :
	m_lsock(-1), m_loop(svr->get_loop_ref())
{
	char addrbuf[addr.get_addrlen()];
	addr.get_addr((sockaddr*)addrbuf);

	m_lsock = m_loop->listen(
			PF_LOCAL, SOCK_STREAM, 0,
			(sockaddr*)&addrbuf, sizeof(addrbuf),
			mp::bind(
				&server_transport::on_accept,
				_1, _2,
				weak_server(mp::static_pointer_cast<server_impl>(svr->shared_from_this()))
				));
}

server_transport::~server_transport()
{
	close();
}

void server_transport::close()
{
	if(m_lsock >= 0) {
		m_loop->remove_handler(m_lsock);
		m_lsock = -1;
	}
	// FIXME m_sock->remove_handler();
}

void server_transport::on_accept(int fd, int err, weak_server wsvr)
{
	shared_server svr = wsvr.lock();
	if(!svr) {
		return;
	}

	// FIXME
	if(fd < 0) {
		LOG_ERROR("accept failed");
		return;
	}
	LOG_TRACE("accepted fd=",fd);

	try {
		svr->get_loop_ref()->add_handler<server_socket>(fd, svr);
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

std::auto_ptr<client_transport> unix_builder::build(
		session_impl* s, const address& addr) const
{
	return std::auto_ptr<client_transport>(
			new transport::unix::client_transport(s, addr, *this));
}


unix_listener::unix_listener(const std::string& path) :
	m_addr(path_address(path)) { }

unix_listener::unix_listener(const address& addr) :
	m_addr(addr) { }

unix_listener::~unix_listener() { }

std::auto_ptr<server_transport> unix_listener::listen(server_impl* svr) const
{
	return std::auto_ptr<server_transport>(
			new transport::unix::server_transport(svr, m_addr));
}


}  // namespace rpc
}  // namespace msgpack

