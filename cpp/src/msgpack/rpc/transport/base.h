//
// msgpack::rpc::transport::base - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_BASE_H__
#define MSGPACK_RPC_TRANSPORT_BASE_H__

#include "../types.h"
#include "../protocol.h"
#include "../session_impl.h"
#include "../server_impl.h"
#include "../transport_impl.h"
#include "cclog/cclog.h"

namespace msgpack {
namespace rpc {
namespace transport {


#ifndef MSGPACK_RPC_STREAM_BUFFER_SIZE
#define MSGPACK_RPC_STREAM_BUFFER_SIZE (64*1024)
#endif

#ifndef MSGPACK_RPC_STREAM_RESERVE_SIZE
#define MSGPACK_RPC_STREAM_RESERVE_SIZE (8*1024)
#endif

#ifndef MSGPACK_RPC_DGRAM_BUFFER_SIZE
#define MSGPACK_RPC_DGRAM_BUFFER_SIZE (64*1024)
#endif

class closed_exception { };


template <typename MixIn>
class stream_handler : public mp::wavy::handler, public message_sendable {
public:
	stream_handler(int fd, loop lo);
	~stream_handler();

	void remove_handler();

	mp::shared_ptr<message_sendable> get_response_sender();

	// message_sendable interface
	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

	// mp::wavy::handler interface
	void on_read(mp::wavy::event& e);

	void on_message(object msg, auto_zone z);

	void on_request(msgid_t msgid,
			object method, object params, auto_zone z)
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
	unpacker m_pac;
	loop m_loop;
};

template <typename MixIn>
class dgram_handler : public mp::wavy::handler {
public:
	dgram_handler(int fd, loop lo);
	~dgram_handler();

	void remove_handler();

	mp::shared_ptr<message_sendable> get_response_sender(
			const sockaddr* addrbuf, socklen_t addrlen);

	// message_sendable interface
	class response_sender;
	void send_data(const sockaddr* addrbuf, socklen_t addrlen, sbuffer* sbuf);
	void send_data(const sockaddr* addrbuf, socklen_t addrlen, vrefbuffer* vbuf, shared_zone z);

	// connected dgram
	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone life);

	// mp::wavy::handler interface
	void on_read(mp::wavy::event& e);

	void on_message(object msg, auto_zone z,
			const sockaddr* addrbuf, socklen_t addrlen);

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

private:
	loop m_loop;
	char m_buffer[MSGPACK_RPC_DGRAM_BUFFER_SIZE];
};


template <typename MixIn>
inline stream_handler<MixIn>::stream_handler(int fd, loop lo) :
	mp::wavy::handler(fd),
	m_pac(MSGPACK_RPC_STREAM_BUFFER_SIZE),
	m_loop(lo) { }

template <typename MixIn>
inline stream_handler<MixIn>::~stream_handler() { }

template <typename MixIn>
inline void stream_handler<MixIn>::remove_handler()
{
	m_loop->remove_handler(fd());
}


template <typename MixIn>
inline dgram_handler<MixIn>::dgram_handler(int fd, loop lo) :
	mp::wavy::handler(fd),
	m_loop(lo) { }

template <typename MixIn>
inline dgram_handler<MixIn>::~dgram_handler() { }

template <typename MixIn>
inline void dgram_handler<MixIn>::remove_handler()
{
	m_loop->remove_handler(fd());
}


template <typename MixIn>
inline void stream_handler<MixIn>::send_data(msgpack::sbuffer* sbuf)
{
	m_loop->write(fd(), sbuf->data(), sbuf->size(), &::free, sbuf->data());
	sbuf->release();
}

template <typename MixIn>
inline void stream_handler<MixIn>::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	m_loop->writev(fd(), vbuf->vector(), vbuf->vector_size(), z);
}


template <typename MixIn>
inline void dgram_handler<MixIn>::send_data(const sockaddr* addrbuf, socklen_t addrlen, sbuffer* sbuf)
{
	// FIXME fd is non-blocking mode
	// FIXME check errno == EAGAIN
	sendto(fd(), sbuf->data(), sbuf->size(), 0, addrbuf, addrlen);
}

template <typename MixIn>
inline void dgram_handler<MixIn>::send_data(const sockaddr* addrbuf, socklen_t addrlen, vrefbuffer* vbuf, shared_zone z)
{
	// FIXME fd is non-blocking mode
	// FIXME check errno == EAGAIN
	struct msghdr msg;
	memset(&msg, 0, sizeof(msg));
	msg.msg_name = const_cast<sockaddr*>(addrbuf);
	msg.msg_namelen = addrlen;
	msg.msg_iov = const_cast<struct iovec*>(vbuf->vector());
	msg.msg_iovlen = vbuf->vector_size();
	sendmsg(fd(), &msg, 0);
}

template <typename MixIn>
inline void dgram_handler<MixIn>::send_data(msgpack::sbuffer* sbuf)
{
	// FIXME?
	m_loop->write(fd(), sbuf->data(), sbuf->size(), &::free, sbuf->data());
	sbuf->release();
}

template <typename MixIn>
inline void dgram_handler<MixIn>::send_data(msgpack::vrefbuffer* vbuf, shared_zone z)
{
	// FIXME?
	m_loop->writev(fd(), vbuf->vector(), vbuf->vector_size(), z);
}


template <typename MixIn>
void stream_handler<MixIn>::on_message(object msg, auto_zone z)
{
	msg_rpc rpc;
	msg.convert(&rpc);

	switch(rpc.type) {
	case REQUEST: {
			msg_request<object, object> req;
			msg.convert(&req);
			static_cast<MixIn*>(this)->on_request(
					req.msgid, req.method, req.param, z);
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
void dgram_handler<MixIn>::on_message(object msg, auto_zone z,
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
void stream_handler<MixIn>::on_read(mp::wavy::event& e)
try {
	while(true) {
		if(m_pac.execute()) {
			object msg = m_pac.data();
			LOG_TRACE("obj received: ",msg);
			auto_zone z( m_pac.release_zone() );
			m_pac.reset();

			e.more();  // FIXME next()?
			stream_handler<MixIn>::on_message(msg, z);
			return;
		}

		m_pac.reserve_buffer(MSGPACK_RPC_STREAM_RESERVE_SIZE);

		ssize_t rl = ::read(ident(), m_pac.buffer(), m_pac.buffer_capacity());
		if(rl <= 0) {
			if(rl == 0) { throw closed_exception(); }
			if(errno == EAGAIN || errno == EINTR) { return; }
			else { throw mp::system_error(errno, "read error"); }
		}

		m_pac.buffer_consumed(rl);
	}

} catch(msgpack::type_error& ex) {
	LOG_WARN("connection: type error");
	e.remove();
	return;
} catch(closed_exception& ex) {
	e.remove();
	return;
} catch(std::exception& ex) {
	LOG_WARN("connection: ", ex.what());
	e.remove();
	return;
} catch(...) {
	LOG_WARN("connection: unknown error");
	e.remove();
	return;
}


template <typename MixIn>
void dgram_handler<MixIn>::on_read(mp::wavy::event& e)
try {
	msgpack::unpacked result;
	struct sockaddr_storage addrbuf;
	socklen_t addrlen;

	while(true) {
		addrlen = sizeof(addrbuf);

		ssize_t rl = ::recvfrom(ident(), m_buffer, MSGPACK_RPC_DGRAM_BUFFER_SIZE,
				0, (sockaddr*)&addrbuf, &addrlen);
		if(rl <= 0) {
			if(rl == 0) { throw closed_exception(); }
			if(errno == EAGAIN || errno == EINTR) { return; }
			else { throw mp::system_error(errno, "read error"); }
		}

		e.next();  // FIXME more()?

		msgpack::unpack(&result, m_buffer, rl);

		dgram_handler<MixIn>::on_message(result.get(), result.zone(), (struct sockaddr*)&addrbuf, addrlen);
	}

} catch(msgpack::type_error& ex) {
	LOG_ERROR("connection: type error");
	e.remove();
	return;
} catch(closed_exception& ex) {
	e.remove();
	return;
} catch(std::exception& ex) {
	LOG_ERROR("connection: ", ex.what());
	e.remove();
	return;
} catch(...) {
	LOG_ERROR("connection: unknown error");
	e.remove();
	return;
}


template <typename MixIn>
mp::shared_ptr<message_sendable> inline stream_handler<MixIn>::get_response_sender()
{
	return shared_self<stream_handler<MixIn> >();
}


template <typename MixIn>
class dgram_handler<MixIn>::response_sender : public message_sendable {
public:
	response_sender(mp::shared_ptr<dgram_handler<MixIn> > handler,
		   	const sockaddr* addrbuf, socklen_t addrlen);

	~response_sender();

	void send_data(sbuffer* sbuf);
	void send_data(vrefbuffer* vbuf, shared_zone z);

private:
	mp::shared_ptr<dgram_handler<MixIn> > m_handler;
	struct sockaddr_storage m_addrbuf;
	size_t m_addrlen;

private:
	response_sender();
	response_sender(const response_sender&);
};

template <typename MixIn>
dgram_handler<MixIn>::response_sender::response_sender(
		mp::shared_ptr<dgram_handler<MixIn> > handler,
		const sockaddr* addrbuf, socklen_t addrlen) :
	m_handler(handler),
	m_addrlen(addrlen)
{
	if(addrlen > sizeof(m_addrbuf)) {
		throw std::runtime_error("invalid sizeof address");
	}
	memcpy((void*)&m_addrbuf, (const void*)addrbuf, addrlen);
}

template <typename MixIn>
dgram_handler<MixIn>::response_sender::~response_sender() { }

template <typename MixIn>
void dgram_handler<MixIn>::response_sender::send_data(sbuffer* sbuf)
{
	m_handler->send_data((struct sockaddr*)&m_addrbuf, m_addrlen, sbuf);
}

template <typename MixIn>
void dgram_handler<MixIn>::response_sender::send_data(vrefbuffer* vbuf, shared_zone z)
{
	m_handler->send_data((struct sockaddr*)&m_addrbuf, m_addrlen, vbuf, z);
}

template <typename MixIn>
inline mp::shared_ptr<message_sendable> dgram_handler<MixIn>::get_response_sender(
		const sockaddr* addrbuf, socklen_t addrlen)
{
	return mp::shared_ptr<message_sendable>(
			new response_sender(
				shared_self<dgram_handler<MixIn> >(),
				addrbuf, addrlen));
}


}  // namespace transport
}  // namespace rpc
}  // namespace msgpack

#endif /* transport/base.h */

