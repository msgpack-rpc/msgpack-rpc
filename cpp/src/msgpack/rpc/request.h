//
// msgpack::rpc::request - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_REQUEST_H__
#define MSGPACK_RPC_REQUEST_H__

#include "protocol.h"
#include "impl_fwd.h"
#include "types.h"

namespace msgpack {
namespace rpc {


class request {
public:
	request(shared_request pimpl) : m_pimpl(pimpl) { }
	~request() { }

	object method();
	object params();
	auto_zone& zone();

	template <typename Result>
	void result(Result res);

	template <typename Result>
	void result(Result res, auto_zone z);

	template <typename Result>
	void result(Result res, shared_zone z);

	void result_nil();

	template <typename Error>
	void error(Error err);

	template <typename Error>
	void error(Error err, auto_zone z);

	template <typename Error>
	void error(Error err, shared_zone z);

	template <typename Error, typename Result>
	void error(Error err, Result res);

	template <typename Error, typename Result>
	void error(Error err, Result res, auto_zone z);

	template <typename Error, typename Result>
	void error(Error err, Result res, shared_zone z);

	template <typename Result>
	class type;

private:
	template <typename Result, typename Error>
	void call(Result& res, Error& err);

	template <typename Result, typename Error>
	void call(Result& res, Error& err, shared_zone z);

private:
	bool is_sent() const;  // FIXME public?

	uint32_t get_msgid() const;
	void send_data(sbuffer* sbuf);
	void send_data(std::auto_ptr<with_shared_zone<vrefbuffer> > vbuf);

private:
	shared_request m_pimpl;
};


template <typename Result>
class request::type : public request {
public:
	type(const request& req) : request(req) { }
	~type() { }

	void result(Result res)
		{ request::result(res); }

	void result(Result res, auto_zone z)
		{ request::result(res, z); }

	void result(Result res, shared_zone z)
		{ request::result(res, z); }
};

template <>
class request::type<void> : public request {
public:
	type(const request& req) : request(req) { }
	~type() { }

	void result()
		{ request::result_nil(); }
};


template <typename Result, typename Error>
inline void request::call(Result& res, Error& err)
{
	if(is_sent()) { return; }

	msgpack::sbuffer sbuf;
	msg_response<Result&, Error> msgres(res, err, get_msgid());
	msgpack::pack(sbuf, msgres);

	send_data(&sbuf);
}

template <typename Result, typename Error>
inline void request::call(Result& res, Error& err, shared_zone z)
{
	if(is_sent()) { return; }

	std::auto_ptr<with_shared_zone<vrefbuffer> > vbuf(
			new with_shared_zone<vrefbuffer>(z));
	msg_response<Result&, Error> msgres(res, err, get_msgid());
	msgpack::pack(*vbuf, msgres);

	send_data(vbuf);
}

template <typename Result>
void request::result(Result res)
{
	msgpack::type::nil err;
	call(res, err);
}

template <typename Result>
void request::result(Result res, auto_zone z)
{
	msgpack::type::nil err;
	shared_zone sz(z.release());
	call(res, err, sz);
}

template <typename Result>
void request::result(Result res, shared_zone z)
{
	msgpack::type::nil err;
	call(res, err, z);
}

inline void request::result_nil()
{
	msgpack::type::nil res;
	msgpack::type::nil err;
	call(res, err);
}

template <typename Error>
void request::error(Error err)
{
	msgpack::type::nil res;
	call(res, err);
}

template <typename Error>
void request::error(Error err, auto_zone z)
{
	msgpack::type::nil res;
	shared_zone sz(z.release());
	call(res, err, sz);
}

template <typename Error>
void request::error(Error err, shared_zone z)
{
	msgpack::type::nil res;
	call(res, err, z);
}

template <typename Error, typename Result>
void request::error(Error err, Result res)
{
	call(res, err);
}

template <typename Error, typename Result>
void request::error(Error err, Result res, auto_zone z)
{
	call(res, err, z);
}

template <typename Error, typename Result>
void request::error(Error err, Result res, shared_zone z)
{
	call(res, err, z);
}


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/request.h */

