//
// msgpack::rpc::message - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_MESSAGE_H__
#define MSGPACK_RPC_MESSAGE_H__

#include <msgpack.hpp>
#include "option.h"

namespace msgpack {
namespace rpc {


typedef uint32_t msgid_t;
typedef uint32_t method_t;

typedef uint8_t message_type_t;
typedef uint8_t option_type_t;

static const message_type_t REQUEST  = 0;
static const message_type_t RESPONSE = 1;
static const message_type_t NOTIFY   = 2;
static const message_type_t SESSION  = 3;
static const message_type_t OPTION   = 4;
static const option_type_t OPT_DEFLATE = 0x01;

// FIXME
namespace error {
	static const int SESSION_LOST_ERROR   = 1;
	static const int TIMEOUT_ERROR        = 2;
	//static const int UNKNOWN_ERROR        = 3;
	//static const int PROTOCOL_ERROR       = 4;
	//static const int SERVER_ERROR         = 5;
}  // namespace error


struct msg_rpc {
	msg_rpc() { }

	message_type_t type;

	bool is_request()  const { return type == REQUEST;  }
	bool is_response() const { return type == RESPONSE; }
	bool is_notify()   const { return type == NOTIFY;   }
	bool is_option()   const { return type == OPTION;   }

	MSGPACK_DEFINE(type);
};

template <typename Method, typename Parameter>
struct msg_request {
	msg_request() :
		type(REQUEST),
		msgid(0) { }

	msg_request(
			Method method,
			typename msgpack::type::tuple_type<Parameter>::transparent_reference param,
			msgid_t msgid) :
		type(REQUEST),
		msgid(msgid),
		method(method),
		param(param) { }

	message_type_t type;
	msgid_t msgid;
	Method method;
	Parameter param;

	MSGPACK_DEFINE(type, msgid, method, param);
};

template <typename Result, typename Error>
struct msg_response {
	msg_response() :
		type(RESPONSE),
		msgid(0) { }

	msg_response(
			typename msgpack::type::tuple_type<Result>::transparent_reference result,
			typename msgpack::type::tuple_type<Error >::transparent_reference error,
			msgid_t msgid) :
		type(RESPONSE),
		msgid(msgid),
		error(error),
		result(result) { }

	message_type_t type;
	msgid_t msgid;
	Error error;
	Result result;

	MSGPACK_DEFINE(type, msgid, error, result);
};

template <typename Method, typename Parameter>
struct msg_notify {
	msg_notify() :
		type(NOTIFY) { }

	msg_notify(
			Method method,
			typename msgpack::type::tuple_type<Parameter>::transparent_reference param) :
		type(NOTIFY),
		method(method),
		param(param) { }

	message_type_t type;
	Method method;
	Parameter param;

	MSGPACK_DEFINE(type, method, param);
};


struct msg_option {
	msg_option() :
		type(OPTION) { }

	msg_option(
			const exchange_option& txopt,
			const exchange_option& rxopt) :
		type(OPTION),
		txopt(txopt),
		rxopt(rxopt) { }

	message_type_t type;
	exchange_option txopt;
	exchange_option rxopt;

	MSGPACK_DEFINE(type, txopt, rxopt);
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/message.h */

