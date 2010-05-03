//
// msgpack::rpc::exception - MessagePack-RPC for C++
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
#ifndef MSGPACK_RPC_EXCEPTION_H__
#define MSGPACK_RPC_EXCEPTION_H__

#include "types.h"
#include "future.h"
#include <stdexcept>

namespace msgpack {
namespace rpc {


struct rpc_error : public std::runtime_error {
	rpc_error(const std::string& msg) :
		std::runtime_error(msg) {}
};


struct timeout_error : rpc_error {
	timeout_error() :
		rpc_error("request timed out") {}

	timeout_error(const std::string& msg) :
		rpc_error(msg) {}
};

struct connect_error : timeout_error {
	connect_error() :
		timeout_error("connect failed") {}

	connect_error(const std::string& msg) :
		timeout_error(msg) {}
};


struct call_error : rpc_error {
	call_error(const std::string& msg) :
		rpc_error(msg) {}
};

struct no_method_error : call_error {
	no_method_error() :
		call_error("method not found") {}

	no_method_error(const std::string& msg) :
		call_error(msg) {}
};

struct argument_error : call_error {
	argument_error() :
		call_error("argument mismatch") {}

	argument_error(const std::string& msg) :
		call_error(msg) {}
};


struct remote_error : rpc_error {
	remote_error(object err, future f) :
		rpc_error("remote error"), m_future(f) {}

	remote_error(const std::string& msg, future f) :
		rpc_error(msg), m_future(f) {}

	~remote_error() throw() try { } catch(...) { }

	object result() const { return m_future.result(); }
	object error() const { return m_future.error(); }

	template <typename T>
	T result_as() const { return m_future.result_as<T>(); }

	template <typename T>
	T error_as() const { return m_future.error_as<T>(); }

	auto_zone& zone() { return m_future.zone(); }
	const auto_zone& zone() const { return m_future.zone(); }

private:
	future m_future;
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/exception.h */

