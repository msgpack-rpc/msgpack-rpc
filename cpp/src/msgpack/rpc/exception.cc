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
#include "exception_impl.h"
#include "protocol.h"
#include <sstream>
#include <string.h>

namespace msgpack {
namespace rpc {


static const char* TIMEOUT_ERROR_PTR = "request timed out";
static const char* CONNECT_ERROR_PTR = "connect failed";


const msgpack::object TIMEOUT_ERROR( msgpack::type::raw_ref(
			TIMEOUT_ERROR_PTR, strlen(TIMEOUT_ERROR_PTR)
			) );

const msgpack::object CONNECT_ERROR( msgpack::type::raw_ref(
			CONNECT_ERROR_PTR, strlen(CONNECT_ERROR_PTR)
			) );


void throw_exception(future_impl* f)
{
	object err = f->error();

	if(err.type == msgpack::type::RAW &&
			err.via.raw.ptr == TIMEOUT_ERROR_PTR) {
		throw timeout_error();

	} else if(err.type == msgpack::type::RAW &&
			err.via.raw.ptr == CONNECT_ERROR_PTR) {
		throw connect_error();

	} else if(err.type == msgpack::type::POSITIVE_INTEGER &&
			err.via.u64 == NO_METHOD_ERROR) {
		throw no_method_error();

	} else if(err.type == msgpack::type::POSITIVE_INTEGER &&
			err.via.u64 == ARGUMENT_ERROR) {
		throw argument_error();

	} else {
		std::ostringstream os;
		os << "remote error: ";
		os << err;
		throw remote_error(os.str(), future(f->shared_from_this()));
	}
}


}  // namespace rpc
}  // namespace msgpack

