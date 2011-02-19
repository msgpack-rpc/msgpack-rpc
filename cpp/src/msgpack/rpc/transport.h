//
// msgpack::rpc::transport - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_TRANSPORT_H__
#define MSGPACK_RPC_TRANSPORT_H__

#include "loop.h"
#include "impl_fwd.h"
#include "address.h"

namespace msgpack {
namespace rpc {


class server_transport;
class client_transport;


class builder {
public:
	builder() : m_timeout(30) { }
	virtual ~builder() { }

	virtual std::auto_ptr<client_transport> build(
			session_impl* s, const address& addr) const = 0;

	template <typename IMPL>
	class base;

	virtual std::auto_ptr<builder> copy() const = 0;

public:
	void set_timeout(unsigned int sec)
		{ m_timeout = sec; }

	unsigned int get_timeout() const
		{ return m_timeout; }

private:
	unsigned int m_timeout;
};

class listener {
public:
	virtual ~listener() { }

	virtual std::auto_ptr<server_transport> listen(
			server_impl* svr) const = 0;

	template <typename IMPL>
	class base;

	virtual std::auto_ptr<listener> copy() const = 0;
};


template <typename IMPL>
class builder::base : public builder {
public:
	std::auto_ptr<builder> copy() const
	{
		return std::auto_ptr<builder>(new IMPL(*static_cast<const IMPL*>(this)));
	}
};

template <typename IMPL>
class listener::base : public listener {
public:
	std::auto_ptr<listener> copy() const
	{
		return std::auto_ptr<listener>(new IMPL(*static_cast<const IMPL*>(this)));
	}
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/transport.h */

