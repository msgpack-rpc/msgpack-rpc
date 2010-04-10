//
// msgpack::rpc::option - MessagePack-RPC for C++
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
#ifndef MSGPACK_RPC_OPTION_H__
#define MSGPACK_RPC_OPTION_H__

#include <msgpack.hpp>

namespace msgpack {
namespace rpc {


struct exchange_option {
	exchange_option(uint32_t flags = 0) :
		m_flags(flags) { }

	// FIXME include-loop

	//void set_deflate(bool val = true)
	//{ val ? m_flags |= OPT_DEFLATE : m_flags &= ~OPT_DEFLATE; }

	//bool is_deflate() const
	//{ return m_flags & OPT_DEFLATE != 0; }

	void reset()
	{
		m_flags = 0;
	}

	bool operator== (const exchange_option& o) const
	{
		return m_flags == o.m_flags;
	}

private:
	uint32_t m_flags;

public:
	template <typename Packer>
	void msgpack_pack(Packer& pk) const
	{ pk.pack(m_flags); }

	void msgpack_unpack(msgpack::object o)
	{ o.convert(&m_flags); }
};


struct stream_option {
	stream_option(
			exchange_option tx = exchange_option(),
			exchange_option rx = exchange_option()) :
		txopt(tx), rxopt(rx) { }

	exchange_option txopt;
	exchange_option rxopt;

	bool is_default() const
	{
		return txopt == exchange_option() &&
			rxopt == exchange_option();
	}

	bool operator== (const stream_option& o) const
	{
		return txopt == o.txopt && rxopt == o.rxopt;
	}

	MSGPACK_DEFINE(txopt, rxopt);
};


struct transport_option : public stream_option {
	transport_option() { }
};


struct option : public transport_option {
	option() { }
};


}  // namespace rpc
}  // namespace msgpack

#endif /* msgpack/rpc/option.h */

