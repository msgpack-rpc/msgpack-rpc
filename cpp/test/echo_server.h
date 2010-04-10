#ifndef ECHO_SERVER_H_
#define ECHO_SERVER_H_

#include <msgpack/rpc/server.h>

namespace rpc {
	using namespace msgpack;
	using namespace msgpack::rpc;
}  // namespace rpc

class myecho : public rpc::dispatcher {
public:
	typedef rpc::request request;
	typedef rpc::auto_zone auto_zone;

	void dispatch(request req, auto_zone z)
	{
		std::string method = req.method().as<std::string>();
		if(method == "add") {
			add(req, z);
			return;
		} else if(method == "echo") {
			echo(req, z);
			return;
		} else {
			throw rpc::type_error();
		}
	}

	void add(request req, auto_zone z)
	{
		msgpack::type::tuple<int, int> params;
		req.params().convert(&params);
		req.result(params.get<0>() + params.get<1>());
	}

	void echo(request req, auto_zone z)
	{
		req.result(req.params());
	}
};


#endif /* echo_server.h */

