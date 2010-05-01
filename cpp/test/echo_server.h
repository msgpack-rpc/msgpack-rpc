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

	void dispatch(request req)
	try {
		std::string method = req.method().as<std::string>();

		if(method == "add") {
			add(req);
			return;
		} else if(method == "echo") {
			echo(req);
			return;
		} else {
			throw rpc::type_error();
		}

	} catch (std::exception& e) {
		req.error(std::string(e.what()));
	}

	void add(request req)
	{
		msgpack::type::tuple<int, int> params;
		req.params().convert(&params);
		req.result(params.get<0>() + params.get<1>());
	}

	void echo(request req)
	{
		req.result(req.params());
	}
};


#endif /* echo_server.h */

