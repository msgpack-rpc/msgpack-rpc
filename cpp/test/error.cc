#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

int main(void)
{
	cclog::reset(new cclog_tty(cclog::TRACE, std::cout));

	// run server {
	rpc::server svr;

	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());

	svr.listen("0.0.0.0", 18811);

	svr.start(4);
	// }


	try {
		rpc::client cli("127.0.0.1", 16396);
		cli.call("add", 1, 2).get<int>();
	} catch(msgpack::rpc::connect_error& e) {
		std::cout << "ok: "<< e.what() << std::endl;
	}

	rpc::client cli("127.0.0.1", 18811);

	try {
		cli.call("sub", 2, 1).get<int>();
	} catch(msgpack::rpc::no_method_error& e) {
		std::cout << "ok: " << e.what() << std::endl;
	}

	try {
		cli.call("add", 1).get<int>();
	} catch(msgpack::rpc::argument_error& e) {
		std::cout << "ok: " << e.what() << std::endl;
	}

	try {
		cli.call("err").get<int>();
	} catch(msgpack::rpc::remote_error& e) {
		std::cout << "ok: " << e.what() << std::endl;
		std::cout << "error object: " << e.error() << std::endl;
		std::cout << "result object: " << e.result() << std::endl;
	}
}

