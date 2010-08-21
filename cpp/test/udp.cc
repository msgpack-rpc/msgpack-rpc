#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <msgpack/rpc/transport/udp.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

int main(void)
{
	cclog::reset(new cclog_tty(cclog::TRACE, std::cout));

	// run server {
	rpc::server svr;

	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());

	svr.listen( msgpack::rpc::udp_listener("0.0.0.0", 18811) );

	svr.start(4);
	// }


	// create client
	rpc::client cli(msgpack::rpc::udp_builder(), msgpack::rpc::ip_address("127.0.0.1", 18811));

	// call
	std::string msg("MessagePack-RPC");
	std::string ret = cli.call("echo", msg).get<std::string>();

	std::cout << "call: echo(\"MessagePack-RPC\") = " << ret << std::endl;

	return 0;
}

