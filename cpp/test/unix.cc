#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <msgpack/rpc/transport/unix.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

int main(void)
{
	cclog::reset(new cclog_tty(cclog::TRACE, std::cout));

	// run server {
	rpc::server svr;

	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());

	unlink("./unix.sock");
	svr.listen( msgpack::rpc::unix_listener("./unix.sock") );

	svr.start(4);
	// }


	// create client
	rpc::client cli(msgpack::rpc::unix_builder(), msgpack::rpc::path_address("./unix.sock"));

	// call
	std::string msg("MessagePack-RPC");
	std::string ret = cli.call("echo", msg).get<std::string>();

	std::cout << "call: echo(\"MessagePack-RPC\") = " << ret << std::endl;

	return 0;
}

