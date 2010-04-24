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

	svr.listen("0.0.0.0", 8080);

	svr.start(4);
	// }


	// create client
	rpc::client cli("127.0.0.1", 8080);

	// notify
	cli.notify("echo");
	cli.notify("echo", 0);
	cli.notify("echo", 0, 1);

	cli.get_loop()->flush();

	usleep(100000);

	return 0;
}

