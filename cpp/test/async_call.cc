#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/session_pool.h>
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


	// create session pool
	rpc::session_pool sp;

	// get session
	rpc::session s = sp.get_session("127.0.0.1", 8080);

	// async call
	rpc::future fs[10];

	for(int i=0; i < 10; ++i) {
		fs[i] = s.call("add", 1, 2);
	}

	for(int i=0; i < 10; ++i) {
		int ret = fs[i].get<int>();
		std::cout << "async call: add(1, 2) = " << ret << std::endl;
	}

	return 0;
}

