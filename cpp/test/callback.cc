#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/session_pool.h>
#include <mp/functional.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

void add_callback(rpc::future f, rpc::loop lo)
{
	try {
		int result = f.get<int>();
	} catch (msgpack::rpc::remote_error& e) {
		std::cout << e.what() << std::endl;
		exit(1);
	}
	lo->end();
}

using namespace mp::placeholders;

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


	// create session pool
	rpc::session_pool sp;

	// get session
	rpc::session s = sp.get_session("127.0.0.1", 18811);

	// call
	rpc::future f = s.call("add", 1, 2);

	f.attach_callback(
			mp::bind(add_callback, _1, sp.get_loop()));

	sp.run(4);

	return 0;
}

