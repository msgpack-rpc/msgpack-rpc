#include "echo_server.h"
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

using namespace mp::placeholders;

namespace rpc {
	using namespace msgpack::rpc;
}

class myserver : public rpc::dispatcher {
public:
	rpc::server& listen(const std::string& host, uint16_t port)
	{
		m_svr.serve(this);
		m_svr.listen(host, port);
		return m_svr;
	}

	static void callback(rpc::future f, rpc::request req)
	{
		req.result(f.get<int>());
	}

	void dispatch(rpc::request req)
	{
		std::string method = req.method().as<std::string>();
		msgpack::type::tuple<int, int> params(req.params());

		rpc::session s = m_svr.get_session("127.0.0.1", 18811);
		rpc::future f = s.call(method, params.get<0>(), params.get<1>());
		f.attach_callback( mp::bind(&callback, _1, req) );
	}
	
private:
	msgpack::rpc::server m_svr;
};

int main(void)
{
	cclog::reset(new cclog_tty(cclog::TRACE, std::cout));
	signal(SIGPIPE, SIG_IGN);

	// run server {
	rpc::server svr;

	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());

	svr.listen("0.0.0.0", 18811);

	svr.start(4);
	// }


	// start server with 4 threads
	myserver s;
	s.listen("0.0.0.0", 18812).start(4);

	// send rquest from the client
	msgpack::rpc::client c("127.0.0.1", 18812);
	int ret = c.call("add", 1, 2).get<int>();

	std::cout << "call: add(1, 2) = " << ret << std::endl;
}

