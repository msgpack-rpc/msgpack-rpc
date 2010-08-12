#include "attack.h"
//#include <msgpack/rpc/transport/udp.h>
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

#define ATTACK_THREAD 100
#define ATTACK_LOOP 50

void attack_connect()
{
	for(int i=0; i < ATTACK_LOOP; ++i) {
		rpc::client c("127.0.0.1", 8080);
		//rpc::client c(rpc::udp_builder(), rpc::ip_address("127.0.0.1", 8080));
		c.set_timeout(30.0);

		int result = c.call("add", 1, 2).get<int>();
		if(result != 3) {
			LOG_ERROR("invalid response: ",result);
		}
	}
}

int main(void)
{
	cclog::reset(new cclog_tty(cclog::WARN, std::cout));

	std::cout << "connect attack thread="<<ATTACK_THREAD<<" loop="<<ATTACK_LOOP << std::endl;

	rpc::server svr;
	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());
	svr.listen("0.0.0.0", 8080);
	//svr.listen(rpc::udp_listener(rpc::ip_address("0.0.0.0", 8080)));
	svr.start(4);

	attack::run_attacker(ATTACK_THREAD, &attack_connect);

	return 0;
}

