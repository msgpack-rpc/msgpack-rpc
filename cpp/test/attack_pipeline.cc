#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>
#include <vector>

#define ATTACK_PIPELINE 100
#define ATTACK_THREAD 100
#define ATTACK_LOOP 50

using msgpack::type::raw_ref;

rpc::session_pool* sp;

void attack_pipeline()
{
	std::vector<rpc::future> pipeline(ATTACK_PIPELINE);

	for(int i=0; i < ATTACK_LOOP; ++i) {
		rpc::session c = sp->get_session("127.0.0.1", 8080);
		c.set_timeout(30.0);

		for(int j=0; j < ATTACK_PIPELINE; ++j) {
			pipeline[j] = c.call("add", 1, 2);
		}

		for(int j=0; j < ATTACK_PIPELINE; ++j) {
			int result = pipeline[j].get<int>();
			if(result != 3) {
				LOG_ERROR("invalid response: ",result);
			}
		}
	}
}

int main(void)
{
	cclog::reset(new cclog_tty(cclog::WARN, std::cout));

	std::cout << "pipeline attack depth="<<ATTACK_PIPELINE<<" thread="<<ATTACK_THREAD<<" loop="<<ATTACK_LOOP << std::endl;

	rpc::server svr;
	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());
	svr.listen("0.0.0.0", 8080);
	svr.start(4);

	sp = new rpc::session_pool();
	sp->start(4);

	attack::run_attacker(ATTACK_THREAD, &attack_pipeline);

	return 0;
}

