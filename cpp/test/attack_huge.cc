#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

#define ATTACK_SIZE (8*1024*1024)
#define ATTACK_THREAD 20
#define ATTACK_LOOP 20

using msgpack::type::raw_ref;

char* data;

void attack_huge()
{
	rpc::client c("127.0.0.1", 8080);
	c.set_timeout(30.0);

	for(int i=0; i < ATTACK_LOOP; ++i) {
		raw_ref result = c.call("echo_huge", raw_ref(data, ATTACK_SIZE)).get<raw_ref>();

		if(result.size != ATTACK_SIZE) {
			LOG_ERROR("invalid size: ",result.size);
		} else if(memcmp(result.ptr, data, ATTACK_SIZE) != 0) {
			LOG_ERROR("received data don't match with sent data.");
		}
	}
}

int main(void)
{
	cclog::reset(new cclog_tty(cclog::WARN, std::cout));

	std::cout << "huge send/recv attack size="<<(ATTACK_SIZE/1024/1024)<<"MB thread="<<ATTACK_THREAD<<" loop="<<ATTACK_LOOP << std::endl;

	rpc::server svr;
	std::auto_ptr<rpc::dispatcher> dp(new myecho);
	svr.serve(dp.get());
	svr.listen("0.0.0.0", 8080);
	svr.start(4);

	data = (char*)malloc(ATTACK_SIZE);
	if(data == NULL) {
		LOG_FATAL("failed to malloc data");
		return 1;
	}

	attack::run_attacker(ATTACK_THREAD, &attack_huge);

	return 0;
}

