#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

static size_t ATTACK_THREAD;
static size_t ATTACK_LOOP;

static std::auto_ptr<attacker> test;

void attack_connect()
{
	for(size_t i=0; i < ATTACK_LOOP; ++i) {
		rpc::client c(test->builder(), test->address());
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
	signal(SIGPIPE, SIG_IGN);

	ATTACK_THREAD = attacker::option("THREAD", 25, 100);
	ATTACK_LOOP   = attacker::option("LOOP", 5, 50);

	std::cout << "connect attack"
		<< " thread=" << ATTACK_THREAD
		<< " loop="   << ATTACK_LOOP
		<< std::endl;

	test.reset(new attacker());
	test->run(ATTACK_THREAD, &attack_connect);

	return 0;
}

