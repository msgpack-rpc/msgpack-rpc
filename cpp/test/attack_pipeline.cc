#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>
#include <vector>

static size_t ATTACK_DEPTH;
static size_t ATTACK_THREAD;
static size_t ATTACK_LOOP;

static std::auto_ptr<attacker> test;
static std::auto_ptr<rpc::session_pool> sp;

void attack_pipeline()
{
	using msgpack::type::raw_ref;

	std::vector<rpc::future> pipeline(ATTACK_DEPTH);

	for(size_t i=0; i < ATTACK_LOOP; ++i) {
		rpc::session c = sp->get_session(test->address());
		c.set_timeout(30.0);

		for(size_t j=0; j < ATTACK_DEPTH; ++j) {
			pipeline[j] = c.call("add", 1, 2);
		}

		for(size_t j=0; j < ATTACK_DEPTH; ++j) {
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

	ATTACK_DEPTH  = attacker::option("DEPTH",  25, 100);
	ATTACK_THREAD = attacker::option("THREAD", 25, 100);
	ATTACK_LOOP   = attacker::option("LOOP",   5, 50);

	std::cout << "pipeline attack"
		<< " depth="  << ATTACK_DEPTH
		<< " thread=" << ATTACK_THREAD
		<< " loop="   << ATTACK_LOOP
		<< std::endl;

	test.reset(new attacker());

	sp.reset(new rpc::session_pool(test->builder()));
	sp->start(4);

	test->run(ATTACK_THREAD, &attack_pipeline);

	return 0;
}

