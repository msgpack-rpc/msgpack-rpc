#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>

static size_t ATTACK_SIZE;
static size_t ATTACK_THREAD;
static size_t ATTACK_LOOP;

static char* data;

static std::auto_ptr<attacker> test;

void attack_huge()
{
	rpc::client c(test->builder(), test->address());
	c.set_timeout(30.0);

	using msgpack::type::raw_ref;

	for(size_t i=0; i < ATTACK_LOOP; ++i) {
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

	ATTACK_SIZE   = attacker::option("SIZE",   1024*1024, 8*1024*1024);
	ATTACK_THREAD = attacker::option("THREAD", 5, 20);
	ATTACK_LOOP   = attacker::option("LOOP",   2, 20);

	std::cout << "huge send/recv attack"
		<< " size="   << (ATTACK_SIZE/1024/1024) << "MB"
		<< " thread=" << ATTACK_THREAD
		<< " loop="   << ATTACK_LOOP
		<< std::endl;

	data = (char*)malloc(ATTACK_SIZE);
	if(data == NULL) {
		LOG_FATAL("failed to malloc data");
		return 1;
	}

	test.reset(new attacker());
	test->run(ATTACK_THREAD, &attack_huge);

	return 0;
}

