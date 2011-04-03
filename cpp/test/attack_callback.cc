#include "attack.h"
#include <cclog/cclog.h>
#include <cclog/cclog_tty.h>
#include <vector>
#include <mp/functional.h>

static size_t ATTACK_DEPTH;
static size_t ATTACK_THREAD;
static size_t ATTACK_LOOP;
static size_t ATTACK_SIZE = 1024;

static std::auto_ptr<attacker> test;
static std::auto_ptr<rpc::session_pool> sp;

using msgpack::type::raw_ref;
using namespace mp::placeholders;

void callback_func(rpc::future f, raw_ref msg, rpc::shared_zone msglife)
{
	raw_ref result = f.get<raw_ref>();

	if(result.size != msg.size) {
		LOG_ERROR("invalid size: ",result.size);
	} else if(memcmp(result.ptr, msg.ptr, msg.size) != 0) {
		LOG_ERROR("received data don't match with sent data.");
	}
}

void attack_callback()
{
	std::vector<rpc::future> pipeline(ATTACK_DEPTH);

	for(size_t i=0; i < ATTACK_LOOP; ++i) {
		rpc::session s = sp->get_session(test->address());
		s.set_timeout(30.0);

		rpc::shared_zone msglife(new msgpack::zone());
		raw_ref msg = raw_ref((char*)msglife->malloc(ATTACK_SIZE), ATTACK_SIZE);

		for(size_t j=0; j < ATTACK_DEPTH; ++j) {
			pipeline[j] = s.call("echo_huge", msglife, msg).attach_callback(
					mp::bind(callback_func, _1, msg, msglife));
		}

		for(size_t j=0; j < ATTACK_DEPTH; ++j) {
			pipeline[j].join();
		}
	}
}

int main(void)
{
	cclog::reset(new cclog_tty(cclog::WARN, std::cout));
	signal(SIGPIPE, SIG_IGN);

	ATTACK_DEPTH  = attacker::option("DEPTH",  25, 100);
	ATTACK_THREAD = attacker::option("THREAD", 25, 100);
	ATTACK_LOOP   = attacker::option("LOOP",   5, 50);

	std::cout << "callback attack"
		<< " depth="  << ATTACK_DEPTH
		<< " thread=" << ATTACK_THREAD
		<< " loop="   << ATTACK_LOOP
		<< std::endl;

	test.reset(new attacker());

	sp.reset(new rpc::session_pool(test->builder()));
	sp->start(4);

	test->run(ATTACK_THREAD, &attack_callback);

	return 0;
}

