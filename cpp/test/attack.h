#include "echo_server.h"
#include <mp/pthread.h>
#include <iostream>
#include <numeric>
#include <sys/time.h>
#include <cclog/cclog.h>
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <msgpack/rpc/session_pool.h>

namespace attack {


class attacker {
public:
	attacker() :
		m_nthreads(0), m_threads(NULL), m_times(NULL) { }

	~attacker()
	{
		delete[] m_threads;
		delete[] m_times;
	}

	void start(size_t nthreads, mp::function<void ()> func)
	{
		m_threads = new mp::pthread_thread[nthreads];
		m_times = new double[nthreads];
		m_nthreads = nthreads;

		for(size_t i=0; i < m_nthreads; ++i) {
			m_times[i] = 0;
			m_threads[i].run(
					mp::bind(attacker::thread_main, func, &m_times[i]));
		}
	}

	void join()
	{
		for(size_t i=0; i < m_nthreads; ++i) {
			m_threads[i].join();
		}
	}

	void show_status() const
	{
		double sum = std::accumulate(m_times, m_times+m_nthreads, 0.0);
		double ave = sum / m_nthreads;

		std::cout
			<< "total time    : " << sum << "\n"
			<< "average time  : " << ave << std::endl;
	}

private:
	size_t m_nthreads;
	mp::pthread_thread* m_threads;
	double* m_times;

	static void thread_main(mp::function<void ()> func, double* time)
	{
		struct timeval start_time;
		gettimeofday(&start_time, NULL);

		try {
			func();
		} catch (std::exception& e) {
			LOG_ERROR("error: ",e.what());
		} catch (...) {
			LOG_ERROR("error: unknown error");
		}

		struct timeval end_time;
		gettimeofday(&end_time, NULL);

		*time = (end_time.tv_sec - start_time.tv_sec)
			+ (double)(end_time.tv_usec - start_time.tv_usec) / 1000 / 1000;
	}

private:
	attacker(const attacker&);
};


void run_attacker(size_t nthreads, mp::function<void ()> func)
{
	attacker a;
	a.start(nthreads, func);
	a.join();
	a.show_status();
}


}  // namespace attack


