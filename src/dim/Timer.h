#ifndef TIMER_H_
#define TIMER_H_

#include <sys/time.h>

class Timer {
	private:
		struct timeval _start;
		struct timeval _stop;
		struct timeval _duration;
	public:
		Timer() {
			timerclear(&_start);
			timerclear(&_stop);
			timerclear(&_duration);
		}
		void start() {
			gettimeofday(&_start, NULL);
		}
		void stop() {
			gettimeofday(&_stop, NULL);
		}	
		double duration() {
			timersub(&_stop, &_start, &_duration);
			return static_cast<double>(_duration.tv_sec)
				+ static_cast<double>(_duration.tv_usec)/1000000.0;
		}
};

#endif /*TIMER_H_*/
