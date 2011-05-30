#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <sys/time.h>

#ifndef _UTILS_H_
#define _UTILS_H_

class Timer {
    struct timeval _start;
    struct timeval _stop;
    struct timeval _diff;
public:
    Timer() {}
    ~Timer() {}
    void start() {
        gettimeofday(&_start, NULL);
    }
    void stop() {
        gettimeofday(&_stop, NULL);
    }
    double elapsed() {
        timersub(&_stop, &_start, &_diff);
        double ret=_diff.tv_sec+_diff.tv_usec/1e6;
        return(ret);
    }
};

#endif // _UTILS_H_


