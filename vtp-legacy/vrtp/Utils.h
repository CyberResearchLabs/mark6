/*
 *  Utils.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <sys/time.h>

class InternetAddress {
private:
    struct sockaddr _addr;
    socklen_t _addrlen;
public:
        InternetAddress() {
            memset(&_addr, '\0', sizeof(_addr));
            memset(&_addrlen, '\0', sizeof(_addrlen));
        }
    InternetAddress(struct sockaddr addr) {
        memset(&_addr, '\0', sizeof(_addr));
        _addrlen=sizeof(_addr);
    }
};

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
