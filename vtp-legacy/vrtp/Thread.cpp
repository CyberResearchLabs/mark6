/*
 *  Thread.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Logger.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "Thread.h"

extern "C" 
{
    void* func(void* arg)
    {
        Thread* ptr=(Thread*)arg;
        ptr->thread_func();
        return(NULL);
    }  // End thread_func().
}

void* Thread::thread_func()
{
    debug_logger() << "Thread::thread_func()\n";
    test(NULL);
    debug_logger() << "-Thread::thread_func()\n";
    return(NULL);
}  // End run_func().

Thread::Thread(const string& s)
{
    debug_logger() << "Thread::Thread()\n";
    _id=s;
    debug_logger() << "-Thread::Thread()\n";
}  // End Thread().

Thread::~Thread()
{
    debug_logger() << "Thread::~Thread()\n";
    debug_logger() << "-Thread::~Thread()\n";
}  // End ~Thread().

void Thread::thread_create()
{
    debug_logger() << "Thread::thread_create()\n";
    int ret=0;
    if ((ret=pthread_create(&_thread_id, NULL, &func, (void*)this))!=0) {
        debug_logger() << "    Unable to initialize pthread: " << strerror(ret)
                        << "\n";
    }
    // Important note... _thread_id may not be set before run_func() invocation.
    // Therefore, not a good idea to use _thread_id in the above. Use 
    // pthread_self() instead to identify a thread. This may/may not be an 
    // issue.
    debug_logger() << "-Thread::thread_create()\n";
}  // End thread_create().

void Thread::join()
{
    debug_logger() << "Thread::join()\n";
    debug_logger()  << "    pid: " << _id << " Thread::join()";
    pthread_join(_thread_id, NULL);
    debug_logger() << "-Thread::join()\n";
}  // End join().

void Thread::test(void* args)
{
    debug_logger() << "Thread::test()\n";
    debug_logger() << "    id: " << _id << " pid:  " 
                    << (unsigned int)pthread_self() << "\n";
    long int M=10;
    double r=((double)rand()/(double)(RAND_MAX));
    double x=r*M;
    struct timespec rqtp, rmtp;
    rqtp.tv_sec=(time_t)x;
    nanosleep(&rqtp, &rmtp);
    debug_logger() << "id: " << _id << " pid:  " << (unsigned int)pthread_self() 
                    << " Goodbye world " << x << "\n";
    debug_logger() << "id: " << _id << " pid:  " << (unsigned int)pthread_self() 
        << "\n";     
    debug_logger() << "-Thread::test()\n";
}  // End test().

