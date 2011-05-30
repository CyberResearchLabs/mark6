/*
 *  Thread.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include <pthread.h>
#include <Logger.h>
#include <Object.h>

#ifndef _THREAD_H_
#define _THREAD_H_

class Thread: public Object {
protected:
    pthread_t _thread_id;
    string _id;
public:
    virtual void* thread_func();
    Thread(const string& s);
    virtual ~Thread();
    void thread_create();
    void join();
    virtual void test(void* args);
};

#endif // _THREAD_H_

