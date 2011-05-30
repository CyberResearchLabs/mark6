/*
 *  RUDPSender.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>
#include <Thread.h>

#ifndef _RUDPSENDER_H_
#define _RUDPSENDER_H_

class RUDPSocket;

class RUDPSender: public Thread
{
private:
    RUDPSocket* _conn;
public:
    RUDPSender(RUDPSocket* _conn, const string& id);
    ~RUDPSender();
    void run(Options o);
    void* thread_func();
    void send_proc();
    void test(Options o);
};



#endif // _RUDPSENDER_H_

