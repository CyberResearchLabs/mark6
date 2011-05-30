/*
 *  RUDPReceiver.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Mar 07 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Options.h>
#include <Thread.h>

#ifndef _RUDPRECEIVER_H_
#define _RUDPRECEIVER_H_

class RUDPSocket;

class RUDPReceiver: public Thread
{
private:
    RUDPSocket* _conn;
public:
    RUDPReceiver(RUDPSocket* conn, const string& id);
    ~RUDPReceiver();
    void run(Options o);
    void* thread_func();
    void recv_proc();
    void test(Options o);
};



#endif // _RUDPRECEIVER_H_
