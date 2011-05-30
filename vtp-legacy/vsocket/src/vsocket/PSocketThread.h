/*
 *  Thread.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef PSOCKET_THREAD_H
#define PSOCKET_THREAD_H

#include <common.h>
#include <Socket.h>
#include <TCPSocket.h>
#include <Thread.h>
#include <Mutex.h>

class PSocketThread: public Thread {
protected:
	SocketBuffer& _sbuf;
	int _mtu;
	Mutex& _mutex;
	TCPSocket& _sock;
	int _mode;			// 1. rcvr, 2. sender.
public:
    virtual void* thread_func();
    PSocketThread(const string& s, SocketBuffer& sb,
					Mutex& m, TCPSocket& _sock, int& mode);
    virtual ~PSocketThread();
	void thread_create();
    void join();
};

#endif // PSOCKET_THREAD_H

