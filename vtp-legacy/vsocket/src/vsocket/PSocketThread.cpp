/*
 *  PSocketThread.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <PSocketThread.h>

PSocketThread::PSocketThread(const string& s, SocketBuffer& sb, int& mtu, 
							Mutex& m, TCPSocket& t, int& mode):
Thread(s), _sbuf(sb), _mtu(mtu), _mutex(m), _sock(t), _mode(mode)
{
    debug_logger() << "PSocketThread::PSocketThread()\n";
    debug_logger() << "-PSocketThread::PSocketThread()\n";
}  // End PSocketThread().

PSocketThread::~PSocketThread()
{
    debug_logger() << "PSocketThread::~PSocketThread()\n";
    debug_logger() << "-PSocketThread::~PSocketThread()\n";
}  // End ~PSocketThread().

void PSocketThread::thread_create()
{
    debug_logger() << "PSocketThread::thread_func()\n";
	Thread::thread_create();
    debug_logger() << "-PSocketThread::thread_func()\n";
}  // End thread_create().

void* PSocketThread::thread_func()
{
    debug_logger() << "PSocketThread::thread_func()\n";
	switch (_mode) {
	case 1:
		while (true) {
			m.lock();
			int rcv_bytes=_sock.recv(_sbuf, _mtu);
			m.unlock();
		}
		break;
	case 2:
		break;
	}
    debug_logger() << "-PSocketThread::thread_func()\n";
	return(Thread::thread_func());
}  // End thread_func().

