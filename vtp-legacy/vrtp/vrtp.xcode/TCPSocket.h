/*
 *  TCPSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <SocketBuffer.h>

#ifndef _TCP_SOCKET_H_
#define _TCP_SOCKET_H_

class TCPSocket: public Socket 
{
public:
    TCPSocket();
    TCPSocket(int s);
    virtual ~TCPSocket();
    void bind(string ip, int port);
    void listen();
    TCPSocket accept();
    void connect(string ip, int port);
    int recv(SocketBuffer& s);
    int send(SocketBuffer& s);
    void close();
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
};

#endif

