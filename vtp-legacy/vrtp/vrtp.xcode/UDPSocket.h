/*
 *  UDPSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Feb 24 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <SocketBuffer.h>

#ifndef _UDP_SOCKET_H_
#define _UDP_SOCKET_H_

class UDPSocket: public Socket 
{
public:
    UDPSocket();
    UDPSocket(int s);
    virtual ~UDPSocket();
    void bind(string ip, int port);
    int recvfrom(string ip, int port, SocketBuffer& s);
    int sendto(string ip, int port, SocketBuffer& s);
    void close();
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
};

#endif

