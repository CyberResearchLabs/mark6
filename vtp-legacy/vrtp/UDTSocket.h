/*
 *  UDTSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <SocketBuffer.h>
#include <udt.h>

#ifndef _UDT_SOCKET_H_
#define _UDT_SOCKET_H_

class UDTSocket: public Socket 
{
private:
	CUDT* _udt_sock;
public:
    UDTSocket();
    UDTSocket(const UDTSocket &s);
    UDTSocket(int s);
    virtual ~UDTSocket();
    UDTSocket& operator=(const UDTSocket& s);
    void set_sockd(int s);
    const int get_sockd() const;
    
    // Socket API.
    void bind(string ip, int port);
    void listen();
    int accept();
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

