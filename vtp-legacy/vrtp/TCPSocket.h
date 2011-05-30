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
#include <list>
#include <string>

#ifndef _TCP_SOCKET_H_
#define _TCP_SOCKET_H_

const int DEFAULT_SOCK_BUF_SIZE=9000;

class TCPSocket: public Socket 
{
    char _buf[DEFAULT_SOCK_BUF_SIZE];
public:
    TCPSocket();
    TCPSocket(const TCPSocket &s);
    TCPSocket(int s);
    virtual ~TCPSocket();
    TCPSocket& operator=(const TCPSocket& s);
    void set_sockd(int s);
    const int get_sockd() const;
    
    // Socket API.
    void bind(string ip, int port);
    void listen();
    int accept();
    void connect(string ip, int port);
    int recv(SocketBuffer& s);
    int recv(string& s, int n);
    int send(const SocketBuffer& s);
    int send(const string& s, int n);
    int sendv(list<SocketBuffer*>& S);
    void close();
    int shutdown(int howto);
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
};

#endif

