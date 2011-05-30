/*
 *  RUDPSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 06 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <SocketBuffer.h>
#include <SocketBuffer.h>
#include <RUDPSender.h>
#include <RUDPReceiver.h>

#ifndef _RUDPSOCKET_SOCKET_H_
#define _RUDPSOCKET_SOCKET_H_


class RUDPSocket: public Socket 
{
private:
public:
    RUDPSocket();
    RUDPSocket(unsigned int& mw);
    RUDPSocket(const RUDPSocket &s);
    RUDPSocket(int s);
    virtual ~RUDPSocket();
    RUDPSocket& operator=(const RUDPSocket& s);
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

#endif  // End _RUDP_SOCKET_H_


