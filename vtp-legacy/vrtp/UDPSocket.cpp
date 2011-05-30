/*
 *  UDPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Feb 24 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>

#include "UDPSocket.h"

UDPSocket::UDPSocket():
Socket()
{
    debug_logger() << "UDPSocket::UDPSocket()\n";
    _sockd=socket(AF_INET, SOCK_DGRAM, 0);
    debug_logger() << "-UDPSocket::UDPSocket()\n";
}

UDPSocket::UDPSocket(int s):
Socket()
{
    debug_logger() << "UDPSocket::UDPSocket()\n";
    if (_sockd!=-1) {
        
    }
    _sockd=s;
    debug_logger() << "-UDPSocket::UDPSocket()\n";
}

UDPSocket::~UDPSocket()
{
    debug_logger() << "UDPSocket::~UDPSocket()\n";
    if (_sockd!=-1)
        ::close(_sockd);
    debug_logger() << "-UDPSocket::~UDPSocket()\n";
}

void UDPSocket::bind(string ip, int port)
{
    debug_logger() << "UDPSocket::bind()\n";
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }      
    ret=::inet_aton(p, &addr.sin_addr);
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    addr.sin_port=htons(port);
    if (ret==0) {
        SocketException e(1, "Invalid IP address string.");
        throw e;
    }
    if (_sockd==-1) {
        SocketException e(1, "Invalid sock descriptor.");
        throw e;
    }
    ret=::bind(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
    if (ret==-1) {
        perror("UDPSocket ");
        SocketException e(1, "Bind error.");
        throw e;
    }
    debug_logger() << "-UDPSocket::bind()\n";
}

int UDPSocket::recvfrom(string ip, int port, SocketBuffer& s)
{
    debug_logger() << "UDPSocket::recvfrom()\n";
    char* p=(char*)ip.data();
    struct sockaddr_in from;
    int fromlen=sizeof(from);
    int ret=0;
    if (ip.size()<=0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }
    ret=::inet_aton(p, &from.sin_addr);
    if (ret==0) {
        SocketException e(1, "Invalid IP address string.");
        throw e;
    }
    if (s.get_req_size()<=0) {
        SocketException e(1, "Zero size buffer");
        throw e;        
    }
#ifndef LINUX
    from.sin_len=sizeof(from);
#endif // LINUX
    from.sin_family=AF_INET;
    from.sin_port=htons(port);
    ret=::recvfrom(_sockd, (char*)s, s.get_req_size(), 0, (struct sockaddr*)&from, (socklen_t*)&fromlen);
    if (ret<0) {
        SocketException e(1, "::recvfrom() error");
        throw e;            
    }
    debug_logger() << "-UDPSocket::recvfrom()\n";
    return(ret);
}

int UDPSocket::sendto(string ip, int port, SocketBuffer& s)
{
    debug_logger() << "UDPSocket::sendto()\n";
    char* p=(char*)ip.data();
    struct sockaddr_in to;
    int tolen=sizeof(to);
    int ret=0;
    char* b=(char*)s;
    if (ip.size()<=0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }
    ret=::inet_aton(p, &to.sin_addr);
    if (ret==0) {
            SocketException e(1, "Invalid IP address string.");
            throw e;
    }
    if (s.get_size()<=0) {
        SocketException e(1, "Zero size buffer");
        throw e;        
    }
#ifndef LINUX
    to.sin_len=sizeof(to);
#endif // LINUX
    to.sin_family=AF_INET;
    to.sin_port=htons(port);
    ret=::sendto(_sockd, b, s.get_size(), 0, (struct sockaddr*)&to, tolen);
    if (ret<0) {
        SocketException e(1, "::sendto() error");
        throw e;            
    }
    debug_logger() << "-UDPSocket::sendto()\n";
    return(ret);
}

void UDPSocket::close()
{
    debug_logger() << "UDPSocket::close()\n";
    if (_sockd==-1) {
        SocketException e(1, "Attempt to close unallocated socket");
        throw e;
    }
    ::close(_sockd);
    debug_logger() << "-UDPSocket::close()\n";
}

#ifdef _DAVE_DEBUG
virtual int test(void*);
virtual int dump(void*);
#endif

