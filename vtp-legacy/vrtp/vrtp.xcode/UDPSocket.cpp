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
#include <iostream.h>

#include "UDPSocket.h"

UDPSocket::UDPSocket():
Socket()
{
    _sockd=socket(AF_INET, SOCK_DGRAM, 0);
}

UDPSocket::UDPSocket(int s):
Socket()
{
    _sockd=s;
}

UDPSocket::~UDPSocket()
{
    if (_sockd!=-1)
        ::close(_sockd);    
}

void UDPSocket::bind(string ip, int port)
{
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }      
    ret=::inet_aton(p, &addr.sin_addr);
    addr.sin_len=sizeof(addr);
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
}

int UDPSocket::recvfrom(string ip, int port, SocketBuffer& s)
{
    char* p=(char*)ip.data();
    struct sockaddr_in from;
    int fromlen=sizeof(from);
    int ret=0;
    char* b=(char*)s;
    if (ip.size()<=0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }
    ret=::inet_aton(p, &from.sin_addr);
    if (ret==0) {
        SocketException e(1, "Invalid IP address string.");
        throw e;
    }
    if (s.get_max_size()<=0) {
        SocketException e(1, "Zero size buffer");
        throw e;        
    }
    from.sin_len=sizeof(from);
    from.sin_family=AF_INET;
    from.sin_port=htons(port);
    ret=::recvfrom(_sockd, b, s.get_max_size(), 0, (struct sockaddr*)&from, &fromlen);
    if (ret<0) {
        SocketException e(1, "::recvfrom() error");
        throw e;            
    }
    return(ret);
}

int UDPSocket::sendto(string ip, int port, SocketBuffer& s)
{
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
    to.sin_len=sizeof(to);
    to.sin_family=AF_INET;
    to.sin_port=htons(port);
    ret=::sendto(_sockd, b, s.get_size(), 0, (struct sockaddr*)&to, tolen);
    if (ret<0) {
        SocketException e(1, "::sendto() error");
        throw e;            
    }
    return(ret);
}

void UDPSocket::close()
{
    if (_sockd==-1) {
        SocketException e(1, "Attempt to close unallocated socket");
        throw e;
    }
    ::close(_sockd);    
}

#ifdef _DAVE_DEBUG
virtual int test(void*);
virtual int dump(void*);
#endif