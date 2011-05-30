/*
 *  UDPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Feb 24 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <unistd.h>

#include "udpsocket.h"

UDPSocket::UDPSocket()
{
    _sockd=socket(AF_INET, SOCK_DGRAM, 0);
}

UDPSocket::UDPSocket(int s)
{
    if (_sockd!=-1) {
        
    }
    _sockd=s;
}

UDPSocket::~UDPSocket()
{
    if (_sockd!=-1)
        ::close(_sockd);
}

void UDPSocket::bind(const string& ip, const int& port)
{
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<0) {
        throw 1;
    }      
    ret=::inet_aton(p, &addr.sin_addr);
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    addr.sin_port=htons(port);
    if (ret==0) {
        throw exception();
    }
    if (_sockd==-1) {
        throw exception();
    }
    ret=::bind(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1) {
        throw exception();
    }
}

void UDPSocket::connect(const string& ip, const int& port)
{
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<=0) {
        throw 3;
    }
    ret=::inet_aton((char*)ip.data(), &addr.sin_addr);
    if (ret==0) {
        throw 4;
    }
    addr.sin_port=htons(port);
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;

    ret=::connect(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1) {
        throw 5;
    }
}

int UDPSocket::send(const char* s, const int& n)
{
    if (n<=0) {
		throw 6;
    }
    int ret=::send(_sockd, s, n, 0);
    return(ret);
}

int UDPSocket::recv(char* s, const int& n)
{
    if (n<=0) {
		throw 6;
    }
	if (n>UDP_DEFAULT_BUFFER_SIZE)
		throw 1;
    int ret=::recv(_sockd, s, n, 0);
    return(ret);
}

int UDPSocket::recvfrom(const string& ip, const int& port, const char* s, 
						const int& n)
{
    char* p=(char*)ip.c_str();
    struct sockaddr_in from;
    int fromlen=sizeof(from);
    int ret=0;
    if (ip.size()<=0) {
        throw exception();
    }
    ret=::inet_aton(p, &from.sin_addr);
    if (ret==0) {
        throw exception();
    }
    if (n<=0) {
        throw exception();        
    }
#ifndef LINUX
    from.sin_len=sizeof(from);
#endif // LINUX
    from.sin_family=AF_INET;
    from.sin_port=htons(port);
    ret=::recvfrom(_sockd, (char*)s, n, 0, (struct sockaddr*)&from, (socklen_t*)&fromlen);
    if (ret<0) {
        throw exception();            
    }
    return(ret);
}

int UDPSocket::sendto(const string& ip, const int& port, const char* s, 
			const int& n)
{
    char* p=(char*)ip.data();
    struct sockaddr_in to;
    int tolen=sizeof(to);
    int ret=0;
    if (ip.size()<=0) {
        throw 10;
    }
    ret=::inet_aton(p, &to.sin_addr);
    if (ret==0) {
            throw 11;
    }
    if (n<=0) {
        throw 12;        
    }
#ifndef LINUX
    to.sin_len=sizeof(to);
#endif // LINUX
    to.sin_family=AF_INET;
    to.sin_port=htons(port);
    ret=::sendto(_sockd, s, n, 0, (struct sockaddr*)&to, tolen);
    if (ret<0) {
        throw 13;            
    }
    return(ret);
}

void UDPSocket::close()
{
    if (_sockd==-1) {
        throw 14;
    }
    ::close(_sockd);
}

#ifdef _DAVE_DEBUG
virtual int test(void*);
virtual int dump(void*);
#endif

