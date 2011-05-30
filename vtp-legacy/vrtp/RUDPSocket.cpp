/*
 *  RUDPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 06 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RUDPSocket.h"

#include <Socket.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>
#include <Logger.h>

#include "RUDPSocket.h"

RUDPSocket::RUDPSocket():
Socket()
{
    debug_logger() << "RUDPSocket::RUDPSocket()\n";
    _sockd=socket(AF_INET, SOCK_DGRAM, 0);
    debug_logger() << "-RUDPSocket::RUDPSocket()\n";
}

RUDPSocket::RUDPSocket(int s):
Socket()
{
    debug_logger() << "RUDPSocket::RUDPSocket()\n";
    if (_sockd!=-1) {
        ::close(_sockd);
    }
    _sockd=s;
    debug_logger() << "-RUDPSocket::RUDPSocket()\n";
}

RUDPSocket::RUDPSocket(const RUDPSocket &s):
Socket()
{
    debug_logger() << "RUDPSocket::RUDPSocket()\n";
    if (_sockd!=-1) {
        ::close(_sockd);
    }
#if 0
    _sockd=s.get_sockd();
#endif
    debug_logger() << "-RUDPSocket::RUDPSocket()\n";
}

RUDPSocket::~RUDPSocket()
{
    debug_logger() << "RUDPSocket::~RUDPSocket()\n";
    debug_logger() << "-RUDPSocket::~RUDPSocket()\n";
}

RUDPSocket& RUDPSocket::operator=(const RUDPSocket& s)
{
    debug_logger() << "RUDPSocket::operator=()\n";
    if (_sockd!=-1) {
        ::close(_sockd);
    }
    _sockd=s.get_sockd();
    debug_logger() << "-RUDPSocket::operator=()\n";
    return(*this);
}

void RUDPSocket::set_sockd(int s)
{
    debug_logger() << "RUDPSocket::set_sockd()\n";
    if (_sockd!=-1) {
        ::close(_sockd);
    }
    _sockd=s;
    debug_logger() << "-RUDPSocket::set_sockd()\n";
}

const int RUDPSocket::get_sockd() const
{
    debug_logger() << "RUDPSocket::get_sockd()\n";
    debug_logger() << "-RUDPSocket::get_sockd()\n";
    return(_sockd);
}

void RUDPSocket::bind(string ip, int port)
{
    debug_logger() << "RUDPSocket::bind()\n";
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
    debug_logger() << "addr.sin_port: " << (int)addr.sin_port << "\n";
    debug_logger() << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << "\n";
    if (ret==-1) {
        perror("RUDPSocket ");
        SocketException e(1, "Bind error.");
        throw e;
    }
    debug_logger() << "-RUDPSocket::bind()\n";
}

void RUDPSocket::listen()
{
    debug_logger() << "RUDPSocket::listen()\n";
    debug_logger() << "-RUDPSocket::listen()\n";
}

int RUDPSocket::accept()
{
    debug_logger() << "RUDPSocket::accept()\n";
    debug_logger() << "-RUDPSocket::accept()\n";
    return(0);
}

void RUDPSocket::connect(string ip, int port)
{
    debug_logger() << "RUDPSocket::connect()\n";
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<=0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }
    ret=::inet_aton((char*)ip.data(), &addr.sin_addr);
    if (ret==0) {
        SocketException e(1, "Invalid IP address string.");
        throw e;
    }
    addr.sin_port=htons(port);
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    debug_logger() << "addr.sin_port: " << (int)addr.sin_port << "\n";
    debug_logger() << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << "\n";
    
    ret=::connect(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1) {
        SocketException e(1, "Connect error.");
        throw e;
    }
    debug_logger() << "-RUDPSocket::connect()\n";
}

int RUDPSocket::recv(SocketBuffer& s)
{
    debug_logger() << "RUDPSocket::recv()\n";
    debug_logger() << "-RUDPSocket::recv()\n";  
    return(0);
}

int RUDPSocket::send(SocketBuffer& s)
{
    debug_logger() << "RUDPSocket::send()\n";
    debug_logger() << "-RUDPSocket::send()\n";
    return(0);
}

void RUDPSocket::close()
{
    debug_logger() << "RUDPSocket::close()\n";
    debug_logger() << "-RUDPSocket::close()\n";
}

