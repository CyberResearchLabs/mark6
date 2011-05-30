/*
 *  TCPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream.h>

#include "TCPSocket.h"

TCPSocket::TCPSocket():
Socket()
{
    _sockd=socket(AF_INET, SOCK_STREAM, 0);
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

TCPSocket::TCPSocket(int s):
Socket()
{
    _sockd=s;
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

TCPSocket::~TCPSocket()
{
    if (_sockd!=-1)
        ::close(_sockd);
}

void TCPSocket::bind(string ip, int port)
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
        perror("TCPSocket ");
        SocketException e(1, "Bind error.");
        throw e;
    }
}

void TCPSocket::listen()
{
    if (_sockd==-1) {
        SocketException e(1, "Invalid sock descriptor.");
        throw e;
    }
    int ret=::listen(_sockd, 10);
    if (ret==-1) {
        SocketException e(1, "Listen error.");
        throw e;
    }
}

TCPSocket TCPSocket::accept()
{
    if (_sockd==-1) {
        SocketException e(1, "Invalid sock descriptor.");
        throw e;
    }
    struct sockaddr addr;
    socklen_t addrlen=sizeof(addr);
    int ret=::accept(_sockd, &addr, &addrlen);
    if (ret==-1) {
        SocketException e(1, "Accept error.");
        throw e;
    }
    TCPSocket s(ret);
    return(s);
}

void TCPSocket::connect(string ip, int port)
{
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<=0) {
        SocketException e(1, "Zero length IP string.");
        throw e;
    }
    ret=::inet_aton(p, &addr.sin_addr);
    if (ret==0) {
        SocketException e(1, "Invalid IP address string.");
        throw e;
    }
    addr.sin_port=htons(port);
    addr.sin_len=sizeof(addr);
    addr.sin_family=AF_INET;
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
        
    ret=::connect(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1) {
        SocketException e(1, "Connect error.");
        throw e;
    }
}

int TCPSocket::recv(SocketBuffer& s)
{
    int ret=0;
    if (s.get_max_size()<=0) {
        SocketException e(1, "Zero size buffer");
        throw e;
    }
    char* p=(char*)s;
    if (_sockd==-1) {
        SocketException e(1, "Sockd error");
        throw e;
    }
    ret=::recv(_sockd, p, s.get_max_size(), 0);
    if (ret<0) {
        SocketException e(1, "::recv() error");
        throw e;            
    }
    s.set_size(ret);
    return(ret);
}

int TCPSocket::send(SocketBuffer& s)
{
    char* p=(char*)s;
    int ret=ret=::send(_sockd, p, s.get_size(), 0);
    if (ret<0) {
        SocketException e(1, "::send() error");
        throw e;            
    }
    return(ret);
}

void TCPSocket::close()
{
    if (_sockd==-1) {
        SocketException e(1, "Attempt to close unallocated socket");
        throw e;
    }
    ::close(_sockd);
}

