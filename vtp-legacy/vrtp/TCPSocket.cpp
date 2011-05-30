/*
 *  TCPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Socket.h>
#include "TCPSocket.h"
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>

TCPSocket::TCPSocket():
Socket()
{
    debug_logger() << "TCPSocket::TCPSocket()\n";
    _sockd=socket(AF_INET, SOCK_STREAM, 0);
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    debug_logger() << "-TCPSocket::TCPSocket()\n";
}

TCPSocket::TCPSocket(int s):
Socket()
{
    debug_logger() << "TCPSocket::TCPSocket(int)\n";
    _sockd=s;
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    debug_logger() << "-TCPSocket::TCPSocket(int)\n";
}

TCPSocket::TCPSocket(const TCPSocket &s):
Socket()
{
    debug_logger() << "TCPSocket::TCPSocket(&)\n";
    _sockd=s.get_sockd();
    debug_logger() << "    _sockd=" << _sockd << "\n";
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    debug_logger() << "-TCPSocket::TCPSocket(&)\n";
}

TCPSocket::~TCPSocket()
{
    debug_logger() << "TCPSocket::~TCPSocket()\n";
    // XXXX This is an important feature of the TCPSocket class. Especially
    // for copying of TCPSockets.
    // if (_sockd!=-1)
        // ::close(_sockd);
    debug_logger() << "-TCPSocket::~TCPSocket()\n";
}

TCPSocket& TCPSocket::operator=(const TCPSocket& s)
{
    debug_logger() << "TCPSocket::operator=()\n";
    _sockd=s.get_sockd();
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    debug_logger() << "-TCPSocket::operator=()\n";
    return(*this);
}

void TCPSocket::set_sockd(int s)
{
    debug_logger() << "TCPSocket::set_sockd()\n";
    if (_sockd!=-1)
        ::close(_sockd);
    _sockd=s;
    debug_logger() << "-TCPSocket::set_sockd()\n";
}

const int TCPSocket::get_sockd() const
{
    debug_logger() << "TCPSocket::get_sockd()\n";
    debug_logger() << "-TCPSocket::get_sockd()\n";
    return(_sockd);
}

void TCPSocket::bind(string ip, int port)
{
    debug_logger() << "TCPSocket::bind()\n";
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
        perror("TCPSocket ");
        SocketException e(1, "Bind error.");
        throw e;
    }
    debug_logger() << "-TCPSocket::bind()\n";
}

void TCPSocket::listen()
{
    debug_logger() << "TCPSocket::listen()\n";
    if (_sockd==-1) {
        SocketException e(1, "Invalid sock descriptor.");
        throw e;
    }
    int ret=::listen(_sockd, 10);
    if (ret==-1) {
        SocketException e(1, "Listen error.");
        throw e;
    }
    debug_logger() << "-TCPSocket::listen()\n";
}

int TCPSocket::accept()
{
    debug_logger() << "TCPSocket::accept()\n";
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
    debug_logger() << "Accept sock " << ret << "\n";
    debug_logger() << "-TCPSocket::accept()\n";
    return(ret);
}

void TCPSocket::connect(string ip, int port)
{
    debug_logger() << "TCPSocket::connect()\n";
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
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
        
    ret=::connect(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1) {
        SocketException e(1, "Connect error.");
        throw e;
    }
    debug_logger() << "-TCPSocket::connect()\n";
}

int TCPSocket::recv(SocketBuffer& s)
{
    // debug_logger() << "TCPSocket::recv()\n";
    int ret=0;
    if (s.get_req_size()<=0) {
        throw SocketException(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException(1, "Sockd error");
    }
    ret=::recv(_sockd, (char*)s, s.get_req_size(), 0);
    if (ret<0) {
        throw SocketException(1, "::recv() error");
    }
    s.set_size(ret);
    // debug_logger() << "-TCPSocket::recv()\n";
    return(ret);
}

int TCPSocket::recv(string& s, int n)
{
    // debug_logger() << "TCPSocket::recv()\n";
    int ret=0;
    if (n<=0) {
        throw SocketException(1, "Zero size buffer");
    }
    if (n>DEFAULT_SOCK_BUF_SIZE) {
        throw SocketException(1, "recv() n too large");
    }
    
    if (_sockd==-1) {
        throw SocketException(1, "Sockd error");
    }
    ret=::recv(_sockd, _buf, n, 0);
    if (ret<0)
        return(ret);
    s.assign(_buf, ret);
    // debug_logger() << "-TCPSocket::recv()\n";
    return(ret);
}


int TCPSocket::send(const SocketBuffer& s)
{
    // debug_logger() << "TCPSocket::send()\n";
    if (s.get_req_size()<=0) {
        throw SocketException(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException(1, "Sockd error");
    }
    int ret=::send(_sockd, (char*)s, s.get_req_size(), 0);
    if (ret<0) {
        string s("TCPSocket::send() error");
        s+=strerror(errno);
        throw SocketException(1, s);
    }
    // debug_logger() << "TCPSocket::send()\n";
    return(ret);
}

int TCPSocket::send(const string& s, int n)
{
    // debug_logger() << "TCPSocket::send()\n";
    if (n==0) {
        throw SocketException(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException(1, "Sockd error");
    }
    int ret=::send(_sockd, s.data(), n, 0);
    // debug_logger() << "TCPSocket::send()\n";
    return(ret);
}

int TCPSocket::sendv(list<SocketBuffer*>& S)
{
    list<SocketBuffer*>::iterator iter;
    int ret=0;
    for (iter=S.begin(); iter!=S.end(); ++iter) {
        ret+=send(**iter);
    }
    return(ret);
}

void TCPSocket::close()
{
    debug_logger() << "TCPSocket::close()\n";
    if (_sockd==-1) {
        SocketException e(1, "Attempt to close unallocated socket");
        throw e;
    }
    ::close(_sockd);
    debug_logger() << "-TCPSocket::close()\n";
}

int TCPSocket::shutdown(int howto)
{
    debug_logger() << "TCPSocket::shutdown()\n";
    if (_sockd==-1) {
        SocketException e(1, "Attempt to close unallocated socket");
        throw e;
    }
    debug_logger() << "-TCPSocket::shutdown()\n";
    return(::shutdown(_sockd, howto));
}

