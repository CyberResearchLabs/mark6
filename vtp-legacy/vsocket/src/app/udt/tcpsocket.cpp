/*
 *  TCPSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "tcpsocket.h"
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>
#include <unistd.h>
#include <stdexcept>

TCPSocket::TCPSocket()
{
    _sockd=socket(AF_INET, SOCK_STREAM, 0);
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

TCPSocket::TCPSocket(int s)
{
    _sockd=s;
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

TCPSocket::TCPSocket(const TCPSocket &s)
{
    _sockd=s.get_sockd();
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

TCPSocket::~TCPSocket()
{
    // XXXX This is an important feature of the TCPSocket class. Especially
    // for copying of TCPSockets.
    // if (_sockd!=-1)
        // ::close(_sockd);
}

TCPSocket& TCPSocket::operator=(const TCPSocket& s)
{
    _sockd=s.get_sockd();
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    return(*this);
}

void TCPSocket::set_sockd(int s)
{
    if (_sockd!=-1)
        ::close(_sockd);
    _sockd=s;
}

const int TCPSocket::get_sockd() const
{
    return(_sockd);
}

void TCPSocket::bind(string ip, int port)
{
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<0) {
        throw exception();
    }      
    ret=::inet_aton(p, &addr.sin_addr);
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
    cout << "addr.sin_port: " << (int)addr.sin_port << endl;
    cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
    if (ret==-1) {
        perror("TCPSocket ");
        // throw exception();
    }
}

void TCPSocket::listen()
{
    if (_sockd==-1) {
        throw exception();
    }
    int ret=::listen(_sockd, 10);
    if (ret==-1) {
        throw exception();
    }
}

int TCPSocket::accept()
{
    if (_sockd==-1) {
        throw exception();
    }
    struct sockaddr addr;
    socklen_t addrlen=sizeof(addr);
    int ret=::accept(_sockd, &addr, &addrlen);
    if (ret==-1) {
        throw exception();
    }
    return(ret);
}

void TCPSocket::connect(string ip, int port)
{
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<=0) {
        throw exception();
    }
    ret=::inet_aton((char*)ip.data(), &addr.sin_addr);
    if (ret==0) {
        throw exception();
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
        throw exception();
    }
}
# if 0 
int TCPSocket::recv(SocketBuffer& s)
{
    int ret=0;
    if (s.get_req_size()<=0) {
        throw exception()xception(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw exception()xception(1, "Sockd error");
    }
    ret=::recv(_sockd, (char*)s, s.get_req_size(), 0);
    if (ret<0) {
        throw exception()xception(1, "::recv() error");
    }
    s.set_size(ret);
    return(ret);
}

int TCPSocket::recv(string& s, int n)
{
    int ret=0;
    if (n<=0) {
        throw exception()xception(1, "Zero size buffer");
    }
    if (n>TCP_DEFAULT_BUFFER_SIZE) {
        throw exception()xception(1, "recv() n too large");
    }
    
    if (_sockd==-1) {
        throw exception()xception(1, "Sockd error");
    }
    ret=::recv(_sockd, _buf, n, 0);
    if (ret<0)
        return(ret);
    s.assign(_buf, ret);
    return(ret);
}

int TCPSocket::send(SocketBuffer& s)
{
    if (s.get_req_size()<=0) {
        throw exception()xception(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw exception()xception(1, "Sockd error");
    }
    int ret=::send(_sockd, (const char*)s, s.get_req_size(), 0);
    if (ret<0) {
        string s("TCPSocket::send() error");
        s+=strerror(errno);
        throw exception()xception(1, s);
    }
    return(ret);
}

int TCPSocket::send(const string& s, int n)
{
    if (n==0) {
        throw exception()xception(1, "Zero size buffer");
    }
    if (_sockd==-1) {
        throw exception()xception(1, "Sockd error");
    }
    int ret=::send(_sockd, s.data(), n, 0);
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
#endif

int TCPSocket::recv(char* s, int n)
{
    int ret=0;
    if (n<=0) {
        throw exception();
    }
    if (n>TCP_DEFAULT_BUFFER_SIZE) {
        throw exception();
    }
    
    if (_sockd==-1) {
        throw exception();
    }
    ret=::recv(_sockd, s, n, 0);
    if (ret<0)
        return(ret);
    return(ret);
}

int TCPSocket::send(char* s, int n)
{
    if (n==0) {
        throw exception();
    }
    if (_sockd==-1) {
        throw exception();
    }
    int ret=::send(_sockd, s, n, 0);
    return(ret);
}

void TCPSocket::close()
{
    if (_sockd==-1) {
        throw exception();
    }
    ::close(_sockd);
}

int TCPSocket::shutdown(int howto)
{
    if (_sockd==-1) {
        throw exception();
    }
    return(::shutdown(_sockd, howto));
}

