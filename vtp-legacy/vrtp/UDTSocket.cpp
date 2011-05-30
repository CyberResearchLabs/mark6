/*
 *  UDTSocket.cpp
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
#include <iostream>
#include <errno.h>

#include "UDTSocket.h"

UDTSocket::UDTSocket():
Socket()
{
    debug_logger() << "UDTSocket::UDTSocket()\n";
    _udt_sock=new CUDT;
    debug_logger() << "-UDTSocket::UDTSocket()\n";
}

UDTSocket::UDTSocket(int s):
Socket()
{
    debug_logger() << "UDTSocket::UDTSocket()\n";
    debug_logger() << "-UDTSocket::UDTSocket()\n";
}

UDTSocket::UDTSocket(const UDTSocket &s):
Socket()
{
    debug_logger() << "UDTSocket::UDTSocket()\n";
    debug_logger() << "-UDTSocket::UDTSocket()\n";
}

UDTSocket::~UDTSocket()
{
    debug_logger() << "UDTSocket::~UDTSocket()\n";
    if (_udt_sock!=NULL) {
        _udt_sock->close();
		delete(_udt_sock);
		_udt_sock=NULL;
    }
    debug_logger() << "-UDTSocket::~UDTSocket()\n";
}

UDTSocket& UDTSocket::operator=(const UDTSocket& s)
{
    debug_logger() << "UDTSocket::operator=()\n";
    debug_logger() << "-UDTSocket::operator=()\n";
    return(*this);
}

void UDTSocket::set_sockd(int s)
{
    debug_logger() << "UDTSocket::set_sockd()\n";
    debug_logger() << "-UDTSocket::set_sockd()\n";
}

const int UDTSocket::get_sockd() const
{
    debug_logger() << "UDTSocket::get_sockd()\n";
    debug_logger() << "-UDTSocket::get_sockd()\n";
    return(0);
}

void UDTSocket::bind(string ip, int port)
{
    debug_logger() << "UDTSocket::bind()\n";
    _udt_sock->open(port);
    debug_logger() << "-UDTSocket::bind()\n";
}

void UDTSocket::listen()
{
    debug_logger() << "UDTSocket::listen()\n";
    _udt_sock->listen();
    debug_logger() << "-UDTSocket::listen()\n";
}

int UDTSocket::accept()
{
    debug_logger() << "UDTSocket::accept()\n";
    debug_logger() << "-UDTSocket::accept()\n";
    return(0);
}

void UDTSocket::connect(string ip, int port)
{
    debug_logger() << "UDTSocket::connect()\n";
    _udt_sock->open();
    _udt_sock->connect(ip.c_str(), port);
    debug_logger() << "-UDTSocket::connect()\n";
}

int UDTSocket::recv(SocketBuffer& s)
{
    debug_logger() << "UDTSocket::recv()\n";
    _udt_sock->recv((char*)s, s.get_req_size());
    debug_logger() << "-UDTSocket::recv()\n";
    return(s.get_req_size());	// XXXX
}

int UDTSocket::send(SocketBuffer& s)
{
    debug_logger() << "UDTSocket::send()\n";
    _udt_sock->send((char*)s, s.get_req_size());
    debug_logger() << "-UDTSocket::send()\n";
    return(s.get_req_size());	// XXXX
}

void UDTSocket::close()
{
    debug_logger() << "UDTSocket::close()\n";
    _udt_sock->close();
    _udt_sock=NULL;
    debug_logger() << "-UDTSocket::close()\n";
}


