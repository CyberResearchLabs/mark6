/*
 *  RTPIsStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPIsStream.h"

RTPIsStream::RTPIsStream(const string& ip, const int port, const int mtu)
{
    _listen_sock.bind(ip, port);
    _listen_sock.listen();
    int _conn_sock=_listen_sock.accept();
    _in_sock.set_sockd(_conn_sock);
    _mtu=mtu;
}  // End RTPIsStream().

RTPIsStream::~RTPIsStream()
{
    _in_sock.close();
    _listen_sock.close();
}  // End ~RTPIsStream().

int RTPIsStream::read_next_sample(SocketBuffer& s, Timestamp& t)
{
    return(_in_sock.recv(s));
}    // End read_next_sample().

int RTPIsStream::read_next_sample(string& s, int n, Timestamp& t)
{
    return(_in_sock.recv(s, n));
}    // End read_next_sample().



