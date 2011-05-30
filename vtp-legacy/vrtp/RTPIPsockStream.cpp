/*
 *  RTPIPsockStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPIPsockStream.h"

RTPIPsockStream::RTPIPsockStream(string& ip, unsigned int port, int mtu, int streams):
_in_sock(streams, mtu)
{
    debug_logger() << "RTPIPsockStream::RTPIPsockStream()\n";
    _in_sock.bind(ip, port);
    _in_sock.listen();
    _in_sock.accept();
    debug_logger() << "-RTPIPsockStream::RTPIPsockStream()\n";
}

RTPIPsockStream::~RTPIPsockStream()
{
    debug_logger() << "RTPIPsockStream::~RTPIPsockStream()\n";
    _in_sock.shutdown(SHUT_RDWR);
    _in_sock.close();
    debug_logger() << "-RTPIPsockStream::~RTPIPsockStream()\n";    
}

int RTPIPsockStream::read_next_sample(SocketBuffer& s, Timestamp& t)
{
    debug_logger() << "RTPIPsockStream::read_next_sample()\n";
    debug_logger() << "-RTPIPsockStream::read_next_sample()\n";
    return(_in_sock.recv(s));
}

int RTPIPsockStream::read_next_sample(string& s, int n, Timestamp& t)
{
    return(_in_sock.recv(s, n));
}    // End read_next_sample().