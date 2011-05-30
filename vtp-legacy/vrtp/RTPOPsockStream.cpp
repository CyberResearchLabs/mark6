/*
 *  RTPOPsockStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPOPsockStream.h"

RTPOPsockStream::RTPOPsockStream(string& ip, unsigned int port, int mtu, int streams):
_out_sock(streams, mtu)
{
    debug_logger() << "RTPOPsockStream::RTPOPsockStream()\n";
    _out_sock.connect(ip, port);
    debug_logger() << "-RTPOPsockStream::RTPOPsockStream()\n";
}  // End RTPOPsockStream().

RTPOPsockStream::~RTPOPsockStream()
{
    debug_logger() << "RTPOPsockStream::~RTPOPsockStream()\n";
    _out_sock.shutdown(SHUT_RDWR);
    _out_sock.close();
    debug_logger() << "-RTPOPsockStream::~RTPOPsockStream()\n";
}  // End RTPOPsockStream().

int RTPOPsockStream::write_next_sample(SocketBuffer& s, Timestamp& t)
{    
    debug_logger() << "RTPOPsockStream::write_next_sample()\n";
    debug_logger() << "RTPOPsockStream::write_next_sample()\n";
    return(_out_sock.send(s));
}    // End RTPOPsockStream().

int RTPOPsockStream::write_next_sample(string& s, int n, Timestamp& t)
{
    return(_out_sock.send(s, n));
}    // End RTPOsStream().

