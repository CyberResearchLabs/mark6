/*
 *  RTPIsStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPIStream.h>
#include <SocketBuffer.h>
#include <TCPSocket.h>

#ifndef _RTPISSTREAM_H_
#define _RTPISSTREAM_H_

class RTPIsStream: public RTPIStream 
{
protected:
    TCPSocket _listen_sock;
    TCPSocket _in_sock; 
public:
    RTPIsStream(const string& ip, const int port, const int mtu);
    virtual ~RTPIsStream();
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t);
    virtual int read_next_sample(string& s, int n, Timestamp& t);
};

#endif // _RTPISSTREAM_H_


