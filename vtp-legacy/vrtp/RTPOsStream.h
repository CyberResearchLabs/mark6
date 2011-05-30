/*
 *  RTPOsStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPOStream.h>
#include <SocketBuffer.h>
#include <TCPSocket.h>

#ifndef _RTPOSSTREAM_H_
#define _RTPOSSTREAM_H_

class RTPOsStream: public RTPOStream 
{
protected:
    TCPSocket _out_sock; 
public:
    RTPOsStream(const string& ip, const int port, const int mtu);
    virtual ~RTPOsStream();
    virtual int write_next_sample(SocketBuffer& s, Timestamp& t);
    virtual int write_next_sample(string& s, int n, Timestamp& t);
};

#endif // _RTPOSSTREAM_H_

