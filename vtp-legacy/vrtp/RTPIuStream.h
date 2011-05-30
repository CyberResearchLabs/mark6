/*
 *  RTPIuStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPIStream.h>
#include <SocketBuffer.h>
#include <UDTSocket.h>

#ifndef _RTPIUSTREAM_H_
#define _RTPIUSTREAM_H_

class RTPIuStream: public RTPIStream 
{
protected:
    UDTSocket _in_sock; 
public:
    RTPIuStream(string& ip, unsigned int port, unsigned int mtu);
    virtual ~RTPIuStream();
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _RTPIUSTREAM_H_


