/*
 *  RTPOuStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPOStream.h>
#include <SocketBuffer.h>
#include <UDTSocket.h>

#ifndef _RTPOUSTREAM_H_
#define _RTPOUSTREAM_H_

class RTPOuStream: public RTPOStream 
{
protected:
    UDTSocket _out_sock; 
public:
    RTPOuStream(string& ip, unsigned int port, unsigned int mtu);
    virtual ~RTPOuStream();
    virtual int write_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _RTPOUSTREAM_H_

