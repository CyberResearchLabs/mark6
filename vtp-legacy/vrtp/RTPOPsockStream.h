/*
 *  RTPOPsockStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPOStream.h>
#include <SocketBuffer.h>
#include <PSocket.h>

#ifndef _RTPOPSOCKSTREAM_H_
#define _RTPOPSOCKSTREAM_H_

class RTPOPsockStream: public RTPOStream 
{
protected:
    PSocket _out_sock; 
public:
    RTPOPsockStream(string& ip, unsigned int port, int mtu, int streams);
    virtual ~RTPOPsockStream();
    virtual int write_next_sample(SocketBuffer& s, Timestamp& t);
    virtual int write_next_sample(string& s, int n, Timestamp& t);
};

#endif // _RTPOPSOCKSTREAM_H_


