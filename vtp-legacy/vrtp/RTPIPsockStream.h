/*
 *  RTPIPsockStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPIStream.h>
#include <SocketBuffer.h>
#include <PSocket.h>

#ifndef _RTPIPSOCKSTREAM_H_
#define _RTPIPSOCKSTREAM_H_

class RTPIPsockStream: public RTPIStream 
{
protected:
    PSocket _in_sock; 
public:
    RTPIPsockStream(string& ip, unsigned int port, int mtu, int streams);
    virtual ~RTPIPsockStream();
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t);
    virtual int read_next_sample(string& s, int n, Timestamp& t);

};

#endif // _RTPIPSOCKSTREAM_H_

