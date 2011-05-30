/*
 *  RTPIfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <fstream>
#include <RTPIStream.h>
#include <SocketBuffer.h>

#ifndef _RTPIFSTREAM_H_
#define _RTPIFSTREAM_H_

class RTPIfStream: public RTPIStream 
{
protected:
    ifstream _in_file; 
public:
    RTPIfStream(string& if_name);
    virtual ~RTPIfStream();
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t) = 0;
};

#endif // _RTPIFSTREAM_H_

