/*
 *  RTPIStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPStream.h>

#ifndef _RTPISTREAM_H_
#define _RTPISTREAM_H_

class RTPIStream: public RTPStream 
{
protected:
public:
    virtual ~RTPIStream() { }
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t) = 0;
};

#endif // _RTPISTREAM_H_


