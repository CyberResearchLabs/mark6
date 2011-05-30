/*
 *  RTPOStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sat Mar 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPStream.h>

#ifndef _RTPOSTREAM_H_
#define _RTPOSTREAM_H_

class RTPOStream: public RTPStream 
{
protected:
public:
    virtual ~RTPOStream() { }
    virtual int write_next_sample(SocketBuffer& s, Timestamp& t) = 0;
};

#endif // _RTPOSTREAM_H_


