/*
 *  RTPOfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <fstream>
#include <RTPOStream.h>
#include <SocketBuffer.h>

#ifndef _RTPOFSTREAM_H_
#define _RTPOFSTREAM_H_

class RTPOfStream: public RTPOStream 
{
protected:
    ofstream _out_file; 
public:
    RTPOfStream(string& name);
    virtual ~RTPOfStream();
    virtual int write_next_sample(SocketBuffer& s, Timestamp& t) = 0;
};

#endif // _RTPISTREAM_H_

