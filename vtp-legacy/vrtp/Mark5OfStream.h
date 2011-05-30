/*
 *  Mark5OfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPOfStream.h>

#ifndef _MARK5OFSTREAM_H_
#define _MARK5OFSTREAM_H_

class Mark5OfStream: public RTPOfStream 
{
public:
    Mark5OfStream(string& of_name);
    ~Mark5OfStream();
    int write_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _MARK5OFSTREAM_H_

