/*
 *  TestIfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPIfStream.h>

#ifndef _TESTIFSTREAM_H_
#define _TESTIFSTREAM_H_

class TestIfStream: public RTPIfStream 
{
public:
    TestIfStream(string& if_name);
    virtual ~TestIfStream();
    virtual int read_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _TESTIFSTREAM_H_

