/*
 *  TestOfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPOfStream.h>

#ifndef _TESTOFSTREAM_H_
#define _TESTOFSTREAM_H_

class TestOfStream: public RTPOfStream 
{
public:
    TestOfStream(string& of_name);
    ~TestOfStream();
    int write_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _TESTOFSTREAM_H_

