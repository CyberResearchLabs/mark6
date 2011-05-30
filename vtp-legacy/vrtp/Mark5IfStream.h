/*
 *  Mark5IfStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <RTPIfStream.h>

#ifndef _MARK5IFSTREAM_H_
#define _MARK5IFSTREAM_H_

class Mark5IfStream: public RTPIfStream 
{
public:
    Mark5IfStream(string& if_name);
    ~Mark5IfStream();
    int read_next_sample(SocketBuffer& s, Timestamp& t);
};

#endif // _MARK5IFSTREAM_H_

