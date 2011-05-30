/*
 *  Mark5IfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "Mark5IfStream.h"

Mark5IfStream::Mark5IfStream(string& if_name):
RTPIfStream(if_name)
{
}  // End Mark5IfStream().

Mark5IfStream::~Mark5IfStream()
{
}  // End Mark5IfStream().

int Mark5IfStream::read_next_sample(SocketBuffer& s, Timestamp& t)
{
    return(0);
}  // End read_next_sample().

