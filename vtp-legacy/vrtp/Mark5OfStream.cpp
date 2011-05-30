/*
 *  Mark5OfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "Mark5OfStream.h"

Mark5OfStream::Mark5OfStream(string& of_name):
RTPOfStream(of_name)
{
}  // End Mark5OfStream().

Mark5OfStream::~Mark5OfStream()
{
    _out_file.close();
}  // End ~Mark5OfStream().

int Mark5OfStream::write_next_sample(SocketBuffer& s, Timestamp& t)
{
    return(0);
}  // End write_next_sample().


