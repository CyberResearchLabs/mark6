/*
 *  TestOfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "TestOfStream.h"

TestOfStream::TestOfStream(string& of_name):
RTPOfStream(of_name)
{
    debug_logger() << "TestOfStream::TestOfStream()\n";
    debug_logger() << "-TestOfStream::TestOfStream()\n";
}  // End TestOfStream().

TestOfStream::~TestOfStream()
{
    debug_logger() << "TestOfStream::~TestOfStream()\n";
    _out_file.close();
    debug_logger() << "-TestOfStream::~TestOfStream()\n";
}  // End ~TestOfStream().

int TestOfStream::write_next_sample(SocketBuffer& s, Timestamp& t)
{
    debug_logger() << "TestOfStream::write_next_sample()\n";
    debug_logger() << "    buffer size: " << s.get_size() << "\n";
    _out_file.write((char*)s, s.get_req_size());
    debug_logger() << "-TestOfStream::write_next_sample()\n";   
    return(s.get_req_size());
}  // End write_next_sample().

