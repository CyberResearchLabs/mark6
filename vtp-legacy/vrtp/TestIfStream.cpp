/*
 *  TestIfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "TestIfStream.h"

TestIfStream::TestIfStream(string& if_name):
RTPIfStream(if_name)
{
    debug_logger() << "TestIfStream::TestIfStream()\n";
    debug_logger() << "-TestIfStream::TestIfStream()\n";
}  // End TestIfStream().

TestIfStream::~TestIfStream()
{
    debug_logger() << "TestIfStream::~TestIfStream()\n";
    debug_logger() << "-TestIfStream::~TestIfStream()\n";
}  // End TestIfStream().

int TestIfStream::read_next_sample(SocketBuffer& s, Timestamp& t)
{
    debug_logger() << "TestIfStream::read_next_sample()\n";
    // Assume 32 bit sample word.
    // Assume following format for sample word:
    // 1 bit per channel:
    // Bit:        0         1         2         3  
    // Bit:        01234567890123456789012345678901
    // Channel:     3         2         1         0
    // Channel:    10987654321098765432109876543210
    // Timeslot:   00000000000000000000000000000000
    // 2 bits per channel:
    // Bit:        0         1         2         3  
    // Bit:        01234567890123456789012345678901
    // Channel:               1                   0
    // Channel:     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    // Timeslot:   00000000000000000000000000000000
    _in_file.read((char*)s, s.get_req_size());
    s.set_size(_in_file.gcount());
    debug_logger() << "-TestIfStream::read_next_sample()\n";
    return(_in_file.gcount());
}  // End read_next_sample().

