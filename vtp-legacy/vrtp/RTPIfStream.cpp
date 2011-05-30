/*
 *  RTPIfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPIfStream.h"

RTPIfStream::RTPIfStream(string& if_name)
{
    _in_file.open(if_name.c_str(), ios_base::in | ios_base::binary);
}

RTPIfStream::~RTPIfStream()
{
    _in_file.close();
}

