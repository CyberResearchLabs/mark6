/*
 *  RTPOfStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPOfStream.h"

RTPOfStream::RTPOfStream(string& of_name)
{
    _out_file.open(of_name.c_str(), ios_base::out | ios_base::binary);
}  // End RTPOStream().

RTPOfStream::~RTPOfStream()
{
    _out_file.close();
}  // End RTPOStream().

