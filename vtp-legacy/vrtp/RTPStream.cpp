/*
 *  RTPStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPStream.h"

RTPStream::RTPStream(unsigned int mtu):
_buf(mtu)
{
    debug_logger() << "RTPStream::RTPStream()\n";
    _mtu=mtu;
    debug_logger() << "-RTPStream::RTPStream()\n";
}

RTPStream::~RTPStream()
{
    debug_logger() << "RTPStream::~RTPStream()\n";
    debug_logger() << "-RTPStream::~RTPStream()\n";
}

void RTPStream::set_bits_per_sample(unsigned int bps)
{
    debug_logger() << "RTPStream::set_bits_per_sample()\n";
    _bits_per_sample=bps;
    debug_logger() << "-RTPStream::set_bits_per_sample()\n";
}  // End set_bits_per_sample().

void RTPStream::set_channels(unsigned int chnl_mask)
{
    debug_logger() << "RTPStream::set_channels()\n";
    _channel_mask=chnl_mask;
    debug_logger() << "-RTPStream::set_channels()\n";
}  // End set_channels().

void RTPStream::set_mtu(unsigned int mtu)
{
    debug_logger() << "RTPStream::set_mtu()\n";
    _mtu=mtu;
    debug_logger() << "-RTPStream::set_mtu()\n";
}  // End set_mtu().

