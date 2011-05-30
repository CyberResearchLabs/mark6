/*
 *  RTPStream.h
 *  vrtp
 *
 *  Created by David Lapsley on Sun Feb 29 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <Object.h>
#include <SocketBuffer.h>

#ifndef _RTPSTREAM_H_
#define _RTPSTREAM_H_

struct Timestamp {
    unsigned long sec;
    unsigned long frac_sec;
};

class RTPStream: public Object 
{
protected:
    u_int16_t max_seq;        /* highest seq. number seen */
    u_int32_t cycles;         /* shifted count of seq. number cycles */
    u_int32_t base_seq;       /* base seq number */
    u_int32_t bad_seq;        /* last 'bad' seq number + 1 */
    u_int32_t probation;      /* sequ. packets till source is valid */
    u_int32_t received;       /* packets received */
    u_int32_t expected_prior; /* packet expected at last interval */
    u_int32_t received_prior; /* packet received at last interval */
    u_int32_t transit;        /* relative trans time for prev pkt */
    u_int32_t jitter;         /* estimated jitter */
    
    unsigned int _bits_per_sample;
    unsigned int _channel_mask;
    int _mtu;
    SocketBuffer _buf;
public:
    RTPStream(unsigned int mtu=1024);
    virtual ~RTPStream();
    void set_bits_per_sample(unsigned int bps);
    void set_channels(unsigned int chnl_mask);
    void set_mtu(unsigned int mtu);
};

#endif // _RTPSTREAM_H_

