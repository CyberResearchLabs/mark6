/*
 *  RTP.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream.h>

#include "RTP.h"

RTPPacket::RTPPacket(unsigned int buf_size)
{
    _buf=new u_int8_t[buf_size];
    _hdrp=(_hdr*)_buf;
}

RTPPacket::RTPPacket(unsigned int buf_size, 
          unsigned int v,
          unsigned int p,
          unsigned int x,
          unsigned int cc,
          unsigned int m,
          unsigned int pt,
          unsigned int seq,
          unsigned int ts,
          unsigned int ssrc,
          u_int32_t* csrc,
          u_int8_t data[],
          u_int32_t data_size)
{
    _buf=new u_int8_t[buf_size];
    _hdrp=(_hdr*)_buf;    
    _hdrp->version=v;
    _hdrp->p=p;
    _hdrp->x=x;
    _hdrp->cc=cc;
    _hdrp->m=m;
    _hdrp->pt=pt;
    _hdrp->seq=seq;
    _hdrp->ts=ts;
    _hdrp->ssrc=ssrc;
    u_int32_t* pcsrc=&_hdrp->csrc;
    for (int i=0; i<cc; i++) {
        if (csrc==NULL) {
            RTPException e(1, "NULL csrc pointer");
            throw e;
        }
        if (i>(_buf_size-sizeof(_hdr)+4)/4) {
            RTPException e(1, "CSRC list too large for buffer");
            throw e;
        }
        pcsrc[i]=csrc[i];
        p++;
    }
    if (data_size>(_buf_size-sizeof(_hdr)+1)) {
        RTPException e(1, "Data size larger than buffer size");
        throw e;
    }
    memcpy((void*)pcsrc, (const void*)data, (size_t)data_size);
}

RTPPacket::~RTPPacket()
{
    
}

u_int8_t* RTPPacket::get_buf()
{
    return(_buf);
}

u_int32_t RTPPacket::get_version()
{
    return(_hdrp->version);
}

u_int32_t RTPPacket::get_p()
{
    return(_hdrp->p);    
}

u_int32_t RTPPacket::get_cc()
{
    return(_hdrp->cc);    
}

u_int32_t RTPPacket::get_m()
{
    return(_hdrp->m);
}

u_int32_t RTPPacket::get_pt()
{
    return(_hdrp->pt);    
}

u_int32_t RTPPacket::get_seq()
{
    return(_hdrp->seq);    
}

u_int32_t RTPPacket::get_ts()
{
    return(_hdrp->ts);        
}

u_int32_t RTPPacket::get_ssrc()
{
    return(_hdrp->ssrc);
}

u_int32_t* RTPPacket::get_csrc()
{
    return((u_int32_t*)&_hdrp->csrc);
}

u_int8_t* RTPPacket::get_data()
{
    return((u_int8_t*)&_hdrp->csrc+_hdrp->cc);    
}

void RTPPacket::dump(ostream& out)
{
    out << "RTPPacket::dump()" << endl;
    out.setf(ios_base::hex, ios_base::basefield);
    out.width(10);
    out << "version = " << _hdrp->version << endl;
    out << "p       = " << _hdrp->p << endl;
    out << "cc      = " << _hdrp->cc << endl;
    out << "m       = " << _hdrp->m << endl;
    out << "pt      = " << _hdrp->pt << endl;
    out << "seq     = " << _hdrp->seq << endl;
    out << "ts      = " << _hdrp->ts << endl;
    out << "ssrc    = " << _hdrp->ssrc << endl;
    out << "data[0] = " << (int)*(u_int8_t*)(&_hdrp->csrc+_hdrp->cc) << endl;
}