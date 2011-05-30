/*
 *  RTPOsStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPOsStream.h"

RTPOsStream::RTPOsStream(const string& ip, const int port, const int mtu)
{
    _out_sock.connect(ip, port);
    _mtu=mtu;
}  // End RTPOsStream().

RTPOsStream::~RTPOsStream()
{
    _out_sock.close();
}  // End RTPOsStream().

int RTPOsStream::write_next_sample(SocketBuffer& s, Timestamp& t)
{
    return(_out_sock.send(s));
#if 0
    if (s.get_req_size()<=_mtu) {
        return(_out_sock.send(s));
    }
    int ret=0;
    int i=0;
    for (i=0; i<s.get_req_size(); i+=_mtu) {
        strncpy((char*)_buf, &((char*)s)[i], _mtu);	// XXXX yuk!
        _buf.set_size(_mtu);
        ret+=_out_sock.send(_buf);
    }
    strncpy((char*)_buf, &((char*)s)[i], i-s.get_size());	// XXXX yuk!
    ret+=_out_sock.send(_buf);
    return(ret);
#endif
}    // End RTPOsStream().

int RTPOsStream::write_next_sample(string& s, int n, Timestamp& t)
{
    return(_out_sock.send(s, n));
}    // End RTPOsStream().

