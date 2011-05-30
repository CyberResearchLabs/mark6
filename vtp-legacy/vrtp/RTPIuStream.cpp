/*
 *  RTPIuStream.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Tue Mar 02 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include "RTPIuStream.h"

RTPIuStream::RTPIuStream(string& ip, unsigned int port, unsigned int mtu)
{
    _in_sock.bind(ip, port);
    _in_sock.listen();
    _in_sock.accept();
}  // End RTPIuStream().

RTPIuStream::~RTPIuStream()
{
    _in_sock.close();
}  // End ~RTPIuStream().

int RTPIuStream::read_next_sample(SocketBuffer& s, Timestamp& t)
{
	if (s.get_req_size()<=_mtu) {
            return(_in_sock.recv(s));
	}
	int ret=0;
	for (int i=0; i<s.get_req_size(); i+=_mtu) {
            ret+=_in_sock.recv(_buf);
            s+=_buf;
	}
	ret+=_in_sock.recv(_buf);
	s+=_buf;
	return(ret);
}    // End read_next_sample().


