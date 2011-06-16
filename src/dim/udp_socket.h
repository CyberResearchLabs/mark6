/*
 *  UDPSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Tue Feb 24 2004.
 *
 * Copyright 2004, 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <socket.h>
#include <socket_buffer.h>

#ifndef _UDP_SOCKET_H_
#define _UDP_SOCKET_H_

class UDPSocket: public vtp::Socket 
{
public:
    UDPSocket();
    UDPSocket(int s);
    virtual ~UDPSocket();
    
    // Socket API.
    void bind(string ip, int port);
    int recvfrom(string ip, int port, SocketBuffer& s);
    int sendto(string ip, int port, SocketBuffer& s);
    void close();
#ifdef _DAVE_DEBUG
    virtual int test(void*);
    virtual int dump(void*);
#endif
};

#endif

