/*
 *  SocketBuffer.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef _SOCKET_BUFFER_
#define _SOCKET_BUFFER_

class SocketBuffer {
    char* _buf;
    int _max_size;
    int _size;
public:
        SocketBuffer(int s);
    ~SocketBuffer();
    int get_size();
    void set_size(int s);
    int get_max_size();
    operator char*();
};

#endif


