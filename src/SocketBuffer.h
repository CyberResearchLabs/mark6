/*
 *  SocketBuffer.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <string>
#include <string.h>
#include <stdexcept>

#ifndef _SOCKET_BUFFER_
#define _SOCKET_BUFFER_

using namespace std;

const int DEFAULT_SOCKET_BUFFER_SIZE=1024;

class SocketBufferError: public runtime_error {
public:
    SocketBufferError(const string& msg): runtime_error(msg) { }
};

class SocketBuffer {
private:
    char* _buf;
    int _max_size;
    int _size;
    int _req_size;
public:
    SocketBuffer();
    SocketBuffer(int s);
    SocketBuffer(const string& s);
    SocketBuffer(const SocketBuffer& s);
    SocketBuffer& operator=(const SocketBuffer& s);
    SocketBuffer& operator+=(const SocketBuffer& s);
    ~SocketBuffer();
    int get_size() const;
    void set_size(int s);
    int get_req_size() const;
    void set_req_size(int s);
    int get_max_size() const;
    const char* get_buf() const;
    operator char*();
    char& operator[](int i);
    void set(string& s);
    void clear();
};

#endif


