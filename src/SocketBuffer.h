/*
 *  SocketBuffer.cc
 *  vrtp
 *
 *  Created by David Lapsley on Fri Feb 20 2004.
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


