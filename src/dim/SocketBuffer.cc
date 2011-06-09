/*
 *  SocketBuffer.h
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

#include <string.h>
#include <SocketBuffer.h>
#include <iostream>

SocketBuffer::SocketBuffer() {
    _max_size=DEFAULT_SOCKET_BUFFER_SIZE;
    _buf=new char[_max_size];
    memset(_buf, 0, _max_size); // XXX
    _size=0;
    _req_size=_max_size;
}

SocketBuffer::SocketBuffer(const string& s) {
    _max_size=s.length()+1;
    _buf=new char[_max_size];
    _size=_req_size=_max_size;
    s.copy(_buf, s.length());
    _buf[_max_size]=0;
}

SocketBuffer::SocketBuffer(int s) {
    _max_size=s;
    _buf=new char[_max_size];
    _req_size=_max_size;
    _size=0;
}

SocketBuffer::SocketBuffer(const SocketBuffer& s)
{
    _max_size=s.get_max_size();
    _buf=new char[_max_size];
    _size=s.get_size();
    _req_size=s.get_req_size();
    strncpy(_buf, s.get_buf(), s.get_size());
}

SocketBuffer& SocketBuffer::operator=(const SocketBuffer& s)
{
    if (_max_size!=s.get_max_size()) {
        _max_size=s.get_max_size();
        delete []_buf;
        _buf=new char[_max_size];
    }
    strncpy(_buf, s.get_buf(), s.get_size());
    _size=s.get_size();
    _req_size=s.get_req_size();
    return(*this);
}

SocketBuffer& SocketBuffer::operator+=(const SocketBuffer& s)
{
    if ((_max_size-_size-s.get_size())>=0) {
        // room in this buffer to copy new buffer.
        char* write_ptr=_buf+_size;
        strncpy(write_ptr, s.get_buf(), s.get_size());
        _size+=s.get_size();
        return(*this);
    }
    // no room. need to enlarge buffer.
    const int buf_factor=2;
    const int new_max_size=buf_factor*(_size+s.get_size());
    char* tmp_buf=new char[new_max_size];
    strncpy(tmp_buf, _buf, _size);
    delete [] _buf;
    _buf=tmp_buf;
    tmp_buf+=_size;	
    strncpy(tmp_buf, s.get_buf(), s.get_size());
    _max_size=new_max_size;
    _req_size=_max_size;
    return(*this);
}

SocketBuffer::~SocketBuffer() 
{
    delete [] _buf;
}

int SocketBuffer::get_size() const 
{
    return(_size);
}

void SocketBuffer::set_size(int s) 
{
    if (s>_max_size)
        throw SocketBufferError("invalid set_size()");
    _size=s;
}

int SocketBuffer::get_req_size() const 
{
    return(_req_size);
}

void SocketBuffer::set_req_size(int s) 
{
    if (s>_max_size)
        throw SocketBufferError("invalid set_req_size()");
    _req_size=s;
}

int SocketBuffer::get_max_size() const 
{
    return(_max_size);
}

const char* SocketBuffer::get_buf() const
{
    return(_buf);
}

SocketBuffer::operator char*() 
{
    if (_max_size>0) {
        return(_buf);
    } else {
        return(0);
    }
}

char& SocketBuffer::operator[](int i)
{
    if (i>_max_size) {
        throw SocketBufferError("subscript too large");
    }
    return(_buf[i]);
}

void SocketBuffer::set(string& s) 
{
    int size=s.size();
    if (size>_max_size)
        throw SocketBufferError("invalid set()");
    strncpy(_buf, s.c_str(), size);
}

void SocketBuffer::clear() 
{
    _size=0;
}

