/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2003 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE * SOFTWARE. 
 * 
 */


/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2004 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE * SOFTWARE. 
 * 
 */

#ifdef LINUX
#include <unistd.h>
#endif
#include <fcntl.h>
#include <Socket.h>

const int Socket::BLOCKING=0x1;
const int Socket::NONBLOCKING=0x10;
const int Socket::SEL_READ=0x1;
const int Socket::SEL_WRITE=0x10;
const int Socket::SEL_EXCEPT=0x100;

Socket::Socket(const int& type)
{
	START_FUNC("Socket")
	_type=type;
 	_bytes_sent=_bytes_rcvd=_send_calls=_recv_calls=0;
	_sockd=socket(AF_INET, type, 0);
	_blocking=BLOCKING;
	END_FUNC("Socket")
}

Socket::Socket(const int& type, const int& s, const int&m)
{
	START_FUNC("Socket")
	_type=type;
	_sockd=s;
	_mtu=m;
 	_bytes_sent=_bytes_rcvd=_send_calls=_recv_calls=0;
	_blocking=BLOCKING;
	END_FUNC("Socket")
}

Socket::Socket() 
{
	START_FUNC("Socket")
 	_bytes_sent=_bytes_rcvd=_send_calls=_recv_calls=0;
 	_sockd=-1;
 	_mtu=1024;
	_type=SOCK_STREAM;
	_blocking=BLOCKING;
	END_FUNC("Socket")
}

Socket::~Socket() 
{
	START_FUNC("Socket")
	END_FUNC("Socket")
}

// Member access.
int Socket::get_sockd() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_sockd);
}

int Socket::get_type() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_type);
}

int Socket::get_mtu() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_mtu);
}

int Socket::get_bytes_sent() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_bytes_sent);
}   

int Socket::get_bytes_rcvd() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_bytes_rcvd);
}

int Socket::get_send_calls() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_send_calls);
}

int Socket::get_recv_calls() const {
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_recv_calls);
}

// Options.
int Socket::set_so_reuseaddr(const int& yes)
{
	START_FUNC("Socket")
	_so_reuseaddr=yes;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &_so_reuseaddr, 
			sizeof(int)));
}

int Socket::get_so_reuseaddr() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_reuseaddr);
}

int Socket::set_so_linger(const int& onoff, const int& time)
{
	START_FUNC("Socket")
	_so_linger.l_onoff=onoff;
	_so_linger.l_linger=time;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_LINGER, &_so_linger, 
			sizeof(struct linger)));
}

const struct linger& Socket::get_so_linger() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_linger);
}

int Socket::set_so_rcvbuf(const int& sz)
{
	START_FUNC("Socket")
	_so_rcvbuf=sz;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_RCVBUF, &_so_rcvbuf,
            sizeof(int)));	
}

int Socket::get_so_rcvbuf() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_rcvbuf);
}

int Socket::set_so_sndbuf(const int& sz)
{
	START_FUNC("Socket")
	_so_sndbuf=sz;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_SNDBUF, &_so_sndbuf,
            sizeof(int)));	
}

int Socket::get_so_sndbuf() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_sndbuf);
}

int Socket::set_so_rcvlowat(const int& m)
{
	START_FUNC("Socket")
	_so_rcvlowat=m;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_RCVLOWAT, &_so_rcvlowat,
            sizeof(int)));	
}

int Socket::get_so_rcvlowat() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_rcvlowat);
}

int Socket::set_so_sndlowat(const int& m)
{
	START_FUNC("Socket")
    _so_sndlowat=m;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, SOL_SOCKET, SO_SNDLOWAT, &_so_sndlowat,
            sizeof(int)));
}

int Socket::get_so_sndlowat() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(_so_sndlowat);
}

int Socket::set_tcp_nodelay(const int& n)
{
	START_FUNC("Socket")
    if (_type!=SOCK_STREAM)
        return(-1);
    _tcp_nodelay=n;
	END_FUNC("Socket")
    return(::setsockopt(_sockd, IPPROTO_TCP, TCP_NODELAY, &_tcp_nodelay,
            sizeof(int)));
}

int Socket::get_tcp_nodelay() const
{
	START_FUNC("Socket")
    if (_type!=SOCK_STREAM)
        return(-1);
	END_FUNC("Socket")
    return(_tcp_nodelay);
}

// Blocking.
int Socket::set_blocking(const int& b)
{
	START_FUNC("Socket")
	int ret=0;
	if (_blocking==b)
		return(0);
	_blocking=b;
	if (b==NONBLOCKING) {
		_blocking_flags=fcntl(_sockd, F_GETFL, 0);
		ret=fcntl(_sockd, F_SETFL, _blocking_flags | O_NONBLOCK);
	} 
	if (b==BLOCKING) {
		ret=fcntl(_sockd, F_SETFL, _blocking_flags);
	}
	if (ret!=-1)
		ret=0;
	END_FUNC("Socket")
	return(ret);	
}

int Socket::get_blocking() const
{
	START_FUNC("Socket")
	END_FUNC("Socket")
    return(_blocking);
}

int Socket::select(const int rwe, double to)
{
	START_FUNC("Socket")
    fd_set rset, wset, eset;    
	FD_ZERO(&rset);
	wset=eset=rset;
	if (rwe & SEL_READ)
		FD_SET(_sockd, &rset);
	if (rwe & SEL_WRITE)
		FD_SET(_sockd, &wset);
	if (rwe & SEL_EXCEPT)
		FD_SET(_sockd, &eset);
	struct timeval lto;
	int sec=(int)to;
	int usec=int((to-sec)*1000000.0);
	lto.tv_sec=sec;
	lto.tv_usec=usec;
    DEBUG_FUNC("Socket") << "   sec==" << lto.tv_sec 
						<< ": usec==" << lto.tv_usec << ENDL;
    if (::select(_sockd+1, &rset, &wset, &eset, &lto)==-1) {
    	ERROR_FUNC("TCPSocket") << "    select error.\n";            
		return(-1);
    }
	int ret=0;
	if (rwe & SEL_READ)
		ret=FD_ISSET(_sockd, &rset);
	if (rwe & SEL_WRITE)
		ret|=FD_ISSET(_sockd, &wset)<<1;
	if (rwe & SEL_EXCEPT)
		ret|=FD_ISSET(_sockd, &eset)<<2;
    return(ret);
	END_FUNC("Socket")
}

// Socket connection termination...
const int Socket::shutdown(const int& how)
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(::shutdown(_sockd, how));
}

const int Socket::close()
{
	START_FUNC("Socket")
	END_FUNC("Socket")
	return(::close(_sockd));
}


