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


#include <Socket.h>
#include "TCPSocket.h"
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>

TCPSocket::TCPSocket():
Socket(SOCK_STREAM)
{
	START_FUNC("TCPSocket")
	set_so_reuseaddr(1);
	END_FUNC("TCPSocket")
}

TCPSocket::TCPSocket(const int& m):
Socket(SOCK_STREAM)
{
	START_FUNC("TCPSocket")
	set_so_reuseaddr(1);
	_mtu=m;
	END_FUNC("TCPSocket")
}

TCPSocket::TCPSocket(const int& s, const int& m):
Socket(SOCK_STREAM, s, m)
{
	START_FUNC("TCPSocket")
	set_so_reuseaddr(1);
	END_FUNC("TCPSocket")
}

TCPSocket::TCPSocket(const TCPSocket &s):
Socket()
{
	START_FUNC("TCPSocket")
    _sockd=s._sockd;
	_mtu=s._mtu;
	set_so_reuseaddr(1);
	END_FUNC("TCPSocket")
}

TCPSocket::~TCPSocket()
{
	START_FUNC("TCPSocket")
    // This is an important feature of the TCPSocket class. Especially
    // for copying of TCPSockets. TCPSockets do *not* close their
	// sockets when they are destroyed.
	END_FUNC("TCPSocket")
}

TCPSocket& TCPSocket::operator=(const TCPSocket& s)
{
	START_FUNC("TCPSocket")
    _sockd=s._sockd;
	_mtu=s._mtu;
	set_so_reuseaddr(1);
	END_FUNC("TCPSocket")
    return(*this);
}

void TCPSocket::set_sockd(const int& s)
{
	START_FUNC("TCPSocket")
    if (_sockd!=-1)
        ::close(_sockd);
    _sockd=s;
	END_FUNC("TCPSocket")
}

int TCPSocket::bind(const string& ip, const int& port)
{
	START_FUNC("TCPSocket")
    char* p=(char*)ip.data();
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<0) {
        throw SocketException("Zero length IP string.");
    }      
    ret=::inet_aton(p, &addr.sin_addr);
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    addr.sin_port=htons(port);
    if (ret==0) {
        throw SocketException("Invalid IP address string.");
    }
    if (_sockd==-1) {
        throw SocketException("Invalid sock descriptor.");
    }
    ret=::bind(_sockd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret==-1)
    	ERROR_FUNC("TCPSocket") << "Bind error.\n";
	END_FUNC("TCPSocket")
	return(ret);
}

int TCPSocket::listen()
{
	START_FUNC("TCPSocket")
    if (_sockd==-1)
        DEBUG_FUNC("TCPSocket") << "Invalid sock descriptor.\n";
    int ret=::listen(_sockd, 10);
	if (ret==-1)
		ERROR_FUNC("TCPSocket") << "Listen error.\n";
	END_FUNC("TCPSocket")
	return(ret);
}

int TCPSocket::accept()
{
	START_FUNC("TCPSocket")
    if (_sockd==-1) {
        throw SocketException("Invalid sock descriptor.");
    }
    struct sockaddr addr;
    socklen_t addrlen=sizeof(addr);
    int ret=::accept(_sockd, &addr, &addrlen);
	if (ret==-1)
		ERROR_FUNC("TCPSocket") << "Accept error\n";
    DEBUG_FUNC("TCPSocket") << "Accept sock " << ret << "\n";
	END_FUNC("TCPSocket")
    return(ret);
}

int TCPSocket::connect(const string& ip, const int& port)
{
	START_FUNC("TCPSocket")
    struct sockaddr_in addr;
    int ret=0;
    if (ip.size()<=0) {
        throw SocketException("Zero length IP string.");
    }
    ret=::inet_aton((char*)ip.data(), &addr.sin_addr);
    if (ret==0) {
        throw SocketException("Invalid IP address string.");
    }
    addr.sin_port=htons(port);
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    ret=::connect(_sockd, (struct sockaddr*)&addr, sizeof(addr));
	if (ret==-1)
		ERROR_FUNC("TCPSocket") << "connect() error\n";
	END_FUNC("TCPSocket")
	return(ret);
}

int TCPSocket::recv(SocketBuffer& s, const int& n)
{
	START_FUNC("TCPSocket")
	++_recv_calls;
    if (n<0) {
        throw SocketException("Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    int ret=::recv(_sockd, (char*)&s[0], n, MSG_WAITALL);
    if (ret>0)
		_bytes_rcvd+=ret;
	END_FUNC("TCPSocket")
    return(ret);
}

int TCPSocket::recv(SocketBuffer& s, const int& pos, const int& n)
{
	START_FUNC("TCPSocket")
	++_recv_calls;
    if (n<0) {
        throw SocketException("Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    int ret=::recv(_sockd, (char*)&s[pos], n, 0);
    if (ret>0)
		_bytes_rcvd+=ret;
	END_FUNC("TCPSocket")
    return(ret);
}

int TCPSocket::recv(const vector<SocketBufferCtrlBlk>& l)
{
    START_FUNC("TCPSocket")
    if (l.size()<=0) {
		// FIXME
		INFO_FUNC("TCPSocket") <<  "SBCB list size==" << (int)l.size() << "\n";
        return(0);
    }   
    const int iovcnt=l.size();
    iovec* v=new struct iovec[iovcnt];
    for (unsigned int i=0; i<l.size(); ++i) {
        v[i].iov_base=(char*)&(l[i]._sb[0]);
        v[i].iov_len=l[i]._sb.size();
        INFO_FUNC("TCPSocket") << "v[" << i << "].iov_len=="
                                << (int)v[i].iov_len << "\n";
        INFO_FUNC("TCPSocket") << "v[" << i << "].iov_base[0]=="
                                << (int)((char*)v[i].iov_base)[0] << "\n";
    }                           
    int ret=::readv(_sockd, v, iovcnt);
    delete [] v;
    END_FUNC("TCPSocket")
    return(ret);
} 

int TCPSocket::send(const SocketBuffer& s, const int& n)
{
	START_FUNC("TCPSocket")
	++_send_calls;
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    int ret=::send(_sockd, (char*)&s[0], n, 0);
	if (ret>0)
		_bytes_sent+=ret;
	END_FUNC("TCPSocket")
    return(ret);
}

int TCPSocket::send(const SocketBuffer& s, const int& pos, const int& n)
{
	START_FUNC("TCPSocket")
	++_send_calls;
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    int ret=::send(_sockd, (char*)&s[pos], n, 0);
	if (ret>0)
		_bytes_sent+=ret;
	END_FUNC("TCPSocket")
    return(ret);
}

int TCPSocket::send(const vector<SocketBufferCtrlBlk>& l)
{
    START_FUNC("TCPSocket")
    if (l.size()<=0) {
        return(0);
    }
    const int iovcnt=l.size();
    iovec* v=new struct iovec[iovcnt];
    for (unsigned int i=0; i<l.size(); ++i) {
        v[i].iov_base=(char*)&(l[i]._sb[0]);
        v[i].iov_len=l[i]._sb.size();
        INFO_FUNC("TCPSocket") << "v[" << i << "].iov_len=="
                                << (int)v[i].iov_len << "\n";
    }
    int ret=::writev(_sockd, v, iovcnt);
    delete [] v;
    END_FUNC("TCPSocket")
    return(ret);
}

void TCPSocket::close()
{
	START_FUNC("TCPSocket")
    if (_sockd==-1) {
        ERROR_FUNC("TCPSocket") << "Attempt to close unallocated socket\n";
		return;
    }
    ::close(_sockd);
	END_FUNC("TCPSocket")
}

int TCPSocket::shutdown(int howto)
{
	START_FUNC("TCPSocket")
    if (_sockd==-1) {
        throw SocketException("Attempt to close unallocated socket");
    }
	END_FUNC("TCPSocket")
    return(::shutdown(_sockd, howto));
}

ostream& operator<<(ostream& os, const TCPSocket& s)
{
	os << "TCPSocket dump\n";
    os << "    _sockd:         " << s.get_sockd() << endl;
    os << "    _bytes_sent:    " << s.get_bytes_sent() << endl;
    os << "    _bytes_rcvd:    " << s.get_bytes_rcvd() << endl;
    os << "    _send_calls:    " << s.get_send_calls() << endl;
    os << "    _recv_calls:    " << s.get_recv_calls() << endl;
	os << "-TCPSocket dump\n";
	os << "TCPSocket dump\n";
    return(os);
}

static bool parent()
{
    TCPSocket s;
    string ip("0.0.0.0");
    int port(49000);
    s.bind(ip, port);
    s.listen();
    int sd=s.accept();
    if (sd<=0)
        return(false);
    TCPSocket c(sd, 1024);
    SocketBuffer sbuf;
    sbuf.reserve(1024);
    int rcvd_bytes=c.recv(sbuf, 1024);
    if (rcvd_bytes!=1024) {
    	c.close();
    	s.close();
        return(false);
	}
    for (int i=0; i<1024; i++) {
        if (sbuf[i]!=i%10) {
			c.close();
			s.close();
            return(false);
		}
    }
    c.close();
    s.close();
    return(true);
}

static bool child()
{
    string ip("127.0.0.1");
    int port(49000);
    TCPSocket c;
    usleep(2000000);
    c.connect(ip, port);
    SocketBuffer sbuf(1024);
    for (int i=0; i<1024; i++) {
        sbuf[i]=i%10;
    }
    int sent_bytes=c.send(sbuf, 1024);
    if (sent_bytes!=1024) {
		c.close();
        return(false);
	}
    c.close();
    exit(0);
}

bool TCPSocket::test()
{
    pid_t child_id=fork();
    bool ret=false;
    if (child_id==0)
        ret=parent();
    else
        ret=child();
    return(ret);
}
