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
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>

#include "UDPSocket.h"

UDPSocket::UDPSocket():
Socket(SOCK_DGRAM)
{
	START_FUNC("UDPSocket")
	set_so_reuseaddr(1);
	END_FUNC("UDPSocket")
}

UDPSocket::UDPSocket(const int& s, const int& m):
Socket(SOCK_DGRAM, s, m)
{
	START_FUNC("UDPSocket")
	set_so_reuseaddr(1);
	END_FUNC("UDPSocket")
}

UDPSocket::UDPSocket(const UDPSocket& s)
{
	START_FUNC("UDPSocket")
	_sockd=s.get_sockd();
	_mtu=s.get_mtu();
	set_so_reuseaddr(1);
	END_FUNC("UDPSocket")
}

UDPSocket::~UDPSocket()
{
	START_FUNC("UDPSocket")
	END_FUNC("UDPSocket")
    if (_sockd!=-1)
        ::close(_sockd);
	END_FUNC("Socket")
}

UDPSocket& UDPSocket::operator=(const UDPSocket& s)
{
	START_FUNC("UDPSocket")
    _sockd=s.get_sockd();
    int yes=1;
    setsockopt(_sockd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
	_mtu=s.get_mtu();
	END_FUNC("UDPSocket")
    return(*this);
}

void UDPSocket::set_sockd(const int& s)
{
	START_FUNC("UDPSocket")
    if (_sockd!=-1)
        ::close(_sockd);
    _sockd=s;
	END_FUNC("UDPSocket")
}

void UDPSocket::bind(const string& ip, const int& port)
{
	START_FUNC("UDPSocket")
    const char* const p=(char*)ip.c_str();
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
    if (ret==-1) {
        perror("UDPSocket ");
        throw SocketException("Bind error.");
    }
	END_FUNC("UDPSocket")
}

void UDPSocket::connect(const string& ip, const int& port)
{
	START_FUNC("UDPSocket")
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
    if (ret==-1) {
        throw SocketException("Connect error.");
    }
	END_FUNC("UDPSocket")
}

int UDPSocket::recv(SocketBuffer& s, const int& n)
{
    int ret=0;
    if (n<=0) {
        throw SocketException("Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    ret=::recv(_sockd, (char*)&s[0], n, 0);
    return(ret);
}

int UDPSocket::send(const SocketBuffer& s, const int& n)
{
    if (n<=0) {
        throw SocketException("Zero size buffer");
    }
    if (_sockd==-1) {
        throw SocketException("Sockd error");
    }
    int ret=::send(_sockd, (char*)&s[0], n, 0);
    return(ret);
}

int UDPSocket::recv(const vector<SocketBufferCtrlBlk>& l)
{
    if (l.size()<=0) {
		return(0);
    }
	const int iovcnt=l.size();
	iovec* v=new struct iovec[iovcnt];
	for (unsigned int i=0; i<l.size(); ++i) {
		v[i].iov_base=(char*)&(l[i]._sb[0]);
		v[i].iov_len=l[i]._sb.size();
	}
    int ret=::readv(_sockd, v, iovcnt);
	delete [] v;
    return(ret);
}

int UDPSocket::send(const vector<SocketBufferCtrlBlk>& l)
{
    if (l.size()<=0) {
		return(0);
    }
	const int iovcnt=l.size();
	iovec* v=new struct iovec[iovcnt];
	for (unsigned int i=0; i<l.size(); ++i) {
		v[i].iov_base=(char*)&(l[i]._sb[0]);
		v[i].iov_len=l[i]._sb.size();
	}
    int ret=::writev(_sockd, v, iovcnt);
	delete [] v;
    return(ret);
}

int UDPSocket::recvfrom(struct sockaddr& from, int& fromlen,
						SocketBuffer& s, const int& n)
{
    int ret=::recvfrom(_sockd, (void*)&s[0], n, 0, (struct sockaddr*)&from, 
						(socklen_t*)&fromlen);
    return(ret);
}

int UDPSocket::sendto(const struct sockaddr& to, const int& tolen,
					 const SocketBuffer& s, const int& n)
{
    int ret=::sendto(_sockd, (const char*)&s[0], n, 0, (struct sockaddr*)&to, 
				tolen);
    return(ret);
}

void UDPSocket::close()
{
	START_FUNC("UDPSocket")
    if (_sockd==-1) {
        ERROR_FUNC("UDPSocket") << "Attempt to close unallocated socket\n";
    }
    ::close(_sockd);
	END_FUNC("UDPSocket")
}

ostream& operator<<(ostream& os, const UDPSocket& s)
{
    os << "UDPSocket dump\n";
    os << "    _sockd:         " << s.get_sockd() << endl;
    os << "    _bytes_sent:    " << s.get_bytes_sent() << endl;
    os << "    _bytes_rcvd:    " << s.get_bytes_rcvd() << endl;
    os << "    _send_calls:    " << s.get_send_calls() << endl;
    os << "    _recv_calls:    " << s.get_recv_calls() << endl;
    os << "-UDPSocket dump\n";
    return(os);
}

static bool parent()
{   
    UDPSocket s;
    string ip("0.0.0.0");
    int port(49001);
    s.bind(ip, port);
	if (s.get_sockd()<=0)
		return(false);

    SocketBuffer sbuf;
    sbuf.reserve(1024);
    int rcvd_bytes=s.recv(sbuf, 1024);
    if (rcvd_bytes!=1024) {
		s.close();
        return(false);
	}
    for (int i=0; i<1024; i++) {
        if (sbuf[i]!=i%10) {
    		s.close();
            return(false);
		}
    }
    s.close();
    return(true);
}       

static bool child()
{
    string ip("127.0.0.1");
    int port(49001);
    UDPSocket c;
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

bool UDPSocket::test()
{   
    pid_t child_id=fork();
    bool ret=false;
    if (child_id==0)
        ret=parent();
    else
        ret=child();
    return(ret);
}

