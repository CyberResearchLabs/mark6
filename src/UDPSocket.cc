/*
 *  UDPSocket.cc
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
 
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>

#include <Mark6.h>
#include <Socket.h>
#include <UDPSocket.h>

UDPSocket::UDPSocket():
  vtp::Socket()
{
  LOG4CXX_DEBUG(logger, "UDPSocket::UDPSocket()");
  _sockd=socket(AF_INET, SOCK_DGRAM, 0);
}

UDPSocket::UDPSocket(int s):
  vtp::Socket()
{
  LOG4CXX_DEBUG(logger, "UDPSocket::UDPSocket()\n");
  if (_sockd!=-1) {
        
  }
  _sockd=s;
}

UDPSocket::~UDPSocket()
{
  LOG4CXX_DEBUG(logger, "UDPSocket::~UDPSocket()\n");
  if (_sockd!=-1)
    ::close(_sockd);
}

void UDPSocket::bind(string ip, int port)
{
  LOG4CXX_DEBUG(logger, "UDPSocket::bind()\n");
  char* p=(char*)ip.data();
  struct sockaddr_in addr;
  int ret=0;
  if (ip.size()<0) {
    vtp::SocketException e(1, "Zero length IP string.");
    throw e;
  }      
  ret=::inet_aton(p, &addr.sin_addr);
#ifndef LINUX
  addr.sin_len=sizeof(addr);
#endif // LINUX
  addr.sin_family=AF_INET;
  addr.sin_port=htons(port);
  if (ret==0) {
    vtp::SocketException e(1, "Invalid IP address string.");
    throw e;
  }
  if (_sockd==-1) {
    vtp::SocketException e(1, "Invalid sock descriptor.");
    throw e;
  }
  ret=::bind(_sockd, (struct sockaddr*)&addr, sizeof(addr));
  cout << "addr.sin_port: " << (int)addr.sin_port << endl;
  cout << "addr.sin_addr: " << (int)addr.sin_addr.s_addr << endl;
  if (ret==-1) {
    perror("UDPSocket ");
    vtp::SocketException e(1, "Bind error.");
    throw e;
  }
}

int UDPSocket::recvfrom(string ip, int port, SocketBuffer& s)
{
  LOG4CXX_DEBUG(logger, "UDPSocket::recvfrom()");
  char* p=(char*)ip.data();
  struct sockaddr_in from;
  int fromlen=sizeof(from);
  int ret=0;
  if (ip.size()<=0) {
    vtp::SocketException e(1, "Zero length IP string.");
    throw e;
  }
  ret=::inet_aton(p, &from.sin_addr);
  if (ret==0) {
    vtp::SocketException e(1, "Invalid IP address string.");
    throw e;
  }
  if (s.get_req_size()<=0) {
    vtp::SocketException e(1, "Zero size buffer");
    throw e;        
  }
#ifndef LINUX
  from.sin_len=sizeof(from);
#endif // LINUX
  from.sin_family=AF_INET;
  from.sin_port=htons(port);
  ret=::recvfrom(_sockd, (char*)s, s.get_req_size(), 0, (struct sockaddr*)&from, (socklen_t*)&fromlen);
  if (ret<0) {
    vtp::SocketException e(1, "::recvfrom() error");
    throw e;            
  }
  return(ret);
}

int UDPSocket::sendto(string ip, int port, SocketBuffer& s)
{
  LOG4CXX_DEBUG(logger, "UDPSocket::sendto()\n");
  char* p=(char*)ip.data();
  struct sockaddr_in to;
  int tolen=sizeof(to);
  int ret=0;
  char* b=(char*)s;
  if (ip.size()<=0) {
    vtp::SocketException e(1, "Zero length IP string.");
    throw e;
  }
  ret=::inet_aton(p, &to.sin_addr);
  if (ret==0) {
    vtp::SocketException e(1, "Invalid IP address string.");
    throw e;
  }
  if (s.get_size()<=0) {
    vtp::SocketException e(1, "Zero size buffer");
    throw e;        
  }
#ifndef LINUX
  to.sin_len=sizeof(to);
#endif // LINUX
  to.sin_family=AF_INET;
  to.sin_port=htons(port);
  ret=::sendto(_sockd, b, s.get_size(), 0, (struct sockaddr*)&to, tolen);
  if (ret<0) {
    vtp::SocketException e(1, "::sendto() error");
    throw e;            
  }
  return(ret);
}

void UDPSocket::close()
{
  LOG4CXX_DEBUG(logger, "UDPSocket::close()\n");
  if (_sockd==-1) {
    vtp::SocketException e(1, "Attempt to close unallocated socket");
    throw e;
  }
  ::close(_sockd);
}

#ifdef _DAVE_DEBUG
virtual int test(void*);
virtual int dump(void*);
#endif
