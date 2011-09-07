/*
 * Create by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the Li!cense, or
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

#ifndef _SOCKET_MANAGER_H_
#define _SOCKET_MANAGER_H_

// C includes.
#include <errno.h>

// Socket includes
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <errno.h>

// C++ includes.
#include <list>

// Framework includes.
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <queue.h>

struct InvalidSocketOperation: std::exception {
  char const* what() const throw() {
    return "Invalid socket operation.";
  }
};

class SocketManager {
  std::string _IP;
  boost::uint16_t _PORT;
  int _sock;

 public:
  SocketManager(const std::string& id, 
		const std::string& ip,
		const boost::uint16_t port):
    _IP(ip),
    _PORT(port)
    {
      // Setup local interface.
      const char* p = _IP.c_str();
      struct sockaddr_in addr;
      int ret = ::inet_aton(p, &addr.sin_addr);
      addr.sin_family  = AF_INET;
      addr.sin_port = htons(port);
      if (ret == 0)
	throw InvalidSocketOperation();
      
      int sock = socket(AF_INET, SOCK_DGRAM, 0);
      if (sock < 0)
	throw InvalidSocketOperation();
      
      int optval;
      int optlen = sizeof(optlen);
      if (getsockopt(sock, SOL_SOCKET, SO_RCVBUF, &optval, (socklen_t*)&optlen) < 0)
	throw InvalidSocketOperation();

      std::cout << "SO_RCVBUF:" << optval << std::endl;

      const int N = 1048576;
      if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &N, sizeof(N)) < 0)
	throw InvalidSocketOperation();

      if (getsockopt(sock, SOL_SOCKET, SO_RCVBUF, &optval, (socklen_t*)&optlen) < 0)
	throw InvalidSocketOperation();

      std::cout << "+SO_RCVBUF:" << optval << std::endl;

      // Clean up cast below -- seems to be standard practice though.
      ret == ::bind(sock, (struct sockaddr*)&addr, sizeof(addr));
      if (ret < 0)
	throw InvalidSocketOperation();

      _sock = sock;
    }
    
    ~SocketManager() {}

    int read(boost::uint8_t* buf, const int BUF_SIZE) {
      int bytes_left = BUF_SIZE;
      int bytes_read = 0;

      while (bytes_left > 0) {
	int nread = ::read(_sock, buf + bytes_read, bytes_left);
	if (nread > 0) {
	  bytes_read += nread;
	  bytes_left -= nread;
	} else {
	  std::cerr << nread << " bytes read\n";
	}
      }
      
      return bytes_read;
    }
};

#endif // _SOCKET_MANAGER_H_

