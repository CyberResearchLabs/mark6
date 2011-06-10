/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
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

#ifndef _SENDERTHREAD_H_
#define _SENDERTHREAD_H_

// C includes
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <string.h>

// C++ includes.
#include <sstream>

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t

struct Message {
  boost::uint32_t sync;
  boost::uint32_t sn;
  boost::uint32_t len;
  boost::uint32_t crc32;
  boost::uint8_t data[1];
};

class SenderThread {
 private:
  boost::uint8_t *_buf;
  struct Message* _m;
  boost::uint32_t _crc32;

 public:
  // Default constructor to enable copying in boost::thread.
  SenderThread() { }

  // Callable operator. Parameters to class passed in at this point.
  void operator() (const std::string& ip,
		   const boost::uint16_t port,
		   const boost::uint32_t stream_rate,
		   const boost::uint32_t duration,
		   const boost::uint32_t mtu,
		   const boost::uint32_t write_block_size) {
    LOG4CXX_INFO(logger, "SenderThread: " << "<" << ip << "," << port << ">");

    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0)
      throw std::string("Unable to allocate socket");

    LOG4CXX_DEBUG(logger, "Setting up address");
    char* p=(char*)ip.c_str();
    struct sockaddr_in addr;
    int ret=::inet_aton(p, &addr.sin_addr);
    if (ret==0)
      throw std::string("Invalid IP address string.");
#ifndef LINUX
    addr.sin_len=sizeof(addr);
#endif // LINUX
    addr.sin_family=AF_INET;
    addr.sin_port=htons(port);

    LOG4CXX_DEBUG(logger, "Connecting UDP socket");    
    ret=::connect(fd, (struct sockaddr*)&addr, sizeof(addr));
    if (ret<0)
      throw std::string(strerror(errno));

    LOG4CXX_DEBUG(logger, "Connected to "
		  << "addr.sin_port:" << (int)ntohs(addr.sin_port) << " "
		  << "addr.sin_addr:" << inet_ntoa(addr.sin_addr) );

    // Create TVG packet.
    _buf = new boost::uint8_t[mtu];
    _m = (struct Message*)_buf;
    _m->sync = 0xCAFEBABE;
    _m->len = mtu - 4*4;
    for (boost::uint32_t i=0; i<_m->len; ++i) {
      _m->data[i] = static_cast<boost::uint8_t>(i);
    }

    boost::crc_32_type computer;
    computer.reset();
    computer.process_bytes(_m, _m->len);
    _crc32 = computer.checksum();
    _m->crc32 = _crc32;

    // Transmit loop.
    const double packet_rate = stream_rate/(8*mtu);
    const boost::uint32_t packet_count = packet_rate * duration;
    LOG4CXX_DEBUG(logger, "packet_count: " << packet_count);

    for (int j=0; j<packet_count; ++j) {
      _m->sn = j;
      int nwritten = write(fd, _buf, mtu);
      if (nwritten < 0) {
	LOG4CXX_ERROR(logger, "sn:" << _m->sn << " " << strerror(errno));
      } else {
	LOG4CXX_DEBUG(logger, "sn: " << j << " nwritten: " << nwritten);
      }
    }

    delete [] _buf;
  }

  ~SenderThread() {
  }
};

#endif // _SENDERTHREAD_H_
