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

#ifndef _RECEIVERTHREAD_H_
#define _RECEIVERTHREAD_H_

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


class ReceiverThread {
 private:
  boost::uint8_t *_buf;
  boost::uint32_t _crc32;

 public:
  // Default constructor to enable copying in boost::thread.
  ReceiverThread() { }

  // Callable operator. Parameters to class passed in at this point.
  void operator() (const std::string& ip,
		   const boost::uint16_t port,
		   const boost::uint32_t stream_rate,
		   const boost::uint32_t duration,
		   const boost::uint32_t mtu,
		   const boost::uint32_t write_block_size) {
    LOG4CXX_INFO(logger, "ReceiverThread: " << "<" << ip << "," << port << ">");
  }

  ~ReceiverThread() {
  }
};

#endif // _RECEIVERTHREAD_H_
