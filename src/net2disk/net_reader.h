/*
 * Created by David Lapsley on Mon Jun 6 2011.
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

#ifndef _NETREADER_H_
#define _NETREADER_H_

// C includes.

// C++ includes

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>
#include <boost/thread/thread.hpp>

// Local includes
#include <mark6.h>

// TODO: remove globals.
extern long long NUM_PACKETS;
extern long long NUM_BYTES;

class FileWriter;
struct PFR;
class BufferPool;

/**
 * Manages high speed writing of data to file.
 */
class NetReader {
 private:
  const int _id;
  const string _interface;
  const int _snaplen;
  const int _payload_length;
  const int _buffer_size;
  const bool _promiscuous;
  FileWriter* const _fw;
  const int _command_interval;
  PFR* _ring;
  BufferPool* _bp;
  boost::uint8_t* _net_buf;

  bool _running;

  /** State variable used in main processing loop. */
  volatile enum { IDLE, READ_FROM_NETWORK, STOP } _state;

  /** Thread object. */
  boost::thread _thread;

 public:
  /** Constructor. */
  NetReader(const int id,
	    const string interface,
	    const int snaplen,
	    const int payload_length,
	    const int buffer_size,
	    const bool promiscuous,
	    FileWriter* const fw,
	    const double command_interval);
  
  /** Constructor. */
  ~NetReader();

  void start();
  void join();

  void cmd_stop();
  void cmd_read_from_network();

 private:
  /** Main processing loop. */
  void run();
};

#endif // _NETREADER_H_

