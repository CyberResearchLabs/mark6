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

#ifndef _STATSWRITER_H_
#define _STATSWRITER_H_

// C includes.

// C++ includes
#include <fstream>

// Framework includes.
#include <cstddef>    // for std::size_t
#include <boost/thread/mutex.hpp>

// Local includes
#include <mark6.h>
#include <threaded.h>

/**
 * Manages high speed writing of data to file.
 */
class StatsWriter: public Threaded {
 protected:
  const string _stats_file;
  const int _stats_interval;

  boost::uint64_t _num_packets;
  boost::uint64_t _num_bytes;

  std::ofstream _fstream;
  volatile enum { IDLE, WRITE_TO_DISK, STOP } _state;
  boost::mutex _mutex;

  struct timeval _start_time, _last_time;
  double _average_packet_rate;
  double _average_byte_rate;
  boost::uint64_t _last_num_packets;
  boost::uint64_t _last_num_bytes;
  long _interval;
  const double _ALPHA;
  const double _BPS_TO_MBPS;
  const int _PAGE_LENGTH;

 public:
  StatsWriter(const int id,
	      const string& stats_file,
	      const int stats_interval,
	      const double command_interval);
  virtual ~StatsWriter();

  virtual void start();
  virtual void join();

 protected:
  virtual void run();

 public:
  // Comamnds.
  virtual void cmd_stop();
  void cmd_write_to_disk();

 protected:
  // Handlers
  virtual void handle_stop();
  virtual void handle_idle();
  virtual void handle_write_to_disk();

 public:
  // Class specific public API.
  void update(const boost::uint64_t& packets,
	      const boost::uint64_t& bytes);
};

#endif // _STATSWRITER_H_

