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

#ifndef _FILEWRITER_H_
#define _FILEWRITER_H_

// C includes.
#include <poll.h>

// C++ includes
#include <vector>

// Framework includes.
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>
#include <boost/circular_buffer.hpp>
#include <boost/thread/mutex.hpp>

// Local includes
#include <mark6.h>
#include <threaded.h>
#include <buffer_pool.h>

/**
 * Manages high speed writing of data to file.
 */
class FileWriter: public Threaded {
 protected:
  const int _WRITE_BLOCK_SIZE;
  const int _WRITE_BLOCKS;
  const int _POLL_TIMEOUT;

  struct pollfd _pfd;
  circular_buffer<boost::uint8_t*> _cbuf;
  volatile enum { IDLE, WRITE_TO_DISK, STOP } _state;
  boost::mutex _cbuf_mutex;
  const string _capture_file;
  
 public:
  FileWriter(const int id,
	     const int write_block_size,
	     const int write_blocks,
	     const string& capture_file,
	     const int poll_timeout,
	     const double command_interval);
  virtual ~FileWriter();

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
  int open();
  int close();
  bool write(boost::uint8_t* buf);

 protected:
  void write_block(const int fd);
};

#endif // _FILEWRITER_H_

