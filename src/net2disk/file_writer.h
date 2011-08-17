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

class StatsWriter;

/**
 * Manages high speed writing of data to file.
 */
//! Manages the high speed writing of data to file.
//! Includes a circular buffer for storing buffers to be written, as well as
//! a state machine that controls the operation of the thread. The class
//! is an "active" object that runs in its own thread of execution. External
//! objects can interact with it via the start(), join(), cmd_XXX(), 
//! write(), open(), and close() methods.
class FileWriter: public Threaded {
 protected:
  //! The size of blocks to be written to disc.
  const int _WRITE_BLOCK_SIZE;

  //! The maximum number of write blocks that can be buffered at any one time.
  const int _WRITE_BLOCKS;

  //! The timeout for blocking operations.
  const int _POLL_TIMEOUT;

  //! Encapsulates the file descriptor of the file this object manages.
  struct pollfd _pfd;

  //! A circular buffer that contains all of the buffers waiting to be 
  //! written to disk.
  circular_buffer<boost::uint8_t*> _cbuf;

  //! The state of the object.
  volatile enum { IDLE, WRITE_TO_DISK, STOP } _state;

  //! Mutex that protects the circular buffer from multi-threaded access.
  boost::mutex _cbuf_mutex;

  //! The name of the file data will be written to.
  const std::string _capture_file;

  StatsWriter* const _sw;

 protected:
  virtual void run();

  // Handlers
  virtual void handle_stop();
  virtual void handle_idle();
  virtual void handle_write_to_disk();

  void write_block(const int fd);
  
 public:
  FileWriter(const int id,
	     const int write_block_size,
	     const int write_blocks,
	     const std::string& capture_file,
	     const int poll_timeout,
	     StatsWriter * const sw,
	     const double command_interval);
  virtual ~FileWriter();

  virtual void start();
  virtual void join();


 public:
  // Comamnds.
  virtual void cmd_stop();
  void cmd_write_to_disk();

  // Class specific public API.
  int open();
  int close();
  bool write(boost::uint8_t* buf);
};

#endif // _FILEWRITER_H_

