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
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/circular_buffer.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

// Local includes
#include <mark6.h>

using namespace boost::interprocess;

/**
 * Manages high speed writing of data to file.
 */
class FileWriter {
 private:
  /** @name Configuration.
   * Configuration options.
   */
  /**@{**/
  /** Unique message id for ITC messaging. */
  std::string _MID;

  /** Size of individual blocks written to disk (e.g. 4096). */
  boost::uint32_t _WRITE_BLOCK_SIZE;
 
  /** Total number of write blocks to buffer in circular buffer. */
  boost::uint32_t _WRITE_BLOCKS;

  /** Poll() timeout in seconds. */
  boost::uint32_t _POLL_TIMEOUT;

  /** Command message queue check interval in seconds. */
  double _COMMAND_INTERVAL;
  /**@}**/


  /** @name MessageQueue.
   *  Boost-based thread-safe messaging system.
   */
  /**@{**/
  /** Maximum number of messages in the queue. */
  const boost::uint32_t _MAX_QUEUE_SIZE;

  /** Size of individual messages. */
  const boost::uint32_t _MESSAGE_SIZE;

  /** Thread-safe message queue object. */
  message_queue _mq;
  /**@}**/


  /** @name FileStructures.
   *  File data structures.
   */
  /**@{**/
  /** File descriptor. Passed into poll() call. */
  struct pollfd _pfd;

  /** Circular buffer for storing data to be written to disk. */
  circular_buffer<boost::uint8_t*> _cbuf;
  /**@}**/

  /** @name Threading.
   *  Threading data.
   */
  /**@{**/
  /** Flag checked in main processing loop to see whether or not to exit. */
  bool _running;

  /** State variable used in main processing loop. */
  enum { IDLE, WRITE_TO_DISK, STOP } _state;

  /** Thread object. */
  boost::thread _thread;

  /** Mutex that enables thread-safe access to _cbuf. */
  boost::mutex _cbuf_mutex;
  /**@}**/

 public:
  /** Constructor. */
  FileWriter(const std::string mid,
	     const boost::uint32_t write_block_size,
	     const boost::uint32_t write_blocks,
	     const boost::uint32_t poll_timeout,
	     const double command_interval);
  
  /** Constructor. */
  ~FileWriter();


  /** @name ThreadOperations */
  /**@{*/
  /** Start main processing loop in a separate thread. */
  void start();
  
  /** Wait for thread to exit. */
  void join();
  /**@}*/


  /** @name FileOperations */
  /**@{*/
  /** Open file descriptors. */
  int open(const std::string);

  /** Close all file descriptors. */
  int close();
  /**@}*/


  /** @name Write/Read API */
  /**@{*/
  /** Add buffer to circular buffer. Note that caller owns the dealloc. */
  bool write(boost::uint8_t* buf);

 private:
  /** Main processing loop. */
  void run();

  /** Check for control messages. Adjust state as necessary. */
  bool check_control();

  /** Write a single block to disk. */
  void write_block(const int fd);
};

#endif // _FILEWRITER_H_

