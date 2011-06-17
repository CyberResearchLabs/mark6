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

#ifndef _FILEMANAGER_H_
#define _FILEMANAGER_H_

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

using namespace boost::interprocess;

// Types.
typedef std::vector<boost::uint8_t> Buffer;
typedef boost::circular_buffer<Buffer*> CircularBuffer;

class FileManager {
 private:
  // Configuration options.
  std::string _MID;
  std::string _MOUNT_POINT;
  std::string _MOUNT_PREFIX;
  boost::uint32_t _MOUNT_POINTS;
  boost::uint32_t _WRITE_BLOCK_SIZE;
  boost::uint32_t _WRITE_BLOCKS;
  boost::uint32_t _POLL_TIMEOUT;
  double _COMMAND_INTERVAL;

  // Message queue.
  const boost::uint32_t _MAX_QUEUE_SIZE;
  const boost::uint32_t _MESSAGE_SIZE;
  message_queue _mq;

  // File data structures.
  std::vector<struct pollfd> _fds;
  CircularBuffer _cbuf;

  // Threading.
  bool _running;
  enum { IDLE, WRITE_TO_DISK, STOP } _state;
  boost::thread _thread;
  boost::mutex _cbuf_mutex;

 public:
  // Default constructor to enable copying in boost::thread.
  FileManager(const std::string mid,
	      const std::string filename,
	      const std::string mount_point,
	      const boost::uint32_t mount_points,
	      const boost::uint32_t write_block_size,
	      const boost::uint32_t write_blocks,
	      const boost::uint32_t poll_timeout,
	      const double command_interval);
  
  //! Destructor.
  ~FileManager();

  // Thread operations
  void start();
  void join();

  // File operations.
  int open(const std::string);
  int close();
  bool write(Buffer* buf);
  bool read(Buffer* buf);

 private:
  // Thread operations
  void run();

  // Control operations.
  bool check_control();

  // File operations.
  void write_block(const int fd);
};

#endif // _FILEMANAGER_H_

