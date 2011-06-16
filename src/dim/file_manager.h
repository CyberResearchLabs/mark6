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

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>

using namespace boost::interprocess;

class FileManager {
 private:
  const boost::uint32_t MAX_QUEUE_SIZE;
  const boost::uint32_t MESSAGE_SIZE;

  boost::thread _thread;
  message_queue _mq;

  std::string _mid;
  std::string _mount_point;
  std::string _mount_prefix;
  int _NUM_MOUNT_POINTS;
  struct pollfd* _fds;
  nfds_t _NFDS;
  int _POLL_TIMEOUT;

  boost::uint64_t* _write_offset;
  boost::uint32_t _WRITE_BLOCK_SIZE;
  boost::uint8_t* _buf;
  bool _running;


 public:
  // Default constructor to enable copying in boost::thread.
  FileManager(const std::string mid,
	      const std::string filename,
	      const std::string mount_point,
	      const int num_mount_points,
	      const int write_block_size,
	      const int poll_timeout);
  
  //! Destructor.
  ~FileManager();

  // Thread operations
  void start();
  void join();
  void run();

  // Control operations.
  bool check_control();

  // File operations.
  int open(const std::string);
  int close();
  int write(char* buf, int n);
  int read(char* buf, int n);
};

#endif // _FILEMANAGER_H_

