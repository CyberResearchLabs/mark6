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

// C includes
#include <errno.h>
#include <string.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/filesystem.hpp>

using namespace boost::filesystem;

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_manager.h>


FileManager::FileManager(const std::string mid,
			 const std::string mount_point,
			 const std::string mount_prefix,
			 const int num_mount_points,
			 const int write_block_size,
			 const int poll_timeout):
  MAX_QUEUE_SIZE(100),
  MESSAGE_SIZE(sizeof(ControlMessage)),
  _mq(create_only, mid.c_str(), MAX_QUEUE_SIZE, MESSAGE_SIZE), 
  _mid(mid),
  _mount_point(mount_point),
  _mount_prefix(mount_prefix),
  _NUM_MOUNT_POINTS(num_mount_points),
  _fds(0),
  _NFDS(num_mount_points),
  _POLL_TIMEOUT(poll_timeout),
  _write_offset(0),
  _WRITE_BLOCK_SIZE(write_block_size),
  _buf(0),
  _running(false) {
  _fds = new struct pollfd[_NFDS];
  for (nfds_t i=0; i<_NFDS; ++i)
    _fds[i].fd = -1;

  _write_offset = new boost::uint64_t[_NUM_MOUNT_POINTS];
  _buf = new boost::uint8_t[_WRITE_BLOCK_SIZE];
}

FileManager::~FileManager() {
  close();
  delete [] _buf;
  delete [] _write_offset;
  delete [] _fds;
  message_queue::remove(_mid.c_str());
}

void FileManager::start() {
  _running = true;
  _thread = boost::thread(&FileManager::run, this);
}

void FileManager::run() {
  // Main entry point.
  LOG4CXX_INFO(logger, "Running...");
  bool running = true;

  try {
    while (running) {
      if (check_control())
	continue;
      
      // Process data.
      int bytes_left = 1;

      // Poll file descriptors.
      int rv = poll(_fds, _NFDS, _POLL_TIMEOUT);
      if (rv < 0) {
	LOG4CXX_ERROR(logger, "poll returns error: " << strerror(errno));
      } else if (rv == 0) {
	LOG4CXX_DEBUG(logger, "poll returns 0");
	continue;
      }

      // Scan file descriptors writing to next available descriptor.
      for (nfds_t i=0; i<_NFDS; ++i) {
	int fd = _fds[i].fd;
	if ( (_fds[i].revents & POLLOUT) == POLLOUT) {
	  int nb = ::write(fd, _buf, _WRITE_BLOCK_SIZE);
	  if (nb > 0)
	    bytes_left -= nb;
	  if (bytes_left < 0)
	    break;
	}
      }
    }
  } catch(interprocess_exception &ex) {
    LOG4CXX_ERROR(logger, "mq error: " << ex.what());
  }
}

void FileManager::join() {
  _thread.join();
}

bool FileManager::check_control() {
  ControlMessage m;
  unsigned int priority;
  std::size_t recvd_size;

  // Read in next control message.
  bool rcvd_msg = _mq.try_receive(&m, sizeof(m), recvd_size, priority);
  if (!rcvd_msg) {
    return false;
  }

  switch (m._type) {
  case STOP:
    LOG4CXX_DEBUG(logger, "Received stop");
    _running = false;
    break;
  default:
    break;
  }
 
  return true;
}

int FileManager::open(const std::string file_name) {
  std::vector<std::string> paths;
  for (int i=0; i<_NUM_MOUNT_POINTS; ++i) {
    std::ostringstream ss;
    ss << _mount_point << "/" << _mount_prefix << i << "/" << file_name;
    paths.push_back(ss.str());
    ss.clear();
  }

  int ret=0;
  int i=0;

  BOOST_FOREACH(std::string p, paths) {
    LOG4CXX_DEBUG(logger, "path: " << p);

    int fd = ::open(p.c_str(),  O_WRONLY | O_CREAT | O_NONBLOCK, S_IRWXU);
    if (fd<0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << p << " - " 
		    << strerror(errno));
      _fds[i++].fd = -1;
      ret = -1;
    } else {
      _fds[i++].fd = fd;
      _fds[i++].events = POLLOUT;
    }
  }

  return ret;
}

int FileManager::close() {
  int ret = 0, RET = 0;
  for (int i=0; i<_NUM_MOUNT_POINTS; ++i) {
    if (_fds[i].fd > 0) {
      if ( (ret=::close(_fds[i].fd)) < 0) {
	LOG4CXX_ERROR(logger, "Unable to close fd: " << _fds[i].fd
		      << " - " << strerror(errno));
	RET = ret;
      }
    }
  }
  return RET;
}

int FileManager::write(char* buf, int n) {
  return 0;
}

int FileManager::read(char* buf, int n) {
  return 0;
}


