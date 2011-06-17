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
#include <unistd.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/filesystem.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_manager.h>

using namespace boost::filesystem;


FileManager::FileManager(const std::string mid,
			 const std::string mount_point,
			 const std::string mount_prefix,
			 const boost::uint32_t mount_points,
			 const boost::uint32_t write_block_size,
			 const boost::uint32_t write_blocks,
			 const boost::uint32_t poll_timeout,
			 const double command_interval):

  // Initialize data members (in order).
  _MID (mid),
  _MOUNT_POINT (mount_point),
  _MOUNT_PREFIX (mount_prefix),
  _MOUNT_POINTS (mount_points),
  _WRITE_BLOCK_SIZE (write_block_size),
  _WRITE_BLOCKS (write_blocks),
  _POLL_TIMEOUT (poll_timeout),
  _COMMAND_INTERVAL (command_interval),
  _MAX_QUEUE_SIZE (100),
  _MESSAGE_SIZE (sizeof(ControlMessage)),
  _mq (create_only, mid.c_str(), _MAX_QUEUE_SIZE, _MESSAGE_SIZE),
  _fds (0),
  _cbuf (_WRITE_BLOCKS),
  _running (false),
  _state (IDLE),
  _thread(),
  _cbuf_mutex()
 {
   // Reserve space in file descriptor vector.
   _fds.reserve(_MOUNT_POINTS);
   for (nfds_t i=0; i<_MOUNT_POINTS; ++i) {
     struct pollfd pfd;
     pfd.fd = -1;
     _fds.push_back(pfd);
   }
}

FileManager::~FileManager()
{
  close();
  message_queue::remove(_MID.c_str());
}

void FileManager::start()
{
  _running = true;
  _thread = boost::thread(&FileManager::run, this);
}

void FileManager::run()
{
  LOG4CXX_INFO(logger, "Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {

      case WRITE_TO_DISK:
	{
	  if (command_timer.elapsed() > _COMMAND_INTERVAL) {
	    check_control();
	    command_timer.restart();
	    continue;
	  }

	  // Poll file descriptors.
	  int rv = poll(&_fds[0], (nfds_t)_fds.size(), _POLL_TIMEOUT);
	  if (rv < 0) {
	    LOG4CXX_ERROR(logger, "poll returns error: " << strerror(errno));
	    continue;
	  } else if (rv == 0) {
	    LOG4CXX_DEBUG(logger, "poll returns 0");
	    continue;
	  }
	
	  // Scan file descriptors writing to next available descriptor.
	  BOOST_FOREACH(struct pollfd pfd, _fds) {
	    int fd = pfd.fd;
	    if (pfd.revents & POLLOUT) {
	      write_block(fd);
	    }
	  }
	}
	break;

      case IDLE:
	{
	  if (command_timer.elapsed() > _COMMAND_INTERVAL) {
	    check_control();
	    command_timer.restart();
	    usleep(_COMMAND_INTERVAL*1000000);
	    continue;
	  }
	}
	break;

      case STOP:
	_running = false;
	break;

      default:
	LOG4CXX_ERROR(logger, "Unknown state.");
	break;
      }
    }

    LOG4CXX_DEBUG(logger, "elapsed run time: " << run_timer.elapsed());
  } catch(interprocess_exception &ex) {
    LOG4CXX_ERROR(logger, "mq error: " << ex.what());
  }
}

void FileManager::join()
{
  _thread.join();
}

int FileManager::open(const std::string file_name)
{

  // Create individual file paths.
  std::vector<std::string> paths;
  for (boost::uint32_t i=0; i<_MOUNT_POINTS; ++i) {
    std::ostringstream ss;
    ss << _MOUNT_POINT << "/" << _MOUNT_PREFIX << i << "/" << file_name;
    paths.push_back(ss.str());
    ss.clear();
  }

  // Open files for each path.
  int ret=0;
  int i=0;
  BOOST_FOREACH(std::string p, paths) {
    LOG4CXX_DEBUG(logger, "path: " << p);

    int fd = ::open(p.c_str(), O_WRONLY | O_CREAT | O_NONBLOCK, S_IRWXU);
    if (fd<0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << p << " - " 
		    << strerror(errno));
      _fds[i].fd = -1;
      ret = -1;
    } else {
      LOG4CXX_ERROR(logger, "Opening file: " << p << " - fd == " 
		    << fd);
      _fds[i].fd = fd;
      _fds[i].events = POLLOUT;
    }
    ++i;
  }

  // Debug message.
   LOG4CXX_DEBUG(logger, "About to dump pfd.");
   BOOST_FOREACH(struct pollfd pfd, _fds)
     LOG4CXX_DEBUG(logger, "pfd: " << pfd.fd);

  return ret;
}

int FileManager::close()
{
  int ret = 0;

  BOOST_FOREACH(struct pollfd pfd, _fds) {
    if ( (pfd.fd > 0) && (::close(pfd.fd)<0) ) {
      LOG4CXX_ERROR(logger, "Unable to close fd: " << pfd.fd
		    << " - " << strerror(errno));
      ret = -1;
    }
  }

  return ret;
}

bool FileManager::write(Buffer* b)
{
  boost::mutex::scoped_lock lock(_cbuf_mutex);
  if (_cbuf.full())
    return false;

  _cbuf.push_back(b);

  return true;
}

bool FileManager::read(Buffer* b)
{
  return false;
}

bool FileManager::check_control()
{
  ControlMessage m;
  unsigned int priority;
  std::size_t recvd_size;

  // Read in next control message.
  bool rcvd_msg = _mq.try_receive(&m, sizeof(m), recvd_size, priority);
  if (!rcvd_msg) {
    return false;
  }

  switch (m._type) {

  case MSG_STOP:
    LOG4CXX_DEBUG(logger, "Received stop");
    _state = STOP;
    break;

  case MSG_WRITE_TO_DISK:
    LOG4CXX_DEBUG(logger, "Received WRITE_TO_DISK");
    _state = WRITE_TO_DISK;
    break;

  default:
    LOG4CXX_DEBUG(logger, "Received unknown message.");
    break;
  }
 
  return true;
}

void FileManager::write_block(const int fd)
{
  Buffer* buf;

  // Get next buffer from the circular buffer.
  if (_cbuf.empty()) {
    return;
  } else {
    boost::mutex::scoped_lock lock(_cbuf_mutex);
    buf = _cbuf[0];
    _cbuf.pop_front();
  }

  int bytes_left = _WRITE_BLOCK_SIZE;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(fd, &buf[bytes_written], _WRITE_BLOCK_SIZE);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    }
  }
}
