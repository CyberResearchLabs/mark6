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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_writer.h>

using namespace boost::filesystem;


FileWriter::FileWriter(const boost::uint32_t write_block_size,
		       const boost::uint32_t write_blocks,
		       const boost::uint32_t poll_timeout,
		       const double command_interval):
  // Initialize data members (in order).
  _WRITE_BLOCK_SIZE (write_block_size),
  _WRITE_BLOCKS (write_blocks),
  _POLL_TIMEOUT (poll_timeout),
  _COMMAND_INTERVAL (command_interval),
  _pfd(),
  _cbuf (_WRITE_BLOCKS),
  _running (false),
  _state (IDLE),
  _thread(),
  _cbuf_mutex()
{
  // Reserve space in file descriptor vector.
  struct pollfd pfd;
  pfd.fd = -1;
}

FileWriter::~FileWriter() {
  close();
}

void FileWriter::start()
{
  _running = true;
  _thread = boost::thread(&FileWriter::run, this);
}

void FileWriter::run()
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
	    continue;
	  }

	  // Poll file descriptor.
	  int rv = poll(&_pfd, 1, _POLL_TIMEOUT);
	  if (rv < 0) {
	    LOG4CXX_ERROR(logger, "poll returns error: " << strerror(errno));
	    continue;
	  } else if (rv == 0) {
	    LOG4CXX_DEBUG(logger, "poll returns 0");
	    continue;
	  }
	
	  // Scan file descriptor to see if it is ready for writing.
	  if (_pfd.revents & POLLOUT) {
	    write_block(_pfd.fd);
	  }
	}
	break;

      case IDLE:
	{
	  if (command_timer.elapsed() > _COMMAND_INTERVAL) {
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
  } catch(std::exception &ex) {
    LOG4CXX_ERROR(logger, "error: " << ex.what());
  }
}

void FileWriter::join()
{
  _thread.join();
}

int FileWriter::open(const std::string file_name)
{
  LOG4CXX_INFO(logger, "Opening FileWriter file: " << file_name);

  // Open files for each path.
  int ret=0;
  _pfd.fd = ::open(file_name.c_str(), O_WRONLY | O_CREAT | O_DIRECT, S_IRWXU);
  if (_pfd.fd<0) {
    LOG4CXX_ERROR(logger, "Unable to open file: " << file_name << " - " 
		  << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "File: " << file_name << " fd: " << _pfd.fd);
    _pfd.events = POLLOUT;
  }

  // Debug message.
  LOG4CXX_DEBUG(logger, "pfd: " << _pfd.fd);

  return ret;
}

int FileWriter::close()
{
  if ( (_pfd.fd>0) && (::close(_pfd.fd)<0) ) {
    LOG4CXX_ERROR(logger, "Unable to close fd: " << _pfd.fd
		  << " - " << strerror(errno));
    return -1;
  }
  return 0;
}

bool FileWriter::write(boost::uint8_t* b)
{
  boost::mutex::scoped_lock lock(_cbuf_mutex);
  if (_cbuf.full())
    return false;

  _cbuf.push_back(b);

  return true;
}

void FileWriter::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void FileWriter::cmd_write_to_disk() {
  LOG4CXX_INFO(logger, "Received WRITE_TO_DISK");
  _state = WRITE_TO_DISK;
}


void FileWriter::write_block(const int fd)
{
  boost::mutex::scoped_lock lock(_cbuf_mutex);

  boost::uint8_t* buf;
  if (_cbuf.empty()) {
    return;
  } else {
    buf = _cbuf[0];
    _cbuf.pop_front();
  }

  // Write buffer to disk.
  int bytes_left = _WRITE_BLOCK_SIZE;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  BufferPool::instance().free(buf);
}
