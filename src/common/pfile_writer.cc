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
#include <boost/foreach.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_writer.h>
#include <stats_writer.h>
#include <pfile_writer.h>

using namespace boost::filesystem;

PFileWriter::PFileWriter(const int id,
			 const int write_block_size,
			 const int  write_blocks,
			 const std::list<std::string>& capture_files,
			 const int poll_timeout,
			 StatsWriter* const sw,
			 const double command_interval,
			 const unsigned long file_size,
			 const bool preallocated,
			 const bool directio):
  FileWriter(id, write_block_size, write_blocks, std::string(""),
	     poll_timeout, sw, command_interval, file_size, preallocated,
	     directio, false),
  _capture_files(capture_files),
  _pfds(0),
  _fds(0),
  _nfds(0),
  _pfds_idx(0)
{
  _nfds = _capture_files.size();
  _pfds = new struct pollfd[_nfds];
  _fds = new int[_nfds];
}

PFileWriter::~PFileWriter() {
  close();
  delete _pfds;
  delete _fds;
}

int PFileWriter::open() {
  // Open files for each path.
  int ret=0;
  int i = 0;
  BOOST_FOREACH(std::string capture_file, _capture_files) {
    LOG4CXX_INFO(logger, "Opening PFileWriter file: " << capture_file);

    int fd = -1;
    if (_preallocated) {
      fd = ::open(capture_file.c_str(), O_WRONLY | O_DIRECT, S_IRWXU);
    } else {
      if (_directio) {
	fd = ::open(capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT,
		    S_IRWXU);
      } else {
	fd = ::open(capture_file.c_str(), O_WRONLY | O_CREAT, S_IRWXU);
      }
    }

    if (fd<=0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << capture_file
		    << " - " << strerror(errno));
      fd = -1;
      ret = -1;
    } else {
      LOG4CXX_DEBUG(logger, "File: " << capture_file << " fd: " << fd);
      _fds[i] = fd;
    }
    
    if (_preallocated) {
      if (::lseek(fd, 0, SEEK_SET) < 0) {
	LOG4CXX_ERROR(logger, "Unable to seek to beginning of file: "
		      << _capture_file
		      << " - " << strerror(errno));
	fd = -1;
	ret = -1;
      } else {
	LOG4CXX_DEBUG(logger, "Successfully seeked.");
      }
    }
    
    if (i >= _nfds) {
      LOG4CXX_ERROR(logger, "_pfds index out of bounds: " << i);
      break;
    }

    // Don't forget this!
    i++;
  }

  return ret;
}

int PFileWriter::close() {
  for (int i=0; i<_nfds; i++) {
    if ( (_fds[i]>0) && (::close(_fds[i])<0) ) {
      LOG4CXX_ERROR(logger, "Unable to close fd: " << _fds[i]
		    << " - " << strerror(errno));
      return -1;
    }
  }
  return 0;
}

bool PFileWriter::write(boost::uint8_t* buf,
		       const int buf_len) {

  // Block indefinitely.
  int pret = 0;
  while (true) {
    for (int i=0; i<_nfds; i++) {
      _pfds[i].fd = _fds[i];
      _pfds[i].events = POLLOUT;
      _pfds[i].revents = 0;
    }

    pret = poll(_pfds, _nfds, -1);

    if (pret > 0) {
      break;
    } else if (pret < 0) {
      LOG4CXX_ERROR(logger, "Write to disk failed: " << strerror(errno));
      return false;
    }
    // Continue looping if no file descriptors available (pret==0).
  }

  // Find writeable fd.
  int write_fd = -1;
  int idx = 0;
  for (int i=0; i<_nfds; i++) {
    idx = _pfds_idx % _nfds;
    _pfds_idx++;
    if (_pfds[idx].revents & POLLOUT) {
      write_fd = _pfds[idx].fd;
      break;
    }
  }

  if (write_fd <= 0) {
    LOG4CXX_ERROR(logger, "Invalid write_fd: " << write_fd);
    return false;
  }

  // Write buffer to disk.
  int bytes_left = buf_len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(write_fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  if (_sw)
    _sw->update(1, bytes_written, 0, buf_len);
  free_buffer(buf);
}

