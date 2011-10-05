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
			 const double command_interval):
  FileWriter(id, write_block_size, write_blocks, std::string(""),
	     poll_timeout, sw, command_interval),
  _capture_files(capture_files),
  _pfds(0),
  _nfds(0)
{
  _nfds = _capture_files.size();
  _pfds = new struct pollfd[_nfds];
}

PFileWriter::~PFileWriter() {
  close();
  delete _pfds;
}

int PFileWriter::open() {
  // Open files for each path.
  int ret=0;
  int i = 0;
  BOOST_FOREACH(std::string capture_file, _capture_files) {
    LOG4CXX_INFO(logger, "Opening PFileWriter file: " << capture_file);

#define FALLOCATE
#ifdef FALLOCATE
    int fd = ::open(capture_file.c_str(), O_WRONLY | O_DIRECT, S_IRWXU);
#else
#ifdef DIRECT_BUFFER
    int fd = ::open(capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT,
		    S_IRWXU);
#else
    int fd = ::open(capture_file.c_str(), O_WRONLY | O_CREAT, S_IRWXU);
#endif // DIRECT_BUFFER
#endif // FALLOCATE
    if (fd<0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << capture_file
		    << " - " << strerror(errno));
      fd = -1;
      ret = -1;
    } else {
      LOG4CXX_DEBUG(logger, "File: " << capture_file << " fd: " << fd);
  }
    
#ifdef FALLOCATE
    if (::lseek(fd, 0, SEEK_SET) < 0) {
      LOG4CXX_ERROR(logger, "Unable to seek to beginning of file: " << _capture_file
		    << " - " << strerror(errno));
      fd = -1;
      ret = -1;
    } else {
      LOG4CXX_DEBUG(logger, "Successfully seeked.");
    }
#endif // FALLOCATE

    if (i >= _nfds) {
      LOG4CXX_ERROR(logger, "_pfds index out of bounds: " << i);
      break;
    }
    
    struct pollfd pfd = { fd, POLLOUT, 0 };
    _pfds[i] = pfd;
  }

  return ret;
}

int PFileWriter::close() {
  for (int i=0; i<_nfds; i++) {
    if ( (_pfds[i].fd>0) && (::close(_pfds[i].fd)<0) ) {
      LOG4CXX_ERROR(logger, "Unable to close fd: " << _pfds[i].fd
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
  for (int i=0; i<_nfds; i++) {
    if (_pfds[i].revents & POLLOUT) {
      write_fd = _pfds[i].fd;
      break;
    }
  }

  if (write_fd < 0) {
    LOG4CXX_ERROR(logger, "Invalid write_fd.");
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

