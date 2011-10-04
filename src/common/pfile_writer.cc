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
	     poll_timeout, sw, command_interval)
{
}

PFileWriter::~PFileWriter() {
  close();
}

int PFileWriter::open() {
  LOG4CXX_INFO(logger, "Opening PFileWriter file: " << _capture_file);

  // Open files for each path.
  int ret=0;

#define FALLOCATE
#ifdef FALLOCATE
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_DIRECT, S_IRWXU);
#else
#ifdef DIRECT_BUFFER
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT,
		   S_IRWXU);
#else
  _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT, S_IRWXU);
#endif // DIRECT_BUFFER
#endif // FALLOCATE
  if (_pfd.fd<0) {
    LOG4CXX_ERROR(logger, "Unable to open file: " << _capture_file
		  << " - " << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "File: " << _capture_file << " fd: "
		  << _pfd.fd);
    _pfd.events = POLLOUT;
  }

#ifdef FALLOCATE
  if (::lseek(_pfd.fd, 0, SEEK_SET) < 0) {
    LOG4CXX_ERROR(logger, "Unable to seek to beginning of file: " << _capture_file
		  << " - " << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "Successfully seeked.");
  }
#endif // FALLOCATE

  // Debug message.
  LOG4CXX_DEBUG(logger, "pfd: " << _pfd.fd);

  return ret;
}

int PFileWriter::close() {
  if ( (_pfd.fd>0) && (::close(_pfd.fd)<0) ) {
    LOG4CXX_ERROR(logger, "Unable to close fd: " << _pfd.fd
		  << " - " << strerror(errno));
    return -1;
  }
  return 0;
}

bool PFileWriter::write(boost::uint8_t* buf,
		       const int buf_len) {
  // Write buffer to disk.
  int bytes_left = buf_len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(_pfd.fd, &buf[bytes_written], bytes_left);
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

