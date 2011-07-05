/*
 * Create by David Lapsley on Mon Jun 6 2011.
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

#ifndef _WRITER_TASK_H_
#define _WRITER_TASK_H_

// C includes.
#include <errno.h>

// C++ includes.
#include <list>

// Framework includes.
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/ptr_container/ptr_list.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>

class WriterTask {
  boost::uint32_t _id;
  int _fd;
  boost::uint8_t* _buf;
  boost::uint32_t _buf_size;

 public:
  WriterTask(const boost::uint32_t id, const int fd,
	     boost::uint8_t *buf, const boost::uint32_t buf_size):
    _id(id), _fd(fd), _buf(buf), _buf_size(buf_size)
    {}
    
    ~WriterTask() {}

    void operator() () {
      // Write buffer to disk.
      int bytes_left = _buf_size;
      int bytes_written = 0;
      while (bytes_left) {
	int nb = ::write(_fd, _buf + bytes_written, _buf_size);
	if (nb > 0) {
	  bytes_left -= nb;
	  bytes_written += nb;
	} else {
	  std::cout << "Unable to write to disk: " << strerror(errno) << std::endl;
	}
      }
      std::cout << _id << " wrote " << bytes_written << std::endl;
    }

    int fd() {
      return _fd;
    }
    
    boost::uint32_t id() {
      return _id;
    }

    boost::uint8_t* buf() {
      return _buf;
    }
};

#endif // _WRITER_TASK_H_

