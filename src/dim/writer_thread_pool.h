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

#ifndef _WRITER_THREAD_POOL_H_
#define _WRITER_THREAD_POOL_H_

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
  Buffer* _buf;
  boost::uint32_t _buf_size;

 public:
 WriterTask(const boost::uint32_t id, const int fd,
	    Buffer* buf, const boost::uint32_t buf_size):
  _id(id), _fd(fd), _buf(buf), _buf_size(buf_size) {}

  ~WriterTask() {}

  void operator() () {
    LOG4CXX_DEBUG(logger, "Executing write task id==" << _id);

    // Write buffer to disk.
    int bytes_left = _buf_size;
    int bytes_written = 0;
    while (bytes_left) {
      int nb = ::write(_fd, &_buf[bytes_written], _buf_size);
      if (nb > 0) {
	bytes_left -= nb;
	bytes_written += nb;
      }
    }
  }
};


struct WriterTaskTimeout {
  std::string _msg;
WriterTaskTimeout(std::string msg): _msg(msg) {}
};

struct WriterTaskStop {
  std::string _msg;
WriterTaskStop(std::string msg): _msg(msg) {}
};


class WriterThreadPool;


struct WriterThread {
  bool _running;

  WriterThread();
  ~WriterThread();
  void operator() (WriterThreadPool* wtp, const int sleep_time);
};


class WriterThreadPool {

  std::list<WriterTask> _task_list;
  boost::mutex _task_list_mutex;
  boost::condition_variable _task_list_cond;
  boost::ptr_list<boost::thread> _threads;

  boost::uint32_t _TASK_LIST_SIZE;
  boost::uint32_t _THREAD_POOL_SIZE;
  boost::uint32_t _THREAD_SLEEP_TIME;

  bool _running;

 public:
  WriterThreadPool(const boost::uint32_t task_list_size,
		   const boost::uint32_t thread_pool_size,
		   const int thread_sleep_time);
  ~WriterThreadPool();

  void start();
  void stop();
  bool running();
  bool insert_task(const WriterTask& w);
  WriterTask next_task();
};


#endif // _WRITER_THREAD_H_

