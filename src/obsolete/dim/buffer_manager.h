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

#ifndef _BUFFER_MANAGER_H_
#define _BUFFER_MANAGER_H_

// C includes.
#include <errno.h>

// C++ includes.
#include <list>

// Framework includes.
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <queue.h>

class BufferManager {
  int _BUF_POOL_SIZE;
  int _BUF_SIZE;
  int _PAGES_PER_BUFFER;
  int _PAGE_SIZE;

  Queue<boost::uint8_t*> _buf_pool;

 public:
 BufferManager(const std::string& id, const int buf_pool_size,
	       const int timeout):
  _BUF_POOL_SIZE(buf_pool_size),
    _BUF_SIZE(0),
    _PAGES_PER_BUFFER(256),
    _PAGE_SIZE(0),
    _buf_pool(id + "-buf-pool", buf_pool_size, timeout)
      {
	_PAGE_SIZE = getpagesize();
	_BUF_SIZE = _PAGE_SIZE * _PAGES_PER_BUFFER;
	
	void* buf;
	for (int i=0; i<_BUF_POOL_SIZE; ++i) {
	  if (posix_memalign(&buf, _PAGE_SIZE, _BUF_SIZE) < 0) {
	    std::cerr << "Memalign failed\n";
	    throw std::string("Memalign failed.");
	  }
	  _buf_pool.push_back(static_cast<boost::uint8_t*>(buf));
	}
      }
  
  ~BufferManager() {}
    
  bool empty() {
    return _buf_pool.empty();
  }
    
  boost::uint8_t* pop() {
    return _buf_pool.pop_front();
  }

  void push(boost::uint8_t* b) {
    _buf_pool.push_back(b);
  }

  int buf_size() {
    return _BUF_SIZE;
  }
};

#endif // _BUFFER_MANAGER_H_

