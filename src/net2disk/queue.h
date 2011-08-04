/*
 * Create by David Lapsley on Mon Jul 1 2011.
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

#ifndef _QUEUE_H_
#define _QUEUE_H_

// C++ includes.
#include <sstream>

// Framework includes.
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

struct Timeout {
  std::string _msg;
  Timeout(std::string msg): _msg(msg) {}
};

template <class TYPE>
class Queue {
  // Constants.
  std::string _ID;
  boost::uint32_t _QUEUE_SIZE;
  boost::uint32_t _TIMEOUT;

  // Data.
  std::list<TYPE> _list;
  boost::mutex _list_mutex;
  boost::condition_variable _list_read_cond;
  boost::condition_variable _list_write_cond;

 public:
  Queue(const std::string& id,
	const boost::uint32_t list_size,
	const boost::uint32_t timeout);
  ~Queue();
  void push_back(const TYPE& o);
  TYPE pop_front();
  bool empty();
};

template <class TYPE>
Queue<TYPE>::Queue(const std::string& id,
		   const boost::uint32_t list_size,
		   const boost::uint32_t timeout):
  _ID(id),
  _QUEUE_SIZE(list_size),
  _TIMEOUT(timeout),
  _list(),
  _list_mutex(),
  _list_read_cond(),
  _list_write_cond()
{}

template<class TYPE>
Queue<TYPE>::~Queue()
{}

template <class TYPE>
void Queue<TYPE>::push_back(const TYPE& o)
{
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_TIMEOUT*1000);
  
  boost::mutex::scoped_lock lock(_list_mutex);    
  while (_list.size() >= _QUEUE_SIZE) {
    if (!_list_write_cond.timed_wait(lock, timeout))
      throw Timeout("timedout");
  }

  _list.push_back(o);
  _list_read_cond.notify_one();
}

template<class TYPE>
TYPE Queue<TYPE>::pop_front()
{
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_TIMEOUT*1000);
  
  boost::mutex::scoped_lock lock(_list_mutex);
  while (_list.empty()) {
    if (!_list_read_cond.timed_wait(lock, timeout))
      throw Timeout("timedout");
  }

  TYPE o = _list.front();
  _list.pop_front();
  _list_write_cond.notify_one();

  return o;
}

template<class TYPE>
bool Queue<TYPE>::empty()
{
  bool result = false;
  boost::mutex::scoped_lock lock(_list_mutex);
  result = _list.empty();
  return result;
}

#endif // _QUEUE_H_
