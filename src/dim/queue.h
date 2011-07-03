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

struct QueueStats {
  double _ALPHA;
  
  boost::mutex _mutex;
  boost::uint32_t _count;
  boost::uint32_t _last_count;
  Timer _timer;
  double _rate;
  double _last_rate;
  double _last_update;

  QueueStats(const double alpha): _ALPHA(alpha), _mutex(), _count(0),
				  _last_count(0), _timer(), _rate(0.0),
				  _last_rate(0.0), _last_update(0.0)
  {}

  void increment() {
    boost::mutex::scoped_lock lock(_mutex);
    ++_count;
  }

  void update() {
    boost::mutex::scoped_lock lock(_mutex);
    double now = _timer.elapsed();
    double delta_time = now - _last_update;
    boost::uint32_t delta = _count - _last_count;
    _last_rate = delta/delta_time;
    double cum_rate = _count/now;
    _rate = _ALPHA * _last_rate + (1.0 - _ALPHA) * _rate;
    _last_count = _count;
    _last_update = now;
  }

  double rate() {
    boost::mutex::scoped_lock lock(_mutex);
    return _rate;
  }

  double last_rate() {
    boost::mutex::scoped_lock lock(_mutex);
    return _last_rate;
  }

  boost::uint32_t count() {
    boost::mutex::scoped_lock lock(_mutex);
    return _count;
  }

  std::string report() {
    update();
    std::ostringstream ss;
    ss
      << _timer.elapsed() << " " 
      << rate() << " "
      << last_rate() << " "
      << count();
    return ss.str();
  }
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

  // Statistics.
  QueueStats _push_stats;
  QueueStats _pop_stats;
  Timer _timer;
  double _last_update;

 public:
  Queue(const std::string& id,
	const boost::uint32_t list_size,
	const boost::uint32_t timeout);
  ~Queue();
  void push_back(const TYPE& o);
  TYPE pop_front();
  bool empty();
  void print_stats();
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
  _list_write_cond(),
  _push_stats(0.1),
  _pop_stats(0.1),
  _timer(),
  _last_update(0.0)
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

  _push_stats.increment();
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

  _pop_stats.increment();

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

template<class TYPE>
void Queue<TYPE>::print_stats()
{
  std::cout
    << _ID << " push_stats "
    << _push_stats.report()
    << std::endl
    << _ID << " pop_stats "
    << _pop_stats.report()
    << std::endl;
}

#endif // _QUEUE_H_
