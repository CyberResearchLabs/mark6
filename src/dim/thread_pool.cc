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

#include <thread_pool.h>


template <class TASK>
ThreadPool<TASK>::ThreadPool(const boost::uint32_t task_list_size,
		       const boost::uint32_t thread_pool_size,
		       const int thread_sleep_time):
  _TASK_LIST_SIZE(task_list_size),
  _THREAD_POOL_SIZE(thread_pool_size),
  _THREAD_SLEEP_TIME(thread_sleep_time),
  _tasks_completed(0),
  _task_rate(0),
  _timer(),
  _last_update(0),
  _running(true)
{
}

template <class TASK>
ThreadPool<TASK>::~ThreadPool() {
  while (!_threads.empty())
    _threads.pop_front();
}
  
template <class TASK>
void ThreadPool<TASK>::start()
{
  _last_update = 0;
  _timer.restart();
  for (boost::uint32_t i=0; i<_THREAD_POOL_SIZE; ++i) {
    _threads.push_front(new boost::thread(Thread(), this,
					  _THREAD_SLEEP_TIME));
  }
}

template <class TASK>
void ThreadPool<TASK>::stop()
{
  _running = false;
  BOOST_FOREACH(boost::thread &t, _threads)
    t.join();
}

template <class TASK>
void ThreadPool<TASK>::stop_wait()
{
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_THREAD_SLEEP_TIME*1000);
  
  boost::mutex::scoped_lock lock(_task_list_mutex);
  while (!_task_list.empty()) {
    _task_list_write_cond.wait(lock);
    // TODO: timeeout.
  }

  _running = false;
  BOOST_FOREACH(boost::thread &t, _threads)
    t.join();
}

template <class TASK>
bool ThreadPool<TASK>::running()
{
  return _running;
}

template <class TASK>
void ThreadPool<TASK>::insert_task(const TASK& w)
{
  boost::mutex::scoped_lock lock(_task_list_mutex);    
  while (_task_list.size() >= _TASK_LIST_SIZE) {
    _task_list_write_cond.wait(lock);
  }

  _task_list.push_back(w);
  _task_list_read_cond.notify_one();
  // TODO: timeout, nonblocking.
}

template <class TASK>
TASK ThreadPool<TASK>::next_task() {
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_THREAD_SLEEP_TIME*1000);
  
  boost::mutex::scoped_lock lock(_task_list_mutex);
  while (_task_list.empty()) {
    bool timedout = _task_list_read_cond.timed_wait(lock, timeout);
    if (!timedout)
      throw TaskTimeout("timedout");
    if (!running())
      throw TaskStop("stopped");
  }

  TASK n = _task_list.front();
  _task_list.pop_front();

  _task_list_write_cond.notify_one();

  return n;
}

template <class TASK>
void ThreadPool<TASK>::task_completed()
{
  boost::mutex::scoped_lock lock(_tasks_completed_mutex);    
  ++_tasks_completed;
  double now = _timer.elapsed();
  double delta_time = now - _last_update;

  if (delta_time > 1) {
    boost::uint32_t delta_tasks = _tasks_completed - _last_tasks_completed;
    double instant_task_rate = delta_tasks/delta_time;
    double cumulative_task_rate = _tasks_completed/now;
    _task_rate = 0.1*instant_task_rate + 0.9*_task_rate;

    LOG4CXX_DEBUG(logger, "now:" << now
		  << " completed:" << _tasks_completed 
		  << " instant_rate:" << instant_task_rate
		  << " cumulative_rate:" << cumulative_task_rate
		  << " rate:" << _task_rate);
    
    _last_update = now;
    _last_tasks_completed = _tasks_completed;
  }
}
