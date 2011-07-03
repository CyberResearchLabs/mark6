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

#ifndef _THREAD_POOL_H_
#define _THREAD_POOL_H_

// C++ includes.
#include <fstream>

// Framework includes.
#include <boost/thread/thread.hpp>
#include <boost/ptr_container/ptr_list.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <queue.h>


struct TaskStop {
  std::string _msg;
  TaskStop(std::string msg): _msg(msg) {}
};


template <class TASK>
class ThreadPool {

  struct Thread {
    Thread() {}
    ~Thread() {}
    void operator() (ThreadPool* wtp, const int sleep_time) {
      while (wtp->running()) {
	try {
	  TASK w = wtp->pop_task();
	  w();
	  wtp->push_completed(w);
	} catch (Timeout t) {
	  // std::cout << "Timedout waiting for next task." << std::endl;
	} catch (TaskStop s) {
	  std::cout << "Stop called waiting for next task." << std::endl;
	}
      }
    }
  };

  Queue<TASK> _task_queue;
  Queue<TASK> _completed_queue;

  boost::ptr_list<boost::thread> _threads;
  std::ofstream _stats_file;

  boost::uint32_t _TASK_QUEUE_SIZE;
  boost::uint32_t _THREAD_POOL_SIZE;
  boost::uint32_t _THREAD_SLEEP_TIME;
  boost::uint32_t _STATS_UPDATE_INTERVAL;

  Timer _timer;
  double _last_update;

  bool _running;

 public:
  ThreadPool(const boost::uint32_t task_queue_size,
	     const boost::uint32_t thread_pool_size,
	     const boost::uint32_t thread_sleep_time,
	     const std::string stats_file,
	     const boost::uint32_t stats_update_interval);
  ~ThreadPool();
  void start();
  void stop();
  void stop_wait();
  bool running();
  void push_task(const TASK& w);
  TASK pop_task();
  void push_completed(const TASK& t);
  TASK pop_completed();
  void print_stats();
  void dump_stats();
};


template <class TASK>
ThreadPool<TASK>::ThreadPool(const boost::uint32_t task_queue_size,
			     const boost::uint32_t thread_pool_size,
			     const boost::uint32_t thread_sleep_time,
			     const std::string stats_file,
			     const boost::uint32_t stats_update_interval):
  _task_queue(std::string("tasks"), task_queue_size, thread_sleep_time),
  _completed_queue(std::string("completed"), task_queue_size, thread_sleep_time),
  _threads(),
  _stats_file(stats_file.c_str()),
  _TASK_QUEUE_SIZE(task_queue_size),
  _THREAD_POOL_SIZE(thread_pool_size),
  _THREAD_SLEEP_TIME(thread_sleep_time),
  _STATS_UPDATE_INTERVAL(stats_update_interval),
  _timer(),
  _last_update(0),
  _running(true)
{
}

template <class TASK>
ThreadPool<TASK>::~ThreadPool() {
  while (!_threads.empty())
    _threads.pop_front();
  _stats_file.close();
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
  while (!_task_queue.empty()) {
    sleep(1);
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
void ThreadPool<TASK>::push_task(const TASK& w)
{
  // TOOD For now use ingress timing for reports.
  // Migrate to a seperate stats thread.
  double now = _timer.elapsed();
  if (now - _last_update > _STATS_UPDATE_INTERVAL) {
    _task_queue.print_stats();
    _completed_queue.print_stats();
    _last_update = now;
  }

  _task_queue.push_back(w);
}

template <class TASK>
TASK ThreadPool<TASK>::pop_task() {
  return _task_queue.pop_front();
  // TODO
  // if (!running())
  // throw TaskStop("stopped");
}

template <class TASK>
void ThreadPool<TASK>::push_completed(const TASK& t)
{
  _completed_queue.push_back(t);
}

template <class TASK>
TASK ThreadPool<TASK>::pop_completed() {
  return _completed_queue.pop_front();
}

template <class TASK>
void ThreadPool<TASK>::print_stats()
{
  // TODO
  #if 0
  boost::mutex::scoped_lock lock(_completed_task_queue_mutex);    
  double now = _timer.elapsed();
  double cumulative_task_rate = _tasks_completed/now;

  std::cout
    << "now:" << now
    << " completed:" << _tasks_completed 
    << " cumulative_rate:" << cumulative_task_rate
    << " rate:" << _task_rate 
    << std::endl;
  #endif
}

template <class TASK>
void ThreadPool<TASK>::dump_stats()
{
  // TODO
  #if 0
  boost::mutex::scoped_lock lock(_completed_task_queue_mutex);    
  double now = _timer.elapsed();
  double cumulative_task_rate = _tasks_completed/now;

  _stats_file
    << now << ","
    << _tasks_completed << ","
    << cumulative_task_rate << ","
    << _task_rate << ","
    << std::endl;
  #endif
}

#endif // _THREAD_H_

