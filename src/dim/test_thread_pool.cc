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

#define _XOPEN_SOURCE 600

// C includes.
// C++ includes.
#include <iostream>

// Boost includes.
#include <boost/foreach.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/filesystem.hpp>

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <thread_pool.h>
#include <test_thread_pool.h>

using namespace boost;
using namespace boost::interprocess;

CPPUNIT_TEST_SUITE_REGISTRATION (TestThreadPool);


void
TestThreadPool::setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestThreadPool::tearDown (void)
{
}

void
TestThreadPool::basic(void)
{
  std::cout << "TestThreadPool::basic()" << std::endl;

  const boost::uint32_t TASK_QUEUE_SIZE = 100;
  const boost::uint32_t THREAD_POOL_SIZE = 16;
  const boost::uint32_t THREAD_SLEEP_TIME = 1;
  const boost::uint32_t STATS_UPDATE_INTERVAL = 5;
  const std::string STATS_FILE("/tmp/test_thread_pool_stats.dat");

  ThreadPool<int> TP(TASK_QUEUE_SIZE, THREAD_POOL_SIZE, THREAD_SLEEP_TIME,
		STATS_FILE, STATS_UPDATE_INTERVAL);
}
