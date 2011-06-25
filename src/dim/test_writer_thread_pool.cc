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

// C includes.
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <sstream>
#include <list>
#include <algorithm>


// Boost includes.
#include <boost/foreach.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/filesystem.hpp>

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <thread_pool.h>
#include <writer_task.h>
#include <test_writer_thread_pool.h>

using namespace boost;
using namespace boost::interprocess;
namespace bf = boost::filesystem;

CPPUNIT_TEST_SUITE_REGISTRATION (TestWriterThreadPool);

void
TestWriterThreadPool::setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestWriterThreadPool::tearDown (void)
{
}

void
TestWriterThreadPool::basic(void)
{
  LOG4CXX_DEBUG(logger, "TestWriterThreadPool::basic()");

  const int NUMBER_OF_FILES = 16;
  boost::uint32_t i;
  int fds[NUMBER_OF_FILES];

  // Setup directories.
  const std::string FILE_PREFIX("/mnt/disk");
  std::list<std::string> dirs;
  for (i=0; i<NUMBER_OF_FILES; ++i) {
    ostringstream ss;
    ss << FILE_PREFIX << i;
    dirs.push_front(ss.str().c_str());
  }

  i=0;
  BOOST_FOREACH(std::string d, dirs) {
    bf::path p(d);
    if (!bf::exists(p)) {
      bf::create_directories(p);
    }
    ostringstream ss;
    ss << d << "/test.dat";
    // fds[i++] = ::open(ss.str().c_str(), O_WRONLY | O_CREAT | O_NONBLOCK, S_IRWXU);
    fds[i++] = ::open(ss.str().c_str(), O_WRONLY | O_CREAT, S_IRWXU);
  }
    
  const boost::uint32_t BUF_SIZE = 8192;
  boost::uint8_t b[BUF_SIZE];
  for (i=0; i<BUF_SIZE; ++i)
    b[i] = static_cast<boost::uint8_t>(i);

    // b.push_back(static_cast<boost::uint8_t>(i));
  
  const boost::uint32_t TASK_LIST_SIZE = 1000;
  const boost::uint32_t THREAD_POOL_SIZE = 8;
  const boost::uint32_t TOTAL_TASKS = 100000000;
  const boost::uint32_t STATS_UPDATE_INTERVAL = 1;
  const std::string STATS_FILE("thread.csv");
  const int THREAD_SLEEP_TIME = 1;

  ThreadPool <WriterTask> p(TASK_LIST_SIZE, THREAD_POOL_SIZE,
			    THREAD_SLEEP_TIME, STATS_FILE,
			    STATS_UPDATE_INTERVAL);

  LOG4CXX_DEBUG(logger, "Created.");

  p.start();

  LOG4CXX_DEBUG(logger, "Started.");

  double last_update = 0;
  Timer duration;
  for (boost::uint32_t i=0; i<TOTAL_TASKS; ++i) {
    p.insert_task(WriterTask(i, fds[i%NUMBER_OF_FILES], b, BUF_SIZE));
    double now = duration.elapsed();
    if (duration.elapsed() - last_update > 1) {
      p.print_stats();
      p.dump_stats();
      last_update = now;
    }
  }   
  
  sleep(10);

  p.stop();

  double elapsed = duration.elapsed();
  double mbits_written = 8 * TOTAL_TASKS * BUF_SIZE / 1000000;
  double rate = mbits_written/elapsed; // Mbps
  
  LOG4CXX_INFO(logger, "elapsed:" << elapsed);
  LOG4CXX_INFO(logger, "mbits_written:" << mbits_written);
  LOG4CXX_INFO(logger, "mbps:" << rate);

  for (int i=0; i<NUMBER_OF_FILES; ++i)
    close(fds[i]);

  // BOOST_FOREACH(std::string d, dirs) {
  // bf::path p(d);
  // bf::remove_all(p);
  // }

  LOG4CXX_DEBUG(logger, "Joined file manager.");
}
