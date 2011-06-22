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

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <writer_thread_pool.h>
#include <test_writer_thread_pool.h>

using namespace boost;
using namespace boost::interprocess;

CPPUNIT_TEST_SUITE_REGISTRATION (TestWriterThreadPool);

void
TestWriterThreadPool :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestWriterThreadPool :: tearDown (void)
{
}

void
TestWriterThreadPool::basic(void)
{
  LOG4CXX_DEBUG(logger, "TestWriterThreadPool::basic()");

  int fds[5];

  std::string FILE_PREFIX("/tmp/test");
  for (int i=0; i<5; ++i) {
    ostringstream ss;
    ss << FILE_PREFIX << i << ".dat";
    fds[i] = ::open(ss.str().c_str(), O_WRONLY | O_CREAT | O_NONBLOCK, S_IRWXU);
  }
    
  const boost::uint32_t BUF_SIZE = 4096;
  Buffer b;
  for (boost::uint32_t i=0; i<BUF_SIZE; ++i)
    b.push_back(static_cast<boost::uint8_t>(i));
  
  const boost::uint32_t TASK_LIST_SIZE = 100;
  const boost::uint32_t THREAD_POOL_SIZE = 10;
  const int THREAD_SLEEP_TIME = 1;

  WriterThreadPool p(TASK_LIST_SIZE, THREAD_POOL_SIZE, THREAD_SLEEP_TIME);


  for (boost::uint32_t i=0; i<TASK_LIST_SIZE; ++i) {
    p.insert_task(WriterTask(i, fds[i%5], &b, BUF_SIZE));
  }
   
  p.start();
  sleep(10);
  p.stop();

  for (int i=0; i<5; ++i)
    close(fds[i]);

  LOG4CXX_DEBUG(logger, "Joined file manager.");
}
