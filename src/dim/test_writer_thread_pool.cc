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
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

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
#include <buffer_manager.h>
#include <socket_manager.h>
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
TestWriterThreadPool::mem_test(void)
{
  const int N = 1000;
  const int BUF_SIZE= 1048576;
  boost::uint8_t* p = 0;
  boost::uint8_t ref[BUF_SIZE];
  for (int i=0; i<BUF_SIZE; i++)
    ref[i] = (boost::uint8_t)i;

  Timer t;
  for (int i=0; i<N; i++) {
    p = new boost::uint8_t[BUF_SIZE];
    memcpy(p, ref, BUF_SIZE);
    delete p;
  }
  const double elapsed = t.elapsed();
  std::cout << "mem_test()" << std::endl;
  std::cout << "elapsed:" << elapsed << std::endl;
  std::cout << "rate:" << N/elapsed << std::endl;
  std::cout << "per_op_time:" << elapsed/(double)N << std::endl;
}

void
TestWriterThreadPool::basic(void)
{
  std::cout << "TestWriterThreadPool::basic()" << std::endl;

  // Network stuff.
  const std::string SM_ID("socket_manager");
  const std::string IP("192.168.7.1");
  const boost::uint16_t PORT(4242);
  SocketManager SM(SM_ID, IP, PORT);
  
  // Files system stuff.
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

  // DIRECT stuff.
  const int BUF_POOL_SIZE(16);
  const int TIMEOUT(1);
  const std::string BM_ID("buffer_manager");
  BufferManager BM(BM_ID, BUF_POOL_SIZE, TIMEOUT);
  const int BUF_SIZE = BM.buf_size();
  
  i=0;
  BOOST_FOREACH(std::string d, dirs) {
    bf::path p(d);
    if (!bf::exists(p)) {
      bf::create_directories(p);
    }
    ostringstream ss;
    ss << d << "/test.dat";
    int fd = ::open(ss.str().c_str(), O_WRONLY | O_CREAT | O_DIRECT, S_IRWXU);
    if (fd < 0) {
      std::cout << "Couldn't open file descriptor: " << strerror(errno) << std::endl;
      exit(1);
    } else {
      std::cout << "fd:" << fd << std::endl;
      fds[i++] = fd;
    }
  }
    
  const boost::uint32_t TASK_LIST_SIZE = 1000;
  const boost::uint32_t THREAD_POOL_SIZE = 8;
  const boost::uint32_t TOTAL_TASKS = 100000000;
  const boost::uint32_t STATS_UPDATE_INTERVAL = 1;
  const std::string STATS_FILE("thread.csv");
  const int THREAD_SLEEP_TIME = 1;

  ThreadPool <WriterTask> TP(TASK_LIST_SIZE, THREAD_POOL_SIZE,
			     THREAD_SLEEP_TIME, STATS_FILE,
			     STATS_UPDATE_INTERVAL);

  LOG4CXX_DEBUG(logger, "Created.");

  TP.start();

  LOG4CXX_DEBUG(logger, "Started.");

  // Network thread.
  double last_update = 0;
  Timer duration;
  boost::uint64_t total_bytes_read = 0;
  int bytes_read = 0;

  for (boost::uint32_t i=0; i<TOTAL_TASKS; ++i) {
    boost::uint8_t* b(0);
    boost::uint8_t* f(0);

    while (!b) {
      try {
	b = BM.pop();
      } catch (...) {
	while (!TP.empty_completed()) {
	  WriterTask w = TP.pop_completed();
	  BM.push(w.buf());
	}
      }
    }

    // Read bytes from socket.
    bytes_read = SM.read(b, BUF_SIZE);
    total_bytes_read += bytes_read;

    //TP.push_task(WriterTask(i, fds[i%NUMBER_OF_FILES], b, BUF_SIZE));
  }   
  
  sleep(120);

  TP.stop();

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
