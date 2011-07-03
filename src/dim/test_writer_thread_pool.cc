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

// Socket includes
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <errno.h>


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

int setup_socket(const std::string& ip, const int port) {
  // Setup local interface.
  const char* p = ip.c_str();
  struct sockaddr_in addr;
  int ret = 0;

  ret = ::inet_aton(p, &addr.sin_addr);
  addr.sin_family  = AF_INET;
  addr.sin_port = htons(port);
  if (ret == 0)
    std::cerr << "Unable to parse IP address\n";
  
  int sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock < 0) {
    std::cerr << "Unable to create socket.\n";
    return sock;
  }

  // Clean up cast below -- seems to be standard practice though.
  ret == ::bind(sock, (struct sockaddr*)&addr, sizeof(addr));
  if (ret < 0) {
    std::cerr << "Unable to bind socket.\n";
    return ret;
  }
 
  return sock;
}

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
  const std::string IP("192.168.7.1");
  const boost::uint16_t PORT(4242);
  const int NBUF_SIZE(4096);
  boost::uint8_t nbuf[NBUF_SIZE];
  int sockfd = setup_socket(IP, PORT);
  
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
  const int BUF_POOL_SIZE = 16;
  std::vector<boost::uint8_t*> buf_pool;
  buf_pool.reserve(BUF_POOL_SIZE);
  const int ps = getpagesize();
  const boost::uint32_t BUF_SIZE = ps*256;
  void* buf;

  for (int i=0; i<BUF_POOL_SIZE; i++) {
    if (posix_memalign(&buf, ps, ps*256) < 0)
      std::cout << "Memalign failed\n";
    buf_pool.push_back(static_cast<boost::uint8_t*>(buf));
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

  ThreadPool <WriterTask> p(TASK_LIST_SIZE, THREAD_POOL_SIZE,
			    THREAD_SLEEP_TIME, STATS_FILE,
			    STATS_UPDATE_INTERVAL);

  LOG4CXX_DEBUG(logger, "Created.");

  p.start();

  LOG4CXX_DEBUG(logger, "Started.");

  // Network thread.
  double last_update = 0;
  Timer duration;
  boost::uint8_t* b = buf_pool.front();
  boost::uint64_t total_bytes_read = 0;
  for (boost::uint32_t i=0; i<TOTAL_TASKS; ++i) {
    int bytes_left = BUF_SIZE;
    int bytes_read = 0;

    while (bytes_left > 0) {
      int nread = ::read(sockfd, b + bytes_read, bytes_left);
      if (nread > 0) {
	bytes_read += nread;
	bytes_left -= nread;
	total_bytes_read += nread;
      } else {
	std::cerr << nread << " bytes read\n";
      }
    }
    // p.insert_task(WriterTask(i, fds[i%NUMBER_OF_FILES], b, BUF_SIZE));

    double now = duration.elapsed();
    if (duration.elapsed() - last_update > 1) {
      double now = duration.elapsed();
      double rate = 8*total_bytes_read/(1000000*now);
      std::cout << now << " " << rate << " Mbps" << std::endl;
      // p.print_stats();
      // p.dump_stats();
      last_update = now;
    }
  }   
  
  sleep(120);

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
