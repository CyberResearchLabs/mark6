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
#include <file_manager.h>
#include <test_file_manager.h>

using namespace boost;
using namespace boost::interprocess;

CPPUNIT_TEST_SUITE_REGISTRATION (TestFileManager);

void
TestFileManager :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestFileManager :: tearDown (void)
{
}

void
TestFileManager::basic(void)
{
  std::cout << "TestFileManager::basic()" << std::endl;
  const std::string mid("fm1");
  const std::string mount_point("/tmp");
  const std::string mount_prefix("disk");
  const boost::uint32_t num_mount_points(2);
  const boost::uint32_t write_block_size(4096);
  const boost::uint32_t write_blocks(100);
  const boost::uint32_t poll_timeout = 1000; // ms
  const double command_interval = 1; //s
  const std::string file_name("first.m6");

  message_queue::remove(mid.c_str());

  FileManager fm(mid, mount_point, mount_prefix, num_mount_points,
		 write_block_size, write_blocks, poll_timeout,
		 command_interval);

  fm.start();
  fm.open(file_name);

  LOG4CXX_DEBUG(logger, "Started file manager.");
    
  message_queue mq(open_only, mid.c_str());

  boost::uint8_t buf[write_block_size];
  const int NUM_BLOCKS = 100;
  for (boost::uint32_t i=0; i<write_block_size; ++i)
    buf[i] = static_cast<uint8_t>(i);

  for (int i=0; i<NUM_BLOCKS; ++i) {
    Buffer* b = new Buffer();
    b->assign(buf, buf + NUM_BLOCKS);
    fm.write(b);
  }

  ControlMessage m;
  m._type = WRITE_TO_DISK;
  mq.send(&m, sizeof(m), 0);

  sleep(10);
  m._type = STOP;
  mq.send(&m, sizeof(m), 0);

  fm.join();

  fm.close();

  LOG4CXX_DEBUG(logger, "Joined file manager.");
}
