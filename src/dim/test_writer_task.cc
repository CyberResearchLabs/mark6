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
#include <writer_task.h>
#include <test_writer_task.h>

using namespace boost;
using namespace boost::interprocess;

CPPUNIT_TEST_SUITE_REGISTRATION (TestWriterTask);


void
TestWriterTask::setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestWriterTask::tearDown (void)
{
}

void
TestWriterTask::basic(void)
{
  std::cout << "TestWriterTask::basic()" << std::endl;
  const boost::uint32_t BUF_SIZE = 4096;
  boost::uint8_t buf[BUF_SIZE];
  const int N = 1000;
  int i;

  for (i=0; i<BUF_SIZE; i++)
    buf[i] = (boost::uint8_t)i;
  
  int fd = open("/tmp/test_writer_task.dat", O_WRONLY | O_CREAT);
  for (i=0; i<N; i++) {
    WriterTask w(i, fd, buf, BUF_SIZE);
  }
}
