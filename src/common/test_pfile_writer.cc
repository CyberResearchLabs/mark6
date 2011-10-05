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

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfile_writer.h>
#include <test_pfile_writer.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestPFileWriter);

void
TestPFileWriter :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestPFileWriter :: tearDown (void)
{
}

void
TestPFileWriter::basic(void)
{
  std::cout << "TestPFileWriter::basic()" << std::endl;
  const int id = 0;
  std::list<std::string> capture_files;
  const int N = 8;
  for (int i=0; i<N; i++) {
    std::ostringstream oss;
    oss << "/tmp/test_pfile_writer-basic-" << i << ".dat";
    capture_files.push_back(oss.str());
  }

  const boost::uint32_t write_block_size(1048576);
  const boost::uint32_t write_blocks(512);
  const boost::uint32_t poll_timeout = 1000; // ms
  const double command_interval = 1; //s
  const bool preallocated(false);
  const bool directio(true);



  PFileWriter pfw(id, write_block_size, write_blocks, capture_files,
		poll_timeout, 0, command_interval, preallocated, directio);

  pfw.open();
  pfw.start();

  LOG4CXX_DEBUG(logger, "Started pfile writer.");
    
  const int NUM_BLOCKS = 10000;
  boost::uint8_t* buf = pfw.malloc_buffer();
  for (boost::uint32_t i=0; i<write_block_size; ++i) 
    buf[i] = static_cast<uint8_t>(i);

  for (int i=0; i<NUM_BLOCKS; ++i) {
    pfw.write(buf);
  }

  pfw.cmd_write_to_disk();

  sleep(20);

  pfw.cmd_stop();
  pfw.join();

  pfw.close();

  LOG4CXX_DEBUG(logger, "Joined file writer.");
}
