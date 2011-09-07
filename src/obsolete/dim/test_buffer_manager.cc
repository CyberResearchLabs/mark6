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
#include <buffer_manager.h>
#include <test_buffer_manager.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestBufferManager);

void
TestBufferManager :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestBufferManager :: tearDown (void)
{
}

void
TestBufferManager::basic(void)
{
  std::cout << "TestBufferManager::basic()" << std::endl;
  const boost::uint32_t BUF_POOL_SIZE(16);
  std::string ID("buffer_manager");
  const int TIMEOUT(1);

  BufferManager b(ID, BUF_POOL_SIZE, TIMEOUT);
  for (int i=0; i<BUF_POOL_SIZE; i++)
    b.push(b.pop());
}
