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
#include <socket_manager.h>
#include <test_socket_manager.h>

using namespace boost;
using namespace boost::interprocess;

CPPUNIT_TEST_SUITE_REGISTRATION (TestSocketManager);

void
TestSocketManager :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestSocketManager :: tearDown (void)
{
}

void
TestSocketManager::basic(void)
{
  std::cout << "TestSocketManager::basic()" << std::endl;
}
