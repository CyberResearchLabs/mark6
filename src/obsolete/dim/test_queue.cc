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

// C++ includes.
#include <iostream>

// Boost includes.

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <socket_manager.h>
#include <test_queue.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestQueue);

void
TestQueue :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestQueue :: tearDown (void)
{
}

void
TestQueue::basic(void)
{
  std::cout << "TestQueue::basic()" << std::endl;
  const std::string id("Queue");
  const boost::uint32_t size(1000);
  const boost::uint32_t timeout(1);

  Queue<int> q(id, size, timeout);
  for (int i=0; i<size; i++)
    q.push_back(i);

  q.print_stats();

  for (int i=0; i<size; i++)
    q.pop_front();

  q.print_stats();

}
