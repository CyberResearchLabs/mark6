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
#include <unistd.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <sstream>
#include <list>
#include <algorithm>

// Boost includes.
#include <boost/foreach.hpp>
#include <boost/pool/singleton_pool.hpp>

//Local includes.
#include <mark6.h>
#include <buffer_pool.h>
#include <test_pool.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestPool);

void
TestPool::setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestPool::tearDown (void)
{
}

class OutOfMemoryException { 
private:
  string _msg;
public:
  OutOfMemoryException(const string& msg) : _msg(msg) {}
  ~OutOfMemoryException( ) {}
  string getMessage( ) const {
    return(_msg);
  }
};

const int PAGES_PER_BUFFER(256);
const int BUFFER_POOL_SIZE(16);
const int BUFFER_SIZE(1048576);

void
TestPool::basic(void) {
  std::cout << "TestPool::basic()" << std::endl;
  const boost::uint32_t BUF_POOL_SIZE(16);
  std::string ID("buffer_manager");
  const int TIMEOUT(1);

  BufferPool& inst = BufferPool::instance();

  const int BUFFER_POOL_SIZE(32);
  const int PAGES_PER_BUFFER(256);
  const int BUFFER_SIZE(PAGES_PER_BUFFER * getpagesize());
  cout << "BUFFER_SIZE: " << BUFFER_SIZE << endl;
  cout << "sizeof(boost::uint8_t*): " << sizeof(boost::uint8_t*) << endl;
  cout << "sizeof(void*): " << sizeof(void*) << endl;
  inst.reserve_pool(BUFFER_POOL_SIZE, PAGES_PER_BUFFER);

  int MAX_ALLOCS(1000);
  double pool_rate, system_rate;

  Timer t;
  for (int j = 0; j<MAX_ALLOCS; j++) {
    boost::uint8_t* b = inst.malloc();
    inst.free(b);
  }
  double elapsed = t.elapsed();
  pool_rate = (double)MAX_ALLOCS/elapsed;

  cout << setw(20) << left << "Elapsed time: "
       << setw(10) << elapsed << " seconds " 
       << setw(20) << left << "Rate: "
       << setw(10) << pool_rate << " allocs/s"
       << endl;

  t.restart();
  for (int j=0; j<MAX_ALLOCS; j++) {
    void* buf;
    if (posix_memalign(&buf, getpagesize(), BUFFER_SIZE) != 0) {
      std::cerr << "Memalign failed\n";
      throw std::string("Memalign failed.");
    }
    free(buf);
  }
  elapsed = t.elapsed();
  system_rate = (double)MAX_ALLOCS/elapsed;
  cout << setw(20) << left << "Elapsed time: "
       << setw(10) << elapsed << " seconds " 
       << setw(20) << left << "Rate: "
       << setw(10) << system_rate << " allocs/s"
       << endl;

  cout << setw(20) << "Speedup:" << pool_rate/system_rate << endl;
  inst.release_pool();





}
