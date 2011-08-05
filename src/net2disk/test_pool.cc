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

const int DUMMY_BUFFER_SIZE(1);
const int PAGES_PER_BUFFER(256);
const int BUFFER_POOL_SIZE(16);
const int BUFFER_SIZE(1048576);

#if 0
list<boost::uint8_t*> BUFFERS;
void allocate_buffers(list<boost::uint8_t*>& buffers,
		      const int buffer_pool_size,
		      const int page_size,
		      const int pages_per_buffer) {
  void* buf;
  const int BUFFER_SIZE(page_size*pages_per_buffer);
  for (int i=0; i<buffer_pool_size; ++i) {
    if (posix_memalign(&buf, page_size, BUFFER_SIZE) < 0) {
      std::cerr << "Memalign failed\n";
      throw std::string("Memalign failed.");
    }
    buffers.push_back(static_cast<boost::uint8_t*>(buf));
  }
}

struct net2disk_allocate_new_delete {
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  static char * malloc(const size_type bytes) {
    if (BUFFERS.empty())
      throw OutOfMemoryException(string("OOM"));
    boost::uint8_t* block = BUFFERS.front();
    std::cout << "malloc: " << bytes << std::endl;
    std::cout << "malloc: " << sizeof(*block) << std::endl;
    BUFFERS.pop_front();

    return (char*)block;
  }

  static void free(char * const block) {
    BUFFERS.push_back((boost::uint8_t*)block);
  }
};

struct PoolTag { };

typedef boost::singleton_pool<PoolTag, BUFFER_SIZE, net2disk_allocate_new_delete>
buffer_pool;
#endif // 0

void
TestPool::basic(void) {
  std::cout << "TestPool::basic()" << std::endl;
  const boost::uint32_t BUF_POOL_SIZE(16);
  std::string ID("buffer_manager");
  const int TIMEOUT(1);

# if 0
  allocate_buffers(BUFFERS, BUFFER_POOL_SIZE, getpagesize(), PAGES_PER_BUFFER);
  boost::uint8_t* buffer = (boost::uint8_t*)buffer_pool::malloc();
  std::cout << "Sizeof(buffer): " << sizeof(*buffer) << std::endl;
#endif

  BufferPool& inst = BufferPool::instance();

  const int BUFFER_POOL_SIZE(1);
  const int PAGES_PER_BUFFER(256);
  const int BUFFER_SIZE(PAGES_PER_BUFFER * getpagesize());
  cout << "BUFFER_SIZE: " << BUFFER_SIZE << endl;
  cout << "sizeof(boost::uint8_t*): " << sizeof(boost::uint8_t*) << endl;
  cout << "sizeof(void*): " << sizeof(void*) << endl;
  inst.reserve_pool(BUFFER_POOL_SIZE, PAGES_PER_BUFFER);

  for (int j = 0; j<100; j++) {
    boost::uint8_t* b = inst.malloc();
    cout << "b: " << (long long)b << endl;
    cout << "sizeof(b): " << sizeof(b) << endl;
    cout << "sizeof(*b): " << sizeof(*b) << endl;
    cout << (long long)b << endl;
    for (int i=0; i<BUFFER_SIZE; i++) {
      b[i] = i;
    }
    inst.free(b);
  }

  inst.release_pool();
}
