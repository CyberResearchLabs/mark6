#include <unistd.h>

#include <string>
#include <iostream>

#include <boost/foreach.hpp>
#include <buffer_pool.h>

const int DEFAULT_TIMEOUT(10);

BufferPool* BufferPool::_inst = NULL;

BufferPool::BufferPool():
  _buffers(0),
  _capacity(0),
  _tail(0),
  _head(0),
  _TIMEOUT(DEFAULT_TIMEOUT),
  _mutex(),
  _read_cond(),
  _write_cond()
{
}

BufferPool::~BufferPool() {
  delete [] _buffers;
}

BufferPool* BufferPool::instance() {
  if (_inst == 0) {
    _inst = new BufferPool();
  }
  return _inst; 
}

bool BufferPool::push(boost::uint8_t* b) {
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_TIMEOUT*1000);
  
  boost::mutex::scoped_lock lock(_mutex);    
  while (is_full())
    if (!_write_cond.timed_wait(lock, timeout))
      return false;

  _buffers[_tail] = b;
  _tail = increment(_tail);
  _read_cond.notify_one();
  return true;
}

boost::uint8_t* BufferPool::pop() {
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_TIMEOUT*1000);
  
  boost::mutex::scoped_lock lock(_mutex);
  while (is_empty())
    if (!_read_cond.timed_wait(lock, timeout))
      return 0;

  boost::uint8_t* b = _buffers[_head];
  _head = increment(_head);
  _write_cond.notify_one();

  return b;
}

bool BufferPool::is_empty() const {
  return (_head == _tail);
}

bool BufferPool::is_full() const {
  int tail_check = (_tail+1) % _capacity;
  return (tail_check == _head);
}

unsigned int BufferPool::increment(unsigned int idx) const {
  idx = (idx + 1) % _capacity;
  return idx;
}

void BufferPool::reserve_pool(const int buffer_pool_size,
			      const int pages_per_buffer) {
  {
    boost::mutex::scoped_lock lock(_mutex);
    _capacity = buffer_pool_size + 1;
    _buffers = new boost::uint8_t*[_capacity];
  }

  const int page_size = getpagesize();
  const int buffer_size = page_size * pages_per_buffer;

  std::cout << "page_size: " << page_size << std::endl;
  for (int i=0; i<buffer_pool_size; i++) {
    std::cout << i << std::endl;
    void* buf;
    if (posix_memalign(&buf, page_size, buffer_size) != 0) {
      std::cerr << "Memalign failed\n";
      throw std::string("Memalign failed.");
    }
    push(static_cast<boost::uint8_t*>(buf));
  }
}

void BufferPool::release_pool() {
  while (is_empty() == false) {
    boost::uint8_t* b = pop();
    // TODO: figure out why free fails here.
  }
}

boost::uint8_t* BufferPool::malloc() {
  return pop();
}

bool BufferPool::free(boost::uint8_t* b) {
  return push(b);
}

