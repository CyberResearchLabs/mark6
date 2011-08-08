#include <unistd.h>

#include <string>
#include <iostream>

#include <boost/foreach.hpp>
#include <buffer_pool.h>

BufferPool* BufferPool::_inst = NULL;

BufferPool::BufferPool():
  _buffers(0),
  _capacity(0),
  _tail(0),
  _head(0)
{
}

BufferPool::~BufferPool() {
  delete [] _buffers;
}

BufferPool& BufferPool::instance() {
  if (_inst == 0) {
    _inst = new BufferPool();
  }
  return *_inst; 
}

bool BufferPool::push(boost::uint8_t* b) {
  int next_tail = increment(_tail);
  if (next_tail != _head) {
    _buffers[_tail] = b;
    _tail = next_tail;
    return true;
  }

  // Queue full (never happen).
  return false;
}

boost::uint8_t* BufferPool::pop() {
  if (_head == _tail)
    return 0;
  boost::uint8_t* b = _buffers[_head];
  _head = increment(_head);

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
  _capacity = buffer_pool_size + 1;
  _buffers = new boost::uint8_t*[_capacity];

  const int page_size = getpagesize();
  const int buffer_size = page_size * pages_per_buffer;

  std::cout << "page_size: " << page_size << std::endl;
  for (int i=0; i<buffer_pool_size; i++) {
    void* buf;
    if (posix_memalign(&buf, page_size, buffer_size) != 0) {
      std::cerr << "Memalign failed\n";
      throw std::string("Memalign failed.");
    }
    push(static_cast<boost::uint8_t*>(buf));
  }
}

void BufferPool::release_pool() {
  int i = 0;
  while (is_empty() == false) {
    boost::uint8_t* b = pop();

    // TODO: figure out why this fails..
    // if (b)
    // free(b);
    //else
    // std::cout << "AARGH" << std::endl;
  }
}

boost::uint8_t* BufferPool::malloc() {
  if (is_empty())
    return 0;
  return pop();
}

void BufferPool::free(boost::uint8_t* b) {
  push(b);
}

