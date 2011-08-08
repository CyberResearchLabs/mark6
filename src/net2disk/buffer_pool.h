#ifndef _BUFFER_POOL_
#define _BUFFER_POOL_

#include <list>
#include <boost/cstdint.hpp>

class BufferPool {
 protected:
  boost::uint8_t** _buffers;
  int _capacity;
  volatile unsigned int _tail;
  volatile unsigned int _head;

  unsigned int increment(const unsigned int idx) const {
    const int new_idx = (idx + 1) % _capacity;
    return new_idx;
  }

  bool push(boost::uint8_t* b) {
    int next_tail = increment(_tail);
    if (next_tail != _head) {
      _buffers[_tail] = b;
      _tail = next_tail;
      return true;
    }
    return false;
  }

  boost::uint8_t* pop() {
    if (_head == _tail)
      return 0;
    boost::uint8_t* b = _buffers[_head];
    _head = increment(_head);
    return b;
  }

  bool is_empty() const {
    return (_head == _tail);
  }

  bool is_full() const {
    int tail_check = (_tail+1) % _capacity;
    return (tail_check == _head);
  }

 public:
  // This is how clients can access the single instance
  static BufferPool& instance() {
    if (_inst == 0)
      _inst = new BufferPool();
    return *_inst;
  }


  void reserve_pool(const int buffer_pool_size, const int pages_per_buffer) {
    _capacity = buffer_pool_size + 1;
    _buffers = new boost::uint8_t*[_capacity];

    const int page_size = getpagesize();
    const int buffer_size = page_size * pages_per_buffer;

    for (int i=0; i<buffer_pool_size; i++) {
      void* buf;
      if (posix_memalign(&buf, page_size, buffer_size) != 0) {
	throw std::string("Memalign failed.");
      }
      push(static_cast<boost::uint8_t*>(buf));
    }
  }
  
  void release_pool() {
    while (is_empty() == false) {
      boost::uint8_t* b = pop();
      // TODO: figure out why free fails here.
    }
  }

  boost::uint8_t* malloc() {
    if (is_empty())
      return 0;
    return pop();
  }

  bool free(boost::uint8_t* b) {
    if (is_full())
      return false;
    push(b);
    return true;
  }

  virtual ~BufferPool() {
    delete [] _buffers;
  }

 private:
  static BufferPool* _inst;
 BufferPool(): _buffers(0), _capacity(0), _tail(0), _head(0)
    {}

  // private constructor
  BufferPool(const BufferPool&) {}
  BufferPool& operator=(const BufferPool&) {}
};


#endif // _BUFFER_POOL_
