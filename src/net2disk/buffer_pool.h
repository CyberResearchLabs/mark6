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
  unsigned int increment(const unsigned int idx) const;
  bool push(boost::uint8_t* b);
  boost::uint8_t* pop();
  bool is_empty() const;
  bool is_full() const;

 public:
  // This is how clients can access the single instance
  static BufferPool& instance(); 
  void reserve_pool(const int buffer_pool_size, const int pages_per_buffer);
  void release_pool();
  boost::uint8_t* malloc();
  void free(boost::uint8_t* b);
  virtual ~BufferPool();

 private:
  static BufferPool* _inst; // The one, single instance
  BufferPool();

  // private constructor
  BufferPool(const BufferPool&);
  BufferPool& operator=(const BufferPool&);
};


#endif // _BUFFER_POOL_
