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

#ifndef _BUFFER_POOL_H_
#define _BUFFER_POOL_H_

// C++ includes
#include <list>

// Framework includes.
#include <boost/cstdint.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

//! This is thrown on a timeout message.
struct Timeout {

  //! The message to send to the exception handler.
  std::string _msg;

  //! Constructor.
  //! \param msg The message to be sent to the exception handler.
  Timeout(std::string msg): _msg(msg) {}
};

//!----------------------------------------------------------------------------
//! Provides a pool of optimally aligned and sized buffers.
//! These buffers are used to efficiently transport data from network
//! interface to disk. They are aligned to page boundaries and sized to be
//! multiples of the pagesize. This ensures efficient memory to disck transfer
//! using DIRECT_IO.
//!
//! A buffer pool is a threadsafe singleton. In principle, it could be made
//! lockless for single consumer/producer access (the main use case). This 
//! eliminate the need for mutex locking. However, performance is currently
//! adequate, and mutexes have advantages over the lockless implementation
//! (e.g., timed blocking operations).
class BufferPool {

 public:
  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------
  //! Returns a pointer to the single instance of BufferPool.
  static BufferPool* instance(); 

  //! Reserves memory for the buffer pool.
  //! Creates memory aligned buffers, sized according to pages_per_buffer,
  //! then pushes those buffers onto its internal circular buffer.
  //! \param buffer_pool_size The total size of the buffer pool.
  //! \param pages_per_buffer The number of pages per buffer. Buffers are
  //! aligned and sized according to the system pagesize().
  void reserve_pool(const int buffer_pool_size, const int pages_per_buffer);

  //! FIXME
  void reserve_pool_raw(const int buffer_pool_size, const int buffer_size);

  //! Release the memory occupied by the allocated buffers in _buffers.
  void release_pool();

  //! Returns a newly "allocated" buffer. All buffers are pre-allocated, so
  //! malloc simply pops the next available buffer (or 0 if the operation 
  //! times out.
  //! \return A buffer, or 0 if the operation times out (i.e. the buffer pool
  //! is exhausted for > _TIMEOUT seconds.
  boost::uint8_t* malloc();

  //! Recycle a buffer back into the buffer pool.
  //! Inserts a buffer into the buffer pool circular queue.
  //! \param b The buffer to be recyled.
  //! \return True if the buffer is successfully recycled, False if the 
  //! operation times out (i.e. the buffer pool is full for > _TIMEOUT
  //! seconds).
  bool free(boost::uint8_t* b);

  //! Destructor.
  virtual ~BufferPool();

 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------

  //! Pointer to allocated buffers. Buffers are stored in an array and 
  //! accessed by index. This array, along with _head, _tail, and the
  //! associated operations, forms a circular queue.
  boost::uint8_t** _buffers; 

  //! The total capacity of the buffer pool. This is one more than the total
  //! number of buffers that can be stored (to differentiate between empty
  //! and full buffers.
  int _capacity;

  //! An index into _buffers that points to the current tail of the buffer
  //! pool.
  volatile unsigned int _tail;

  //! An index into _buffers that points to the current head of the buffer
  //! pool.
  volatile unsigned int _head;

  //! How to long to wait before timing out on push() or pop() operations.
  int _TIMEOUT;

  //! This is the mutex that serializes multi-threaded resources access.
  boost::mutex _mutex;

  //! Condition variable that signals blocked readers (i.e. pop() operations)
  //! when a new buffer is re-inserted back into the pool.
  boost::condition_variable _read_cond;

  //! Condition variable that signals blocked writers (i.e. push() operations)
  //! when a new buffer is popped from the pool.
  boost::condition_variable _write_cond;

  //! Private pointer to the singleton instance.
  static BufferPool* _inst;


 protected:
  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Increment the _buffer index pointed to by idx and wrap around if
  //! necessary.
  //! \param idx the index to increment.
  //! \retun The value idx takes when incremented.
  unsigned int increment(const unsigned int idx) const;

  //! Push a buffer back into the pool.
  //! \param b The buffer to be returned to the pool.
  //! \return True if buffer successfully pushed, False if not (usually
  //! because the operation timed out).
  bool push(boost::uint8_t* b);

  //! Pop a buffer from the pool.
  //! \return The new buffer allocated, or 0 if the operation timed out.
  boost::uint8_t* pop();

  //! Query state of buffer pool.
  //! \return True if buffer pool is empty, False otherwise.
  bool is_empty() const;

  //! Query state of buffer pool.
  //! \return True if buffer pool is full, False otherwise.
  bool is_full() const;


 private:
  //---------------------------------------------------------------------------
  // "Deleted" methods
  //---------------------------------------------------------------------------

  //! Default constructor cannot be used to create an instance of BufferPool.
  BufferPool();

  //! Copy constructor cannot be used to create an instance of BufferPool.
  BufferPool(const BufferPool&);

  //! Assignment operator cannot be used.
  BufferPool& operator=(const BufferPool&);
};


#endif // _BUFFER_POOL_H_
