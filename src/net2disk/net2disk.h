/*
 *
 * (C) 2005-11 - Luca Deri <deri@ntop.org>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * VLAN support courtesy of Vincent Magnin <vincent.magnin@ci.unil.ch>
 *
 */

#ifndef _NET2DISK_H_
#define _NET2DISK_H_

// C++ includes
#include <string>

// Framework includes.
#include <boost/thread/thread.hpp>
#include <boost/ptr_container/ptr_list.hpp>
#include <pfring.h>

class Net2Disk;

struct PacketConsumerThread {
  PacketConsumerThread();
  ~PacketConsumerThread();

  // Main entry point for thread.
  void operator() (const long id, Net2Disk* net2disk);
  void writer_task(int fd, u_char* buf, int buf_size);
};


class Net2Disk {
 public:
  // Constants.
  const boost::uint32_t SNAPLEN;
  const int NUM_THREADS;
  const std::string DEVICE;
  const int BIND_CORE;
 private:
  /* Globals */
  int LOCAL_PAGES_PER_BUFFER;
  int LOCAL_PAGE_SIZE;

 public:
  int BUFFER_SIZE;
  u_char** bufs; // [NUM_FILES];
  int *fds; // [NUM_FILES];
  pfring  *pd;
  bool WAIT_FOR_PACKET;

 private:
  pfring_stat pfringStats;
  pthread_rwlock_t statsLock;
  struct timeval startTime;
 public:
  unsigned long long *numPkts; // [NUM_THREADS];
  unsigned long long *numBytes; // [NUM_THREADS];

  bool do_shutdown;
  bool verbose;
  boost::uint32_t thiszone;

  boost::ptr_list<boost::thread> _threads;

 public:
  // Methods.
  Net2Disk(const int snaplen,
	   const int num_threads,
	   const std::string& device,
	   const int bind_core,
	   const bool promisc,
	   const bool wait_for_packet,
	   const int direction,
	   const int clusterId,
	   const bool verbose,
	   const int watermark,
	   const int cpu_percentage,
	   const int poll_duration,
	   const bool rehash_rss);

  ~Net2Disk();

  void run();
  void print_stats();
  inline char* in6toa(struct in6_addr addr6);
  void dump_buf(const long thread_id, const u_char* buf);
  void *packet_consumer_thread(void* _id);
  void writer_task(int fd, u_char* buf, int buf_size);
  void setup();

  // Access methods for global signal handler sigproc().
  void shutdown() { do_shutdown = 1; }
  void pfring_close() { ::pfring_close(pd); }
};

#endif // _NET2DISK_H__
