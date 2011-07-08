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
#include <pfring.h>


/* Constants */
#define ALARM_SLEEP            1
#define DEFAULT_SNAPLEN        9000
#define NUM_THREADS        64
#define DEFAULT_DEVICE         "eth0"
#define NUM_FILES       	7 

class Net2Disk {
  // Constants.
  cont u_int32_t ALARM_SLEEP;
  const u_int32_t DEFAULT_SNAPLEN;
  const int NUM_THREADS;
  const int DEFAULT_DEVICE;
  const int NUM_FILES;

  /* Globals */
  int pages_per_buffer;
  int page_size;
  int buffer_size;
  u_char* bufs; // [NUM_FILES];
  int fds; // [NUM_FILES];
  pfring  *pd;
  int num_threads;
  pfring_stat pfringStats;
  pthread_rwlock_t statsLock;

  struct timeval startTime;
  unsigned long long numPkts; // [NUM_THREADS];
  unsigned long long numBytes; // [NUM_THREADS];
  u_int8_t wait_for_packet;
  u_int8_t do_shutdown;

  int verbose;
  uint32_t thiszone;


  // Methods.
  Net2Disk(const int alarm_sleep, const int default_snaplen,
	   const int max_num_threads, const int device,
	   const int number_of_files);
  ~Net2Disk();

  void writer_task(int fd, u_char* buf, int buf_size);
  void print_stats();
  void sigproc(int sig);
  void my_sigalarm(int sig);
  inline char* in6toa(struct in6_addr addr6);
  void printHelp(void);
  void dump_buf(const long thread_id, const u_char* buf);
  void *packet_consumer_thread(void* _id);
  void writer_task(int fd, u_char* buf, int buf_size);
  void setup();
};

#endif // _NET2DISK_H__
