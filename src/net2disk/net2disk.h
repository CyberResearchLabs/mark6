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
#define MAX_NUM_THREADS        64
#define DEFAULT_DEVICE         "eth0"
#define NUMBER_OF_FILES       	7 

/* Globals */
extern int pages_per_buffer;
extern int page_size;
extern int buffer_size;
extern u_char* bufs[NUMBER_OF_FILES];
extern int fds[NUMBER_OF_FILES];
extern pfring  *pd;
extern int verbose;
extern int num_threads;
extern pfring_stat pfringStats;
extern pthread_rwlock_t statsLock;
extern u_int32_t thiszone;

extern struct timeval startTime;
extern unsigned long long numPkts[MAX_NUM_THREADS];
extern unsigned long long numBytes[MAX_NUM_THREADS];
extern u_int8_t wait_for_packet;
extern u_int8_t do_shutdown;

/* Forward declarations. */
extern void writer_task(int fd, u_char* buf, int buf_size);
extern double delta_time (struct timeval * now, struct timeval * before);
extern void print_stats();
extern void sigproc(int sig);
extern void my_sigalarm(int sig);
extern char* etheraddr_string(const u_char *ep, char *buf);
extern char* _intoa(unsigned int addr, char* buf, u_short bufLen);
char* intoa(unsigned int addr);
extern inline char* in6toa(struct in6_addr addr6);
extern std::string proto2str(u_short proto);
extern void dummyProcesssPacket(const struct pfring_pkthdr *h, const u_char *p, const u_char *user_bytes);
extern int32_t gmt2local(time_t t);
extern void printHelp(void);
extern int bind2core(u_int core_id);
extern void dump_buf(const long thread_id, const u_char* buf);
extern void* packet_consumer_thread(void* _id);
extern void writer_task(int fd, u_char* buf, int buf_size);
extern void setup();

#endif // _NET2DISK_H__
