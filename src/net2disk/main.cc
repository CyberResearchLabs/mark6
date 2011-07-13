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


// C includes.
#include <signal.h>

// C++ includes.
#include <string>

// Local includes.
#include <pfring_util.h>
#include <net2disk.h>

const int ALARM_SLEEP = 1;
static Net2Disk* net2disk(0);
const int DEFAULT_SNAPLEN = 9000;
const std::string DEFAULT_DEVICE("eth0");
const int MAX_NUM_THREADS = 16;

//----------------------------------------------------------------------
void sigproc(int sig) {
  static int called = 0;

  fprintf(stderr, "Leaving...\n");
  if (called)
    return;
  else
    called = 1;

  net2disk->shutdown();
  net2disk->print_stats();
  if (net2disk->NUM_THREADS == 1)
    net2disk->pfring_close();

  exit(0);
}

//----------------------------------------------------------------------
void my_sigalarm(int sig) {
  net2disk->print_stats();
  alarm(ALARM_SLEEP);
  signal(SIGALRM, my_sigalarm);
}

//----------------------------------------------------------------------
void usage(void) {
  printf("pfcount\n(C) 2005-11 Deri Luca <deri@ntop.org>\n\n");
  printf("-h              Print this help\n");
  printf("-i <device>     Device name. Use device@channel for channels, and dna:ethX for DNA\n");
  printf("-n <threads>    Number of polling threads (default %d)\n",
	 net2disk->NUM_THREADS);
  printf("-c <cluster id> cluster id\n");
  printf("-e <direction>  0=RX+TX, 1=RX only, 2=TX only\n");
  printf("-l <len>        Capture length\n");
  printf("-g <core_id>    Bind this app to a code (only with -n 0)\n");
  printf("-w <watermark>  Watermark\n");
  printf("-p <poll wait>  Poll wait (msec)\n");
  printf("-b <cpu %%>      CPU pergentage priority (0-99)\n");
  printf("-a              Active packet wait\n");
  printf("-r              Rehash RSS packets\n");
  printf("-v              Verbose\n");
}

//----------------------------------------------------------------------
int main(int argc, char* argv[]) {
  char *device = NULL;
  char c;

  bool promisc;
  bool wait_for_packet = true;
  bool verbose = false;
  int snaplen = DEFAULT_SNAPLEN;
  int num_threads = 0;
  boost:uint32_t clusterId = 0;
  int bind_core = -1;
  packet_direction direction = rx_and_tx_direction;
  boost::uint16_t watermark = 0;
  boost::uint16_t poll_duration = 0;
  boost::uint16_t cpu_percentage = 0;
  boost::uint16_t rehash_rss = 0;

  while((c = getopt(argc,argv,"hi:c:dl:vs:ae:n:w:p:b:rg:" /* "f:" */)) != '?') {
    if((c == 255) || (c == -1)) break;

    switch(c) {
    case 'h':
      usage();
      return(0);
      break;
    case 'a':
      wait_for_packet = true;
      break;
    case 'e':
      switch(atoi(optarg)) {
      case rx_and_tx_direction:
      case rx_only_direction:
      case tx_only_direction:
	direction = (packet_direction)atoi(optarg);
	break;
      }
      break;
    case 'c':
      clusterId = atoi(optarg);
      break;
    case 'l':
      snaplen = atoi(optarg);
      break;
    case 'i':
      device = strdup(optarg);
      break;
    case 'n':
      num_threads = atoi(optarg);
      break;
    case 'v':
      verbose = true;
      break;
    case 'w':
      watermark = atoi(optarg);
      break;
    case 'b':
      cpu_percentage = atoi(optarg);
      break;
    case 'p':
      poll_duration = atoi(optarg);
      break;
    case 'r':
      rehash_rss = 1;
      break;
    case 'g':
      bind_core = atoi(optarg);
      break;
    }
  }

  // Pre-process parameters.
  if (verbose)
    watermark = 1;
  if (device == NULL)
    exit(1);

  if (num_threads > MAX_NUM_THREADS)
    num_threads = MAX_NUM_THREADS;

  if (cpu_percentage > 99)
    cpu_percentage = 99;

  // hardcode: promisc=1, to_ms=500
  promisc = 1;

  // Create Net2Disk instance with supplied parameters.
  net2disk = new Net2Disk(snaplen, num_threads, std::string(device), promisc,
			  wait_for_packet, direction, clusterId, verbose,
			  watermark, cpu_percentage, poll_duration, rehash_rss,
			  bind_core);

  // Setup signal handling.
  signal(SIGINT, sigproc);
  signal(SIGTERM, sigproc);
  signal(SIGINT, sigproc);

  if(!verbose) {
    signal(SIGALRM, my_sigalarm);
    alarm(ALARM_SLEEP);
  }

  // Start capturing.
  net2disk->run();

  // All done.
  return(0);
}
