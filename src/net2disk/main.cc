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

// Local includes.
#include <pfring_util.h>
#include <net2disk.h>

//----------------------------------------------------------------------------
void printHelp(void) {
  printf("pfcount\n(C) 2005-11 Deri Luca <deri@ntop.org>\n\n");
  printf("-h              Print this help\n");
  printf("-i <device>     Device name. Use device@channel for channels, and dna:ethX for DNA\n");
  printf("-n <threads>    Number of polling threads (default %d)\n", num_threads);
  printf("-c <cluster id> cluster id\n");
  printf("-e <direction>  0=RX+TX, 1=RX only, 2=TX only\n");
  printf("-s <string>     String to search on packets\n");
  printf("-l <len>        Capture length\n");
  printf("-g <core_id>    Bind this app to a code (only with -n 0)\n");
  printf("-w <watermark>  Watermark\n");
  printf("-p <poll wait>  Poll wait (msec)\n");
  printf("-b <cpu %%>      CPU pergentage priority (0-99)\n");
  printf("-a              Active packet wait\n");
  printf("-r              Rehash RSS packets\n");
  printf("-v              Verbose\n");
}

//----------------------------------------------------------------------------
int main(int argc, char* argv[]) {
  char *device = NULL, c, *string = NULL, buf[32];
  u_char mac_address[6];
  int promisc, snaplen = DEFAULT_SNAPLEN, rc;
  u_int clusterId = 0;
  int bind_core = -1;
  packet_direction direction = rx_and_tx_direction;
  u_int16_t watermark = 0, poll_duration = 0, cpu_percentage = 0, rehash_rss = 0;

  startTime.tv_sec = 0;
  thiszone = gmt2local(0);

  while((c = getopt(argc,argv,"hi:c:dl:vs:ae:n:w:p:b:rg:" /* "f:" */)) != '?') {
    if((c == 255) || (c == -1)) break;

    switch(c) {
    case 'h':
      printHelp();
      return(0);
      break;
    case 'a':
      wait_for_packet = 0;
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
      verbose = 1;
      break;
    case 's':
      string = strdup(optarg);
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

  setup();

  /* Start */
  
  if (verbose)
    watermark = 1;
  if (device == NULL)
    exit(1);

  if(num_threads > MAX_NUM_THREADS) num_threads = MAX_NUM_THREADS;

  /* hardcode: promisc=1, to_ms=500 */
  promisc = 1;

  if(num_threads > 0)
    pthread_rwlock_init(&statsLock, NULL);

  if(wait_for_packet && (cpu_percentage > 0)) {
    if(cpu_percentage > 99) cpu_percentage = 99;
    pfring_config(cpu_percentage);
  }

  pd = pfring_open(device, promisc,  snaplen, (num_threads > 1) ? 1 : 0);

  if(pd == NULL) {
    printf("pfring_open error (pf_ring not loaded or perhaps you use quick mode and have already a socket bound to %s ?)\n",
	   device);
    return(-1);
  } else {
    u_int32_t version;
    char application_name[] = "net2disk";
    pfring_set_application_name(pd, application_name);
    pfring_version(pd, &version);

    printf("Using PF_RING v.%d.%d.%d\n",
	   (version & 0xFFFF0000) >> 16,
	   (version & 0x0000FF00) >> 8,
	   version & 0x000000FF);
  }

  if(pfring_get_bound_device_address(pd, mac_address) != 0)
    printf("pfring_get_bound_device_address() failed\n");
  else
    printf("Capturing from %s [%s]\n", device, etheraddr_string(mac_address, buf));

  printf("# Device RX channels: %d\n", pfring_get_num_rx_channels(pd));
  printf("# Polling threads:    %d\n", num_threads);

  if(clusterId > 0) {
    rc = pfring_set_cluster(pd, clusterId, cluster_round_robin);
    printf("pfring_set_cluster returned %d\n", rc);
  }

  if((rc = pfring_set_direction(pd, direction)) != 0)
    printf("pfring_set_direction returned [rc=%d][direction=%d]\n", rc, direction);

  if(watermark > 0) {
    if((rc = pfring_set_poll_watermark(pd, watermark)) != 0)
      printf("pfring_set_poll_watermark returned [rc=%d][watermark=%d]\n", rc, watermark);
  }

  if(rehash_rss)
    pfring_enable_rss_rehash(pd);

  if(poll_duration > 0)
    pfring_set_poll_duration(pd, poll_duration);

  signal(SIGINT, sigproc);
  signal(SIGTERM, sigproc);
  signal(SIGINT, sigproc);


  if(!verbose) {
    signal(SIGALRM, my_sigalarm);
    alarm(ALARM_SLEEP);
  }

  pfring_enable_ring(pd);

  if(num_threads > 1) {
    pthread_t my_thread;
    long i;

    for(i=1; i<num_threads; i++)
      pthread_create(&my_thread, NULL, packet_consumer_thread, (void*)i);

    bind_core = -1;
  }

  if(bind_core >= 0)
    bind2core(bind_core);

  // if(1) {
  // pfring_loop(pd, dummyProcesssPacket, (u_char*)NULL);
  // } else
  packet_consumer_thread(0);

  alarm(0);
  sleep(1);
  pfring_close(pd);

  return(0);
}
