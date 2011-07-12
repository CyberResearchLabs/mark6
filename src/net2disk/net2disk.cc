
#define _GNU_SOURCE

// C includes.
#include <signal.h>
#include <sched.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <sys/poll.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip6.h>
#include <net/ethernet.h>
#include <sys/time.h>
#include <time.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <monetary.h>
#include <locale.h>

// C++ includes.
#include <string>
#include <iostream>

// Framework includes.
#include <pfring.h>

// Local includes.
#include <pfring_util.h>
#include <net2disk.h>




Net2Disk::Net2Disk(const int snaplen,
		   const int num_threads,
		   const std::string& device,
		   const int bind_core,
		   const int promisc,
		   const int wait_for_packet,
		   const packet_direction direction,
		   const int clusterId,
		   const int verbose,
		   const int watermark,
		   const int cpu_percentage,
		   const int poll_duration,
		   const int rehash_rss):
  SNAPLEN(snaplen),
  NUM_THREADS(num_threads),
  DEVICE(device),
  BIND_CORE(bind_core),
  //

  //
  pages_per_buffer(0),
  page_size(0),
  buffer_size(0),
  bufs(0),
  fds(0),
  pd(0),
  pfringStats(),
  statsLock(),
  startTime(),
  numPkts(0),
  numBytes(0),
  wait_for_packet(1),
  do_shutdown(0),
  verbose(0)
{
  // Allocate memory structures.
  int i=0;

  bufs = new u_char*[NUM_THREADS];
  for (i = 0; i<NUM_THREADS; i++)
    bufs[i] = 0;

  fds = new int[NUM_THREADS];
  for (i = 0; i<NUM_THREADS; i++)
    fds[i] = 0;

  numPkts = new unsigned long long[NUM_THREADS];
  for (i = 0; i<NUM_THREADS; i++)
    numPkts[i] = 0;

  for (i = 0; i<NUM_THREADS; i++)
    numBytes = new unsigned long long[NUM_THREADS];

  // Setup buffers etc.
  this->setup();

  // Do everything else.
  u_char mac_address[6];
  char buf[32];
  int rc;


  if (NUM_THREADS > 0)
    pthread_rwlock_init(&statsLock, NULL);

  if (wait_for_packet && (cpu_percentage > 0)) {
    pfring_config(cpu_percentage);
  }

  pd = pfring_open((char*)device.c_str(), promisc,  snaplen,
		   (NUM_THREADS > 1) ? 1 : 0);
  if (pd == NULL) {
    std::cout
      << "pfring_open error (pf_ring not loaded or perhaps you use quick mode "
      << " and have already a socket bound to " << device << " ?)\n";
    exit(1);
  } else {
    u_int32_t version;
    char application_name[] = "net2disk";
    pfring_set_application_name(pd, application_name);
    pfring_version(pd, &version);

    std::cout
      << "Using PF_RING v."
      << ((version & 0xFFFF0000) >> 16) << "."
      << ((version & 0x0000FF00) >> 8) << "."
      << (version & 0x000000FF) << "\n";
  }

  if (pfring_get_bound_device_address(pd, mac_address) != 0)
    std::cout << "pfring_get_bound_device_address() failed\n";
  else
    std::cout << "Capturing from " << device
	      << " [" << etheraddr_string(mac_address, buf) << "]\n";

  std::cout
    << "# Device RX channels: " << pfring_get_num_rx_channels(pd) << std::endl
    <<"# Polling threads:    " << NUM_THREADS << std::endl;

  if (clusterId > 0) {
    rc = pfring_set_cluster(pd, clusterId, cluster_round_robin);
    std::cout <<"pfring_set_cluster returned " << rc << std::endl;
  }

  if((rc = pfring_set_direction(pd, direction)) != 0)
    std::cout
      <<"pfring_set_direction returned [rc=" << rc << "]"
      << "[direction=" << direction << "]\n";

  if(watermark > 0) {
    if((rc = pfring_set_poll_watermark(pd, watermark)) != 0)
      std::cout
	<< "pfring_set_poll_watermark returned [rc=" << rc << "]"
	<< "[watermark=" << watermark << "]\n";
  }

  if(rehash_rss)
    pfring_enable_rss_rehash(pd);

  if(poll_duration > 0)
    pfring_set_poll_duration(pd, poll_duration);
}

void Net2Disk::run() {
  pfring_enable_ring(pd);
  if(NUM_THREADS > 1) {
    pthread_t my_thread;
    long i;

    for(i=1; i<NUM_THREADS; i++)
      pthread_create(&my_thread, NULL, packet_consumer_thread, (void*)i);
  } else {
    if(BIND_CORE >= 0)
      bind2core(BIND_CORE);
  }

  packet_consumer_thread(0);

  alarm(0);
  sleep(1);
  ::pfring_close(pd);
}

Net2Disk::~Net2Disk() {
  delete [] bufs;
  delete [] fds;
  delete [] numPkts;
  delete [] numBytes;
}


//---------------------------------------------------------------------------
void Net2Disk::print_stats() {
  pfring_stat pfringStat;
  struct timeval endTime;
  double deltaMillisec;
  static u_int8_t print_all;
  static u_int64_t lastPkts = 0;
  u_int64_t diff;
  static struct timeval lastTime;
  char buf1[64], buf2[64];

  if(startTime.tv_sec == 0) {
    gettimeofday(&startTime, NULL);
    print_all = 0;
  } else
    print_all = 1;

  gettimeofday(&endTime, NULL);
  deltaMillisec = delta_time(&endTime, &startTime);

  if(pfring_stats(pd, &pfringStat) >= 0) {
    double thpt;
    int i;
    unsigned long long nBytes = 0, nPkts = 0;

    for(i=0; i < NUM_THREADS; i++) {
      nBytes += numBytes[i];
      nPkts += numPkts[i];
    }

    thpt = ((double)8*nBytes)/(deltaMillisec*1000);

    double dropped_percent = pfringStat.recv == 0 ? 0 :
      (double)(pfringStat.drop*100)/(double)(pfringStat.recv+pfringStat.drop);
    unsigned int rcv_pkts = pfringStat.recv;
    unsigned int drp_pkts = pfringStat.drop;
    unsigned int tot_pkts = rcv_pkts + drp_pkts;
    double drp_pct = (double)(100*drp_pkts)/(double)(tot_pkts);

    std::cerr
      << "=========================\n"
      << "Absolute Stats: [" << rcv_pkts << " pkts rcvd]"
      << "[" << drp_pkts << " pkts dropped]\n"
      << "Total Pkts=" << tot_pkts 
      <<  "/Dropped="<< drp_pct << " %\n"
      << format_numbers((double)nPkts, buf1, sizeof(buf1), 0) << " pkts "
      << "- " << format_numbers((double)nBytes, buf2, sizeof(buf2), 0)
      << " bytes";

    if (print_all)
      std::cerr
	<< "["
	<< format_numbers((double)(nPkts*1000)/deltaMillisec, buf1,
			  sizeof(buf1), 1)
	<< " pkt/sec - " 
	<< format_numbers(thpt, buf2, sizeof(buf2), 1)
	<< " Mbit/sec]\n";
    else
      std::cerr << std::endl;
    
    if(print_all && (lastTime.tv_sec > 0)) {
      deltaMillisec = delta_time(&endTime, &lastTime);
      diff = nPkts-lastPkts;
      "Actual Stats: %llu pkts [%s ms][%s pkt/sec]\n";
      std::cerr
	<< "=========================\n"
	<< "Actual Stats: " << (long long unsigned int)diff << " pkts"
	<< " [" << format_numbers(deltaMillisec, buf1, sizeof(buf1), 1)
	<< " ms]"
	<< "[" << format_numbers(((double)diff/(double)(deltaMillisec/1000)),
				 buf2, sizeof(buf2), 1)
	<< " pkt/sec]\n";
    }
    lastPkts = nPkts;
  }

  lastTime.tv_sec = endTime.tv_sec, lastTime.tv_usec = endTime.tv_usec;
  std::cout << "=========================\n\n";
}


//---------------------------------------------------------------------------
void Net2Disk::dump_buf(const long thread_id, const u_char* buf) {
  int i;
  std::cout << thread_id << " ";
  for(i=0; i<32; i++)
    std::cout << buf[i];
  std::cout << "\n";
}

//---------------------------------------------------------------------------
void* Net2Disk::packet_consumer_thread(void* _id) {
  long thread_id = (long)_id;
  int fd = fds[thread_id];
  u_char* filebuf = bufs[thread_id];
  u_char* netbuf;
  u_int numCPU = sysconf( _SC_NPROCESSORS_ONLN );
  u_long core_id = thread_id % numCPU;
  struct pfring_pkthdr hdr;

  std::cout <<"packet_consumer_thread(" << thread_id << ")\n";

  if((NUM_THREADS > 1) && (numCPU > 1)) {
    if(bind2core(core_id) == 0)
      std::cout << "Set thread "<< thread_id << " on core "<< core_id << "/"
		<< numCPU << "\n";
  }

  memset(&hdr, 0, sizeof(hdr));

  while(1) {
    if (do_shutdown)
      break;

    int bytes_left = buffer_size;
    int bytes_read = 0;
    while (bytes_left > 0) {
      if (pfring_recv(pd, &netbuf, 0, &hdr, wait_for_packet) > 0) {
	numPkts[thread_id]++, numBytes[thread_id] += hdr.len;
#if 0
	dump_buf(thread_id, netbuf);
#endif
	if (do_shutdown)
	  break;

	int buffer_free = buffer_size - bytes_read;
	if (buffer_free >= SNAPLEN) {
	  /* Copy entire received packet. */
	  memcpy(filebuf + bytes_read, netbuf, SNAPLEN);
	  bytes_left -= SNAPLEN;
	  bytes_read += SNAPLEN;
	} else {
	  /* Pad out rest of buffer then write. */
	  memset(filebuf + bytes_read, 0, buffer_free);
	  writer_task(fd, filebuf, buffer_size);

	  /* Reset. */
	  bytes_left = buffer_size;
	  bytes_read = 0;
	}
      } else {
	if(wait_for_packet == 0)
	  sched_yield();
      }
    }
  }

  return(NULL);
}


//---------------------------------------------------------------------------
void Net2Disk::writer_task(int fd, u_char* buf, int buf_size) {
  // Write buffer to disk.
  int bytes_left = buf_size;
  int bytes_written = 0;

#if 0
  dump_buf((long)fd, buf);
#endif

  while (bytes_left) {
    int nb = write(fd, buf + bytes_written, buf_size);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      perror("Unable to write to disk.");
    }
  }
}

//---------------------------------------------------------------------------
void Net2Disk::setup() {
  pages_per_buffer = 256;
  page_size = getpagesize();
  buffer_size = pages_per_buffer * page_size;

  char FILE_PREFIX[] = "/mnt/disk";
  char *PATHS[NUM_THREADS];
  int PATH_LENGTH = 255;
  int i;

  for (i=0; i<NUM_THREADS; i++) {
    if (posix_memalign((void**)&bufs[i], page_size, buffer_size) < 0) {
      perror("Memalign failed.");
      exit(1);
    }
  }

  for (i=0; i<NUM_THREADS; i++) {
    PATHS[i] = (char*)malloc(PATH_LENGTH);
    snprintf(PATHS[i], PATH_LENGTH, "%s%d/test.m6", FILE_PREFIX, i);

    int fd = open(PATHS[i], O_WRONLY | O_CREAT | O_DIRECT, S_IRWXU);
    if (fd < 0) {
      std::cout <<"i==" << i << " fd=="<< fd <<"\n";
      perror("Unable to open file descriptor.");
      exit(1);
    } else {
      std::cout <<"fds[" << i << "]=" << fd << "\n";
      fds[i] = fd;
    }
  }

  std::cout <<"page_size: " << page_size << std::endl;
  std::cout <<"buffer_size: " << buffer_size << std::endl;
}

