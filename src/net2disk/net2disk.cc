
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

// Framework includes.
#include <pfring.h>

// Local includes.
#include <pfring_util.h>
#include <net2disk.h>


Net2Disk::Net2Disk(const int alarm_sleep, const int default_snaplen,
		   const int num_threads, const std::string& device,
		   const int num_files):
  ALARM_SLEEP(alarm_sleep),
  DEFAULT_SNAPLEN(default_snaplen),
  NUM_THREADS(num_threads),
  DEVICE(device),
  NUM_FILES(num_files),
  pages_per_buffer(0),
  page_size(0),
  buffer_size(0),
  bufs(0),
  fds(0),
  pd(0),
  num_threads(0),
  pfrintStats(),
  statsLock(),
  startTime(),
  numPkts(0),
  numBytes(0),
  wait_for_packet(1),
  do_shutdown(0),
  verbose(0)
{
  int i=0;

  bufs = new u_char*[NUM_FILES];
  for (i = 0; i<NUM_FILES; i++)
    bufs[i] = 0;

  fds = new int[NUM_FILES];
  for (i = 0; i<NUM_FILES; i++)
    fds[i] = 0;

  numPkts = new unsigned long long[NUM_THREADS];
  for (i = 0; i<NUM_THREADS; i++)
    numPkts[i] = 0;

  for (i = 0; i<NUM_THREADS; i++)
    numBytes = new unsigned long long[NUM_THREADS];
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

    for(i=0; i < num_threads; i++) {
      nBytes += numBytes[i];
      nPkts += numPkts[i];
    }

    thpt = ((double)8*nBytes)/(deltaMillisec*1000);

    fprintf(stderr, "=========================\n"
	    "Absolute Stats: [%u pkts rcvd][%u pkts dropped]\n"
	    "Total Pkts=%u/Dropped=%.1f %%\n",
	    (unsigned int)pfringStat.recv, (unsigned int)pfringStat.drop,
	    (unsigned int)(pfringStat.recv+pfringStat.drop),
	    pfringStat.recv == 0 ? 0 :
	    (double)(pfringStat.drop*100)/(double)(pfringStat.recv+pfringStat.drop));
    fprintf(stderr, "%s pkts - %s bytes", 
	    format_numbers((double)nPkts, buf1, sizeof(buf1), 0),
	    format_numbers((double)nBytes, buf2, sizeof(buf2), 0));

    if(print_all)
      fprintf(stderr, " [%s pkt/sec - %s Mbit/sec]\n",
	      format_numbers((double)(nPkts*1000)/deltaMillisec, buf1, sizeof(buf1), 1),
	      format_numbers(thpt, buf2, sizeof(buf2), 1));
    else
      fprintf(stderr, "\n");

    if(print_all && (lastTime.tv_sec > 0)) {
      deltaMillisec = delta_time(&endTime, &lastTime);
      diff = nPkts-lastPkts;
      fprintf(stderr, "=========================\n"
	      "Actual Stats: %llu pkts [%s ms][%s pkt/sec]\n",
	      (long long unsigned int)diff,
	      format_numbers(deltaMillisec, buf1, sizeof(buf1), 1),
	      format_numbers(((double)diff/(double)(deltaMillisec/1000)),  buf2, sizeof(buf2), 1));
    }

    lastPkts = nPkts;
  }

  lastTime.tv_sec = endTime.tv_sec, lastTime.tv_usec = endTime.tv_usec;

  fprintf(stderr, "=========================\n\n");
}

//---------------------------------------------------------------------------
void Net2Disk::sigproc(int sig) {
  static int called = 0;

  fprintf(stderr, "Leaving...\n");
  if (called)
    return;
  else
    called = 1;
  do_shutdown = 1;
  print_stats();

  if(num_threads == 1)
    pfring_close(pd);

  exit(0);
}

//---------------------------------------------------------------------------
void my_sigalarm(int sig) {
  print_stats();
  alarm(ALARM_SLEEP);
  signal(SIGALRM, my_sigalarm);
}

//---------------------------------------------------------------------------
void Net2Disk::dump_buf(const long thread_id, const u_char* buf) {
  int i;
  printf("%ld ", thread_id);
  for(i=0; i<32; i++)
    printf("%02X ", buf[i]);
  printf("\n");
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

  printf("packet_consumer_thread(%lu)\n", thread_id);

  if((num_threads > 1) && (numCPU > 1)) {
    if(bind2core(core_id) == 0)
      printf("Set thread %lu on core %lu/%u\n", thread_id, core_id, numCPU);
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
	if (buffer_free >= DEFAULT_SNAPLEN) {
	  /* Copy entire received packet. */
	  memcpy(filebuf + bytes_read, netbuf, DEFAULT_SNAPLEN);
	  bytes_left -= DEFAULT_SNAPLEN;
	  bytes_read += DEFAULT_SNAPLEN;
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
  char *PATHS[NUM_FILES];
  int PATH_LENGTH = 255;
  int i;

  for (i=0; i<NUM_FILES; i++) {
    if (posix_memalign((void**)&bufs[i], page_size, buffer_size) < 0) {
      perror("Memalign failed.");
      exit(1);
    }
  }

  for (i=0; i<NUM_FILES; i++) {
    PATHS[i] = (char*)malloc(PATH_LENGTH);
    snprintf(PATHS[i], PATH_LENGTH, "%s%d/test.m6", FILE_PREFIX, i);

    int fd = open(PATHS[i], O_WRONLY | O_CREAT | O_DIRECT, S_IRWXU);
    if (fd < 0) {
      printf("i==%d fd==%d\n", i, fd);
      perror("Unable to open file descriptor.");
      exit(1);
    } else {
      printf("fds[%d]=%d\n", i, fd);
      fds[i] = fd;
    }
  }

  printf("page_size: %d", page_size);
  printf("buffer_size: %d", buffer_size);
}

