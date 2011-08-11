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

// C includes.
#include <unistd.h>
#include <fcntl.h>
#include <sys/select.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <signal.h>
#include <sched.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>
#include <bitset>
#include <sstream>
#include <list>
#include <deque>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread.hpp>
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
#include <buffer_pool.h>
#include <file_writer.h>

// Namespaces.
namespace po = boost::program_options;

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8234);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const string DEFAULT_LOG_CONFIG("net2disk-log.cfg");
const int DEFAULT_PAYLOAD_LENGTH(8192);
const int DEFAULT_SMP_AFFINITY(0);
const int DEFAULT_RING_BUFFERS(128);
const int DEFAULT_WRITE_BLOCKS(32);

// Other constants.
const int MAX_SNAPLEN(9014);
const int STATS_SLEEP(1);
const int PAYLOAD_LENGTH(DEFAULT_PAYLOAD_LENGTH);

//----------------------------------------------------------------------
// Global variables.
//----------------------------------------------------------------------
long long NUM_PACKETS(0);
long long NUM_BYTES(0);

const int LOCAL_PAGES_PER_BUFFER(256);
const int RING_BUFFER_TIMEOUT(10);

int LOCAL_PAGE_SIZE(0);
int BUFFER_SIZE(0);
std::vector<FileWriter*> FILE_WRITERS;


// Used by to control threads.
bool RUNNING(true);

const int NUM_THREADS(4);

//----------------------------------------------------------------------
// Utility functions.
//----------------------------------------------------------------------
// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  cout
    << "net2disk [options]" << endl
    << desc;
}

void
print_stats() {
  static struct timeval start_time, last_time;
  struct timeval end_time;
  struct timeval diff, instant_diff;
  static double average_packet_rate = 0;
  static double average_byte_rate = 0;
  static long long last_num_packets = 0;
  static long long last_num_bytes = 0;
  static long interval = 0;
  const double ALPHA = 0.5;
  const double BPS_TO_MBPS = 8/1e6;
  const int PAGE_LENGTH = 10;

  // Update time variables.
  if (start_time.tv_sec == 0) {
    gettimeofday(&start_time, NULL);
  }
  gettimeofday(&end_time, NULL);

  // Elapsed interval.
  timersub(&end_time, &start_time, &diff);
  timersub(&end_time, &last_time, &instant_diff);
  const double delta_seconds = diff.tv_sec + diff.tv_usec/1000000.0;
  const double instant_delta_seconds = instant_diff.tv_sec
    + instant_diff.tv_usec/1000000.0;
  

  const double instant_packet_rate = (NUM_PACKETS - last_num_packets)/instant_delta_seconds;
  const double instant_byte_rate = BPS_TO_MBPS*(NUM_BYTES - last_num_bytes)/instant_delta_seconds;

  const double lifetime_packet_rate = NUM_PACKETS/delta_seconds;
  const double lifetime_byte_rate = BPS_TO_MBPS*NUM_BYTES/delta_seconds;

  average_packet_rate = (1.0-ALPHA)*average_packet_rate + ALPHA*instant_packet_rate;
  average_byte_rate = (1.0-ALPHA)*average_byte_rate + ALPHA*instant_byte_rate;

  if (interval % PAGE_LENGTH == 0) {
    std::cout
      << "+------------------------------------------------------------------------------+" << endl
      << "|"
      << setw(10) << "time" << " "
      << setw(10) << "pkt rate" << " "
      << setw(10) << "pkt rate"   << " "
      << setw(10) << "pkt rate"  << " "
      << "|"
      << setw(10) << "mbps" << " "
      << setw(10) << "mbps"   << " "
      << setw(10) << "mbps"  << " "
      << "|\n"
      << "|"
      << setw(10) << "(s)" << " "
      << setw(10) << "(inst)" << " "
      << setw(10) << "(lifetime)"   << " "
      << setw(10) << "(average)"  << " "
      << "|"
      << setw(10) << "(inst)" << " "
      << setw(10) << "(lifetime)"   << " "
      << setw(10) << "(average)"  << " "
      << "|\n"
      << "+------------------------------------------------------------------------------+"
      << endl;
  }
  ++interval;

  std::cout
    << "|"
    << setw(10) << delta_seconds << " "
    << setw(10) << instant_packet_rate << " "
    << setw(10) << lifetime_packet_rate << " "
    << setw(10) << average_packet_rate << " "
    << "|"
    << setw(10) << instant_byte_rate << " "
    << setw(10) << lifetime_byte_rate << " "
    << setw(10) << average_byte_rate << " "
    << "|\n";
  
  // Update state.
  gettimeofday(&last_time, NULL);
  last_num_packets = NUM_PACKETS;
  last_num_bytes = NUM_BYTES;
}

void
handle_sigalarm(int sig) {
  print_stats();
  alarm(STATS_SLEEP);
  signal(SIGALRM, handle_sigalarm);
}

void
sigproc(int sig) {
  static int called = 0;

  if (called)
    return;
  else called = 1;

  RUNNING = false;
}

void
net2mem(const int id, const string interface, const int snaplen,
	const bool promiscuous, const int buffer_size, FileWriter* fw) {
  LOG4CXX_INFO(logger, "Starting net2mem thread " << id);
  LOG4CXX_INFO(logger, "  interface: " << interface);
  LOG4CXX_INFO(logger, "  snaplen:   " << snaplen);

  PFR ring(interface.c_str(), snaplen, promiscuous);
  if (ring.get_pcap()) {
    LOG4CXX_INFO(logger, "Successfully opened device: " << interface);
  } else {
    LOG4CXX_ERROR(logger, "Problems opening device: " << interface << " - "
		  << ring.get_last_error());
    return;
  }

  BufferPool* bp(BufferPool::instance());
  
  // Capture packets.
  char stats[32];
  // #define NETONLY
  boost::uint8_t net_buf[MAX_SNAPLEN];

#ifdef NETONLY
  boost::uint8_t* file_buf = bp->malloc();
#endif

  while (RUNNING) {
    struct pfring_pkthdr hdr;
    struct simple_stats *the_stats = (struct simple_stats*)stats;
    
    int bytes_left = buffer_size;
    int bytes_read = 0;

#ifndef NETONLY
    boost::uint8_t* file_buf = bp->malloc();
#endif

    int payload_length;
    boost::uint8_t* payload_ptr;

#ifndef NETONLY
    while (bytes_left > 0) {
      // Get next packet.
#endif
      if (ring.get_next_packet(&hdr, net_buf, snaplen) > 0) {
	// Successful read.

	// Extract offsets etc. from pfring structures.
	struct pfring_extended_pkthdr& pep(hdr.extended_hdr);
	struct pkt_parsing_info& ppi(pep.parsed_pkt);
	struct pkt_offset po(ppi.offset);
	const boost::uint16_t eth_offset(po.eth_offset);
	const boost::uint16_t l3_offset(po.l3_offset);
	const boost::uint16_t l4_offset(po.l4_offset);
	const boost::uint16_t payload_offset(po.payload_offset);

	// Get payload information.
	payload_ptr = &net_buf[payload_offset];
	payload_length = hdr.caplen - payload_offset;

	// Check for validity of capture.
	if (hdr.caplen != snaplen) {
	  LOG4CXX_ERROR(logger, "Short capture(caplen/snaplen): " << hdr.caplen << "/" << snaplen);
	  continue;
	}

	if (hdr.caplen != hdr.len) {
	  LOG4CXX_ERROR(logger, "Short capture(caplen/snaplen): " << hdr.caplen << "/" << hdr.len);
	  continue;
	}

	if (payload_length != PAYLOAD_LENGTH) {
	  LOG4CXX_ERROR(logger, "Short capture (caplen/PAYLOAD_LENGTH): " << hdr.caplen << "/" << PAYLOAD_LENGTH);
	  continue;
	}

	// #define DUMP
#ifdef DUMP
	cout << "Packet dump.\n";
	cout << "caplen:         " << hdr.caplen << endl;
	cout << "len:            " << hdr.len << endl;
	cout << "parsed_hdr_len: " << pep.parsed_header_len << endl;
	cout << "eth_offset:     " << eth_offset << endl;
	cout << "l3_offset:      " << l3_offset << endl;
	cout << "l4_offset:      " << l4_offset << endl;
	cout << "payload_offset: " << payload_offset << endl;
	cout << "payload_length: " << payload_length << endl;

	const int dumplen(128);
	for (int i=0; i<dumplen; i++) {
	  printf("%02x ", (unsigned char)net_buf[i]);
	  if ((i+1)%8 == 0)
	    printf(" ");
	  if ((i+1)%16 == 0)
	    printf("\n");
	}
	printf("\n");
#endif // DUMP	

	// Update stats.
	NUM_PACKETS++;
	NUM_BYTES += hdr.caplen;
      } else {
	LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
		      << strerror(errno));
	continue;
      }

#ifndef NETONLY
      // Accumulate or flush data to disk.
      if (bytes_left < PAYLOAD_LENGTH) {
	// Pad out rest of buffer then write.
	memset(&file_buf[bytes_read], 0, bytes_left);
	fw->write(file_buf);
	break;
      } else {
	// Copy captured payload to file buffer.
	memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
      }

      // Update state variables.
      bytes_read += hdr.caplen;
      bytes_left -= hdr.caplen;

    } 
#endif
  } 
  cout << "Exiting net2mem..." << endl;
}

//----------------------------------------------------------------------
// Program entry point.
//----------------------------------------------------------------------
int
main (int argc, char* argv[]) {
  // Variables to store options.
  string log_config; 
  string config;
  int snaplen;
  bool promiscuous;
  int time;
  vector<string> interfaces;
  vector<string> capture_files;
  int smp_affinity;
  int ring_buffers;
  int write_blocks;

  // Declare supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("v", "print version message")
    ("snaplen", po::value<int>(&snaplen)->default_value(DEFAULT_SNAPLEN),
     "capture snap length")
    ("promiscuous", po::value<bool>(&promiscuous)->default_value(DEFAULT_PROMISCUOUS),
     "enable promiscuous mode")
    ("time", po::value<int>(&time)->default_value(DEFAULT_TIME),
     "capture interval")
    ("log_config", po::value<string>(&log_config)->default_value(DEFAULT_LOG_CONFIG),
     "log configuration file")
    ("interfaces", po::value< vector<string> >(&interfaces)->multitoken(),
     "list of interfaces from which to capture data")
    ("capture_files", po::value< vector<string> >(&capture_files)->multitoken(),
     "list of capture files")
    ("smp_affinity", po::value<int>(&smp_affinity)->default_value(DEFAULT_SMP_AFFINITY),
     "smp processor affinity")
    ("ring_buffers", po::value<int>(&ring_buffers)->default_value(DEFAULT_RING_BUFFERS),
     "total number of ring buffers")
    ("write_blocks", po::value<int>(&write_blocks)->default_value(DEFAULT_WRITE_BLOCKS),
     "per thread number of write blocks")
    ;

  // Parse options.
  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  // Configure log subsystem.
  init_logger(log_config);

  // Check various options.
  if (vm.count("help")) {
    usage(desc);
    return 1;
  }

  if (vm.count("v")) {
    cout << "net2disk version: 0.1"
         << endl;
    return 1;
  }

  const int NUM_INTERFACES = interfaces.size();
  if (capture_files.size() != NUM_INTERFACES) {
    LOG4CXX_ERROR(logger, "Arg length mismatch, capture_files");
    usage(desc);
    return 1;
  }

  cout
    << "+------------------------------------------------------------------------------+\n"
    << "|                                                                              |\n"
    << "|                              Net2disk v0.1                                   |\n"
    << "|                                                                              |\n"
    << "|                                                                              |\n"
    << "|                  Copyright 2011 MIT Haystack Observatory                     |\n"
    << "|                            del@haystack.mit.edu                              |\n"
    << "|                                                                              |\n"
    << "+------------------------------------------------------------------------------+\n"
    << endl << endl;

  cout << setw(20) << left << "interfaces:";
  BOOST_FOREACH(string s, interfaces)
    cout << s << " ";
  cout << endl;

  cout << setw(20) << left << "capture_files:";
  BOOST_FOREACH(string s, capture_files)
    cout << s << " ";
  cout << endl;

  cout
    << setw(20) << left << "snaplen:" << snaplen << endl
    << setw(20) << left << "promiscuous:" << promiscuous << endl
    << setw(20) << left << "time:" << time << endl
    << setw(20) << left << "log_config:" << log_config << endl
    << setw(20) << left << "smp_affinity:" << smp_affinity << endl
    << setw(20) << left << "ring_buffers:" << ring_buffers << endl
    << setw(20) << left << "write_blocks:" << write_blocks << endl;










  // Start processing.
  try {
    LOG4CXX_INFO(logger, "Creating PFR manager.");

    // Set processor affinity.
    pid_t pid;
    const unsigned int cpu_setsize (sizeof(cpu_set_t));
    cpu_set_t mask;

    pid = 0;
    int ret = sched_getaffinity(pid, cpu_setsize, &mask);
    if (ret < 0)
      LOG4CXX_ERROR(logger, "Unable to get process affinity.");

    CPU_ZERO(&mask);
    CPU_SET(smp_affinity, &mask);
    ret = sched_setaffinity(pid, cpu_setsize, &mask);
    if (ret < 0)
      LOG4CXX_ERROR(logger, "Unble to set process affinity.");

    ret = sched_getaffinity(pid, cpu_setsize, &mask);
    if (ret < 0)
      LOG4CXX_ERROR(logger, "Unable to get process affinity.");
    
    // Setup buffer pool.
    BufferPool* bp = BufferPool::instance();
    bp->reserve_pool(ring_buffers, LOCAL_PAGES_PER_BUFFER);
    const int BUFFER_SIZE(getpagesize()*LOCAL_PAGES_PER_BUFFER);

    // Start statistics reporting.
    signal(SIGALRM, handle_sigalarm);
    alarm(STATS_SLEEP);

    // Setup shutdown handler.
    signal(SIGINT, sigproc);

    // Startup mem2disk threads.
    for (int i=0; i<NUM_INTERFACES; i++) {
      const string capture_file(capture_files[i]);
      const int POLL_TIMEOUT(1);
      const int COMMAND_INTERVAL(1);
      FileWriter* fw = new FileWriter(BUFFER_SIZE,
				      write_blocks,
				      POLL_TIMEOUT,
				      COMMAND_INTERVAL);
      fw->open(capture_file);
      fw->start();
      fw->cmd_write_to_disk();
      FILE_WRITERS.push_back(fw);
    }

    // Startup net2mem_threads.
    boost::ptr_list<boost::thread> net2mem_threads;
    for (int i=0; i<NUM_INTERFACES; i++) {
      net2mem_threads.push_front(new boost::thread(net2mem, i, interfaces[i], snaplen,
						   promiscuous, BUFFER_SIZE,
						   FILE_WRITERS[i]));
    }
    
    // Join threads.
    BOOST_FOREACH(boost::thread& t, net2mem_threads)
      t.join();
    LOG4CXX_INFO(logger, "Joined net2mem threads...");

    BOOST_FOREACH(FileWriter *fw, FILE_WRITERS) {
      fw->cmd_stop();
      fw->join();
    }
    
    LOG4CXX_INFO(logger, "Joined mem2disk threads...");
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}

