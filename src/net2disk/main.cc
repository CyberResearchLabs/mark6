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

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>
#include <bitset>
#include <sstream>
#include <list>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>

// Namespaces.
namespace po = boost::program_options;

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACE("eth0");
const int DEFAULT_SNAPLEN(9000);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const string DEFAULT_LOG_CONFIG("net2disk-log.cfg");
const string DEFAULT_CAPTURE_FILE("/mnt/disk0/capture.m6");

// Other constants.
const int MAX_SNAPLEN(9100);
const int STATS_SLEEP(1);

//----------------------------------------------------------------------
// Global variables.
//----------------------------------------------------------------------
long long NUM_PACKETS = 0;
long long NUM_BYTES = 0;

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
write_to_disk(int fd, const u_char* buf, int buf_size) {
  // Write buffer to disk.
  int bytes_left = buf_size;
  int bytes_written = 0;

  while (bytes_left) {
    int nb = write(fd, (void*)(buf + bytes_written), buf_size);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Unable to write to disk: " << strerror(errno));
    }
  }
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

//----------------------------------------------------------------------
// Program entry point.
//----------------------------------------------------------------------
int
main (int argc, char* argv[])
{
  // Variables to store options.
  string log_config; 
  string config;
  string interface;
  int snaplen;
  bool promiscuous;
  int time;
  string capture_file;

  // Declare supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("v", "print version message")
    ("interface", po::value<string>(&interface)->default_value(DEFAULT_INTERFACE),
     "interface from which to capture data")
    ("snaplen", po::value<int>(&snaplen)->default_value(DEFAULT_SNAPLEN),
     "capture snap length")
    ("promiscuous", po::value<bool>(&promiscuous)->default_value(DEFAULT_PROMISCUOUS),
     "enable promiscuous mode")
    ("time", po::value<int>(&time)->default_value(DEFAULT_TIME),
     "capture interval")
    ("log_config", po::value<string>(&log_config)->default_value(DEFAULT_LOG_CONFIG),
     "log configuration file")
    ("capture_file", po::value<string>(&capture_file)->default_value(DEFAULT_CAPTURE_FILE),
     "capture file")
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
    << endl << endl
    << "interface:   " << interface << endl
    << "snaplen:     " << snaplen << endl
    << "promiscuous: " << promiscuous << endl
    << "time:        " << time << endl
    << "log_config:  " << log_config << endl;

  // Start processing.
  try {
    LOG4CXX_INFO(logger, "Creating PFR manager.");

    PFR ring(interface.c_str(), snaplen, promiscuous);
    if (ring.get_pcap()) {
      LOG4CXX_INFO(logger, "Successfully opened device: " << interface);
    } else {
      LOG4CXX_ERROR(logger, "Problems opening device: " << interface << " - "
		    << ring.get_last_error());
      return (-1);
    }

    if (false) {
      filtering_rule the_rule;
      int rc;
      u_int16_t rule_id = 99;

      ring.toggle_filtering_policy(false); /* Default to drop */
      memset(&the_rule, 0, sizeof(the_rule));

      the_rule.rule_id = rule_id;
      the_rule.rule_action = forward_packet_and_stop_rule_evaluation;
      the_rule.core_fields.proto = 1 /* icmp */;
      the_rule.plugin_action.plugin_id = 1; /* Dummy plugin */
      rc = ring.add_filtering_rule(&the_rule);

      LOG4CXX_INFO(logger, "Added filtering rule " << rule_id << " [rc=" << rc << "]\n");
    }

    // Setup buffers.
    u_char* file_buf;
    const int LOCAL_PAGES_PER_BUFFER = 256;
    const int LOCAL_PAGE_SIZE = getpagesize();
    const int BUFFER_SIZE = LOCAL_PAGES_PER_BUFFER * LOCAL_PAGE_SIZE;

    if (posix_memalign((void**)&file_buf, LOCAL_PAGE_SIZE, BUFFER_SIZE) < 0) {
      LOG4CXX_ERROR(logger, "Memalign failed: " << strerror(errno));
      exit(1);
    }

    int fd = open(capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT, S_IRWXU);
    if (fd < 0) {
      LOG4CXX_ERROR(logger, "Unable to open file: " << strerror(errno));
      exit(1);
    } else {
      LOG4CXX_INFO(logger, "Allocated fd:  " << fd);
    }

    LOG4CXX_INFO(logger, "LOCAL_PAGE_SIZE: " << LOCAL_PAGE_SIZE);
    LOG4CXX_INFO(logger, "BUFFER_SIZE:     " << BUFFER_SIZE);

    // Start statistics reporting.
    signal(SIGALRM, handle_sigalarm);
    alarm(STATS_SLEEP);

    // Capture packets.
    char stats[32];
    u_char pkt[MAX_SNAPLEN];
    while (true) {
      struct pfring_pkthdr hdr;
      struct simple_stats *the_stats = (struct simple_stats*)stats;

      int bytes_left = BUFFER_SIZE;
      int bytes_read = 0;

      while (bytes_left > 0) {
	// Get next packet.
	if (ring.get_next_packet(&hdr, pkt, snaplen) > 0) {
	  // LOG4CXX_DEBUG(logger, "Got " << hdr.len << " byte packet");
	} else {
	  LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
			<< strerror(errno));
	}

	// Update stats.
	NUM_PACKETS++;
	NUM_BYTES += hdr.len;

	// Accumulate or flush to data to disk.
	int buffer_free = BUFFER_SIZE - bytes_read;
	if (buffer_free >= snaplen) {
	  /* Copy entire received packet. */
	  memcpy(file_buf + bytes_read, pkt, snaplen);
	  bytes_left -= snaplen;
	  bytes_read += snaplen;
	} else {
	  /* Pad out rest of buffer then write. */
	  memset(file_buf + bytes_read, 0, buffer_free);
	  write_to_disk(fd, file_buf, BUFFER_SIZE);

	  /* Reset. */
	  bytes_left = BUFFER_SIZE;
	  bytes_read = 0;
	} // if.
      } // while.
    }
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}


