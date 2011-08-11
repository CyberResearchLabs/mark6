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
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>
#include <sched.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>

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
#include <boost/algorithm/string.hpp> 

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
#include <buffer_pool.h>
#include <file_writer.h>
#include <net_reader.h>

// Namespaces.
namespace po = boost::program_options;

//----------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------
void banner();
void main_cli(const vector<pid_t>& child_pids, const vector<int>& child_fds);
void child_cli(const int parent_fd);

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
std::vector<NetReader*> NET_READERS;


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
      << HLINE
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
      << HLINE
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
  else
    called = 1;

  RUNNING = false;

  // Join threads.
  BOOST_FOREACH(NetReader* nr, NET_READERS)
    nr->cmd_stop();
  
  BOOST_FOREACH(FileWriter *fw, FILE_WRITERS)
    fw->cmd_stop();
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
  vector<int> smp_affinities;
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
    ("smp_affinities", po::value< vector<int> >(&smp_affinities)->multitoken(),
     "smp processor affinities")
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

  banner();

  cout << setw(20) << left << "interfaces:";
  BOOST_FOREACH(string s, interfaces)
    cout << s << " ";
  cout << endl;

  cout << setw(20) << left << "capture_files:";
  BOOST_FOREACH(string s, capture_files)
    cout << s << " ";
  cout << endl;

  cout << setw(20) << left << "smp_affinities:";
  BOOST_FOREACH(int s, smp_affinities)
    cout << s << " ";
  cout << endl;

  cout
    << setw(20) << left << "snaplen:" << snaplen << endl
    << setw(20) << left << "promiscuous:" << promiscuous << endl
    << setw(20) << left << "time:" << time << endl
    << setw(20) << left << "log_config:" << log_config << endl
    << setw(20) << left << "ring_buffers:" << ring_buffers << endl
    << setw(20) << left << "write_blocks:" << write_blocks << endl;

  // Start processing.
  vector<pid_t> child_pids;
  vector<int> child_fds;
  try {
    LOG4CXX_INFO(logger, "Creating PFR manager.");

    const int COMMAND_INTERVAL(1);
    const int POLL_TIMEOUT(1);

    pid_t pid;
    for (int i=0; i<NUM_INTERFACES; i++) {
      int fd[2];
      if (pipe(fd) < 0)
	LOG4CXX_ERROR(logger, "Pipe error");

      if ( (pid = fork()) < 0) {
	LOG4CXX_ERROR(logger, "Unable to fork. Exiting.");
      } else if (pid == 0) {
	// Child. Do stuff then exit.

	// Clean up pipe for receiving commands from parent. fd[0] will be read fd.
	close(fd[1]);

	// Setup shutdown handler.
	signal(SIGINT, sigproc);

	// Set SMP affinity.
	const unsigned int cpu_setsize (sizeof(cpu_set_t));
	cpu_set_t mask;
	const pid_t mypid(0);
	CPU_ZERO(&mask);
	CPU_SET(smp_affinities[i], &mask);
	if (sched_setaffinity(mypid, cpu_setsize, &mask) < 0)
	  LOG4CXX_ERROR(logger, "Unble to set process affinity.");

	// Setup buffer pool.
	BufferPool* bp = BufferPool::instance();
	bp->reserve_pool(ring_buffers, LOCAL_PAGES_PER_BUFFER);
	const int BUFFER_SIZE(getpagesize()*LOCAL_PAGES_PER_BUFFER);

	// Startup FileWriter threads.
	FileWriter* fw = new FileWriter(BUFFER_SIZE,
					write_blocks,
					capture_files[i],
					POLL_TIMEOUT,
					COMMAND_INTERVAL);
	FILE_WRITERS.push_back(fw);

	// Startup NetReader threads.
	NetReader* nr = new NetReader(i,
				      interfaces[i],
				      snaplen,
				      PAYLOAD_LENGTH,
				      BUFFER_SIZE,
				      promiscuous,
				      FILE_WRITERS[i],
				      COMMAND_INTERVAL);
	NET_READERS.push_back(nr);

	// Wait for threads to finish.
	child_cli(fd[0]);
	break;
      } else {
	// Parent.

	// Clean up pipe for communicating with child. fd[1] will be write fd.
	close(fd[0]);
	child_fds.push_back(fd[0]);
      }
    }

    // TODO: think about multi-thread data structures.. Useful or not.
    // TODO: separate log files.
    // TODO clean up.
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  main_cli(child_pids, child_fds);

  return 0;
}

void main_cli(const vector<pid_t>& child_pids, const vector<int>& child_fds) {
  // Command line interpreter.
  while (true) {
    string line_read;
    cout << "mark6>";
    cin >> line_read;
    trim(line_read);
    if (line_read.size() == 0)
      continue;

    vector<string> results;
    string cmd;
    split(results, line_read, is_any_of(" \t"));
    if (results.size() > 0) {
      cmd = results[0];

      if (cmd == "quit") {
	exit(0);
      } else if (cmd == "help") {
	cout
	  << endl
	  << "This is the online help system." << endl
	  << "The following commands are available:" << endl
	  << setw(20) << " " << "start" << endl
	  << setw(20) << " " << "stop" << endl
	  << "Type 'help <command>' to see a description of that command." 
	  << endl
	  << endl;
      } else if (cmd == "start") {
	cout << "Received start()";
	// Start statistics reporting.
	signal(SIGALRM, handle_sigalarm);
	alarm(STATS_SLEEP);

	BOOST_FOREACH(int fd, child_fds)
	  write(fd, "start\n", 6);

      } else if (cmd == "stop") {
	cout << "Received stop()";
	BOOST_FOREACH(int fd, child_fds)
	  write(fd, "stop\n", 5);

	BOOST_FOREACH(pid_t p, child_pids) {
	  waitpid(p, NULL, 0);
	  cout << "PID: " << (int)p << " terminated..." << endl;
	}
      } else {
	cout
	  << endl
	  << "Unknown command. Please try again or type 'help'"
	  << endl
	  << endl;
      }
    }
  }
}

void child_cli(int parent_fd) {
  FILE* parent_file = fdopen(parent_fd, "r");
  if (parent_file == NULL) {
    LOG4CXX_ERROR(logger, "Unable to create file stream.");
    exit(1);
  }
    
  int bytes_read;
  size_t nbytes(256);
  char* line_read;
  while (true) {
    line_read = (char*)malloc(nbytes+1);
    bytes_read = getline(&line_read, &nbytes, parent_file);
    if (bytes_read < 0) {
      LOG4CXX_ERROR(logger, "Invalid read.");
      free(line_read);
    } else {
      string s(line_read);
      free (line_read);
      vector<string> results;
      string cmd;
      split(results, s, is_any_of(" \t"));
      if (results.size() == 0)
	continue;
      
      cmd = results[0];
      if (cmd == "start") {
	cout << "Starting\n";
	// BOOST_FOREACH(FileWriter* fw, FILE_WRITERS) {
	// fw->open(capture_files[i]);
	// fw->start();
	// fw->cmd_write_to_disk();
	// }
	// BOOST_FOREACH(NetReader* nr, NET_READERS) {
	// nr->start();
	// nr->cmd_read_from_network();
	// }
      } else if (cmd == "stop") {
	cout << "Stopping\n";
	// BOOST_FOREACH(FileWriter* fw, FILE_WRITERS)
	// fw->cmd_stop();
	// BOOST_FOREACH(NetReader* nr, NET_READERS)
	// fw->cmd_stop();
      }
    }
  }

  // Join threads.
  // BOOST_FOREACH(NetReader* nr, NET_READERS)
  //nr->join();
  //LOG4CXX_INFO(logger, "Joined NetReader threads...");
	
  //BOOST_FOREACH(FileWriter *fw, FILE_WRITERS)
  // fw->join();
  // LOG4CXX_INFO(logger, "Joined FileWriter threads...");
}


void banner() {
  cout
    << HLINE
    << endl
    << "|                                                                              |\n"
    << "|                              Net2disk v0.1                                   |\n"
    << "|                                                                              |\n"
    << "|                                                                              |\n"
    << "|                  Copyright 2011 MIT Haystack Observatory                     |\n"
    << "|                            del@haystack.mit.edu                              |\n"
    << "|                                                                              |\n"
    << HLINE
    << endl
    << endl;
}
