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
#include <fcntl.h>
#include <sys/select.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>
#include <sched.h>
#include <unistd.h>
// #include <readline/readline.h>
// #include <readline/history.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
#include <buffer_pool.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

// Namespaces.
namespace po = boost::program_options;

//----------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------
void banner();
void main_cli(const std::vector<pid_t>& child_pids,
	      const std::vector<int>& child_fds);
void child_cli(const int parent_fd);

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const std::string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8234);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const std::string DEFAULT_LOG_CONFIG("net2disk-log.cfg");
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
const int LOCAL_PAGES_PER_BUFFER(256);
const int RING_BUFFER_TIMEOUT(10);

int LOCAL_PAGE_SIZE(0);
int BUFFER_SIZE(0);

FileWriter* FILE_WRITER(0);
NetReader* NET_READER(0);
StatsWriter* FILE_WRITER_STATS(0);
StatsWriter* NET_READER_STATS(0);

//----------------------------------------------------------------------
// Utility functions.
//----------------------------------------------------------------------
// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  std::cout
    << "net2disk [options]" << std::endl
    << desc;
}

void
sigproc(int sig) {
  static int called = 0;

  if (called)
    return;
  else
    called = 1;

  // Join threads.
  NET_READER->cmd_stop();
  NET_READER_STATS->cmd_stop();

  FILE_WRITER->cmd_stop();
  FILE_WRITER_STATS->cmd_stop();
}

void just_work() {
}

void make_things_easier_for_me() {
}

void get_me_out_of_here() {
}

//----------------------------------------------------------------------
// Program entry point.
//----------------------------------------------------------------------
int
main (int argc, char* argv[]) {
  // Variables to store options.
  std::string log_config; 
  std::string config;
  int snaplen;
  bool promiscuous;
  int time;
  std::vector<std::string> interfaces;
  std::vector<std::string> capture_files;
  std::vector<int> smp_affinities;
  int ring_buffers;
  int write_blocks;

  // Declare supported options, defaults, and variable bindings.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")

    ("v", "print version message")

    ("snaplen",
     po::value<int>(&snaplen)->default_value(DEFAULT_SNAPLEN),
     "capture snap length")

    ("promiscuous",
     po::value<bool>(&promiscuous)->default_value(DEFAULT_PROMISCUOUS),
     "enable promiscuous mode")

    ("time",
     po::value<int>(&time)->default_value(DEFAULT_TIME),
     "capture interval")

    ("log_config",
     po::value<std::string>(&log_config)->default_value(DEFAULT_LOG_CONFIG),
     "log configuration file")

    ("interfaces",
     po::value< std::vector<std::string> >(&interfaces)->multitoken(),
     "list of interfaces from which to capture data")

    ("capture_files",
     po::value< std::vector<std::string> >(&capture_files)->multitoken(),
     "list of capture files")

    ("smp_affinities",
     po::value< std::vector<int> >(&smp_affinities)->multitoken(),
     "smp processor affinities")

    ("ring_buffers",
     po::value<int>(&ring_buffers)->default_value(DEFAULT_RING_BUFFERS),
     "total number of ring buffers")

    ("write_blocks",
     po::value<int>(&write_blocks)->default_value(DEFAULT_WRITE_BLOCKS),
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
    std::cout << "net2disk version: 0.1"
         << std::endl;
    return 1;
  }

  const int NUM_INTERFACES = interfaces.size();
  if (capture_files.size() != NUM_INTERFACES) {
    LOG4CXX_ERROR(logger, "Arg length mismatch, capture_files");
    usage(desc);
    return 1;
  }

  banner();

  std::cout << std::setw(20) << std::left << "interfaces:";
  BOOST_FOREACH(std::string s, interfaces)
    std::cout << s << " ";
  std::cout << std::endl;

  std::cout << std::setw(20) << std::left << "capture_files:";
  BOOST_FOREACH(std::string s, capture_files)
    std::cout << s << " ";
  std::cout << std::endl;

  std::cout << std::setw(20) << std::left << "smp_affinities:";
  BOOST_FOREACH(int s, smp_affinities)
    std::cout << s << " ";
  std::cout << std::endl;

  std::cout
    << std::setw(20) << std::left << "snaplen:" << snaplen << std::endl
    << std::setw(20) << std::left << "promiscuous:" << promiscuous << std::endl
    << std::setw(20) << std::left << "time:" << time << std::endl
    << std::setw(20) << std::left << "log_config:" << log_config << std::endl
    << std::setw(20) << std::left << "ring_buffers:" << ring_buffers << std::endl
    << std::setw(20) << std::left << "write_blocks:" << write_blocks << std::endl;

  // Start processing.
  std::vector<pid_t> child_pids;
  std::vector<int> child_fds;
  try {
    LOG4CXX_DEBUG(logger, "Starting.");

    const int COMMAND_INTERVAL(1);
    const int STATS_INTERVAL(1);
    const int POLL_TIMEOUT(1);

    pid_t pid;
    for (int i=0; i<NUM_INTERFACES; i++) {
      int fd[2];
      if (pipe(fd) < 0) {
	LOG4CXX_ERROR(logger, "Pipe error");
	exit(1);
      }

      if ( (pid = fork()) < 0) {
	LOG4CXX_ERROR(logger, "Unable to fork. Exiting.");
	exit(1);
      } else if (pid == 0) {
	// Child. Do stuff then exit.
	LOG4CXX_DEBUG(logger, "Forked child: " << i);

	// Clean pipe for receiving commands from parent. fd[0] will be
	// read fd.
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

	// Create FileWriter threads.
	FILE_WRITER_STATS = new StatsWriter(i,
					    std::string("fw_") + interfaces[i],
					    STATS_INTERVAL,
					    COMMAND_INTERVAL);
	FILE_WRITER = new FileWriter(i,
				     BUFFER_SIZE,
				     write_blocks,
				     capture_files[i],
				     POLL_TIMEOUT,
				     (StatsWriter* const)FILE_WRITER_STATS,
				     COMMAND_INTERVAL);
	FileWriter * const FW(FILE_WRITER);

	// Create NetReader threads.
	NET_READER_STATS = new StatsWriter(i+1, // TODO: fix index.
					   std::string("nr_") + interfaces[i],
					   STATS_INTERVAL,
					   COMMAND_INTERVAL);
	NET_READER = new NetReader(i,
				   interfaces[i],
				   snaplen,
				   PAYLOAD_LENGTH,
				   BUFFER_SIZE,
				   promiscuous,
				   (FileWriter* const)FILE_WRITER,
				   (StatsWriter* const)NET_READER_STATS,
				   COMMAND_INTERVAL);

	// Wait for threads to finish.
	child_cli(fd[0]);
	break;
      } else {
	// Parent.
	LOG4CXX_DEBUG(logger, "Parent still here after fork.");

	// Clean up pipe for communicating with child. fd[1] will be write fd.
	close(fd[0]);
	child_fds.push_back(fd[1]);
      }
    }
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
  }

  main_cli(child_pids, child_fds);

  return 0;
}

void main_cli(const std::vector<pid_t>& child_pids,
	      const std::vector<int>& child_fds) {
  // Command line interpreter.
  const int nbytes(256);
  char line[nbytes];

  while (true) {
    std::cout << "mark6>";
    if (!std::cin.good()) {
      LOG4CXX_ERROR(logger, "CLI input stream closed. Exiting...");
      break;
    }

    std::cin.getline(line, nbytes);
    std::string line_read(line);
    trim(line_read);
    if (line_read.size() == 0) {
      continue;
    }

    std::vector<std::string> results;
    std::string cmd;
    split(results, line_read, is_any_of(" \t"));
    if (results.size() > 0) {
      cmd = results[0];

      if (cmd.compare("quit") == 0) {
	exit(0);
      } else if (cmd.compare("help") == 0) {
	std::cout
	  << std::endl
	  << "This is the online help system." << std::endl
	  << "The following commands are available:" << std::endl
	  << std::setw(20) << " " << "start" << std::endl
	  << std::setw(20) << " " << "stop" << std::endl
	  << "Type 'help <command>' to see a description of that command." 
	  << std::endl
	  << std::endl;
      } else if (cmd.compare("start") == 0) {
	std::cout << "Received start()";
	BOOST_FOREACH(int fd, child_fds)
	  write(fd, "start\n", 6);
      } else if (cmd.compare("stop") == 0) {
	std::cout << "Received stop()";
	BOOST_FOREACH(int fd, child_fds)
	  write(fd, "stop\n", 5);

	BOOST_FOREACH(pid_t p, child_pids) {
	  waitpid(p, NULL, 0);
	  std::cout << "PID: " << (int)p << " terminated..." << std::endl;
	}
      } else {
	std::cout
	  << std::endl
	  << "Unknown command. Please try again or type 'help'"
	  << std::endl
	  << std::endl;
      }
    }
  }
}

void child_cli(int parent_fd) {
  LOG4CXX_DEBUG(logger, "Started child_cli");

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
      break;
    } else {
      std::string s(line_read);
      free (line_read);
      LOG4CXX_DEBUG(logger, "child_cli() received " << s);
      std::vector<std::string> results;
      std::string cmd;
      split(results, s, is_any_of(" \t"));
      if (results.size() == 0)
	continue;
      
      cmd = results[0];
      trim(cmd);
      if (cmd.compare("start") == 0) {
	LOG4CXX_DEBUG(logger, "child_cli() received start cmd");

	std::cout << "Starting file writer stats...\n";
	FILE_WRITER_STATS->start();
	FILE_WRITER_STATS->cmd_write_to_disk();
	std::cout << "Started.\n";

	std::cout << "Starting file writer...\n";
	FILE_WRITER->open();
	FILE_WRITER->start();
	FILE_WRITER->cmd_write_to_disk();
	std::cout << "Sta[rted.\n";

	std::cout << "Starting net reader stats...\n";
	NET_READER_STATS->start();
	NET_READER_STATS->cmd_write_to_disk();
	std::cout << "Started.\n";

	std::cout << "Starting net reader...\n";
	NET_READER->start();
	NET_READER->cmd_read_from_network();
	std::cout << "Started.\n";
      } else if (cmd.compare("stop") == 0) {
	NET_READER_STATS->cmd_stop();
	NET_READER_STATS->join();
	std::cout << "Stopped net reader stats.\n";

	NET_READER->cmd_stop();
	NET_READER->join();
	std::cout << "Stopped net reader.\n";

	FILE_WRITER_STATS->cmd_stop();
	FILE_WRITER_STATS->join();
	std::cout << "Stopped file writer stats.\n";

	FILE_WRITER->cmd_stop();
	FILE_WRITER->join();
	std::cout << "Stopped file writer.\n";
      }
    }
  }
}


void banner() {
  std::cout
    << HLINE
    << std::endl
    << "|                                                                              |\n"
    << "|                              Net2disk v0.1                                   |\n"
    << "|                                                                              |\n"
    << "|                                                                              |\n"
    << "|                  Copyright 2011 MIT Haystack Observatory                     |\n"
    << "|                            del@haystack.mit.edu                              |\n"
    << "|                                                                              |\n"
    << HLINE
    << std::endl
    << std::endl;
}
