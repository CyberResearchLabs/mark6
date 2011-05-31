/*
 * Created by David Lapsley on Mon May 30 2011.
 *
 * Copyright 2003, 2011 MIT Haystack Observatory 
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

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>
#include <bitset>

// Framework includes.
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

// Local includes.
#include <Mark6.h>

#if 0
#include <iostream>
#include <exception>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <Options.h>
#include <SocketBuffer.h>
#include <Client.h>
#include <Server.h>
#include <Test.h>
#include <RTP.h>
#include <RTPSession.h>
#include <Logsystem.h>
#include <Logger.h>
#endif

// Namespaces.
namespace po = boost::program_options;

// Run simple tests.
void run_tests() {
}

// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  cout
    << "mark6 [options]" << endl
    << desc;
}

// Global logger definition.
LoggerPtr logger(Logger::getLogger("mark6"));


// Program entry point.
int main (int argc, char* argv[])
{
  // Variables to store options.
  string log_config; 
  int port = 0;
  string data_file;
  string hash_type;
  string config;
  string schema_config;
  string cdr_select_string;
  int topx_size = 0;
  int window_size = 0;
  int reorder_window = 0;
  int rt_flag = 0;

  // Declare supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("v", "print version message")
    ("run-tests", "Run test programs")
    (
     "data-file",
     po::value<string>(&data_file)->default_value(string("mark6.dat")),
     "Output data file name"
     )
    (
     "config",
     po::value<string>(&config)->default_value(string("mark6.xml")),
     "XML configuration file name"
     )
    (
     "schema-config",
     po::value<string>(&schema_config)
     ->default_value(string("mark6-schema.cfg")),
     "schema configuration file name"
     )
    (
     "log-config",
     po::value<string>(&log_config)
     ->default_value(string("mark6-log.cfg")),
     "Log configuration file name"
     )
    (
     "hash-type",
     po::value<string>(&hash_type)->default_value(string("static")),
     "Hash type to use (static | dynamic)"
     )
    (
     "port",
     po::value<int>(&port)->default_value(10000),
     "Listening port"
     )
    (
     "topx-size",
     po::value<int>(&topx_size)->default_value(10),
     "Size of topx list"
     )
    (
     "window-size",
     po::value<int>(&window_size)->default_value(900),
     "Size of accumulation window(s)"
     )
    (
     "reorder-window",
     po::value<int>(&reorder_window)->default_value(3600),
     "Size of reordering buffer(s)"
     )
    (
     "rt-flag",
     po::value<int>(&rt_flag)->default_value(0),
     "Enable real time processing(1) or batch processing (0)"
     )
    (
     "cdr-select",
     po::value<string>(&cdr_select_string)
     ->default_value(string("SELECT * FROM cdrs;")),
     "CDR select string."
     );

  // Parse options.
  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);;
  po::notify(vm);	

  // Configure log subsystem.
  PropertyConfigurator::configure(log_config);

  // Check various options.
  if (vm.count("help")) {
    usage(desc);
    return 1;
  }

  if (vm.count("v")) {
    cout << "Mark6 version: 0.1.0"
         << endl;
    return 1;
  }

  if (vm.count("run-tests")) {
    run_tests();
    return 0;
  }

  if (!vm.count("port") || !vm.count("data-file")
      || !vm.count("config") ) {
    usage(desc);
    return 1;
  }

  // Start processing.
  try {
    LOG4CXX_INFO(logger, "Creating mark6 manager.");

    // Create the server.
    LOG4CXX_INFO(logger, "Creating tcp server.");
    // TCPServer server(IO_SERVICE, port, BLOOM_MANAGER);

    // Initialize the timer.
    if (rt_flag) {
      LOG4CXX_INFO(logger, "Initializing timers.");
      // PROC_TIMER.expires_from_now(boost::posix_time::seconds(PROC_INTERVAL));
      // PROC_TIMER.async_wait(proc_cb);
    } else {
      LOG4CXX_INFO(logger, "Operating in batch mode.");
    }

    // Fire up the reactor.
    LOG4CXX_INFO(logger, "Starting reactor.");
    // IO_SERVICE.run();
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}


