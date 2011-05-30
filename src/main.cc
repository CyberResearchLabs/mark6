/*
 *  Created by David Lapsley on Mon May 30 2011.
 *
 * This software is released under the terms of the MIT License(included below). 
 *  
 * Copyright (c) 2003, 2011 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    
 * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    
 * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
 * SOFTWARE. 
 * 
 */


// C includes.
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>
#include <bitset>

// Framework includes.
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/propertyconfigurator.h>

// Local includes.


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
using namespace log4cxx;
using namespace log4cxx::helpers;

extern LoggerPtr logger;

// Run simple tests.
void run_tests() {
}

// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  std::cout
    << "mark6 [options]" << std::endl
    << desc;
}

// Global logger definition.
LoggerPtr logger(Logger::getLogger("mark6"));


// Program entry point.
int main (int argc, char* argv[])
{
  // Variables to store options.
  std::string log_config; 
  int port = 0;
  std::string data_file;
  std::string hash_type;
  std::string config;
  std::string schema_config;
  std::string cdr_select_string;
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
     po::value<std::string>(&data_file)->default_value(std::string("mark6.dat")),
     "Output data file name"
     )
    (
     "config",
     po::value<std::string>(&config)->default_value(std::string("mark6.xml")),
     "XML configuration file name"
     )
    (
     "schema-config",
     po::value<std::string>(&schema_config)
     ->default_value(std::string("mark6-schema.cfg")),
     "schema configuration file name"
     )
    (
     "log-config",
     po::value<std::string>(&log_config)
     ->default_value(std::string("mark6-log.cfg")),
     "Log configuration file name"
     )
    (
     "hash-type",
     po::value<std::string>(&hash_type)->default_value(std::string("static")),
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
     po::value<std::string>(&cdr_select_string)
     ->default_value(std::string("SELECT * FROM cdrs;")),
     "CDR select std::string."
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
    std::cout << "Netbloom version: "
         << "$Id: main.cc,v 1.14 2009-06-12 13:44:01 dlapsley Exp $"
         << std::endl;
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
    std::cerr << e.what() << std::endl;
  }

  return 0;
}


