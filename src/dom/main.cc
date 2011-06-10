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
#include <Mark6.h>
#include <Configuration.h>
#include <SenderThread.h>

// Namespaces.
namespace po = boost::program_options;

// Constants
// Entry point for regression tests.
const std::string DEFAULT_CONFIG_FILE("dom6.xml");
const std::string LOGGER_NAME("dom6");

void
run_tests() {
  Configuration c;
  c.load(DEFAULT_CONFIG_FILE);
  std::cout << c;

  std::list<boost::thread*> _sender_threads;
  std::string ip("127.0.0.1");
  boost::uint16_t port = 4242;
  try {
    for (int i=0; i<1; ++i) {
      std::cout << i << std::endl;
      _sender_threads.push_back(new boost::thread(SenderThread(), ip, port,
						  c._stream_rate,
						  c._duration,
						  c._mtu,
						  c._write_block_size));
    }
  } catch (std::string e) {
    LOG4CXX_ERROR(logger, e);
  }

  BOOST_FOREACH(boost::thread* t, _sender_threads) {
    t->join();
  }

}

// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  cout
    << "dom6 [options]" << endl
    << desc;
}

// Global logger definition.
LoggerPtr logger(Logger::getLogger(LOGGER_NAME));

// Program entry point.
int
main (int argc, char* argv[])
{
  // Variables to store options.
  string log_config; 
  string config;

  // Declare supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("v", "print version message")
    ("run-tests", "Run test programs")
    (
     "config",
     po::value<string>(&config)->default_value(string(DEFAULT_CONFIG_FILE)),
     "XML configuration file name"
     )
    (
     "log-config",
     po::value<string>(&log_config)
     ->default_value(string("dom6-log.cfg")),
     "Log configuration file name"
     )
    ;

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
    cout << "Dom6 version: 0.1.1"
         << endl;
    return 1;
  }

  if (vm.count("run-tests")) {
    run_tests();
    return 0;
  }

  // Start processing.
  try {
    LOG4CXX_INFO(logger, "Creating dom6 manager.");

    // Create the server.
    LOG4CXX_INFO(logger, "Creating tcp server.");
    // TCPServer server(IO_SERVICE, port, BLOOM_MANAGER);

    // Fire up the reactor.
    LOG4CXX_INFO(logger, "Starting reactor.");
    // IO_SERVICE.run();
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}


