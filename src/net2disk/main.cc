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
#include <mark6.h>
#include <logger.h>
#include <pfr.h>

// Namespaces.
namespace po = boost::program_options;

// Constants
// Entry point for regression tests.
const std::string DEFAULT_CONFIG_FILE("dim6.xml");

// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  cout
    << "dim6 [options]" << endl
    << desc;
}

// Option defaults.
const string DEFAULT_INTERFACE("eth0");
const int DEFAULT_SNAPLEN(9000);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const string DEFAULT_LOG_CONFIG("net2disk-log.cfg");


// Program entry point.
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
    LOG4CXX_INFO(logger, "Creating Net2disk manager.");

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

 
    char stats[32];
    while (true) {
      u_char pkt[9000];
      struct pfring_pkthdr hdr;
      struct simple_stats *the_stats = (struct simple_stats*)stats;

      if (ring.get_next_packet(&hdr, pkt, sizeof(pkt)) > 0) {
	LOG4CXX_INFO(logger, "Got " << hdr.len << " byte packet");
      } else {
	LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
		      << strerror(errno));
      }
    }
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}


