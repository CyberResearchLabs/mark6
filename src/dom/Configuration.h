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

#ifndef _CONFIGURATION_H_
#define _CONFIGURATION_H_

// C++ includes
#include <string>
#include <set>
#include <iostream>

// Framework includes.
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

// Local includes.
#include <Mark6.h>

// Namespaces.
using boost::property_tree::ptree;

typedef std::set<IPEndpoint, IPEndpoint> IPEndpointSet;

// Load/save information from XML configuration file.
// Format of configuration file:
//  <gen>
//    <destinations>
//      <ip>X.X.X.X</ip>
//      <port>X.X.X.X</port>
//    </destinations>
//  </gen>
struct Configuration {
  // UDP endpoints for data streams.
  IPEndpointSet _destinations;

  // Stream rate in bps.
  unsigned int _stream_rate;

  // Duration of run (in seconds).
  unsigned int _duration;

  // Write block size (bytes).
  unsigned int _write_block_size;

  // Format of data ("random", "vdif", "Mark5C").
  std::string _format;

  // Constructor.
  Configuration()
  : _stream_rate(1024),
    _duration(10),
    _format("random") {}
  
  // Load configuration from XML file.
  void load(const std::string &filename) {
    ptree pt;

    read_xml(filename, pt);

    _stream_rate = pt.get<unsigned int>("gen.data.<xmlattr>.stream_rate");
    _duration = pt.get<unsigned int>("gen.data.<xmlattr>.duration");
    _format = pt.get<std::string>("gen.data.<xmlattr>.format");
    _write_block_size = pt.get<unsigned int>("gen.data.<xmlattr>.write_block_size");

    BOOST_FOREACH(ptree::value_type &v,
		  pt.get_child("gen.receivers")) {
      ptree &rt = v.second;
      IPEndpoint ep;
      ep._ip_address = rt.get<std::string>("<xmlattr>.ip");
      ep._port = rt.get<int>("<xmlattr>.port");
      _destinations.insert(ep);
    }
  }

  // Ostream serializer.
  friend ostream& operator<<(ostream& out, Configuration c) {
    out << "Configuration {\n";

    BOOST_FOREACH(IPEndpoint ep, c._destinations) {
      out << ep << std::endl;
    }

    out
      << "_stream_rate:" << c._stream_rate << std::endl
      << "_duration:" << c._duration << std::endl
      << "_format:" << c._format << std::endl
      << "_write_block_size:" << c._write_block_size << std::endl
      << "}\n";

    return out;
  }

};

#endif // _CONFIGURATION_H_
