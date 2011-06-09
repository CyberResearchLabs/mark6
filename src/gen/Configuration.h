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

// Framework includes.
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

// Namespaces.
using boost::property_tree::ptree;

struct IPEndpoint {
  std::string _ip_address;
  int _port;
};

// Load/save information from XML configuration file.
// Format of configuration file:
//  <gen>
//    <destinations>
//      <ip>X.X.X.X</ip>
//      <port>X.X.X.X</port>
//    </destinations>
//  </gen>
struct Configuration {
  // std::set<IPEndpoint> _destinations;

  // Load configuration from XML file.
  void load(const std::string &filename) {
    ptree pt;

    read_xml(filename, pt);

      // Iterate over the debug.modules section and store all found
      // modules in the m_modules set. The get_child() function
      // returns a reference to the child at the specified path; if
      // there is no such child, it throws. Property tree iterators
      // are models of BidirectionalIterator.
      BOOST_FOREACH(ptree::value_type &v,
      		    pt.get_child("gen.receivers")) {
      	std::cout << v.first <<  std::endl;

	ptree &rt = v.second;
	std::string ip = rt.get<std::string>("<xmlattr>.ip");
	std::cout << ip << std::endl;

	/*	const ptree rt = v.second;
	const ptree::value_type &w = rt.get_child("<xmlattr>");
	std::cout << w.first << std::endl;
	*/
	
	// ptree::value_type &w = v.second.get_child("<xmlattr>");
	// std::cout << w.first << std::endl;
        // m_modules.insert(v.second.data());
      }

  }

  void save(const std::string &filename) {
  }
};

#endif // _CONFIGURATION_H_
