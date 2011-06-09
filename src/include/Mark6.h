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

#ifndef MARK6_H_
#define MARK6_H_

// Common definitions.
typedef unsigned int u_int32_t;
typedef unsigned short int u_int16_t;
typedef unsigned char u_int8_t;

typedef signed int int32_t;
typedef signed short int int16_t;
typedef signed char int8_t;

// Framework includes.
#include <boost/date_time/posix_time/posix_time.hpp>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/propertyconfigurator.h>

using namespace std;
using namespace boost;
using namespace boost::posix_time;
using namespace boost::gregorian;
using namespace log4cxx;
using namespace log4cxx::helpers;

extern LoggerPtr logger;

// Convert CDR data/time to epoch.
extern time_t date_time_to_epoch(const string& start_date,
                          const string& start_time);

// Convenience class for keeping IP and port information together.
struct IPEndpoint {
  std::string _ip_address;
  int _port;

  friend ostream& operator<<(ostream& out, const IPEndpoint& ep) {
    out
      << "IPEndpoint {\n"
      << "_ip_address:" << ep._ip_address << std::endl
      << "_port:" << ep._port << std::endl
      << "}";
    return out;
  }

  bool operator() (IPEndpoint ep1, IPEndpoint ep2) {
    if (ep1._ip_address == ep2._ip_address)
      return ep1._port < ep2._port;
    return ep1._ip_address < ep2._ip_address;
  }
};

#endif /*MARK6_H_*/
