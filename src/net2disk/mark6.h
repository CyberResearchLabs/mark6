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
#include <sys/time.h>

// Framework includes.
#include <boost/cstdint.hpp>  // for boost::uint16_t
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/foreach.hpp>
#include <boost/circular_buffer.hpp>

using namespace boost;
using namespace boost::posix_time;
using namespace boost::gregorian;

// Convert CDR data/time to epoch.
extern time_t date_time_to_epoch(const std::string& start_date,
				 const std::string& start_time);

// Convenience class for keeping IP and port information together.
struct IPEndpoint {
  std::string _ip_address;
  int _port;

  friend std::ostream& operator<<(std::ostream& out, const IPEndpoint& ep) {
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

class Timer {
 private:
  struct timeval _start;
  struct timeval _stop;
  struct timeval _duration;
 public:
  Timer() {
    timerclear(&_start);
    timerclear(&_stop);
    timerclear(&_duration);
    gettimeofday(&_start, NULL);
  }
  void restart() {
    gettimeofday(&_start, NULL);
  }
  double elapsed() {
    gettimeofday(&_stop, NULL);
    timersub(&_stop, &_start, &_duration);
    return static_cast<double>(_duration.tv_sec)
      + static_cast<double>(_duration.tv_usec)/1000000.0;
  }
};

// Control messages.
enum  MessageType { MSG_WRITE_TO_DISK, MSG_READ_FROM_DISK, MSG_STOP };

struct ControlMessage {
  MessageType _type;
};

// FIXME
// typedef std::vector<boost::uint8_t> Buffer;
// typedef boost::circular_buffer<Buffer*> CircularBuffer;

#define HLINE "+------------------------------------------------------------------------------+"

#endif /*MARK6_H_*/
