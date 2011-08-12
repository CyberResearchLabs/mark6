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

// C includes
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <stats_writer.h>

const double DEFAULT_ALPHA = 0.5;
const int DEFAULT_PAGE_LENGTH = 10;

StatsWriter::StatsWriter(const int id,
			 const string& stats_file,
			 const int stats_interval,
			 const double command_interval):
  Threaded(id, command_interval),
  _stats_file(stats_file),
  _stats_interval(stats_interval),
  _fstream(stats_file.c_str(), fstream::out | fstream::trunc),
  _state(IDLE),
  _mutex(),
  _average_packet_rate(0),
  _average_byte_rate(0),
  _last_num_packets(0),
  _last_num_bytes(0),
  _interval(0),
  _ALPHA(DEFAULT_ALPHA),
  _BPS_TO_MBPS(8/1e6),
  _PAGE_LENGTH(10)
{
}

StatsWriter::~StatsWriter() {
  _fstream.close();
}

void StatsWriter::start() {
  _running = true;
  _thread = boost::thread(&StatsWriter::run, this);
}

void StatsWriter::join() {
  _thread.join();
}

void StatsWriter::run() {
  LOG4CXX_INFO(logger, "StatsWriter Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {
      case WRITE_TO_DISK:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_write_to_disk();
	break;

      case IDLE:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_idle();
	break;

      case STOP:
	handle_stop();
	break;

      default:
	LOG4CXX_ERROR(logger, "Unknown state.");
	break;
      }
    }
    LOG4CXX_DEBUG(logger, "elapsed run time: " << run_timer.elapsed());
  } catch(std::exception &ex) {
    LOG4CXX_ERROR(logger, "error: " << ex.what());
  }
}

void StatsWriter::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void StatsWriter::cmd_write_to_disk() {
  LOG4CXX_INFO(logger, "Received WRITE_TO_DISK");
  _state = WRITE_TO_DISK;
}

void StatsWriter::handle_stop() {
  _running = false;
}

void StatsWriter::handle_idle() {
  usleep(_command_interval*1000000);
}

void StatsWriter::handle_write_to_disk() {
  struct timeval end_time;
  struct timeval diff, instant_diff;

  // Cache stats variables.
  boost::uint64_t num_packets;
  boost::uint64_t num_bytes;
  {
    boost::mutex::scoped_lock lock(_mutex);
    num_packets = _num_packets;
    num_bytes = _num_bytes;
  }

  // Update time variables.
  if (_start_time.tv_sec == 0) {
    gettimeofday(&_start_time, NULL);
  }
  gettimeofday(&end_time, NULL);

  // Elapsed _interval.
  timersub(&end_time, &_start_time, &diff);
  timersub(&end_time, &_last_time, &instant_diff);

  const double delta_seconds = diff.tv_sec + diff.tv_usec/1000000.0;
  const double instant_delta_seconds = instant_diff.tv_sec
    + instant_diff.tv_usec/1000000.0;
  const double instant_packet_rate = (num_packets - _last_num_packets)
    /instant_delta_seconds;
  const double instant_byte_rate = _BPS_TO_MBPS*(num_bytes - _last_num_bytes)
    /instant_delta_seconds;
  const double lifetime_packet_rate = num_packets/delta_seconds;
  const double lifetime_byte_rate = _BPS_TO_MBPS*num_bytes/delta_seconds;

  _average_packet_rate = (1.0-_ALPHA)*_average_packet_rate
    + _ALPHA*instant_packet_rate;
  _average_byte_rate = (1.0-_ALPHA)*_average_byte_rate
    + _ALPHA*instant_byte_rate;

  if (_interval % _PAGE_LENGTH == 0) {
    _fstream
      << HLINE
      << "+"
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
  ++_interval;

  _fstream
    << "|"
    << setw(10) << delta_seconds << " "
    << setw(10) << instant_packet_rate << " "
    << setw(10) << lifetime_packet_rate << " "
    << setw(10) << _average_packet_rate << " "
    << "|"
    << setw(10) << instant_byte_rate << " "
    << setw(10) << lifetime_byte_rate << " "
    << setw(10) << _average_byte_rate << " "
    << "|\n";
  
  // Update state.
  gettimeofday(&_last_time, NULL);
  _last_num_packets = num_packets;
  _last_num_bytes = num_bytes;

  sleep(_stats_interval);
}

void StatsWriter::update(const boost::uint64_t& packets,
			 const boost::uint64_t& bytes) {
  boost::mutex::scoped_lock lock(_mutex);
  _num_packets += packets;
  _num_bytes += bytes;
}
