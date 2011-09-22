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

// C++ includes.
#include <iostream>

// Framework includes.

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
#include <buffer_pool.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

NetReader::NetReader(const int id,
		     const std::string interface,
		     const int snaplen,
		     const int payload_length,
		     const int buffer_size,
		     const bool promiscuous,
		     FileWriter* const fw,
		     StatsWriter* const sw,
		     const double command_interval):
  Threaded(id, command_interval),
  _interface(interface),
  _snaplen(snaplen),
  _payload_length(payload_length),
  _buffer_size(buffer_size),
  _promiscuous(promiscuous),
  _fw(fw),
  _sw(sw),
  _ring(0),
  _bp(0),
  _net_buf(0),
  _state(IDLE) {
  _ring = new PFR(interface.c_str(), snaplen, _promiscuous);
  _bp = BufferPool::instance();
  _net_buf = new boost::uint8_t[snaplen];
}

NetReader::~NetReader() {
  delete _bp;
  delete _ring;
}

void NetReader::start() {
  _running = true;
  _thread = boost::thread(&NetReader::run, this);
}

void NetReader::join() {
  _thread.join();
}

void NetReader::run() {
  LOG4CXX_INFO(logger, "NetReader Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {

      case READ_FROM_NETWORK:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_read_from_network();
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

void NetReader::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void NetReader::cmd_read_from_network() {
  LOG4CXX_INFO(logger, "Received READ_FROM_NETWORK...");
  _state = READ_FROM_NETWORK;
}

void NetReader::handle_stop() {
  _running = false;
}

void NetReader::handle_idle() {
  usleep(_command_interval*1000000);
}

void NetReader::handle_read_from_network() {
  struct pfring_pkthdr hdr;
  int payload_length;
  boost::uint8_t* payload_ptr;
  
  int bytes_left = _buffer_size;
  int bytes_read = 0;
  boost::uint8_t* file_buf = _bp->malloc();
  // static boost::uint8_t file_buf[1048576];
  // static boost::uint8_t file_buf[822400];
  // int bytes_left = 822400;
  boost::uint64_t num_packets = 0;
  boost::uint64_t num_bytes = 0;

  // Fill the new file_buf;
  while (bytes_left > 0) {
    if (_ring->get_next_packet(&hdr, _net_buf, _snaplen) > 0) {
      // Successful read.

      // Extract offsets etc. from pfring structures.
      struct pfring_extended_pkthdr& pep(hdr.extended_hdr);
      struct pkt_parsing_info& ppi(pep.parsed_pkt);
      struct pkt_offset po(ppi.offset);
      const boost::uint16_t eth_offset(po.eth_offset);
      const boost::uint16_t l3_offset(po.l3_offset);
      const boost::uint16_t l4_offset(po.l4_offset);
      const boost::uint16_t payload_offset(po.payload_offset);

      // Get payload information.
      payload_ptr = &_net_buf[payload_offset];
      payload_length = hdr.caplen - payload_offset;

      // Check for validity of capture.
      if (hdr.caplen != _snaplen) {
	  // || hdr.caplen != hdr.len
	  // || payload_length != _payload_length) {
#define LOG_SHORT
#ifdef LOG_SHORT
	LOG4CXX_ERROR(logger, "Short capture(caplen/snaplen"
		      "len/payload_length/PAYLOAD_LENGTH"
		      << hdr.caplen << "/" << _snaplen << "/"
		      << hdr.len << "/" << payload_length << "/"
		      << _payload_length);
#endif // LOG_SHORT
	continue;
      }
	      
// #define DUMP
#ifdef DUMP
      std::cout << "Packet dump.\n";
      std::cout << "caplen:         " << hdr.caplen << std::endl;
      std::cout << "len:            " << hdr.len << std::endl;
      std::cout << "parsed_hdr_len: " << pep.parsed_header_len << std::endl;
      std::cout << "eth_offset:     " << eth_offset << std::endl;
      std::cout << "l3_offset:      " << l3_offset << std::endl;
      std::cout << "l4_offset:      " << l4_offset << std::endl;
      std::cout << "payload_offset: " << payload_offset << std::endl;
      std::cout << "payload_length: " << payload_length << std::endl;

      const int dumplen(128);
      for (int i=0; i<dumplen; i++) {
	printf("%02x ", (unsigned char)_net_buf[i + payload_offset]);
	if ((i+1)%8 == 0)
	  printf(" ");
	if ((i+1)%16 == 0)
	  printf("\n");
      }
      printf("\n");
#endif // DUMP	

      // Update stats.
      num_packets++;
      num_bytes += hdr.caplen;
    } else {
      LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
		    << strerror(errno));
      continue;
    }
    // Accumulate or flush data to disk.
    // Copy captured payload to file buffer.
    memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
    bytes_read += hdr.caplen;
    bytes_left -= hdr.caplen;
  }
  // Pad out rest of buffer then write.
  memset(&file_buf[bytes_read], 2, bytes_left);
  _fw->write(file_buf);

  // Update stats.
  _sw->update(num_packets, num_bytes);
}
