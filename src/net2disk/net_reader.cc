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
#include <assert.h>

// C++ includes.
#include <iostream>

// Framework includes.

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
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
  _net_buf(0),
  _state(IDLE) {
  _ring = new PFR(interface.c_str(), snaplen, _promiscuous);
  _net_buf = new boost::uint8_t[snaplen];
}

NetReader::~NetReader() {
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

// Example PCAP header
// 0000000 c3d4 a1b2 0002 0004 0000 0000 0000 0000
// 0000020 ffff 0000 0001 0000

// FIXME
//
// 0000000 d4c3 b2a1 0200 0400 0000 0000 0000 0000
// 0000020 ffff 0000 0100 0000
//
// 0000000 c2d4 a1b2 0002 0004 0000 0000 0000 0000
// 0000020 ffff 0000 0001 0000

// 09:39:30.018515 01:00:4a:20:00:00 (oui Unknown) > 2a:97:80:4e:44:bf (oui Unknown), ethertype Unknown (0x4a20), length 8266: 

boost::uint8_t PCAP_HEADER[] = {
	0xd4, 0xc3, 0xb2, 0xa1, 0x2,  0x0,  0x4,  0x0,
	0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,	
	0xff, 0xff, 0x0,  0x0,  0x1,  0x0,  0x0,  0x0
};

const int PCAP_HEADER_LENGTH = 24;

struct PcapPacketHeader {         
  boost::uint32_t ts_sec;         /* timestamp seconds */         
  boost::uint32_t ts_usec;        /* timestamp microseconds */         
  boost::uint32_t incl_len;       /* number of octets of packet saved in file */         
  boost::uint32_t orig_len;       /* actual length of packet */
};

const int PCAP_PACKET_HEADER_LENGTH = 16;

void NetReader::handle_read_from_network() {
  struct pfring_pkthdr hdr;
  int payload_length;
  boost::uint8_t* payload_ptr;
  
  int bytes_read = 0;
  static int remainder_len = 0;
  static boost::uint8_t remainder_buf[9000];
  static bool first_write = true;
  static boost::uint8_t net_buf[9000];
  static PcapPacketHeader pph;

#define DYNAMIC_BUFFER
#ifdef DYNAMIC_BUFFER
  int bytes_left = _buffer_size;
  boost::uint8_t* file_buf = _fw->malloc_buffer();
  if (!file_buf) {
    LOG4CXX_ERROR(logger, "File buffer's full.");
    return;
  }
  assert(file_buf);
#else
  const int BUFFER_SIZE = 1048576;
  int bytes_left = BUFFER_SIZE;
  static boost::uint8_t file_buf[BUFFER_SIZE];
#endif
  boost::uint64_t dropped_packets = 0;
  boost::uint64_t num_packets = 0;
  boost::uint64_t num_bytes = 0;

  if (first_write) {
    memcpy(remainder_buf, PCAP_HEADER, PCAP_HEADER_LENGTH);
    remainder_len = PCAP_HEADER_LENGTH;
    first_write = false;
  }

  // Copy partial packet.
  if (remainder_len > 0) {
    memcpy(file_buf, remainder_buf, remainder_len);
    bytes_read += remainder_len;
    bytes_left -= remainder_len;
    remainder_len = 0;
  }

  // Fill the new file_buf;
  while (bytes_left > 0) {
    if (_ring->get_next_packet(&hdr, &net_buf[PCAP_PACKET_HEADER_LENGTH],
			       _snaplen) > 0) {
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
      // FIXME payload_ptr = &_net_buf[payload_offset];
      payload_ptr = net_buf;
      // FIXME payload_length = hdr.caplen - payload_offset;
      payload_length = hdr.len + PCAP_PACKET_HEADER_LENGTH;

      pph.ts_sec = hdr.ts.tv_sec;
      pph.ts_usec = hdr.ts.tv_usec;
      pph.incl_len = hdr.len;
      pph.orig_len = hdr.len;
      memcpy(net_buf, &pph, PCAP_PACKET_HEADER_LENGTH);

      // Check for validity of capture.
      if (hdr.caplen != _snaplen) {
	  // || hdr.caplen != hdr.len
	  // || payload_length != _payload_length) {
	LOG4CXX_DEBUG(logger, "Short capture(caplen/snaplen"
		      "len/payload_length/PAYLOAD_LENGTH) -> ("
		      << hdr.caplen << "/" << _snaplen << "/"
		      << hdr.len << "/" << payload_length << "/"
		      << _payload_length << ")");
	continue;
      }

      // Update stats.
      num_packets++;
      num_bytes += hdr.len;
    } else {
      LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
		    << strerror(errno));
      continue;
    }

    // Accumulate or flush data.
    if (bytes_left < payload_length) {
      // Copy fragment into buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, bytes_left);
      bytes_read += bytes_left;
      
      // Copy remainder into remainder_buf.
      remainder_len = payload_length - bytes_left;
      memcpy(remainder_buf, payload_ptr + bytes_left, remainder_len);

#define BUFFERED_WRITE      
#ifdef BUFFERED_WRITE
      if (!_fw->write(file_buf))
	++dropped_packets;
#else
      _fw->write_unbuffered(file_buf, BUFFER_SIZE);
#endif
      // Update stats.
      _sw->update(num_packets, num_bytes, dropped_packets, 0);
      bytes_left = 0;
      break;
    } else if (bytes_left == payload_length) {
      // Copy captured payload to file buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
      bytes_read += payload_length;
      bytes_left -= payload_length;
#ifdef BUFFERED_WRITE
      if (!_fw->write(file_buf))
	++dropped_packets;
#else
      _fw->write_unbuffered(file_buf, BUFFER_SIZE);
#endif
    } else {
      // Copy captured payload to file buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
      bytes_read += payload_length;
      bytes_left -= payload_length;
    }
  }
}

