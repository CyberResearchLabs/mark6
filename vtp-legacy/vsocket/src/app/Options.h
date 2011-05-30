/*
 *  Options.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */


#ifndef OPTIONS_H
#define OPTIONS_H

#include <common.h>
#include <string>
#include <RTP.h>
#include <Scan.h>
#ifdef XMLPARSING
#include <XMLParser.h>
#endif // XMLPARSING

using namespace std;



struct Options {
  // Mode selection.
  Mode _mode;
  int _debug_level;

  // Network.
  int _data_port;
  int _control_port;
  string _remote_ip;
  int _host_mtu;
  int _network_mtu;
  int _max_window;
  int _max_bucket;
  u_int32_t _ssrc;
  TSpec _tspec;

#if VSIEPARAMS
  // VSI-E
  u_int32_t _sampling_frequency;
  u_int32_t _packet_size;
  u_int32_t _bits_per_sample;
  u_int32_t _samples_per_packet;
#endif

  // Test.
  double _tvg_duration;

  // Misc.
  string _from_file;
  string _to_file;
  int _protocol;
  string _xml_file;

  Options(int argc, char* const argv[]);
  ~Options();

  void usage(ostream& c) const;
  void dump(ostream& c) const;
};


#endif

