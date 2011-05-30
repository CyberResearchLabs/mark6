/*
 *  VRServerSocket.h
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *
 */

/* 
 * This software is released under the terms of the MIT License(included below). 
 *  
 * Copyright (c) 2003 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    
 * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    
 * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
 * SOFTWARE. 
 * 
 */

#ifndef VRSERVERSOCKET_H
#define VRSERVERSOCKET_H

#include <common.h>
#include <Socket.h>
#include <UDPSocket.h>
#include <TCPSocket.h>
#include <string>
#include <iostream>
#include <UDPSocket.h>
#include <VRSocket.h>
#include <syslog.h>

/** VRServerSocket class
 * This class implements an RTP server socket. It inherits from VRSocketA.
 */
class VRServerSocket: public VRSocket
{
  /** File.
   * Name of input file.
   */
  string _in_file_name;
  /** Network parameter.
   * Maximum size of leaky bucket used for rate limiting.
   */
  const u_int32_t _max_bucket;
 public:
  /** Utility function.
   * This process implements the ipd sleep.
   */
  void mysleep(struct timespec& ipd_req, int& bucket);
  /** Processing function.
   * This function does all of the processing.
   */
  void run();
  
  // FIXME
  void run_tvg();
  void run1();
  void run2();
  double sleep_times[65536];
  u_int16_t sleep_idx;

  /** Constructor.
   * This constructor creates a socket and sets the internal MTU member
   * to be equal to the supplied parameter.
   * @param t Traffic specification.
   * @param ip Remote IP address.
   * @param uport UDP socket port.
   * @param tport TCP socket port.
   * @param cport TCP control socket port.
   * @param tvg Test Vector Generator switch.
   * @param tvg_duration Duration of test vector generation.
   * @param freq Aggregate sampling frequency.
   * @param packet_size size of packet payload in bits.
   * @param bits_per_sample number of bits per sample.
   * @param in_file_name source file.
   * @param scan_vecotor vector of scans to transfer.
   * @param max_window maximum send window size (in packets).
   * @param max_bucket maximum size of rate limiting leaky bucket.
   */
  VRServerSocket(const TSpec& t, const string& ip, const int& uport, 
                 const int& tport, const int& cport, const bool& tvg, 
                 const double& tvg_duration, const u_int32_t& freq, 
                 const u_int32_t& packet_size, 
                 const u_int32_t& bits_per_sample,
                 const string& in_file_name,
                 const ScanVector& scan_vector,
                 const u_int16_t& max_window,
                 const u_int32_t& max_bucket);
  /** Copy constructor.
   * Copies values of other VRServerSocket into this object.
   * @param s VRServerSocket to copy.
   */
  VRServerSocket(const VRServerSocket &s);
  /** Constructor.
   * This constructor is provided three initialization parameters.
   * @param s socket descriptor to encapsulate.
   * @param t traffic specification.
   * @param tvg Test Vector Generator switch.
   * @param tvg_duration Duration of test vector generation.
   * @param freq Aggregate sampling frequency.
   * @param packet_size size of packet payload in bits.
   * @param bits_per_sample number of bits per sample.
   * @param in_file_name source file.
   * @param max_window maximum send window size (in packets).
   * @param max_bucket maximum size of rate limiting leaky bucket.
   * @param n maximum window size.
   */
  VRServerSocket(const int& s, const TSpec& t, 
                 const bool& tvg, const double& tvg_duration,
                 const u_int32_t& freq, const u_int32_t& packet_size,
                 const u_int32_t& bits_per_sample,
                 const string& in_file_name,
                 const u_int16_t& max_window,
                 const u_int32_t& max_bucket);
  /** Destructor. */
  virtual ~VRServerSocket();
};

ostream& operator<<(ostream& os, const VRServerSocket& s);

// #define NANOSLEEP 0
inline void VRServerSocket::mysleep(struct timespec& ipd_req, int& bucket) {
  struct timespec ipd_rem;
  //#ifdef NANOSLEEP
  struct timespec myts;
  //#endif
  if (bucket==0) {
    Timer sleep_timer;
    sleep_timer.start();
    //#ifdef NANOSLEEP
    // FIXME
    //myts.tv_sec=ipd_req.tv_sec*_max_bucket;
    //myts.tv_nsec=ipd_req.tv_nsec*_max_bucket;
    myts.tv_sec=0;
    myts.tv_nsec=2000000;
    if (nanosleep(&myts, &ipd_rem)!=0)  {
    	syslog(LOG_ERR, "PS_TX_DATA: nanosleep intr: %s", 
       	strerror(errno));
    }
    //#else
    //myto.tv_sec=ipd_req.tv_sec*_max_bucket;
    //myto.tv_usec=int(ipd_req.tv_nsec/1000.0*_max_bucket);
    //if (select (0, NULL, NULL, NULL, &myto)!=0)
    //syslog(LOG_ERR, "PS_RETX_DATA: select intr: %s", 
    //strerror(errno));
    //#endif
    sleep_timer.stop();
    sleep_times[sleep_idx++]=sleep_timer.elapsed();
#ifdef SYSLOG
    syslog(LOG_DEBUG, "PS_RETX_SLEEP_TIMER: %f\n", 
	   sleep_timer.elapsed());
#endif
    bucket=_max_bucket;
  } else {
    --bucket;
  }
}

#endif

