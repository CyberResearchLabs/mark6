/*
 *  VRClientSocket.h
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

#ifndef VRCLIENTSOCKET_H
#define VRCLIENTSOCKET_H

#include <common.h>
#include <Socket.h>
#include <UDPSocket.h>
#include <TCPSocket.h>
#include <string>
#include <iostream>
#include <UDPSocket.h>
#include <VRSocket.h>

/** VRClientSocket class
 * This class implements an RTP client socket. It inherits from VRSocket.
 */
class VRClientSocket: public VRSocket
{
  /** File.
   * Output file name.
   */
  string _output_file_name;
 public:
  /** Processing function.
   * This function does all of the processing.
   */
  void run();
  void run_tvg();
  void run1();
  void run2();
  /** Constructor.
   * This calls the base constructor to create a UDP socket.
   */
  VRClientSocket(const TSpec& t);
  /** Constructor.
   * This constructor creates a socket and sets the internal MTU member
   * to be equal to the supplied parameter.
   * @param t Traffic specification.
   * @param ip Remote IP address.
   * @param uport UDP socket port.
   * @param tport TCP socket port.
   * @param tvg Test Vector Generator switch.
   * @param tvg_duration Duration of test vector generation.
   * @param sampling_frequency Aggregate sampling frequency.
   * @param packet_size size of packet payload in bits.
   * @param bits_per_sample number of bits per sample.
   * @param output_file_name output file name.
   * @param scan_vector vecgtor of scans to transfer.
   * @param max_window maximum recv window size.
   */
  VRClientSocket(const TSpec& t, const string& ip, const int& uport, 
                 const int& tport, const int& cport, const bool& tvg, 
                 const double& tvg_duration, const u_int32_t& sampling_frequency, 
                 const u_int32_t& packet_size, const u_int32_t& bits_per_sample,
                 const string& output_file_name,
                 const ScanVector& scan_vector,
                 const u_int16_t& max_window);
  /** Copy constructor.
   * Copies values of other VRClientSocket into this object.
   * @param s VRClientSocket to copy.
   */
  VRClientSocket(const VRClientSocket &s);
  /** Destructor. */
  virtual ~VRClientSocket();
};

ostream& operator<<(ostream& os, const VRClientSocket& s);

#endif

