/*
 *  VRSocket.h
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

#ifndef VRSOCKET_H
#define VRSOCKET_H

#include <common.h>
#include <Socket.h>
#include <string>
#include <iostream>
#include <Object.h>
#include <RTP.h>
#include <RTPSession.h>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <Scan.h>

/** VRSocket class
 * @doc This class acts as the base class for VRClientSocket and 
 * VRServerSocket. It encapsulates a tcp and udp socket stream.
 */

const int SEQ_SPACE_SIZE=65636;
typedef struct iovec Iovec;
#if FIXME
typedef vector<Iovec*> IovecMap;
#endif
typedef Iovec* IovecMap;

struct VRSocket: public Object
{

  /** RTP Session state. */
  RTPSession _rtp_session;
  /** Definition of packet types */
  typedef enum { 
    PS_CONN_INIT, PS_SCAN_INIT, PS_SELECT, PS_TX_DATA, PS_RX_DATA, PS_RETX_DATA,
    PS_BYE, PS_APP_BYE, PS_RX_RTCP, PS_TX_RTCP
  } ProcStateType;

  /** State variable.
   * Maintains the state for the processing loop (either recv_proc()
   * or send_proc()).
   */
  ProcStateType _state;
  /** Socket.
   * Used for receiving/transmitting data.
   */
  UDPSocket _udp_sock;
  /** Socket.
   * Used for interfacing with localhost.
   * socket.
   */
  TCPSocket _tcp_sock;
  /** Control transmission.
   * Used for transferring control information between client and server.
   */
  TCPSocket _ctrl_sock;
  /** Accept sock.
   * Used to accept incoming connections.
   */
  TCPSocket _tcp_accept_sock;
  /** Traffic Parameter.
   * Controls traffic parameters.
   */
  TSpec _t_spec;
  /** Address parameter.
   * UDP port for TCP socket.
   */
  int _udp_port;
  /** Address parameter.
   * TCP port for TCP Data socket.
   */
  int _ctrl_port;
  /** Address parameter.
   * Remote IP address.
   */
  string _remote_ip;
  /** Mode.
   * Whether or not to generate tvg test data.
   */
  Mode _mode;
  /** Test parameter.
   * How long to generate test data for (in seconds).
   */
  double _tvg_duration;
  /** Network parameter.
   * Maximum size of send/recv window.
   */
  u_int16_t _max_window;
  /** Processing function.
   * This function does all of the processing. It is overridde by 
   * inheriting classes.
   */
  virtual void run()=0;
  /** Constructor.
   * @param t A traffic specification.
   */
  VRSocket(const TSpec& t);
  /** Constructor.
   * This constructor creates a socket and sets the internal MTU member
   * to be equal to the supplied parameter.
   * @param m Mode specification.
   * @param t Traffic specification.
   * @param ip Remote IP address.
   * @param uport UDP socket port.
   * @param cport TCP socket control port.
   * @param tvg_duration duration of tvg data generation interval.
   * @param max_window maximum size of send/recv window.
   */
  VRSocket(const Mode& m, const TSpec& t, const string& ip, const int& uport, 
           const int& cport, const double& tvg_duration, const u_int16_t& max_window);
  /** Copy constructor.
   * Copies values of other VRSocket into this object.
   * @param s VRSocket to copy.
   */
  VRSocket(const VRSocket &s);
  /** Destructor. */
  virtual ~VRSocket();
  /** Assigment operator.
   * Copies supplied socket fields into this object.
   * @param s socket to copy.
   */
  VRSocket& operator=(const VRSocket& s);
  /** Utility function.
   * Prints out resource usage for the calling process.
   */
  void print_rusage();

  /** 
   * @doc Test method.
   */
  static bool test();
};

ostream& operator<<(ostream& os, const VRSocket& s);

#endif

