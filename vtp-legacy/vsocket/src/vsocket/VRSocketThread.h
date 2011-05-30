/*
 *  VRSocketThread.h
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *
 */

/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2003 MIT Haystack Observatory 
 *  
 * Permission is hereby granted, free of charge, to any person obtaining a   
 * copy of this software and associated documentation files (the "Software"),    * to deal in the Software without restriction, including without limitation   
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   
 * and/or sell copies of the Software, and to permit persons to whom the  
 * Software is furnished to do so, subject to the following conditions: 
 *  
 * The above copyright notice and this permission notice shall be included in    * all copies or substantial portions of the Software. 
 *   
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE * SOFTWARE. 
 * 
 */

#ifndef VRSOCKET_H
#define VRSOCKET_H

#include <common.h>
#include <Socket.h>
#include <UDPSocket.h>
#include <TCPSocket.h>
#include <string>
#include <iostream>
#include <Thread.h>
#include <UDPSocket.h>

/** VRSocketThread class
  * @doc This class encapsulates a UDP stream and implements the VLBI Real-time
  * protocol on top of it.
  */

class VRSocketThread: public Thread
{
	/** Definition of packet types */
	typedef enum { PT_DATA=0x1, PT_ACK=0x2, PT_FIN=0x3 } PacketType;

	/** Definition of packet types */
	typedef enum { 
		PS_INIT, PS_TX_DATA, PS_RX_DATA, PS_TX_ACK, PS_RX_ACK, PS_RETX, PS_BYE, 
		PS_APP_BYE
	} ProcStateType;

	/** State variable.
	  * Maintains the state for the processing loop (either recv_proc()
	  * or send_proc()).
	  */
	ProcStateType _state;
	/** Data transmission.
	  * All data is transmitted in one direction along this socket.
	  */
	UDPSocket _data_sock;
	/** Control transmission.
	  * All control information is transmitted in one direction along this 
	  * socket.
	  */
	TCPSocket _ctrl_sock;
	/** Accept sock.
	  * Used to accept incoming connections.
	  */
	TCPSocket _ctrl_accept_sock;
	/** SockProc mode.
	  * This determines which direction data is flowing: out of the object
	  * (send) or into the object(recv). For this implementation, the mode
	  * is fixed for the duration of the object's life. In the future, it
	  * may be possible to dynamically change it. In any case, a full
	  * duplex implementation is the ultimate aim.
	  */
	int _mode;
	/** MTU size.
	  * MTU size for data transmission, receptions.
	  */
	int _mtu;
	/** Thread callback.
	  * This function is called when the base class method "thread_create()" is
	  * called. This results in thread_func() being executed in it's own
	  * thread. Depending on the mode, either recv_proc() or send_proc() will
	  * be called by thread_func(). This means that recv_proc() or send_proc()
	  * will be running in their own thread of execution.
	  */
	void* thread_func();
	/** Processing function.
	  * This function does all of the processing when the object is in recv
	  * mode.
	  */
	void recv_proc();
	/** Processing function.
	  * This function does all of the processing when the object is in send
	  * mode.
	  */
	void send_proc();
	/** @doc Used for storing buffers containing received data. */
	SocketBufferCtrlBlkMap _recv_sockbufctrlblk_map;
	/** @doc Used for storing buffers for transmission. */
	SocketBufferCtrlBlkList _send_sockbufctrlblk_list;
	/** @doc Used for storing buffers that have been transmitted, but not yet
	  * acknowledged.
	  */
	SocketBufferCtrlBlkMap _outstanding_sockbufctrlblk_map;
	/** Mutex.
	  * Controls multi-threaded access to send data members.
	  */
	Mutex _send_mutex;
	/** Mutex.
	  * Controls multi-threaded access to rcv data members.
	  */
	Mutex _recv_mutex;
	/** Mutex.
	  * Controls multi-threaded access to outstanding data deque.
	  */
	Mutex _outstanding_mutex;
	/** Mutex.
	  * Controls multi-threaded access to ack buffer.
	  */
	Mutex _rx_sn_mutex;
	/** Sequence number.
 	  * Sequence number of the first packet in the recv queue.
	  */
	unsigned int _first_rx_sn;
	/** Sequence number.
 	  * Sequence number of the last packet in the recv queue.
	  */
	unsigned int _last_rx_sn;
	/** Mutex.
	  * Controls multi-threaded access to ack buffer.
	  */
	Mutex _tx_sn_mutex;
	/** Sequence number.
 	  * Sequence number of the first unacknowledged packet in the tx queue.
	  */
	unsigned int _first_tx_sn;
	/** Sequence number.
 	  * Sequence number of the last packet transmitted.
	  */
	unsigned int _last_tx_sn;
public:
	/** Mode constant. */
	static const int receiver;
	/** Mode constant. */
	static const int sender;
	/** Default constructor.
	  * This calls the base constructor to create a UDP socket and
	  * initialize a thread to service it.
	  * @param mode The mode for this socket (either send or receive).
	  */
    VRSocketThread(const int& mode);
	/** Constructor.
	  * This constructor creates a socket and sets the internal MTU member
	  * to be equal to the supplied parameter.
	  * @param mode The mode for this socket (either send or receive).
	  * @param m MTU for this socket.
	  */
    VRSocketThread(const int& mode, const int &m);
	/** Copy constructor.
	  * Copies values of other VRSocketThread into this object.
	  * @param mode The mode for this socket (either send or receive).
	  * @param s VRSocketThread to copy.
	  */
    VRSocketThread(const VRSocketThread &s);
	/** Constructor.
	  * This constructor is provided three initialization parameters.
	  * @param mode The mode for this socket (either send or receive).
	  * @param s socket descriptor to encapsulate.
	  * @param m MTU for this socket.
	  */
    VRSocketThread(const int& mode, const int& s, const int& m);
	/** Destructor. */
    virtual ~VRSocketThread();
	/** Assigment operator.
	  * Copies supplied socket fields into this object.
	  * @param s socket to copy.
	  */
    VRSocketThread& operator=(const VRSocketThread& s);
    /** Socket API method.
      * Binds socket to supplied address.
      * @param ip IP address to bind to.
      * @param dport data port to bind to.
      * @param cport control port to bind to.
      * @return 0: on success, -1 on failure.
      */
    void bind(const string& ip, const int& dport, const int& cport);
    /** Socket API method.
      * Attempt to establish a connection to the supplied address.
      * Note that UDP is connectionless and that in this case "connection" 
      * refers to the fact that the destination address fields are 
      * set to the parameters supplied in order that subsequent calls to
      * send() use these parameters. Otherwise, an address must be supplied
      * with each call to sendto().
      * @param ip IP address to connect to.
      * @param dport data port to bind to.
      * @param cport control port to bind to.
      * @return 0: on success, -1 on failure.
      */
    void connect(const string& ip, const int& dport, const int& cport);
	/** Socket API method.
	  * Place holder. Doesn't actually do anything. 
	  */
	void listen();
	/** Socket API method.
	  * Waits for incoming connection and then assigns _ctrl_sock to it.
	  */
	void accept();
	/** Socket API method.
	  * Attempt to recieve data into supplied buffer.
	  * @param s socket buffer to store data in.
	  * @param pos starting position in the supplied buffer (s) at which to
	  *        store received data.
	  * @param n number of bytes to atttempt to receive.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes read. A value
	  * 		of -1 is returned to indicate End Of Reception.
	  */
    int recv(SocketBuffer& s, const int& n);
	/** Socket API method.
	  * Attempt to recieve data into supplied buffer.
	  * @param s socket buffer to store data in.
	  * @param pos starting position in the supplied buffer (s) at which to
	  *        store received data.
	  * @param n number of bytes to atttempt to receive.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes read. A value
	  * 		of -1 is returned to indicate End Of Transmission.
	  */
    int send(const SocketBuffer& s, const int& n);
    /** Socket API method.
      * @doc shutdown connection as per standard socket library.
      * @param how method to use to shutdown socket: SHUT_RD, SHUT_WR, 
      * SHUT_RDWR.
      */    
	int shutdown(int howto);
    /** Socket API method.
	  * @doc close socket connection.      
	  * @return 0: on success, -1: on failure.
      */
    void close();
	/** Access method.
	  * Returns the size of send_buf (in number of buffers). This 
	  * provides a method for the caller to determine the progress of their 
	  * request.
	  * @return The number of buffers awaiting transmission.
	  */
	int get_send_buf_size();
	/** Access method.
	  * Returns the size of the outstanding buffer (in number of buffers). This 
	  * provides a method for the caller to determine the progress of their 
	  * request.
	  * @return The number of buffers awaiting potential re-transmission.
	  */
	int get_outstanding_buf_size();
	/**  Utility function.
	  *  Prints a packet header to debug stream.
	  */
	void print_pkt_hdr(const SocketBuffer& sb);
	/** Utility function.
	  * Builds an ACK packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store ack header in.
	  * @param sn Sequence number to be stored in the acknowledgement field.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	int build_ack_hdr(SocketBuffer& sb, const int& sn, const int& sz);
	/** Utility function.
	  * Builds a DATA packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store data header in.
	  * @param sn Sequence number to be stored in the acknowledgement field.
	  * @param sz Size of the header.
	  * @param length Length of the data field.
	  * @return 0: on success, -1 on error.
	  */
	int build_data_hdr(SocketBuffer& sb, const int& sn, const int& sz,
						const int& length);
	/** Utility function.
	  * Builds a FIN packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store data header in.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	int build_fin_hdr(SocketBuffer& sb, const int& sz);
	/** 
	  * @doc Test method.
	  */
    static bool test();
};

ostream& operator<<(ostream& os, const VRSocketThread& s);

#endif

