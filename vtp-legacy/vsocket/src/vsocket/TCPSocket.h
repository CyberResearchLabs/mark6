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


/* 
 * This software is released under the terms of the MIT License(included below). *  
 * Copyright (c) 2004 MIT Haystack Observatory 
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


#ifndef TCP_SOCKET_H
#define TCP_SOCKET_H

#include <common.h>
#include <Socket.h>
#include <list>
#include <string>
#include <iostream>

/** TCPSocket class
  * @doc This class encapsulates a TCP stream.
  */

/** TCPSocket class
  * @doc This class encapsulates a TCP Stream. It inherits from the Socket
  * class and provides methods that closely match those of the BSD sockets
  * API. The main aim of this class is to simplify the creation, 
  * connection establishment and transmission/reception procedures of TCP 
  * sockets. This class has been deliberately designed to NOT handle errors 
  * for the calling context. Instead, it returns error codes back to the 
  * calling context so that it may work out how best to deal with it. The 
  * reason for doing this is that the TCPSocket does not have the wider 
  * context of the caller and so cannot determine if an error return is 
  * indeed an error or expected behavior.
  */
class TCPSocket: public Socket 
{
public:
	/** Default constructor.
	  * This calls the base constructor to create an opened TCP
	  * socket (i.e. socket of type SOCK_STREAM)
	  */
    TCPSocket();
	/** Constructor.
	  * This constructor creates a socket and sets the internal MTU member
	  * to be equal to the supplied parameter.
	  * @param m MTU for this socket.
	  */
    TCPSocket(const int &m);
	/** Copy constructor.
	  * Copies values of other TCPSocket into this object.
	  * @param s TCPSocket to copy.
	  */
    TCPSocket(const TCPSocket &s);
	/** Constructor.
	  * This constructor is provided two initizialization parameters.
	  * @param s socket descriptor to encapsulate.
	  * @param m MTU for this socket.
	  */
    TCPSocket(const int& s, const int& m);
	/** Destructor. */
    virtual ~TCPSocket();
	/** Assigment operator.
	  * Copies supplied socket fields into this object.
	  * @param s socket to copy.
	  */
    TCPSocket& operator=(const TCPSocket& s);
	/** Access method.
	  * Allows setting of TCPSocket's internal socket descriptor. This is
	  * useful for allowing a TCPSocket object to encapsulate a socket
	  * descriptor that has been returned from another function, e.g.
	  * accept().
	  * @param s socket descriptor to encapsulate.
	  */
    void set_sockd(const int& s);
	/** Socket API method.
	  * Binds socket to supplied address.
	  * @param ip IP address to bind to.
	  * @param port port to bind to.
	  * @return 0: on success, -1 on failure.
	  */
    int bind(const string& ip, const int& port);
	/** Socket API method.
	  * Prepare a socket for accepting incoming connections.
	  * @return 0: on success, -1 on failure.
	  */
    int listen();
	/** Socket API method.
	  * Wait for incoming connections on a socket. 
	  * @return -1: on failure, positive integer on success. On succes
	  *         the return value is a valid socket descriptor.
	  */
    int accept();
	/** Socket API method.
	  * Attempt to establish a connection to the supplied address.
	  * @param ip IP address to connect to.
	  * @param port port to connect to.
	  * @return 0: on success, -1 on failure.
	  */
    int connect(const string& ip, const int& port);
	/** Socket API method.
	  * Attempt to receive data into supplied buffer.
	  * @param s socket buffer to store data in.
	  * @param n number of bytes to attempt to receive.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes read.
	  */
    inline int recv(SocketBuffer& s, const int& n);
	/** Socket API method.
	  * Attempt to recieve data into supplied buffer.
	  * @param s socket buffer to store data in.
	  * @param pos starting position in the supplied buffer (s) at which to
	  *        store received data.
	  * @param n number of bytes to atttempt to receive.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes read.
	  */
    int recv(SocketBuffer& s, const int& pos, const int& n);
    /** Socket API method.
      * Attempt to receive data into supplied buffers.
      * @param l list of socket buffer control blocks to store data in.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes read.
      */
    int recv(const vector<SocketBufferCtrlBlk>& l);
	/** Socket API method.
	  * Attempt to send data from supplied buffer.
	  * @param s socket buffer to from which to get data to send.
	  * @param n number of bytes to attempt to send.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes sent.
	  */
    inline int send(const SocketBuffer& s, const int& n);
	/** Socket API method.
	  * Attempt to send data from supplied buffer.
	  * @param s socket buffer to from which to get data to send.
	  * @param pos starting position in the supplied buffer (s) at which to
	  *        start transmitting data.
	  * @param n number of bytes to attempt to send.
	  * @return 0: on success, positive integer on success. On success
	  *         the value returned is the number of bytes sent.
	  */
    int send(const SocketBuffer& s, const int& pos, const int& n);
    /** Socket API method.
      * Attempt to send data from supplied buffers. In this case, the 
      * destination is the address specified in the last call to connect().
      * @param l socket buffer control blocks from which to get data to send.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes sent.
      */
    int send(const vector<SocketBufferCtrlBlk>& l);
    /** Socket API method.
	  * @doc shutdown connection as per standard socket library.
      * @param how method to use to shutdown socket: SHUT_RD, SHUT_WR, 
      * SHUT_RDWR.
      */
    int shutdown(int howto);
	/** Socket API method.
      * @doc close socket connection.
      */
    void close();
	/** 
	  * @doc Test method.
	  */
    static bool test();
};

ostream& operator<<(ostream& os, const TCPSocket& s);

#endif

