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


/** UDPSocket class.
  * @doc This class encapsulates a UDP packet stream. 
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  */ 
/** 
  */

#ifndef UDP_SOCKET_H
#define UDP_SOCKET_H

#include <common.h>
#include <Socket.h>

/** UDPSocket class.
  * @doc This class encapsulates a UDP packet stream. It inherits from the 
  * socket class and provides methods that closely match those of the BSD 
  * sockets API. The main aim of this class is to simplify the creation and
  * data transmission/reception procedures of UDP sockets.  This class has been   * deliberately designed to NOT handle errors for the calling context. 
  * Instead, it returns error codes back to the calling context so that it may 
  * work out how best to deal with it. The reason for doing this is that the 
  * TCPSocket does not have the wider context of the caller and so cannot 
  * determine if an error return is indeed an error or expected behavior.
  */
class UDPSocket: public Socket 
{
public:
    /** Default constructor.
      * This calls the base constructor to create an opened UDP
      * socket (i.e. socket of type SOCK_DGRAM)
      */
    UDPSocket();
    /** Constructor.
      * This constructor is provided two initizialization parameters.
      * @param s socket descriptor to encapsulate.
      * @param m MTU for this socket.
      */
    UDPSocket(const int& s, const int& m);
    /** Copy constructor.
      * Copies values of other UDPSocket into this object.
      * @param s UDPSocket to copy.
      */
	UDPSocket(const UDPSocket& s);
    /** Destructor. */
    virtual ~UDPSocket();
    /** Assigment operator.
      * Copies supplied socket fields into this object.
      * @param s socket to copy.
      */
	UDPSocket& operator=(const UDPSocket& s);
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
    void bind(const string& ip, const int& port);
    /** Socket API method.
      * Attempt to establish a connection to the supplied address.
      * Note that UDP is connectionless and that in this case "connection" 
	  * refers to the fact that the destination address fields are 
	  * set to the parameters supplied in order that subsequent calls to
	  * send() use these parameters. Otherwise, an address must be supplied
	  * with each call to sendto().
      * @param ip IP address to connect to.
      * @param port port to connect to.
      * @return 0: on success, -1 on failure.
      */
    void connect(const string& ip, const int& port);
    /** Socket API method.
      * Attempt to receive data into supplied buffer.
      * @param s socket buffer to store data in.
      * @param n number of bytes to attempt to receive.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes read.
      */
    inline int recv(SocketBuffer& s, const int& n);
    /** Socket API method.
      * Attempt to receive data into supplied buffers.
      * @param l list of socket buffer control blocks to store data in.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes read.
      */
    int recv(const vector<SocketBufferCtrlBlk>& l);
    /** Socket API method.
      * Attempt to send data from supplied buffer. In this case, the 
	  * destination is the address specified in the last call to connect().
      * @param s socket buffer to from which to get data to send.
      * @param n number of bytes to attempt to send.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes sent.
      */
    inline int send(const SocketBuffer& s, const int& n);
    /** Socket API method.
      * Attempt to send data from supplied buffers. In this case, the 
	  * destination is the address specified in the last call to connect().
      * @param l socket buffer control blocks from which to get data to send.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes sent.
      */
    int send(const vector<SocketBufferCtrlBlk>& l);
    /** Socket API method.
      * Attempt to recv data from supplied buffer and store the destination.
      * @param from Place to store address of sender.
      * @param fromlen Length of from address.
      * @param s socket buffer to store data in.
      * @param n number of bytes to attempt to recv.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes received.
      */
	int recvfrom(struct sockaddr& from, int& fromlen, SocketBuffer& s, 
				const int& n);
    /** Socket API method.
      * Attempt to send data from supplied buffer to specified destination.
      * @param ip IP address to send data to.
      * @param port port to send data to.
      * @param s socket buffer to from which to get data to send.
      * @param n number of bytes to attempt to send.
      * @return 0: on success, positive integer on success. On success
      *         the value returned is the number of bytes sent.
      */
    int sendto(const struct sockaddr& to, const int& tolen, 
				const SocketBuffer& s, const int& n);
    /** Socket API method.
      * @doc close socket connection.
      * @return 0: on success, -1: on failure.
      */
    void close();
    /** 
      * @doc Test method.
      */
    static bool test();
};

ostream& operator<<(ostream& os, const UDPSocket& s);

#endif

