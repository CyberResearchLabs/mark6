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


#ifndef SOCKET_H
#define SOCKET_H

#include <common.h>
#include <string>
#include <vector>
#include <deque>
#include <map>
#include <iostream>
#ifdef LINUX
#include <unistd.h>
#endif // LINUX
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <Object.h>
#include <Utils.h>

using namespace std;

/** Socket module
  * @doc This module implements base level socket functionality. It
  * provides a number of key classes that are utilized by derived classes.
  * @author David Lapsley (dlapsley@haystack.mit.edu)
  * 
  */

/** Minimal implementation of a socket buffer. 
  * This is just an STL vector. May extend this in the future.	
  */
typedef vector<unsigned char> SocketBuffer;

/** Socket buffer control block.
  * Used to maintain state information along with the data itself.
  */
struct SocketBufferCtrlBlk {
	/** Bytes read originally read into socket buffer(_sb). */
    int _bytes_read;
	/** Bytes that have been transmitted from the buffer(_sb). */
    int _bytes_sent;
	/** Internal storage of data. */
    SocketBuffer _sb;
	/** Timer used for transmission timeouts */
	Timer _timer;
	/** Constructor.
	  * @param size maximum number of bytes to store in buffer.
	  */
    SocketBufferCtrlBlk(const int size);
	/** Default Constructor.  */
    SocketBufferCtrlBlk();
	/** Copy Constructor.  
	  * @param s buffer to be iniitalized from. 
	  */
    SocketBufferCtrlBlk(const SocketBufferCtrlBlk& s);
	/** Assignment operator
	  * @param s buffer to be initialized from.
	  */
    SocketBufferCtrlBlk& operator=(const SocketBufferCtrlBlk& s);
	/** Destructor */
    ~SocketBufferCtrlBlk();
	/**  
	  * @return number of bytes read. 
	  */
    int get_bytes_read() const;
	/** 
	  * @return number of bytes sent. 
	  */
    int get_bytes_sent() const;
	/** 
	  * @return reference to socket buffer.
	  */
    const SocketBuffer& get_sb() const;
};

/** A deque of socket buffer control blocks */
typedef deque<SocketBufferCtrlBlk> SocketBufferCtrlBlkList;
    
/** This is used to hold SocketBufferCtrlBlks that have been transmitted
  * but are still pending acknowledgment.
  */
typedef map<int, SocketBufferCtrlBlk> SocketBufferCtrlBlkMap;


/** Used for reportin errors */
class SocketException {
private:
    string _message;
public:
    SocketException(const string& s):
	_message(s) { }
    ~SocketException()
    {
    }
    const char* what() {
        return (_message.c_str());
    }
};

/** Base level socket class.
  * All other socket classes are derived from this one. This class inherits
  * from the Object class.
  */
class Socket: public Object {
protected:
	/** @doc Encapsulated socket descriptor.  */
    int _sockd;
	/** @doc Socket type: SOCK_DGRAM(UDP) or SOCK_STREAM(TCP). Support for
	  * othe types may be added in the future.
	  */
	int _type;
	/** @doc Maximum Transmission Unit for this socket.  */
	int _mtu;
	/** @doc Total number of bytes sent during the lifetime of this socket.  */
	int _bytes_sent;
	/** @doc Total number of bytes received during the lifetime of this 
	  * socket.  
	  */
    int _bytes_rcvd;
	/** @doc Total number of send calls made during the lifetime of this 
	  * socket.
      */
    int _send_calls;
	/** @doc Total number of recv calls made during the lifetime of this socket.
      */
    int _recv_calls;
	/** @doc Current value of socket option.  */
	int _so_reuseaddr;
	/** @doc Current value of socket option.  */
	struct linger _so_linger;
	/** @doc Current value of socket option.  */
	int _so_rcvbuf;
	/** @doc Current value of socket option.  */
	int _so_sndbuf;
	/** @doc Current value of socket option.  */
	int _so_rcvlowat;
	/** @doc Current value of socket option.  */
	int _so_sndlowat;
	/** @doc Current value of tcp option.  */
	int _tcp_nodelay;
	/** @doc Current value of ip option.  */
	int _ip_tos;
	/** @doc Current value of socket blocking option.  */
	int _blocking;
	/** @doc Stored value of blocking flags. */
	int _blocking_flags;
public:
	/** @doc Constant flag value used in set_blocking().  */
	static const int BLOCKING;
	/** @doc Constant flag value used in set_blocking().  */
	static const int NONBLOCKING;
	/** @doc Constant flag value used in select() */
	static const int SEL_READ;
	/** @doc Constant flag value used in select() */
	static const int SEL_WRITE;
	/** @doc Constant flag value used in select() */
	static const int SEL_EXCEPT;
	/** Constructor. 
	  * This constructor will create a socket of the 
	  * supplied type by calling the C socket() function.
	  * @param type type of socket: currently either SOCK_STREAM or 
	  *        SOCK_STREAM. 
	  * @return none
	  * @doc constructs a Socket object.
	  */
    Socket(const int& type);
	/** Constructor. 
	  * This constructor does NOT create a socket. It assigns 
	  * the internal socket descriptor member (_sockd) to the supplied 
	  * socket descriptor.
	  * @param type type of socket (as per C socket() call).
	  * @param s pre-opened socket descriptor (e.g. as returned from 
	  *        C socket() or accept() call). 
      * @param m socket Maximum Transmission Unit.
	  * @return none
	  * @doc constructs a Socket object.
	  */
	Socket(const int& type, const int& s, const int&m);
	/** Default constructor. 
	  * Maximum Transmission Unit is set to 1024 by 
	  * default. Socket type is set to SOCK_STREAM.
	  */
    Socket();
	/** Destructor. Important! The destructor does not close() the socket. 
	  * This is because there are situations when it is necessary to have an
	  * opened socket survive the Socket object that created it.
      * Users of the Socket class are responsible for closing the 
      * any sockets created using the close() or shutdown() methods.
	  */
	virtual ~Socket();
	/** Access method.
	  * @return value of sockd.  
	  */
	int get_sockd() const;
	/** Access method.
 	  * @return value of type.  
	  */
	int get_type() const;
	/**  Access method.
	  * @return value of mtu.  
	  */
	int get_mtu() const;
	/** Access method.
	  * @return value of bytes sent.  
	  */
	int get_bytes_sent() const;
	/** Access method.
	  * @return value of bytes received.  
	  */
	int get_bytes_rcvd() const;
	/** Access method.
	  * @return value of send calls.  
	  */
	int get_send_calls() const;
	/** Access method.
	  * @return value of recv calls.  
	  */
	int get_recv_calls() const;
	/** Access method.
	  * @doc set SO_REUSEADDR socket option
	  * @param yes 0: allow address reuse, 1: disallow address reuse.
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_reuseaddr(const int& yes);
	/** Access method.
	  * @return value of so_reuseaddr.  
	  */
	int get_so_reuseaddr() const;
	/** Access method.
	  * @doc set SO_LINGER socket option
	  * @param onoff 0: disable linger option, 1: enable linger option.
	  * @param time if linger enabled, how long to linger for. 
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_linger(const int& onoff, const int& time);
	/** Access method.
	  * @return value of so_linger.  
	  */
	const struct linger& get_so_linger() const;
	/** Access method.
	  * @doc set SO_RCVBUF socket option
	  * @param sz size to set so_rcvbuf.
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_rcvbuf(const int& sz);
	/** Access method.
	  * @return value of so_rcvbuf.  
	  */
	int get_so_rcvbuf() const;
	/** @doc set SO_SNDBUF socket option
	  * @param sz size to set so_sndbuf
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_sndbuf(const int& sz);
	/** Access method.
	  * @return value of so_sndbuf.  
	  */
	int get_so_sndbuf() const;
	/** Access method.
	  * @doc set SO_RCVLOWAT socket option
	  * @param m value to set rcv_lowat
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_rcvlowat(const int& m);
	/** Access method.
	  * @return value of rcv_lowat.  
	  */
	int get_so_rcvlowat() const;
	/** @doc set SO_SNDLOWAT socket option
	  * @param m size to set so_sndlowat
	  * @return 0: on success, -1: on failure.
	  */
	int set_so_sndlowat(const int& m);	
	/** Access method.
	  * @return value of so_sndlowat.  
	  */
	int get_so_sndlowat() const;	
	/** Access method.
	  * @doc set TCP_NODELAY socket option
	  * @param n value to set nodelay to. 0: allow delayed sending, 1: disallow
	  * 	   delayed sending.
	  * @return 0: on success, -1: on failure.
	  */
	int set_tcp_nodelay(const int& n);	
	/** Accessm method.
	  * @return value of tcp_nodelay.  
	  */
	int get_tcp_nodelay() const;	
	/** Access method.
	  * @doc set O_NONBLOCK socket option
	  * @param b blocking mode: Socket::blocking: node is blocking (default),
	  * Socket::nonblocking: node is non-blocking.
	  * @return 0: on success, -1: on failure.
	  */
	int set_blocking(const int& b);
	/** Access method.
	  * @doc get current blocking state o fthe socket
	  * @return current socket blocking mode.
	  */
	int get_blocking() const;
	/** Access method.
	  * @doc Determine if socket is ready for reading/writing/exception.
	  * @param rwe Read/Write/Exception checking values correspond to:
	  * Socket::SEL_READ, SEL_WRITE, SEL_EXCEPT.
	  * @return true: if socket ready, false if not.
	  */
	virtual int select(const int rwe, double to);
	/** Access method.
	  * @doc shutdown connection as per standard socket library.
	  * @param how method to use to shutdown socket: SHUT_RD, SHUT_WR, 
	  * SHUT_RDWR.
	  */
    const int shutdown(const int& how);
	/** Access method.
	  * @doc close socket connection.
	  * @return 0: on success, -1: on failure.
	  */
    const int close();
};

#endif


