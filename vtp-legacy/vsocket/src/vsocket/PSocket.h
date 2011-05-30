/*
 *  PSocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Fri Mar 19 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef PSOCKET_H
#define PSOCKET_H

#include <common.h>
#include <list>
#include <Socket.h>
#include <TCPSocket.h>
#include <stdexcept>
#include <sys/select.h>

/** Used for reporting errors */
class PSocketError: public runtime_error {
public:
    PSocketError(const string& msg): runtime_error(msg) { }
};

/** Parallel sockets.
  * This class is used to implement parallel TCP sockets across which data
  * can be striped. Intended use is to improve TCP performance in a reasonably
  * TCP-friendly manner across wide area networks.
  */
class PSocket: public Socket 
{
	/** Socket list.
	  * This contains a list of the TCP sockets that are used to transmit the
	  * data.
	  */
    list<TCPSocket> _sock_list;
	/** Socket Control Block list.
	  * This contains a list of socket control blocks that associated with
	  * each TCPSocket.
	  */
    list<SocketBufferCtrlBlk> _sock_ctrlblk_list;
	/** Iterator.
	  * An iterator for traversing the list of TCPSockets.
	  */
    list<TCPSocket>::iterator _sock_list_iter;
	/** Socket.
	  * The TCPSocket used by a server to accept incoming connections.
	  */
    TCPSocket _accept_sock;
	/** Data member.
	  * Used to store the number of ports (i.e. the total number of sockets
	  * to be used to stripe the data across.
	  */
    int _num_ports;
    int _mtu;
	fd_set _read_fds;
	fd_set _write_fds;
	int _nfds;
public:
	// Constructors/Destructors
    PSocket(const int& num_ports, const int& mtu);
    PSocket(const PSocket &s);
    virtual ~PSocket();
    PSocket& operator=(const PSocket& s);
	// Member access.
    const int get_mtu() const;
    const int get_num_ports() const;
    // Socket API.
    void bind(const string& ip, const int& port);
    void listen();
    void accept();
    void connect(const string& ip, const int& port);
    int recv(SocketBuffer& s, const int& n);
    // int mrecv(vector<SocketBuffer>& sl, const int& n);
    int send(const SocketBuffer& s, const int& n);
    // int msend(const vector<SocketBuffer>& sl, const int& n);
    void close();
    int shutdown(const int& howto);
	// Options.
    int set_so_reuseaddr(const int& yes);
    int get_so_reuseaddr() const;
    int set_so_linger(const int& onoff, const int& time);
    const struct linger& get_so_linger() const;
    int set_so_rcvbuf(const int& sz);
    int get_so_rcvbuf() const;
    int set_so_sndbuf(const int& sz);
    int get_so_sndbuf() const;
    int set_so_rcvlowat(const int& m);
    int get_so_rcvlowat() const;
    int set_so_sndlowat(const int& m);
    int get_so_sndlowat() const;
	int set_tcp_nodelay(const int& m);
    int get_tcp_nodelay() const;
	int set_blocking(const int& b);
    int get_blocking() const;
    static bool test();
};

#endif

