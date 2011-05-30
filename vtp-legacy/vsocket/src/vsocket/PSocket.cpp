/*
 *  PSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Fri Mar 19 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <sys/socket.h>
#include <iostream>
#include <exception>
#include "PSocket.h"

PSocket::PSocket(const int& num_ports, const int& mtu)
{
	START_FUNC("PSocket")
    _num_ports=num_ports;
    _mtu=mtu;
	_nfds=_num_ports;
	FD_ZERO(&_read_fds);
	FD_ZERO(&_write_fds);
#if 0
	if (_sock_list.size()<(num_ports+3))
		_sock_list.resize(num_ports+3);	
	if (_sock_ctrlblk_list.size()<(num_ports+3))
		_sock_ctrlblk_list.resize(num_ports+3);	
#endif
	END_FUNC("PSocket")
}

PSocket::PSocket(const PSocket &s)
{
	START_FUNC("PSocket")
	_num_ports=s.get_num_ports();
	_mtu=s.get_mtu();
	_nfds=_num_ports;
	FD_ZERO(&_read_fds);
	FD_ZERO(&_write_fds);
#if 0
	if (_sock_list.size()<(_num_ports+3))
		_sock_list.resize(_num_ports+3);	
	if (_sock_ctrlblk_list.size()<(_num_ports+3))
		_sock_ctrlblk_list.resize(_num_ports+3);	
#endif
	END_FUNC("PSocket")
}

PSocket::~PSocket()
{
	START_FUNC("PSocket")
	END_FUNC("PSocket")
}

PSocket& PSocket::operator=(const PSocket& s)
{
    return(*this);
}

const int PSocket::get_mtu() const
{
    return(_mtu);
}

const int PSocket::get_num_ports() const
{
    return(_num_ports);
}

void PSocket::bind(const string& ip, const int& port)
{
	START_FUNC("PSocket")
    try {
		_accept_sock.bind(ip, port);
    } catch (...) {
        DEBUG_FUNC("PSocket") << "    Error in bind()\n";
    }
	END_FUNC("PSocket")
}

void PSocket::listen()
{
	START_FUNC("PSocket")
    try {
		_accept_sock.listen();
    } catch (...) {
        DEBUG_FUNC("PSocket") << "    Error in listen()\n";        
    }
	END_FUNC("PSocket")
}

void PSocket::accept()
{
	START_FUNC("PSocket")
    try {
		for (int i=0; i<_num_ports; i++) {
			int ret=_accept_sock.accept();
			if (ret==-1) {
				DEBUG_FUNC("PSocket") << "    accept returned error\n";
				return;
			}
			TCPSocket s(ret, _mtu);
			_sock_list.push_back(s);
			SocketBufferCtrlBlk scb;
			_sock_ctrlblk_list.push_back(scb);
			FD_SET(s.get_sockd(), &_read_fds);
			FD_SET(s.get_sockd(), &_write_fds);
            DEBUG_FUNC("PSocket") << "    accept sock list\n";
            DEBUG_FUNC("PSocket") << "         _sockd=" << s.get_sockd()  << "\n";
		}
        // Important! Have to set back to the beginning!
        _sock_list_iter=_sock_list.begin();
    } catch (...) {
        DEBUG_FUNC("PSocket") << "    Error in accept()\n";
    }
	END_FUNC("PSocket")
}

void PSocket::connect(const string& ip, const int& port)
{
	START_FUNC("PSocket")
    try {
		for (int i=0; i<_num_ports; i++) {
			TCPSocket s(_mtu);
			s.connect(ip, port);
			_sock_list.push_back(s);
			SocketBufferCtrlBlk scb;
			_sock_ctrlblk_list.push_back(scb);
			FD_SET(s.get_sockd(), &_read_fds);
			FD_SET(s.get_sockd(), &_write_fds);
		}
        // Important! Have to set back to the beginning!
        _sock_list_iter=_sock_list.begin();
    } catch (...) {
        DEBUG_FUNC("PSocket") << "    Error in connect to: " << ip << ":" << port
        << "\n";
    }
	END_FUNC("PSocket")
}

int PSocket::recv(SocketBuffer& s, const int& n)
{
	START_FUNC("PSocket")
	++_recv_calls;
    int bytes_rcvd=_sock_list_iter->recv(s, n);
	if (bytes_rcvd<=0)
		return(bytes_rcvd);
	if (bytes_rcvd<n)
		// short recv(). return without incrementing socket.
		return(bytes_rcvd);
	_bytes_rcvd+=bytes_rcvd;
    ++_sock_list_iter;
    if (_sock_list_iter==_sock_list.end())
    	_sock_list_iter=_sock_list.begin();
	END_FUNC("PSocket")
    return(bytes_rcvd);
}
#if 0
int PSocket::mrecv(vector<SocketBuffer>& sl, const int& n)
{
	++_recv_calls;
	int total_bytes_left=n;
	for (int i=3; i<3+_nfds; ++i) {
		_sock_ctrlblk_list[i]._bytes_rcvd=0;
		if (total_bytes_left>=_mtu) {
			_sock_ctrlblk_list[i]._bytes_to_rcv=_mtu;
			total_bytes_left-=_mtu;
		} else if (total_bytes_left>0) {
			_sock_ctrlblk_list[i]._bytes_to_rcv=total_bytes_left;
		} else {
			_sock_ctrlblk_list[i]._bytes_to_rcv=0;
		}
	}
	total_bytes_left=n;
	// Recv loop.
	do {
		fd_set read_fds=_read_fds;
		if (select(_nfds, &read_fds, NULL, NULL, NULL)!=0) {
			// Error in select.
			switch (errno) {
			case EBADF:
				error_logger() << "    mrecv(): bad file descriptor\n";
				break;
			case EINTR:
				DEBUG_FUNC("PSocket") << "    mrecv(): interrupted select call\n";
				break;
			case EINVAL:
				error_logger() << "    mrecv(): invalid time limit\n";
				break;
			}
		} else {
			// Select returned ok. Start reading data from selectable sockets.
			for (int i=3; i<3+_nfds; ++i) {
				if (FD_ISSET(i, &read_fds)) {
					int& bytes_to_rcv=_sock_ctrlblk_list[i]._bytes_to_rcv;
					int& bytes_rcvd=_sock_ctrlblk_list[i]._bytes_rcvd;
					if (bytes_to_rcv-bytes_rcvd<=0)
						continue;
					int rcvd=_sock_list[i].recv(sl[i], bytes_rcvd, 
													bytes_to_rcv-bytes_rcvd);
					if (rcvd<=0) {
						error_logger() << "    mrecv(): sock down\n";
						break;
					}
					bytes_rcvd+=rcvd;
					total_bytes_left-=bytes_rcvd;
				}
			}
		}
	} while (total_bytes_left>0);
    return(n);
}
#endif

int PSocket::send(const SocketBuffer& s, const int& n)
{
	++_send_calls;
    int bytes_sent=_sock_list_iter->send(s, n);
	if (bytes_sent<=0)
		return(bytes_sent);
	if (bytes_sent<n)
		// short send(). return without incrementing socket.
		return(bytes_sent);
	_bytes_sent+=bytes_sent;
    ++_sock_list_iter;
    if (_sock_list_iter==_sock_list.end())
   		_sock_list_iter=_sock_list.begin();
    return(bytes_sent);
}

#if 0
int PSocket::msend(const vector<SocketBuffer>& sl, const int& n)
{
	++_send_calls;
	int total_bytes_left=n;
	for (int i=3; i<3+_nfds; ++i) {
		_sock_ctrlblk_list[i]._bytes_sent=0;
		if (total_bytes_left>=_mtu) {
			_sock_ctrlblk_list[i]._bytes_to_send=_mtu;
			total_bytes_left-=_mtu;
		} else if (total_bytes_left>0) {
			_sock_ctrlblk_list[i]._bytes_to_send=total_bytes_left;
		} else {
			_sock_ctrlblk_list[i]._bytes_to_send=0;
		}
	}
	total_bytes_left=n;
	// Recv loop.
	do {
		fd_set write_fds=_write_fds;
		if (select(_nfds, NULL, &write_fds, NULL, NULL)!=0) {
			// Error in select.
			switch (errno) {
			case EBADF:
				error_logger() << "    msend(): bad file descriptor\n";
				break;
			case EINTR:
				DEBUG_FUNC("PSocket") << "    msend(): interrupted select call\n";
				break;
			case EINVAL:
				error_logger() << "    msend(): invalid time limit\n";
				break;
			}
		} else {
			// Select returned ok. Start reading data from selectable sockets.
			for (int i=3; i<3+_nfds; ++i) {
				if (FD_ISSET(i, &write_fds)) {
					int& bytes_to_send=_sock_ctrlblk_list[i]._bytes_to_send;
					int& bytes_sent=_sock_ctrlblk_list[i]._bytes_sent;
					if (bytes_to_send-bytes_sent<=0)
						continue;
					int sent=_sock_list[i].send(sl[i], bytes_sent, 
													bytes_to_send-bytes_sent);
					if (sent<=0) {
						error_logger() << "    msend(): sock down\n";
						break;
					}
					bytes_sent+=sent;
					total_bytes_left-=bytes_sent;
				}
			}
		}
	} while (total_bytes_left>0);
    return(n);
}
#endif

void PSocket::close()
{
	START_FUNC("PSocket")
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        _sock_list_iter->close();
    _accept_sock.close();
	END_FUNC("PSocket")
}

int PSocket::shutdown(const int& howto)
{
	START_FUNC("PSocket")
    int ret=0;
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->shutdown(howto);
    ret+=_accept_sock.shutdown(howto);
    if (ret!=0)
        error_logger() << "    Unable to shutdown all TCP connections\n";
	// Restore old _sock_list_iter in case application still wants to 
	// read the socket after doing a shutdown(SHUT_WR).
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

// Options.
int PSocket::set_so_reuseaddr(const int& yes)
{
	START_FUNC("PSocket")
    _so_reuseaddr=yes;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_reuseaddr(yes);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_reuseaddr on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

int PSocket::get_so_reuseaddr() const
{
        return(_so_reuseaddr);
}

int PSocket::set_so_linger(const int& onoff, const int& time)
{
	START_FUNC("PSocket")
    _so_linger.l_onoff=onoff;
    _so_linger.l_linger=time;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_linger(onoff, time);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_linger on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

const struct linger& PSocket::get_so_linger() const
{
        return(_so_linger);
}

int PSocket::set_so_rcvbuf(const int& sz)
{
	START_FUNC("PSocket")
    _so_rcvbuf=sz;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_rcvbuf(sz);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_recvbuf on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
	return(ret);
}

int PSocket::get_so_rcvbuf() const
{
        return(_so_rcvbuf);
}

int PSocket::set_so_sndbuf(const int& sz)
{
	START_FUNC("PSocket")
    _so_sndbuf=sz;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_sndbuf(sz);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_sndbuf on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
	return(ret);
}

int PSocket::get_so_sndbuf() const
{
        return(_so_sndbuf);
}

int PSocket::set_so_rcvlowat(const int& m)
{
	START_FUNC("PSocket")
    _so_rcvlowat=m;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_rcvlowat(m);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_linger on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

int PSocket::get_so_rcvlowat() const
{
	return(_so_rcvlowat);
}

int PSocket::set_so_sndlowat(const int& m)
{
	START_FUNC("PSocket")
    _so_rcvlowat=m;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_so_sndlowat(m);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_so_linger on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

int PSocket::get_so_sndlowat() const
{
	return(_so_sndlowat);
}

int PSocket::set_tcp_nodelay(const int& n)
{
	START_FUNC("PSocket")
    _tcp_nodelay=n;
	int ret=0;
	// Save old value.
#if 0
	vector<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#else
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
#endif
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_tcp_nodelay(n);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_tcp_nodelay on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

int PSocket::get_tcp_nodelay() const
{
	return(_tcp_nodelay);
}

int PSocket::set_blocking(const int& b)
{
	START_FUNC("PSocket")
    _blocking=b;
	int ret=0;
	list<TCPSocket>::iterator old_sock_list_iter=_sock_list_iter;
    for (_sock_list_iter=_sock_list.begin(); _sock_list_iter!=_sock_list.end();
         ++_sock_list_iter)
        ret+=_sock_list_iter->set_blocking(b);
    if (ret!=0)
        error_logger() << 
				"    Unable to set_blocking on all TCP connections\n";
	// Restore old value.
	_sock_list_iter=old_sock_list_iter;
	END_FUNC("PSocket")
    return(ret);
}

int PSocket::get_blocking() const
{
	return(_blocking);
}

static bool parent()
{
    PSocket s(8, 1024);
    string ip("0.0.0.0");
    int port(49002);
    s.bind(ip, port);
    s.listen();
    s.accept();
    SocketBuffer sbuf;
    sbuf.reserve(1024);
	for (int j=0; j<1000; j++) {
		(*Logsystem::debug_logger()) << "    psocket: recv-ing j==" << j << "\n";
    	int rcvd_bytes=s.recv(sbuf, 1024);
		(*Logsystem::debug_logger()) << "    psocket: rcvd_bytes==" << rcvd_bytes 
								<< "\n";
    	if (rcvd_bytes!=1024) {
			cerr << "    psocket: parent() short READ!!!\n";
        	return(false);
		}
    	for (int i=0; i<1024; i++) {
        	if (sbuf[i]!=i%10) {
				cerr << "    psocket: parent() invalid data!!!\n";
            	return(false);
			}
    	}
}
    s.close();
    return(true);
}

static bool child()
{
    string ip("127.0.0.1");
    int port(49002);
    PSocket c(8, 1024);
    usleep(2000000);
    c.connect(ip, port);
    SocketBuffer sbuf(1024);
    for (int i=0; i<1024; i++) {
        sbuf[i]=i%10;
    }
	int sent_bytes=0;

	for (int j=0; j<1000; j++) {
		cerr << "    psocket: send-ing j==" << j << endl;
    	sent_bytes=c.send(sbuf, 1024);
    	if (sent_bytes!=1024)
        	return(false);
		usleep(100000);
	}
    c.close();
    exit(0);
}

bool PSocket::test()
{
    pid_t child_id=fork();
    bool ret=false;
    if (child_id==0)
        ret=parent();
    else
        ret=child();
    return(ret);
}



