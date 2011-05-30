/*
 *  VRSocketA.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <VRSocketA.h>
#include <Utils.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <sys/uio.h>
#include <unistd.h>
#include <syslog.h>


const int VRSocketA::receiver=0;
const int VRSocketA::sender=1;
const int HDR_SIZE=12;
const int RTP_VERSION=12;

void* VRSocketA::thread_func()
{
	START_FUNC("VRSocketA")
	openlog("VRSocketA", LOG_NDELAY | LOG_PID, LOG_DAEMON);
	switch (_mode) {
	case VRSocketA::receiver:
		recv_proc();
		break;
	case VRSocketA::sender:
		send_proc();
		break;
	default:
		DEBUG_FUNC("VRSocketA") << "Unknown _mode: " << _mode << "\n"; 
	}
	closelog();
	return(NULL);
	END_FUNC("VRSocketA")
}

void VRSocketA::recv_proc()
{
	START_FUNC("VRSocketA")
	struct timeval to;
	to.tv_sec=10; to.tv_usec=0;
	int total_bytes_rcvd=0, total_bytes_sent=0, data_bytes_rcvd=0;
	uint16_t length=0, sn=0, rn=0;
	uint32_t ts=0, ssrc=0;
	int discards=0;
	Timer send_timer, duration_timer;
	int IN_FD=0, OUT_FD=0, BUF_SIZE=0;
	uint8_t* buf=NULL, *fbuf=NULL, *hbuf=NULL;
	struct iovec vbuf[2];
	while (true) {
		switch (_state) {
		case PS_INIT:
			{
				int policy=SCHED_FIFO;
				struct sched_param param;
				param.sched_priority=99;
				int ret=pthread_setschedparam(pthread_self(), policy, &param);
				if (ret!=0) {
					syslog(LOG_ERR, "setshcedparam: %s", strerror(errno));
				}
				syslog(LOG_DEBUG, "---- PS_INIT ----\n");
				// Initialize connections.
				_udp_sock.set_so_rcvbuf(4000000);
				_udp_sock.set_so_rcvlowat(_mtu);
				printf("Binding to 0.0.0.0/%d\n", _udp_port);
				_udp_sock.bind("0.0.0.0", _udp_port);
				printf("Connecting to 127.0.0.1/%d\n", _tcp_port);
				_tcp_sock.connect("127.0.0.1", _tcp_port);
				IN_FD=_udp_sock.get_sockd();
				OUT_FD=_tcp_sock.get_sockd();
				BUF_SIZE=_mtu;
				printf("IN_FD:   %d\n", IN_FD);
				printf("OUT_FD:  %d\n", OUT_FD);
				printf("MTU:     %d\n", _mtu);
				buf=new uint8_t[BUF_SIZE];
				fbuf=new uint8_t[BUF_SIZE];
				hbuf=new uint8_t[HDR_SIZE];
				for (int i=0; i<BUF_SIZE; i+=4) {
					fbuf[i]=0x11;
					fbuf[i+1]=0x22;
					fbuf[i+2]=0x33;
					fbuf[i+3]=0x44;
				}
				send_timer.start();
				duration_timer.start();
				_state=PS_RX_DATA;
				vbuf[0].iov_base=(char*)hbuf;
				vbuf[0].iov_len=HDR_SIZE;
				vbuf[1].iov_base=(char*)buf;
				vbuf[1].iov_len=BUF_SIZE;
			}
			break;
		case PS_RX_DATA:
			{
				syslog(LOG_DEBUG, "---- PS_RX_DATA ----\n");
    			int ret=::readv(IN_FD, vbuf, 2);
				if (ret<HDR_SIZE) {
					syslog(LOG_INFO, "RX_DATA: %s\n", strerror(errno));
					syslog(LOG_ERR, "RX_DATA: Invalid recv(): ret==%d", ret);
					continue;
				}
				// Valid length read.
				total_bytes_rcvd+=ret;
				// Process the packet.
				RTPHdr* hdr=(RTPHdr*)vbuf[0].iov_base;
				sn=ntohs(hdr->_sn);
				ts=ntohl(hdr->_ts);
				ssrc=ntohl(hdr->_ssrc);
				switch (hdr->_pt) {
				case PT_DATA:
					syslog(LOG_DEBUG, "RX_DATA: Received PT_DATA");
					length=_mtu;
					break;
				case PT_FIN:
					syslog(LOG_DEBUG, "RX_DATA: Received PT_FIN");
					_state=PS_FIN;
					continue;
					break;
				default:
					syslog(LOG_ERR, "RX_DATA: Received unknown packet");
					continue;
				}
				// print_pkt_hdr(sbcb_vec[0]._sb);
				if (ret!=(length+HDR_SIZE)) {
					syslog(LOG_ERR, "Short read: ret==%d\n", ret);
					syslog(LOG_ERR, "Short read: length==%d\n", length);
					data_bytes_rcvd+=ret-HDR_SIZE;
				} else {
					data_bytes_rcvd+=length;
				}
				_state=PS_TX_DATA;
			}
			break;
		case PS_TX_DATA:
			{
				syslog(LOG_DEBUG, "---- PS_TX_DATA ----\n");
				// Update sequence number and send data.
				int ret=0;
				if (rn==sn) {
					syslog(LOG_DEBUG, "Accept packet: sn==%d\n", sn);
    				ret=::write(OUT_FD, (char*)vbuf[1].iov_base, length);
					if (ret<0)
						syslog(LOG_ERR, "Short send: ret==%d\n", ret);
				} else {
					while (rn!=sn) {
						if (rn==sn) {
							syslog(LOG_DEBUG, "Accept packet: sn==%d\n", sn);
    						unsigned int r=::write(OUT_FD, (char*)vbuf[1].iov_base, 
										length);
							if (r<vbuf[1].iov_len) 
								syslog(LOG_ERR, "Short send: ret==%d\n", r);
							ret+=r;
							break;
						} else {
							syslog(LOG_DEBUG, "Discard packet: sn==%d\n", rn);
    						unsigned int r=::write(OUT_FD, fbuf, _mtu);
							if (r<(unsigned int)_mtu) 
								syslog(LOG_ERR, "Short send: ret==%d\n", r);
							ret+=r;
							++discards;
						}
						++rn;
					}
				} // rn==sn
				++rn;	// rn==sn+1
				total_bytes_sent+=ret; // Valid length read.
				_state=PS_RX_DATA;
			}
			break;
		case PS_FIN:
			{
				syslog(LOG_NOTICE, "---- PS_FIN ----");
				syslog(LOG_NOTICE, "Total discard packets==%d", discards);
				syslog(LOG_NOTICE, "Total discard bytes==%d", discards*1024);
				duration_timer.stop();
				const double RX_TIME=duration_timer.elapsed();
				printf("RX_DATA: duration==%f\n", RX_TIME);
				printf("RX_DATA: total_bytes_rcvd==%d\n", total_bytes_rcvd);
				printf("RX_DATA: data_bytes_rcvd==%d\n", data_bytes_rcvd);
				printf("RX_DATA: discards==%d\n", discards);
				printf("RX_DATA: Average rate==%f Mbps\n",
						8.0*double(total_bytes_rcvd)/(RX_TIME*1e6));
				_state=PS_APP_FIN;
				return;
			}
			break;
		default:
			syslog(LOG_NOTICE, "Unknown state.");
			break;
		}
	}
	END_FUNC("VRSocketA")
}

void VRSocketA::send_proc()
{
	START_FUNC("VRSocketA")
	struct timeval to;
	to.tv_sec=30; to.tv_usec=0;
	int total_bytes_sent=0, total_bytes_rcvd=0, data_bytes_sent=0;
	int ret=0;
	Timer send_timer, duration_timer;
	int length=0;
	uint16_t sn_max=0;
	uint32_t ssrc=3, ts=0;
	uint8_t pt=PT_DATA;
	const unsigned int PEAK_RATE=_tspec._peak_rate;
	const double PACKET_INTERVAL=double(_mtu)*8.0/double(PEAK_RATE);
	const unsigned int PACKET_INTERVAL_US=int(PACKET_INTERVAL*1e6);
	fd_set read_fds;
	int IN_FD=0, OUT_FD=0;
	int RBUF_SIZE=_mtu;
	uint8_t* buf=NULL, *hbuf=NULL;
	struct iovec vbuf[2];
	struct timespec req, rem;
	req.tv_sec=0;
	req.tv_nsec=PACKET_INTERVAL_US*1e3;
	while (true) {
		switch (_state) {
		case PS_INIT:
			{
				int policy=SCHED_FIFO;
				struct sched_param param;
				param.sched_priority=99;
				int ret=pthread_setschedparam(pthread_self(), policy, &param);
				if (ret!=0) {
					syslog(LOG_ERR, "setshcedparam: %s", strerror(errno));
				}
				syslog(LOG_DEBUG, "---- PS_INIT ----\n");
				_tcp_sock.set_so_rcvbuf(4000000);
				// FIXME
				// _udp_sock.set_so_sndbuf(4000000);
				printf("UDP lowat=%d\n", _udp_sock.get_so_sndlowat());
				_udp_sock.set_so_sndlowat(_mtu*2);
				printf("UDP lowat=%d\n", _udp_sock.get_so_sndlowat());
				_tcp_sock.set_so_rcvlowat(_mtu);
				buf=new uint8_t[RBUF_SIZE];
				hbuf=new uint8_t[HDR_SIZE];
				if (_tvg) {
					for (int i=0; i<_mtu; ++i)
						buf[i]=i%10;
					IN_FD=-1;
					printf("Test Vector Generator enabled\n");
				} else {
					// Initialize incoming connections.
					printf("Binding to 0.0.0.0/%d\n", _tcp_port);
					_tcp_accept_sock.bind("0.0.0.0", _tcp_port);
					printf("Listening\n");
					_tcp_accept_sock.listen();
					printf("Accepting incoming connection\n");
					int sd=_tcp_accept_sock.accept();
					if (sd<=0) {
						syslog(LOG_ERR, "Invalid accept\n");
						return;
					}
					_tcp_sock=TCPSocket(sd, _mtu);
					IN_FD=_tcp_sock.get_sockd();
				}
				printf("Connecting to %s/%d\n", _remote_ip.c_str(), _udp_port);
				_udp_sock.connect(_remote_ip, _udp_port);
				OUT_FD=_udp_sock.get_sockd();
				printf("IN_FD:   %d\n", IN_FD);
				printf("OUT_FD:  %d\n", OUT_FD);
				printf("MTU:     %d\n", _mtu);
				RBUF_SIZE=_mtu;
				send_timer.start();
				duration_timer.start();
				vbuf[0].iov_base=(char*)hbuf;
				vbuf[0].iov_len=HDR_SIZE;
				vbuf[1].iov_base=(char*)buf;
				_state=PS_RX_DATA;
			}
			break;
		case PS_RX_DATA:
			{
				syslog(LOG_NOTICE, "---- PS_RX_DATA ----\n");
				if (_tvg) { // use tvg data.
					length=_mtu;
					_state=PS_TX_DATA;
					continue;
				}
				FD_ZERO(&read_fds);
				FD_SET(IN_FD, &read_fds);
				if (select(IN_FD+1, &read_fds, NULL, NULL, &to)==0) {
					syslog(LOG_DEBUG, "SELECT: %s", strerror(errno));
					_state=PS_FIN;
					continue;
				}
				if (!FD_ISSET(IN_FD, &read_fds)) {
					syslog(LOG_INFO, "RX_DATA: recv() not rdy");
					continue;
				}
    			int ret=::read(IN_FD, buf, RBUF_SIZE);
				if (ret==0) {
					syslog(LOG_ERR, "RX_DATA: Short recv(): ret==%d", ret);
					_state=PS_FIN;
					continue;
				} else if (ret<0) {
					syslog(LOG_ERR, "RX_DATA: Error recv(): %s", strerror(errno));
					continue;
				} else if (ret<_mtu) {
					syslog(LOG_ERR, "RX_DATA: Short recv(): ret==%d", ret);
				}
				length=ret;
				// Valid read.
				total_bytes_rcvd+=ret;
				syslog(LOG_NOTICE, "RX_DATA: total bytes rcvd==%d",
								total_bytes_rcvd);
				_state=PS_TX_DATA;
			}
			break;
		case PS_TX_DATA:
			{
				syslog(LOG_NOTICE, "---- PS_TX_DATA ----\n");
				// We have something to send.
				if (build_data_hdr(hbuf, pt, sn_max, ts, ssrc, HDR_SIZE)!=0) {
					syslog(LOG_ERR, "TX_DATA: Unable to build data packet header");
					_state=PS_FIN;
					continue;
				}
				vbuf[1].iov_len=length;
				syslog(LOG_DEBUG, "TX_DATA: Packet to be sent");
				// print_pkt_hdr(sbcb_hdr._sb);
    			// int ret=writev(OUT_FD, sbcb_data, sbcb_hdr);
				if (nanosleep(&req, &rem)!=0)  {
					syslog(LOG_ERR, "TX_DATA: nanosleep intr");
					continue;
				}
    			ret=::writev(OUT_FD, vbuf, 2);
				if (ret<0) {
					syslog(LOG_NOTICE, "PS_TX_DATA: %s", strerror(errno));
					_state=PS_RX_DATA;
					continue;
				} else if (ret<(length+HDR_SIZE)) {
					syslog(LOG_NOTICE, "TX_DATA: Short send(): ret==%d", ret);
					_state=PS_FIN;
					continue;
				}
				// Valid send.
				total_bytes_sent+=ret;
				data_bytes_sent+=ret-HDR_SIZE;
				sn_max++;
				ts++;
				if (_tvg && total_bytes_sent>_tvg_sz) {
					_state=PS_FIN;
					continue;
				}
				_state=PS_RX_DATA;
			}
			break;
		case PS_FIN:
			{
				syslog(LOG_NOTICE, "---- PS_FIN ----");
				bool fin=false;
				int fin_count=10;
				while (fin!=true)
				{
					syslog(LOG_NOTICE, "FIN loop");
					if (build_fin_hdr(hbuf, ssrc, HDR_SIZE)!=0) {
						syslog(LOG_ERR, "FIN: Unable to build FIN packet header\n");
						continue;
					}
					syslog(LOG_INFO, "FIN packet to be sent");
					// print_pkt_hdr(sbcb_hdr._sb);
					vbuf[0].iov_base=(char*)hbuf;
					vbuf[0].iov_len=HDR_SIZE;
					vbuf[1].iov_base=(char*)buf;
					vbuf[1].iov_len=_mtu;
    				int ret=::writev(OUT_FD, vbuf, 2);
					if (ret<=0) {
						syslog(LOG_ERR, "FIN: Failed re-send!!!");
						continue;
					}
					// Successfully transmitted (at least) part of the packet.
					if (ret!=(_mtu+HDR_SIZE))
						syslog(LOG_ERR, "FIN: Short send: ret==%d", ret);
					if (--fin_count<=0)
						fin=true;
				}
				duration_timer.stop();
				const double TX_TIME=duration_timer.elapsed();
				printf("TX_DATA: Sent bytes==%d\n",  ret);
				printf("TX_DATA: total_bytes_sent==%d\n", total_bytes_sent);
				printf("TX_DATA: data_bytes_sent==%d\n", data_bytes_sent);
				printf("TX_DATA: Average rate==%f Mbps\n",
					8.0*double(total_bytes_sent)/(TX_TIME*1e6));
				_mode=PS_APP_FIN;
				return;
			}
			break;
		default:
			syslog(LOG_ERR, "send_proc(): unknown state");
			break;
		}
	}
	END_FUNC("VRSocketA")
}

VRSocketA::VRSocketA(const int& mode):
_mode(mode), _mtu(DEFAULT_MTU)
{
	START_FUNC("VRSocketA")
	_state=PS_INIT;
	_udp_sock.set_so_reuseaddr(1);
	_tcp_sock.set_so_reuseaddr(1);
	_tcp_accept_sock.set_so_reuseaddr(1);
	_tspec._peak_rate=DEFAULT_PEAK_RATE;
	_tvg=false;
	_tvg_sz=0;
	thread_create();
	END_FUNC("VRSocketA")
}

VRSocketA::VRSocketA(const int& mode, const int& m, const TSpec& t,
 const string& ip, const int& uport, const int& tport, const bool& tvg,
 const int& tvg_sz=0):
_mode(mode), _mtu(m), _tspec(t), _tvg(tvg), _tvg_sz(tvg_sz)
{
	START_FUNC("VRSocketA")
	_state=PS_INIT;
	_udp_sock.set_so_reuseaddr(1);
	_tcp_sock.set_so_reuseaddr(1);
	_tcp_accept_sock.set_so_reuseaddr(1);
	_udp_port=uport;
	_tcp_port=tport;
	_remote_ip=ip;
	thread_create();
	END_FUNC("VRSocketA")
}

VRSocketA::VRSocketA(const int& mode, const int& s, const int& m, 
					const TSpec& t, const bool& tvg, const int& tvg_sz=0):
_mode(mode), _mtu(m), _tspec(t), _tvg(tvg), _tvg_sz(tvg_sz)
{
	START_FUNC("VRSocketA")
	_state=PS_INIT;
	_udp_sock.set_so_reuseaddr(1);
	_tcp_sock.set_so_reuseaddr(1);
	_tcp_accept_sock.set_so_reuseaddr(1);
	thread_create();
	END_FUNC("VRSocketA")
}

VRSocketA::VRSocketA(const VRSocketA &s)
{
	START_FUNC("VRSocketA")
	_mtu=s._mtu;
	_state=s._state;
	_tspec=s._tspec;
	_udp_sock=s._udp_sock;
	_tcp_sock=s._tcp_sock;
	_udp_sock.set_so_reuseaddr(1);
	_tcp_sock.set_so_reuseaddr(1);
	_tcp_accept_sock.set_so_reuseaddr(1);
	_tvg=s._tvg;
	_tvg_sz=s._tvg_sz;
	thread_create();
	END_FUNC("VRSocketA")
}

VRSocketA::~VRSocketA()
{
	START_FUNC("VRSocketA")
    // This is an important feature of the VRSocketA class. Especially
    // for copying of VRSocketAs. VRSocketAs do *not* close their
	// sockets when they are destroyed.
	END_FUNC("VRSocketA")
}

VRSocketA& VRSocketA::operator=(const VRSocketA& s)
{
	START_FUNC("VRSocketA")
    _mtu=s._mtu;
    _tspec=s._tspec;
    _udp_sock=s._udp_sock;
    _tcp_sock=s._tcp_sock;
	_udp_sock.set_so_reuseaddr(1);
	_tcp_sock.set_so_reuseaddr(1);
	_tcp_accept_sock.set_so_reuseaddr(1);
	END_FUNC("VRSocketA")
    return(*this);
}

void VRSocketA::bind(const string& ip, const int& dport, const int& cport)
{
	START_FUNC("VRSocketA");
	_udp_sock.bind(ip, dport);
	_tcp_accept_sock.bind(ip, cport);
	END_FUNC("VRSocketA");
}

void VRSocketA::connect(const string& ip, const int& dport, const int& cport)
{
	START_FUNC("VRSocketA");
	if (dport>0) 
		_udp_sock.connect(ip, dport);
	if (cport>0)
		_tcp_sock.connect(ip, cport);
	perror("Connect\n");
	while (!_tcp_sock.select(Socket::SEL_WRITE, 2.0)<0)
		ERROR_FUNC("Waiting for connect\n");
	WARNING_FUNC("Got connect\n");
	END_FUNC("VRSocketA");
}

void VRSocketA::listen()
{
	START_FUNC("VRSocketA");
	_tcp_accept_sock.listen();
	END_FUNC("VRSocketA");
}

void VRSocketA::accept()
{
	START_FUNC("VRSocketA");
	int sd=_tcp_accept_sock.accept();
	if (sd<=0) {
		ERROR_FUNC("VRSocketA") << "invalid accept\n";
		return;
	}
	_tcp_sock=TCPSocket(sd, _mtu);
	END_FUNC("VRSocketA");
}

int VRSocketA::shutdown(const int howto)
{
	START_FUNC("VRSocketA")
	int ret=_udp_sock.shutdown(howto);
	ret+=_tcp_sock.shutdown(howto);
	ret+=_tcp_accept_sock.shutdown(howto);
	END_FUNC("VRSocketA")
    return(ret);
}

void VRSocketA::close()
{
	START_FUNC("VRSocketA")
	_udp_sock.close();
	_tcp_sock.close();
	_tcp_accept_sock.close();
	END_FUNC("VRSocketA")
}

int VRSocketA::get_send_buf_size()
{
	int ret=0;
	_send_mutex.lock();
	ret=_sockbufctrlblk_list.size();
	_send_mutex.unlock();
	return(ret);
}

ostream& operator<<(ostream& os, const VRSocketA& s)
{
	os << "VRSocketA dump\n";
	os << "-VRSocketA dump\n";
    return(os);
}

void VRSocketA::print_pkt_hdr(const SocketBuffer& sb)
{
	START_FUNC("VRSocketA")
	if (sb.size()<6)
		return;
	DEBUG_FUNC("VRSocketA") << "Header==" 
		<< sb[0] << ":" << sb[1] << ":"
		<< sb[2] << ":" << sb[3] << ":"
		<< sb[4] << ":" << sb[5] << "\n";
	DEBUG_FUNC("VRSocketA") << "Type==" 
		<< sb[1] << "\n";
	unsigned char hl=sb[2];
	unsigned char ll=sb[3];
	unsigned char hsn=sb[4];
	unsigned char lsn=sb[5];
	unsigned int length=(hl<<8)+ll;
	unsigned int sn=(hsn<<8)+lsn;
	DEBUG_FUNC("VRSocketA") << "Length==" 
		<<  length << "\n";
	DEBUG_FUNC("VRSocketA") << "SN==" 
		<<  sn << "\n";
	END_FUNC("VRSocketA")
}

int VRSocketA::build_ack_hdr(SocketBuffer& sb, const int& sn, const int& sz)
{
	START_FUNC("VRSocketA")
	if (sz<HDR_SIZE)
		return(-1);
	sb[0]='a';		// "Synch" word.
	sb[1]=PT_ACK;	// Packet type.
	sb[2]=(0);		// Data length (==0 for ACK).
	sb[3]=(0);		// Data length (==0 for ACK).
	sb[4]=(sn>>8);	// Sequence number MSB.
	sb[5]=(sn);		// Sequence number LSB.
	return(0);
	END_FUNC("VRSocketA")
}

int VRSocketA::build_data_hdr(SocketBuffer& sb, const int& sn, const int& sz,
							const int& length)
{
	START_FUNC("VRSocketA")
	if (sz<HDR_SIZE)
		return(-1);
	sb[0]='a';				// "Synch" word.
	sb[1]=PT_DATA;			// Packet type.
	sb[2]=(length>>8);		// Data length (==0 for ACK).
	sb[3]=(length);			// Data length (==0 for ACK).
	sb[4]=(sn>>8);			// Sequence number MSB.
	sb[5]=(sn);				// Sequence number LSB.
	return(0);
	END_FUNC("VRSocketA")
}

int VRSocketA::build_data_hdr(uint8_t* sb, const uint32_t& pt, 
							const uint16_t& sn,
							const uint32_t& ts,
							const uint32_t& ssrc,
							const int& sz) 
{
	if (sz<HDR_SIZE)
		return(-1);
	RTPHdr* h=(RTPHdr*)sb;
	h->_v=0x2;
	h->_pt=pt;
	h->_sn=htons(sn);
	h->_ts=htonl(ts);
	h->_ssrc=htonl(ssrc);
	return(0);
}

int VRSocketA::build_fin_hdr(SocketBuffer& sb, const int& sz)
{
	START_FUNC("VRSocketA")
	if (sz<HDR_SIZE)
		return(-1);
	sb[0]='a';		// "Synch" word.
	sb[1]=PT_FIN;	// Packet type.
	sb[2]=(0);		// Data length (==0 for ACK).
	sb[3]=(0);		// Data length (==0 for ACK).
	sb[4]=(0);		// Sequence number MSB.
	sb[5]=(0);		// Sequence number LSB.
	return(0);
	END_FUNC("VRSocketA")
}

int VRSocketA::build_fin_hdr(uint8_t* sb, const uint32_t& ssrc, const int& sz)
{
	if (sz<HDR_SIZE)
		return(-1);
	RTPHdr* h=(RTPHdr*)sb;
	h->_v=0x2;
	h->_pt=PT_FIN;
	h->_sn=0;
	h->_ts=0;
	h->_ssrc=ssrc;
	return(0);
}

bool VRSocketA::test()
{
    pid_t child_id=fork();
    bool ret=false;
    if (child_id==0)
        ; // ret=parent();
    else
        ; // ret=child();
    return(ret);
}

int VRSocketA::writev(const int& fd, const SocketBufferCtrlBlk& data)
{
	const int IOVCNT=1;
   	struct iovec v[IOVCNT];
    v[0].iov_base=(char*)&(data._sb[0]);
    v[0].iov_len=data._sb.size();
    return(::writev(fd, v, IOVCNT));
}

int VRSocketA::writev(const int& fd, const SocketBufferCtrlBlk& data,
                     const SocketBufferCtrlBlk& hdr)
{
	const int IOVCNT=2;
	struct iovec v[IOVCNT];
	v[0].iov_base=(char*)&(hdr._sb[0]);
	v[0].iov_len=hdr._sb.size();
	v[1].iov_base=(char*)&(data._sb[0]);
	v[1].iov_len=data._sb.size();
	return(::writev(fd, v, IOVCNT));
}

int VRSocketA::readv(const int& fd, const SocketBufferCtrlBlk& data)
{
	const int IOVCNT=1;
   	struct iovec v[IOVCNT];
    v[0].iov_base=(char*)&(data._sb[0]);
    v[0].iov_len=data._sb.size();
    return(::readv(fd, v, IOVCNT));
}

int VRSocketA::readv(const int& fd, const SocketBufferCtrlBlk& data,
                     const SocketBufferCtrlBlk& hdr)
{
	const int IOVCNT=2;
    struct iovec v[IOVCNT];
    v[0].iov_base=(char*)&(hdr._sb[0]);
    v[0].iov_len=hdr._sb.size();
    v[1].iov_base=(char*)&(data._sb[0]);
    v[1].iov_len=data._sb.size();
    return(::readv(fd, v, IOVCNT));
}
