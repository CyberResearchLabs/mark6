/*
 *  VRClientSocket.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <VRClientSocket.h>
#include <Utils.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>		// For times().
#include <sys/resource.h>	// For getrusage().
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <sys/uio.h>
#include <unistd.h>			// For getrusage().
#include <syslog.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/ioctl.h>

static bool RCVD_RTCP=true;
const int WATCHDOG_INTERVAL=30;
static void
sig_alrm(int signo)
{
  // Watchdog process.
  syslog(LOG_ERR, "signal: Alarm");
  if (RCVD_RTCP) {
    RCVD_RTCP=false;
    alarm(WATCHDOG_INTERVAL);
  } else {
    exit(1);
  }
}

void VRClientSocket::run()
{
  if (_tvg)
    run_tvg();
  else 
    run1();
}

void VRClientSocket::run_tvg()
{
  struct timeval to;
  to.tv_sec=10; to.tv_usec=0;
  struct timeval zero_to;
  zero_to.tv_sec=0; zero_to.tv_usec=0;
  unsigned long long total_bytes_rcvd=0, total_bytes_sent=0;
  unsigned long long data_bytes_rcvd=0;
  unsigned long long good_total_bytes_rcvd=0, good_data_bytes_rcvd=0;
  u_int16_t length=0, sn=0;
  u_int32_t ts=0;
  Timer send_timer, duration_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int IN_FD=-1, OUT_FD=-1, CTRL_FD=-1, BUF_SIZE=0;
  u_int8_t* fill_buf=NULL;
  Iovec *rtp_buf=NULL;
  Iovec rtcp_buf[1];
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  for (int i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval now, diff;
  double ddiff=0;
  int ret=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  const bool RETRANSMISSION=true;
  // FIXME const double IDLE_TIMEOUT=10;
  const double rtp_ts_interval=double(_rtp_session._packet_size)/
    (double(_rtp_session._bits_per_sample)
     *double(_rtp_session._sampling_frequency*1e3));

#ifdef WATCHDOG
  // Install watchdog handler.
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  // PS_SELECT vars.
  fd_set rfd;
  int n=0;

  // FIXME
  ofstream sn_stream("sn.dat");

  // FIXME.
  BUF_SIZE=_t_spec._mtu;
 
  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
	int policy=SCHED_FIFO;
	struct sched_param param;
	param.sched_priority=99;
	ret=pthread_setschedparam(pthread_self(), policy, &param);
	if (ret!=0) {
	  syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
	}
	syslog(LOG_DEBUG, "---- PS_CONN_INIT ----");

        // Initialize connections.
	_udp_sock.set_so_rcvbuf(4000000);
	_udp_sock.set_so_rcvlowat(_t_spec._mtu);

	// Connection setup.
	// 1. Setup data port for receiving.
	cout << "Binding to 0.0.0.0/" << _udp_port << endl;
	_udp_sock.bind("0.0.0.0", _udp_port);
	// 2. Connect to remote control port.b
	cout << "Connecting to " << _remote_ip.c_str() << "/"
	     <<  _ctrl_port << " control port\n";
	_ctrl_sock.connect(_remote_ip, _ctrl_port);
	// 3. Connection to data port made from remote side.
	// 4. CONNECTION ESTABLISHED.
				 
	IN_FD=_udp_sock.get_sockd();
	CTRL_FD=_ctrl_sock.get_sockd();
	cout << "IN_FD:   " << IN_FD << endl;
	cout << "OUT_FD:  " << OUT_FD << endl;
	cout << "CTRL_FD:  " << CTRL_FD << endl;
	cout << "MTU:     " << _t_spec._mtu << endl;
	fill_buf=new u_int8_t[BUF_SIZE];
	for (int i=0; i<BUF_SIZE; i+=4) {
	  fill_buf[i]=0x11;
	  fill_buf[i+1]=0x22;
	  fill_buf[i+2]=0x33;
	  fill_buf[i+3]=0x44;
	}
	rtcp_timer.start();
	rtcp_buf[0].iov_base=new char[BUF_SIZE];
	rtcp_buf[0].iov_len=BUF_SIZE;
	current_scan=_scan_vector.begin();
	_state=PS_SELECT;
      }
      break;
    case PS_SELECT:
      {
	FD_ZERO(&rfd);
	if (CTRL_FD>0)
	  FD_SET(CTRL_FD, &rfd);
	if (IN_FD>0)
	  FD_SET(IN_FD, &rfd);
	n=CTRL_FD>IN_FD?CTRL_FD:IN_FD;
	++n;
	ret=0;
        do {
          ret=select(n, &rfd, NULL, NULL, &zero_to);
          if (ret<0 && errno!=EINTR) {
            syslog(LOG_ERR, "PS_SELECT: select returns %d: %s", ret,
                   strerror(errno));
            break;
          }
        } while (ret<0 && errno==EINTR);
	if (CTRL_FD>0 && FD_ISSET(CTRL_FD, &rfd)) {
	  _state=PS_RX_RTCP;
	  continue;
	}
	rtcp_timer.stop();
	if (rtcp_timer.elapsed()>RTCP_TIMEOUT) {
	  rtcp_timer.start();
	  _state=PS_TX_RTCP;	
	  continue;
	}
	if (IN_FD>0 && FD_ISSET(IN_FD, &rfd)) {
	  _state=PS_RX_DATA;
	  continue;
	}
      }
      break;
    case PS_RX_DATA:
      {
	syslog(LOG_DEBUG, "---- PS_RX_DATA ----");
        if (first) {
	  start_clock=times(&start_tms);
	  duration_timer.start();
	  first=false;
	}
	rtp_buf=new Iovec[2];
	rtp_buf[0].iov_base=new char[HDR_SIZE];
	rtp_buf[0].iov_len=HDR_SIZE;
	rtp_buf[1].iov_base=new char[BUF_SIZE];
	rtp_buf[1].iov_len=BUF_SIZE;
	do {
	  ret=::readv(IN_FD, rtp_buf, 2);
          syslog(LOG_DEBUG, "PS_RX_DATA: In Receive loop...");
	} while (ret<0 && errno==EINTR);
	// TODO More generic loss module. 
	if (ret<HDR_SIZE) {
	  syslog(LOG_ERR, "PS_RX_DATA: recv() error: %s", 
		 strerror(errno));
	  delete [] (u_int8_t*)rtp_buf[0].iov_base;
	  delete [] (u_int8_t*)rtp_buf[1].iov_base;
	  delete [] rtp_buf;
	  rtp_buf=NULL;
	  length=0;
	  _state=PS_TX_DATA;
	  continue;
	}
	// Process the packet.
	RTPHdr* hdr=(RTPHdr*)rtp_buf[0].iov_base;
	sn=ntohs(hdr->_sn);
        // FIXME
        sn_stream << sn << endl;
        ts=ntohl(hdr->_ts);
	_rtp_session._ssrc=ntohl(hdr->_ssrc);
	struct timeval last_sr;
	double arrival;
	u_int32_t transit, d;
	switch (hdr->_pt) {
	case PT_DATA:
          {
            if (gettimeofday(&now, NULL)!=0) 
              syslog(LOG_ERR, "gettimeofday: %s", strerror(errno));
            last_sr.tv_sec=_rtp_session._r._ntp_sec;
            last_sr.tv_usec=_rtp_session._r._ntp_frac;
            timersub(&now, &last_sr, &diff);
            // Calculating packet jitter(from RFC3550).
            // If Si is the RTP timestamp from packet i, and Ri is 
            // the time of arrival in RTP timestamp units for packet 
            // i, then for two packets i and j, D may be expressed 
            // as :
            // D(i,j) = (Rj - Ri) - (Sj - Si) = (Rj - Sj) - (Ri - Si)
            // The interarrival jitter SHOULD be calculated 
            // continuously as each data packet i is received from 
            // source SSRC_n, using this difference D for that packet 
            // and the previous packet i-1 in order of arrival (not 
            // necessarily in sequence), according to the formula
            // J(i) = J(i-1) + (|D(i-1,i)| - J(i-1))/16
            // Get time since last SR.
            ddiff=diff.tv_sec+diff.tv_usec/1e6;
            // Convert to units of rtp_ts.
            ddiff/=rtp_ts_interval;
            // Add offset.
            ddiff+=_rtp_session._r._rtp_ts;
            arrival=ddiff;
            transit = (u_int32_t)(arrival - ts);
            d = transit - _rtp_session._r._transit;
            if (d<0) d = -d;
            _rtp_session._r._jitter += (1./16.)*((double)d - 
                                                 _rtp_session._r._jitter);
            syslog(LOG_DEBUG, "PS_RX_DATA: <sn max_seq>==%d %d", 
                   sn, _rtp_session._r._max_seq);
            length=ret-HDR_SIZE;
            total_bytes_rcvd+=ret;
            data_bytes_rcvd+=length;
            if (RETRANSMISSION) {
              u_int16_t max_seq=_rtp_session._r._max_seq;
              if (sn==max_seq) {
                // Accept.
                // Valid length read.
                good_total_bytes_rcvd+=ret;
                good_data_bytes_rcvd+=length;
                // FIXME rtp_map[sn]=rtp_buf;
                syslog(LOG_DEBUG, "PS_RX_DATA: Accept packet: sn==%d", sn);
                ++max_seq;
                for (int i=0; i<_max_window; ++i) {
                  // Clean out buffer.
                  if (rtp_map[max_seq]) {
                    delete [] (u_int8_t*) (rtp_map[max_seq])[0].iov_base;
                    delete [] (u_int8_t*) (rtp_map[max_seq])[1].iov_base;
                    delete [] rtp_map[max_seq];
                    // FIXME.
                    rtp_map[max_seq]=NULL;
                    ++max_seq;
                  } else {
                    break;
                  }
                }
                _rtp_session._r._max_seq=max_seq;
              } else if ((sn-max_seq)<(sn+_max_window-max_seq)) {
                // Buffer.
                syslog(LOG_DEBUG, "PS_RX_DATA: Buffering packet: sn==%d", sn);
                rtp_map[sn]=rtp_buf;
                rtp_buf=NULL;
              } else {
                // Discard.
                syslog(LOG_DEBUG, "PS_RX_DATA: Discard packet: sn==%d", 
                       sn);
                delete [] (u_int8_t*) rtp_buf[0].iov_base;
                delete [] (u_int8_t*) rtp_buf[1].iov_base;
                delete [] rtp_buf;
                rtp_buf=NULL;
                length=0;
              }
            } else {
              // No retransmissions.
              _rtp_session._r._max_seq=sn; 
              length=ret-HDR_SIZE;                
              good_total_bytes_rcvd+=ret;            
              good_data_bytes_rcvd+=length;
            }
          }
	  break;
	default:
	  syslog(LOG_ERR, "PS_RX_DATA: Received unknown packet");
	  delete [] (u_int8_t*) rtp_buf[0].iov_base;
	  delete [] (u_int8_t*) rtp_buf[1].iov_base;
	  delete [] rtp_buf;
	  rtp_buf=NULL;
          length=0;
	  break;
	}
	// print_pkt_hdr(sbcb_vec[0]._sb);
	if (length!=_t_spec._mtu)
	  syslog(LOG_ERR, "PS_RX_DATA: Short read: %s==%d %d", 
		 "<ret length>", ret, length);
#ifdef RTP_SEQ_UPDATE
	// TODO: incorporate configurable lossy + lossless 
	// updating into here.
	if (_rtp_session.update_seq(sn)==1) {
	  syslog(LOG_DEBUG, "Accept packet: sn==%d", sn);
	} else {
	  syslog(LOG_DEBUG, "Discard packet: sn==%d", sn);
	}
#endif
	_state=PS_TX_DATA;
      }
      // FIXME This is fast, but don't forget that it's here.
      // break;
    case PS_TX_DATA:
      {
	syslog(LOG_DEBUG, "---- PS_TX_DATA ----");
        ret=length;
        if (rtp_buf) {
          delete [] (u_int8_t*) rtp_buf[0].iov_base;
          delete [] (u_int8_t*) rtp_buf[1].iov_base;
          delete [] rtp_buf;
          rtp_buf=NULL;
        }
        if (ret<0) {
	  syslog(LOG_ERR, "PS_TX_DATA: send error %s", 
		 strerror(errno));
	} else {
	  total_bytes_sent+=ret; // Valid length read.
	}
	_state=PS_SELECT;
      }
      break;
    case PS_RX_RTCP:
      {
	syslog(LOG_DEBUG, "---- PS_RX_RTCP ----");
	do {
	  ret=::readv(CTRL_FD, rtcp_buf, 1);
	} while (ret<0 && errno==EINTR);
	RTCPCommon* h=(RTCPCommon*)rtcp_buf[0].iov_base;
	RTCPPacket* hh=NULL;
	u_int32_t ntp_sec=0, ntp_frac=0;
	switch (h->_pt) {
	case RTCP_SR:
	  syslog(LOG_DEBUG, "PS_RX_RTCP: Received SR!");
          // Set watchdog flag. It not set within WATCHDOG_INTERVAL
          // watchdog thread will terminate process.
          RCVD_RTCP=true;
	  if (gettimeofday(&_rtp_session._r._lsrr, NULL)!=0)
	    syslog(LOG_ERR, "PS_RX_RTCP: gettimeofday error: %s", 
		   strerror(errno));
	  hh=(RTCPPacket*)rtcp_buf[0].iov_base;
	  _rtp_session._ssrc=ntohl(hh->r.sr._ssrc);
	  ntp_sec=ntohl(hh->r.sr._ntp_sec);
	  ntp_frac=ntohl(hh->r.sr._ntp_frac);
	  _rtp_session._r._ntp_sec=ntp_sec;
	  _rtp_session._r._ntp_frac=ntp_frac;
	  _rtp_session._r._rtp_ts=ntohl(hh->r.sr._rtp_ts);
	  _rtp_session._s._psent=ntohl(hh->r.sr._psent);
	  _rtp_session._s._osent=ntohl(hh->r.sr._osent);
	  _rtp_session._r._lsr=(ntp_sec<<16)+(ntp_frac>>16);
	  _rtp_session.print_sr((u_int8_t*)rtcp_buf[0].iov_base);
	  break;
	case RTCP_SDES:
	  syslog(LOG_NOTICE, "Received SDES!");
	  break;
	case RTCP_BYE:
	  syslog(LOG_NOTICE, "Received BYE!");
	  _state=PS_BYE;
	  continue;
	case RTCP_APP:
	  syslog(LOG_NOTICE, "Received APP!");
	  break;
	default:
	  syslog(LOG_NOTICE, "Received unknown RTCP packet!");
	  break;
	}
	_state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
	syslog(LOG_DEBUG, "---- PS_TX_RTCP ----");
        unsigned int count=1;
	// <Ref: RFC3550:Appendix A.3.>
	u_int32_t extended_max=_rtp_session._r._cycles+
	  _rtp_session._r._max_seq;
	u_int32_t expected=extended_max-_rtp_session._r._base_seq+1;
	int32_t lost=expected-_rtp_session._r._received;
	lost=(lost>0x7fffff)?0x7fffff:lost;
	lost=(lost<0)?0x800000:lost;
	_rtp_session._r._lost=lost;
	u_int32_t expected_interval=expected-
	  _rtp_session._r._expected_prior;
	_rtp_session._r._expected_prior=expected;
	u_int32_t received_interval=_rtp_session._r._received-
	  _rtp_session._r._received_prior;
	_rtp_session._r._received_prior=_rtp_session._r._received;
	u_int32_t lost_interval=expected_interval-received_interval;
	if (expected_interval==0 || lost_interval<=0) 
	  _rtp_session._r._fraction=0;
	else 
	  _rtp_session._r._fraction=(lost_interval<<8)/
	    expected_interval;
	syslog(LOG_DEBUG, "FRACTION: %u", _rtp_session._r._fraction);
	syslog(LOG_DEBUG, "LOST_INTERVAL: %u", lost_interval);
	syslog(LOG_DEBUG, "EXPECTED_INTERVAL: %u", expected_interval);
	// </Ref: RFC3550:Appendix A.3.>
	if (gettimeofday(&now, NULL)!=0) 
	  syslog(LOG_ERR, "PS_TX_RTCP: gettimeofday error: %s", 
		 strerror(errno));
	timersub(&now, &_rtp_session._r._lsrr, &diff);
	ddiff=diff.tv_sec+(diff.tv_usec/1e6); 
	ddiff=ddiff*65536;
	_rtp_session._r._dlsr=int(ddiff);
	RTCPPacket* hh;
	hh=(RTCPPacket*)_rtp_session.build_rr(
					      (u_int8_t*)rtcp_buf[0].iov_base, 
					      rtcp_buf[0].iov_len,
					      count, 
					      _rtp_session._ssrc, 
					      _rtp_session._r._fraction, 
					      _rtp_session._r._lost, 
					      extended_max,
					      (u_int32_t)_rtp_session._r._jitter, 
					      _rtp_session._r._lsr, 
					      _rtp_session._r._dlsr);
	const u_int16_t rr=0;
	const u_int16_t pid=_rtp_session._r._max_seq;
	const u_int16_t blp=0;
	const u_int16_t blp_sz=1;
	_rtp_session.build_ack((u_int8_t*)hh, 
			       rtcp_buf[0].iov_len,
			       _rtp_session._ssrc,
			       _rtp_session._ssrc,
			       &rr,
			       &pid,
			       &blp,
			       blp_sz);
	_rtp_session.print_rr((u_int8_t*)rtcp_buf[0].iov_base);
	_rtp_session.print_ack((u_int8_t*)hh);
	unsigned int r;
	do {
	  r=::writev(CTRL_FD, rtcp_buf, 1);
	} while (r<0 && errno==EINTR);
	// Update average packet size.
	_rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
	syslog(LOG_DEBUG, "PS_TX_RTCP: bytes_sent==%d", r);
	_rtp_session.report(stdout);
	_state=PS_SELECT;
      }
      break;
    case PS_BYE:
      {
	syslog(LOG_DEBUG, "---- PS_BYE ----");
	syslog(LOG_DEBUG, "STATS:\tdiscard_packets\t\t%d", 
	       (int)_rtp_session._r._lost);
	syslog(LOG_DEBUG, "STATS:\tdiscard_bytes\t\t%d", 
	       (int)_rtp_session._r._lost*_t_spec._mtu);
	duration_timer.stop();
	const double RX_TIME=duration_timer.elapsed();
	stop_clock=times(&stop_tms);


	printf("<STATS>\n");
	printf("    <duration> %f </duration>\n", RX_TIME);
	printf("    <total_bytes_rcvd> %lld </total_bytes_rcvd>\n",
	       total_bytes_rcvd);
	printf("    <data_bytes_rcvd>  %lld </data_bytes_rcvd>\n",
	       data_bytes_rcvd);	
        printf("    <good_total_bytes_rcvd> %lld </good_total_bytes_rcvd>\n",
	       good_total_bytes_rcvd);
	printf("    <good_data_bytes_rcvd>  %lld </good_data_bytes_rcvd>\n",
	       good_data_bytes_rcvd);
	printf("    <discards> %d </discards>\n", 
	       _rtp_session._r._lost);
	printf("    <average_throughput> %f </average_throughput>\n",
	       8.0*double(total_bytes_rcvd)/(RX_TIME*1e6));	
        printf("    <average_goodput> %f </average_goodput>\n",
	       8.0*double(good_total_bytes_rcvd)/(RX_TIME*1e6));
	const clock_t d_clock=stop_clock-start_clock;
	const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
	const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
	const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
	const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
	printf("    <duration_clocks> %ld </duration_clocks>\n",
	       d_clock);
	printf("    <tms_utime> %ld </tms_utime>\n", d_utime);
	printf("    <tms_stime> %ld </tms_stime>\n", d_stime);
	printf("    <tms_cutime> %ld </tms_cutime>\n", d_cutime);
	printf("    <tms_cstime> %ld </tms_cstime>\n", d_cstime);
	printf("    <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",
	       sysconf(_SC_CLK_TCK));
	const double cpu_utilization = double(d_utime+d_stime)/
	  double(d_clock);
	printf("    <cpu_utilization> %f </cpu_utilization>\n", 
	       cpu_utilization) ;
	printf("</STATS>\n");

	_state=PS_APP_BYE;
	// Get resource usage.
	print_rusage();
	::close(IN_FD);
	::close(OUT_FD);
	::close(CTRL_FD);
	return;
      }
      break;
    default:
      syslog(LOG_NOTICE, "Unknown state.");
      break;
    }
  }
  // FIXME
  sn_stream.close();
}

void VRClientSocket::run1()
{
  struct timeval to;
  to.tv_sec=10; to.tv_usec=0;
  struct timeval zero_to;
  zero_to.tv_sec=0; zero_to.tv_usec=0;
  unsigned long long total_bytes_rcvd=0, total_bytes_sent=0;
  unsigned long long data_bytes_rcvd=0;
  unsigned long long good_total_bytes_rcvd=0, good_data_bytes_rcvd=0;
  u_int16_t length=0, sn=0;
  u_int32_t ts=0;
  Timer send_timer, duration_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int IN_FD=-1, OUT_FD=-1, CTRL_FD=-1, BUF_SIZE=0;
  u_int8_t* fill_buf=NULL;
  Iovec *rtp_buf=NULL;
  Iovec rtcp_buf[1];
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  for (int i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval now, diff;
  double ddiff=0;
  int ret=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  const bool RETRANSMISSION=false;
  // FIXME const double IDLE_TIMEOUT=10;
  const double rtp_ts_interval=double(_rtp_session._packet_size)/
    (double(_rtp_session._bits_per_sample)
     *double(_rtp_session._sampling_frequency*1e3));

#ifdef WATCHDOG
  // Install watchdog handler.
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  // PS_SELECT vars.
  fd_set rfd;
  int n=0;

  // FIXME
  ofstream sn_stream("sn.dat");

  // FIXME.
  BUF_SIZE=_t_spec._mtu;
  rtp_buf=new Iovec[2];
  rtp_buf[0].iov_base=new char[HDR_SIZE];
  rtp_buf[0].iov_len=HDR_SIZE;
  rtp_buf[1].iov_base=new char[BUF_SIZE];
  rtp_buf[1].iov_len=BUF_SIZE;
 
  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
	int policy=SCHED_FIFO;
	struct sched_param param;
	param.sched_priority=99;
	ret=pthread_setschedparam(pthread_self(), policy, &param);
	if (ret!=0) {
	  syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
	}
#ifdef SYSLOG
	syslog(LOG_DEBUG, "---- PS_CONN_INIT ----");
#endif	
        // Initialize connections.
	_udp_sock.set_so_rcvbuf(4000000);
	_udp_sock.set_so_rcvlowat(_t_spec._mtu);

	// Connection setup.
	// 1. Setup data port for receiving.
	cout << "Binding to 0.0.0.0/" << _udp_port << endl;
	_udp_sock.bind("0.0.0.0", _udp_port);
	// 2. Connect to remote control port.b
	cout << "Connecting to " << _remote_ip.c_str() << "/"
	     <<  _ctrl_port << " control port\n";
	_ctrl_sock.connect(_remote_ip, _ctrl_port);
	// 3. Connection to data port made from remote side.
	// 4. CONNECTION ESTABLISHED.
				 
	IN_FD=_udp_sock.get_sockd();
	CTRL_FD=_ctrl_sock.get_sockd();
	cout << "IN_FD:   " << IN_FD << endl;
	cout << "OUT_FD:  " << OUT_FD << endl;
	cout << "CTRL_FD:  " << CTRL_FD << endl;
	cout << "MTU:     " << _t_spec._mtu << endl;
	fill_buf=new u_int8_t[BUF_SIZE];
	for (int i=0; i<BUF_SIZE; i+=4) {
	  fill_buf[i]=0x11;
	  fill_buf[i+1]=0x22;
	  fill_buf[i+2]=0x33;
	  fill_buf[i+3]=0x44;
	}
	rtcp_timer.start();
	rtcp_buf[0].iov_base=new char[BUF_SIZE];
	rtcp_buf[0].iov_len=BUF_SIZE;
	current_scan=_scan_vector.begin();
	_state=PS_SCAN_INIT;
      }
      break;
    case PS_SCAN_INIT:
      {
	if (_tvg) {
	  // Discard all data.
	  OUT_FD=-1;
	  _state=PS_SELECT;
	  continue;
	}
	if (current_scan!=_scan_vector.begin())
	  ++current_scan;
	if (current_scan->_scan_file.size()) {
	  /* File source */
	  FILE* fout=fopen(current_scan->_scan_file.c_str(), "w");
	  if (fout==NULL) {
	    cout << "Unable to open Output file\n";
	    return; 
	  } else {
	    cout << "Output scan:" << current_scan->_scan_file << endl;
	  }   
	  OUT_FD=fileno(fout);
	} else {
	  syslog(LOG_ERR, "Zero length scan_file\n");
	}
	_state=PS_SELECT;
      }
      break;
    case PS_SELECT:
      {
	FD_ZERO(&rfd);
	if (CTRL_FD>0)
	  FD_SET(CTRL_FD, &rfd);
	if (IN_FD>0)
	  FD_SET(IN_FD, &rfd);
	n=CTRL_FD>IN_FD?CTRL_FD:IN_FD;
	++n;
	ret=0;
        do {
          ret=select(n, &rfd, NULL, NULL, &zero_to);
          if (ret<0 && errno!=EINTR) {
            syslog(LOG_ERR, "PS_SELECT: select returns %d: %s", ret,
                   strerror(errno));
            break;
          }
        } while (ret<0 && errno==EINTR);
	if (CTRL_FD>0 && FD_ISSET(CTRL_FD, &rfd)) {
	  _state=PS_RX_RTCP;
	  continue;
	}
	rtcp_timer.stop();
	if (rtcp_timer.elapsed()>RTCP_TIMEOUT) {
	  rtcp_timer.start();
	  _state=PS_TX_RTCP;	
	  continue;
	}
	if (IN_FD>0 && FD_ISSET(IN_FD, &rfd)) {
	  _state=PS_RX_DATA;
	  continue;
	}
      }
      break;
    case PS_RX_DATA:
      {
#ifdef SYSLOG
	syslog(LOG_DEBUG, "---- PS_RX_DATA ----");
#endif	
        if (first) {
	  start_clock=times(&start_tms);
	  duration_timer.start();
	  first=false;
	}
#ifdef FIXME
	rtp_buf=new Iovec[2];
	rtp_buf[0].iov_base=new char[HDR_SIZE];
	rtp_buf[0].iov_len=HDR_SIZE;
	rtp_buf[1].iov_base=new char[BUF_SIZE];
	rtp_buf[1].iov_len=BUF_SIZE;
#endif
	do {
	  ret=::readv(IN_FD, rtp_buf, 2);
#ifdef SYSLOG          
          syslog(LOG_DEBUG, "PS_RX_DATA: In Receive loop...");
#endif
	} while (ret<0 && errno==EINTR);
	// TODO More generic loss module. 
	if (ret<HDR_SIZE) {
	  syslog(LOG_ERR, "PS_RX_DATA: recv() error: %s", 
		 strerror(errno));
#ifdef FIXME
	  delete [] (u_int8_t*)rtp_buf[0].iov_base;
	  delete [] (u_int8_t*)rtp_buf[1].iov_base;
	  delete [] rtp_buf;
	  rtp_buf=NULL;
#endif
	  length=0;
	  _state=PS_TX_DATA;
	  continue;
	}
	// Process the packet.
	RTPHdr* hdr=(RTPHdr*)rtp_buf[0].iov_base;
	sn=ntohs(hdr->_sn);
        // FIXME
        sn_stream << sn << endl;
        ts=ntohl(hdr->_ts);
	_rtp_session._ssrc=ntohl(hdr->_ssrc);
	struct timeval last_sr;
	double arrival;
	u_int32_t transit, d;
	switch (hdr->_pt) {
	case PT_DATA:
          {
            if (gettimeofday(&now, NULL)!=0) 
              syslog(LOG_ERR, "gettimeofday: %s", strerror(errno));
            last_sr.tv_sec=_rtp_session._r._ntp_sec;
            last_sr.tv_usec=_rtp_session._r._ntp_frac;
            timersub(&now, &last_sr, &diff);
            // Calculating packet jitter(from RFC3550).
            // If Si is the RTP timestamp from packet i, and Ri is 
            // the time of arrival in RTP timestamp units for packet 
            // i, then for two packets i and j, D may be expressed 
            // as :
            // D(i,j) = (Rj - Ri) - (Sj - Si) = (Rj - Sj) - (Ri - Si)
            // The interarrival jitter SHOULD be calculated 
            // continuously as each data packet i is received from 
            // source SSRC_n, using this difference D for that packet 
            // and the previous packet i-1 in order of arrival (not 
            // necessarily in sequence), according to the formula
            // J(i) = J(i-1) + (|D(i-1,i)| - J(i-1))/16
            // Get time since last SR.
            ddiff=diff.tv_sec+diff.tv_usec/1e6;
            // Convert to units of rtp_ts.
            ddiff/=rtp_ts_interval;
            // Add offset.
            ddiff+=_rtp_session._r._rtp_ts;
            arrival=ddiff;
            transit = (u_int32_t)(arrival - ts);
            d = transit - _rtp_session._r._transit;
            if (d<0) d = -d;
            _rtp_session._r._jitter += (1./16.)*((double)d - 
                                                 _rtp_session._r._jitter);
#ifdef SYSLOG            
            syslog(LOG_DEBUG, "PS_RX_DATA: Received PT_DATA");
            syslog(LOG_DEBUG, "PS_RX_DATA: <sn max_seq>==%d %d", 
                   sn, _rtp_session._r._max_seq);
#endif            
            length=ret-HDR_SIZE;
            total_bytes_rcvd+=ret;
            data_bytes_rcvd+=length;
            if (RETRANSMISSION) {
              u_int16_t max_seq=_rtp_session._r._max_seq;
              if (sn==max_seq) {
                // Accept.
                // Valid length read.
                good_total_bytes_rcvd+=ret;
                good_data_bytes_rcvd+=length;
                // FIXME rtp_map[sn]=rtp_buf;
#ifdef SYSLOG                
                syslog(LOG_DEBUG, "PS_RX_DATA: Accept packet: sn==%d", sn);
#endif                
                ++max_seq;
                for (int i=0; i<_max_window; ++i) {
                  // Clean out buffer.
                  if (rtp_map[max_seq]) {
                    delete [] (u_int8_t*) (rtp_map[max_seq])[0].iov_base;
                    delete [] (u_int8_t*) (rtp_map[max_seq])[1].iov_base;
                    delete [] rtp_map[max_seq];
                    // FIXME.
                    rtp_map[max_seq]=NULL;
                    ++max_seq;
                  } else {
                    break;
                  }
                }
                _rtp_session._r._max_seq=max_seq;
              } else if ((sn-max_seq)<(sn+_max_window-max_seq)) {
                // Buffer.
                rtp_map[sn]=rtp_buf;
                rtp_buf=NULL;
              } else {
                // Discard.
#ifdef SYSLOG
                syslog(LOG_DEBUG, "PS_RX_DATA: Discard packet: sn==%d", 
                       sn);
#endif             
                delete [] (u_int8_t*) rtp_buf[0].iov_base;
                delete [] (u_int8_t*) rtp_buf[1].iov_base;
                delete [] rtp_buf;
                rtp_buf=NULL;
                length=0;
              }
            } else {
              // No retransmissions.
              _rtp_session._r._max_seq=sn; 
              length=ret-HDR_SIZE;                
              good_total_bytes_rcvd+=ret;            
              good_data_bytes_rcvd+=length;
            }
          }
	  break;
	default:
	  syslog(LOG_ERR, "PS_RX_DATA: Received unknown packet");
#ifdef FIXME
	  delete [] (u_int8_t*) rtp_buf[0].iov_base;
	  delete [] (u_int8_t*) rtp_buf[1].iov_base;
	  delete [] rtp_buf;
	  rtp_buf=NULL;
#endif	  
          length=0;
	  break;
	}
	// print_pkt_hdr(sbcb_vec[0]._sb);
	if (length!=_t_spec._mtu)
	  syslog(LOG_ERR, "PS_RX_DATA: Short read: %s==%d %d", 
		 "<ret length>", ret, length);
#ifdef RTP_SEQ_UPDATE
	// TODO: incorporate configurable lossy + lossless 
	// updating into here.
	if (_rtp_session.update_seq(sn)==1) {
	  syslog(LOG_DEBUG, "Accept packet: sn==%d", sn);
	} else {
	  syslog(LOG_DEBUG, "Discard packet: sn==%d", sn);
	}
#endif
	_state=PS_TX_DATA;
      }
      // FIXME This is fast, but don't forget that it's here.
      // break;
    case PS_TX_DATA:
      {
#ifdef SYSLOG
	syslog(LOG_DEBUG, "---- PS_TX_DATA ----");
#endif	
        ret=0;
	if (_tvg) {
	  ret=length;
          if (rtp_buf) {
#ifdef FIXME
	    delete [] (u_int8_t*) rtp_buf[0].iov_base;
	    delete [] (u_int8_t*) rtp_buf[1].iov_base;
	    delete [] rtp_buf;
	    rtp_buf=NULL;
#endif
          }
	} else {
	  if (rtp_buf) {
	    do {
	      ret=::write(OUT_FD, (char*)rtp_buf[1].iov_base, length);
	    } while (ret<0 && errno==EINTR);
	    delete [] (u_int8_t*) rtp_buf[0].iov_base;
	    delete [] (u_int8_t*) rtp_buf[1].iov_base;
	    delete [] rtp_buf;
	    rtp_buf=NULL;
	  } else {
	    // packet was discarded
	  }
	} 
        if (ret<0) {
	  syslog(LOG_ERR, "PS_TX_DATA: send error %s", 
		 strerror(errno));
	} else {
	  total_bytes_sent+=ret; // Valid length read.
	}
	_state=PS_SELECT;
      }
      break;
    case PS_RX_RTCP:
      {
#ifdef SYSLOG
	syslog(LOG_DEBUG, "---- PS_RX_RTCP ----");
#endif
	do {
	  ret=::readv(CTRL_FD, rtcp_buf, 1);
	} while (ret<0 && errno==EINTR);
	RTCPCommon* h=(RTCPCommon*)rtcp_buf[0].iov_base;
	RTCPPacket* hh=NULL;
	u_int32_t ntp_sec=0, ntp_frac=0;
	switch (h->_pt) {
	case RTCP_SR:
#ifdef SYSLOG
	  syslog(LOG_NOTICE, "PS_RX_RTCP: Received SR!");
#endif          
          // Set watchdog flag. It not set within WATCHDOG_INTERVAL
          // watchdog thread will terminate process.
          RCVD_RTCP=true;
	  if (gettimeofday(&_rtp_session._r._lsrr, NULL)!=0)
	    syslog(LOG_ERR, "PS_RX_RTCP: gettimeofday error: %s", 
		   strerror(errno));
	  hh=(RTCPPacket*)rtcp_buf[0].iov_base;
	  _rtp_session._ssrc=ntohl(hh->r.sr._ssrc);
	  ntp_sec=ntohl(hh->r.sr._ntp_sec);
	  ntp_frac=ntohl(hh->r.sr._ntp_frac);
	  _rtp_session._r._ntp_sec=ntp_sec;
	  _rtp_session._r._ntp_frac=ntp_frac;
	  _rtp_session._r._rtp_ts=ntohl(hh->r.sr._rtp_ts);
	  _rtp_session._s._psent=ntohl(hh->r.sr._psent);
	  _rtp_session._s._osent=ntohl(hh->r.sr._osent);
	  _rtp_session._r._lsr=(ntp_sec<<16)+(ntp_frac>>16);
	  _rtp_session.print_sr((u_int8_t*)rtcp_buf[0].iov_base);
	  break;
	case RTCP_SDES:
	  syslog(LOG_NOTICE, "Received SDES!");
	  break;
	case RTCP_BYE:
	  syslog(LOG_NOTICE, "Received BYE!");
	  _state=PS_BYE;
	  continue;
	case RTCP_APP:
	  syslog(LOG_NOTICE, "Received APP!");
	  break;
	default:
	  syslog(LOG_NOTICE, "Received unknown RTCP packet!");
	  break;
	}
	_state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
#ifdef SYSLOG
	syslog(LOG_DEBUG, "---- PS_TX_RTCP ----");
#endif	
        unsigned int count=1;
	// <Ref: RFC3550:Appendix A.3.>
	u_int32_t extended_max=_rtp_session._r._cycles+
	  _rtp_session._r._max_seq;
	u_int32_t expected=extended_max-_rtp_session._r._base_seq+1;
	int32_t lost=expected-_rtp_session._r._received;
	lost=(lost>0x7fffff)?0x7fffff:lost;
	lost=(lost<0)?0x800000:lost;
	_rtp_session._r._lost=lost;
	u_int32_t expected_interval=expected-
	  _rtp_session._r._expected_prior;
	_rtp_session._r._expected_prior=expected;
	u_int32_t received_interval=_rtp_session._r._received-
	  _rtp_session._r._received_prior;
	_rtp_session._r._received_prior=_rtp_session._r._received;
	u_int32_t lost_interval=expected_interval-received_interval;
	if (expected_interval==0 || lost_interval<=0) 
	  _rtp_session._r._fraction=0;
	else 
	  _rtp_session._r._fraction=(lost_interval<<8)/
	    expected_interval;
	syslog(LOG_DEBUG, "FRACTION: %u", _rtp_session._r._fraction);
	syslog(LOG_DEBUG, "LOST_INTERVAL: %u", lost_interval);
	syslog(LOG_DEBUG, "EXPECTED_INTERVAL: %u", expected_interval);
	// </Ref: RFC3550:Appendix A.3.>
	if (gettimeofday(&now, NULL)!=0) 
	  syslog(LOG_ERR, "PS_TX_RTCP: gettimeofday error: %s", 
		 strerror(errno));
	timersub(&now, &_rtp_session._r._lsrr, &diff);
	ddiff=diff.tv_sec+(diff.tv_usec/1e6); 
	ddiff=ddiff*65536;
	_rtp_session._r._dlsr=int(ddiff);
	RTCPPacket* hh;
	hh=(RTCPPacket*)_rtp_session.build_rr(
					      (u_int8_t*)rtcp_buf[0].iov_base, 
					      rtcp_buf[0].iov_len,
					      count, 
					      _rtp_session._ssrc, 
					      _rtp_session._r._fraction, 
					      _rtp_session._r._lost, 
					      extended_max,
					      (u_int32_t)_rtp_session._r._jitter, 
					      _rtp_session._r._lsr, 
					      _rtp_session._r._dlsr);
	const u_int16_t rr=0;
	const u_int16_t pid=_rtp_session._r._max_seq;
	const u_int16_t blp=0;
	const u_int16_t blp_sz=1;
	_rtp_session.build_ack((u_int8_t*)hh, 
			       rtcp_buf[0].iov_len,
			       _rtp_session._ssrc,
			       _rtp_session._ssrc,
			       &rr,
			       &pid,
			       &blp,
			       blp_sz);
	_rtp_session.print_rr((u_int8_t*)rtcp_buf[0].iov_base);
	_rtp_session.print_ack((u_int8_t*)hh);
	unsigned int r;
	do {
	  r=::writev(CTRL_FD, rtcp_buf, 1);
	} while (r<0 && errno==EINTR);
	// Update average packet size.
	_rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
	syslog(LOG_DEBUG, "PS_TX_RTCP: bytes_sent==%d", r);
	_rtp_session.report(stdout);
	_state=PS_SELECT;
      }
      break;
    case PS_BYE:
      {
	syslog(LOG_NOTICE, "---- PS_BYE ----");
	syslog(LOG_NOTICE, "STATS:\tdiscard_packets\t\t%d", 
	       (int)_rtp_session._r._lost);
	syslog(LOG_NOTICE, "STATS:\tdiscard_bytes\t\t%d", 
	       (int)_rtp_session._r._lost*_t_spec._mtu);
	duration_timer.stop();
	const double RX_TIME=duration_timer.elapsed();
	stop_clock=times(&stop_tms);


	printf("<STATS>\n");
	printf("    <duration> %f </duration>\n", RX_TIME);
	printf("    <total_bytes_rcvd> %lld </total_bytes_rcvd>\n",
	       total_bytes_rcvd);
	printf("    <data_bytes_rcvd>  %lld </data_bytes_rcvd>\n",
	       data_bytes_rcvd);	
        printf("    <good_total_bytes_rcvd> %lld </good_total_bytes_rcvd>\n",
	       good_total_bytes_rcvd);
	printf("    <good_data_bytes_rcvd>  %lld </good_data_bytes_rcvd>\n",
	       good_data_bytes_rcvd);
	printf("    <discards> %d </discards>\n", 
	       _rtp_session._r._lost);
	printf("    <average_throughput> %f </average_throughput>\n",
	       8.0*double(total_bytes_rcvd)/(RX_TIME*1e6));	
        printf("    <average_goodput> %f </average_goodput>\n",
	       8.0*double(good_total_bytes_rcvd)/(RX_TIME*1e6));
	const clock_t d_clock=stop_clock-start_clock;
	const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
	const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
	const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
	const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
	printf("    <duration_clocks> %ld </duration_clocks>\n",
	       d_clock);
	printf("    <tms_utime> %ld </tms_utime>\n", d_utime);
	printf("    <tms_stime> %ld </tms_stime>\n", d_stime);
	printf("    <tms_cutime> %ld </tms_cutime>\n", d_cutime);
	printf("    <tms_cstime> %ld </tms_cstime>\n", d_cstime);
	printf("    <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",
	       sysconf(_SC_CLK_TCK));
	const double cpu_utilization = double(d_utime+d_stime)/
	  double(d_clock);
	printf("    <cpu_utilization> %f </cpu_utilization>\n", 
	       cpu_utilization) ;
	printf("</STATS>\n");

	_state=PS_APP_BYE;
	// Get resource usage.
	print_rusage();
	::close(IN_FD);
	::close(OUT_FD);
	::close(CTRL_FD);
	return;
      }
      break;
    default:
      syslog(LOG_NOTICE, "Unknown state.");
      break;
    }
  }
  // FIXME
  sn_stream.close();
}

void VRClientSocket::run2()
{
  struct timeval to;
  to.tv_sec=10; to.tv_usec=0;
  struct timeval zero_to;
  zero_to.tv_sec=0; zero_to.tv_usec=0;
  unsigned long long total_bytes_rcvd=0, total_bytes_sent=0;
  unsigned long long data_bytes_rcvd=0;
  unsigned long long good_total_bytes_rcvd=0, good_data_bytes_rcvd=0;
  u_int16_t length=0, sn=0;
  u_int32_t ts=0;
  Timer send_timer, duration_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int IN_FD=-1, OUT_FD=-1, CTRL_FD=-1, BUF_SIZE=0;
  u_int8_t* fill_buf=NULL;
  Iovec *rtp_buf=NULL;
  Iovec rtcp_buf[1];
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  for (int i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval now, diff;
  double ddiff=0;
  int ret=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  // FIXME const double IDLE_TIMEOUT=10;
  const double rtp_ts_interval=double(_rtp_session._packet_size)/
    (double(_rtp_session._bits_per_sample)
     *double(_rtp_session._sampling_frequency*1e3));

#ifdef WATCHDOG
  // Install watchdog handler.
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  // PS_SELECT vars.
  fd_set rfd;
  int n=0;

  // FIXME
  ofstream sn_stream("sn.dat");

  // FIXME.
  BUF_SIZE=_t_spec._mtu;
  rtp_buf=new Iovec[2];
  rtp_buf[0].iov_base=new char[HDR_SIZE];
  rtp_buf[0].iov_len=HDR_SIZE;
  rtp_buf[1].iov_base=new char[BUF_SIZE];
  rtp_buf[1].iov_len=BUF_SIZE;
 
  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
	int policy=SCHED_FIFO;
	struct sched_param param;
	param.sched_priority=99;
	ret=pthread_setschedparam(pthread_self(), policy, &param);
	if (ret!=0) {
	  syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
	}	
        // Initialize connections.
	_udp_sock.set_so_rcvbuf(4000000);
	_udp_sock.set_so_rcvlowat(_t_spec._mtu);

	// Connection setup.
	// 1. Setup data port for receiving.
	cout << "Binding to 0.0.0.0/" << _udp_port << endl;
	_udp_sock.bind("0.0.0.0", _udp_port);
	// 2. Connect to remote control port.b
	cout << "Connecting to " << _remote_ip.c_str() << "/"
	     <<  _ctrl_port << " control port\n";
	_ctrl_sock.connect(_remote_ip, _ctrl_port);
	// 3. Connection to data port made from remote side.
	// 4. CONNECTION ESTABLISHED.
				 
	IN_FD=_udp_sock.get_sockd();
	CTRL_FD=_ctrl_sock.get_sockd();
	BUF_SIZE=_t_spec._mtu;
	cout << "IN_FD:   " << IN_FD << endl;
	cout << "OUT_FD:  " << OUT_FD << endl;
	cout << "CTRL_FD:  " << CTRL_FD << endl;
	cout << "MTU:     " << _t_spec._mtu << endl;
	fill_buf=new u_int8_t[BUF_SIZE];
	for (int i=0; i<BUF_SIZE; i+=4) {
	  fill_buf[i]=0x11;
	  fill_buf[i+1]=0x22;
	  fill_buf[i+2]=0x33;
	  fill_buf[i+3]=0x44;
	}
	rtcp_timer.start();
	rtcp_buf[0].iov_base=new char[BUF_SIZE];
	rtcp_buf[0].iov_len=BUF_SIZE;
	current_scan=_scan_vector.begin();
	_state=PS_SELECT;

	if (_tvg) {
	  // Discard all data.
	  OUT_FD=-1;
        }
	_state=PS_SELECT;
      }
      break;
    case PS_SELECT:
      {
	FD_ZERO(&rfd);
	if (CTRL_FD>0)
	  FD_SET(CTRL_FD, &rfd);
	if (IN_FD>0)
	  FD_SET(IN_FD, &rfd);
	n=CTRL_FD>IN_FD?CTRL_FD:IN_FD;
	++n;
	ret=0;
        do {
          ret=select(n, &rfd, NULL, NULL, &zero_to);
          if (ret<0 && errno!=EINTR) {
            syslog(LOG_ERR, "PS_SELECT: select returns %d: %s", ret,
                   strerror(errno));
            break;
          }
        } while (ret<0 && errno==EINTR);
	if (CTRL_FD>0 && FD_ISSET(CTRL_FD, &rfd)) {
	  _state=PS_RX_RTCP;
	  continue;
	}
	rtcp_timer.stop();
	if (rtcp_timer.elapsed()>RTCP_TIMEOUT) {
	  rtcp_timer.start();
	  _state=PS_TX_RTCP;	
	  continue;
	}
	if (IN_FD>0 && FD_ISSET(IN_FD, &rfd)) {
	  _state=PS_RX_DATA;
	  continue;
	}
      }
      break;
    case PS_RX_DATA:
      {
        if (first) {
	  start_clock=times(&start_tms);
	  duration_timer.start();
	  first=false;
	}
        int queued=0;
        if (ioctl(IN_FD, FIONREAD, &queued)<0) {
          syslog(LOG_ERR, "ioctl error: %s", strerror(errno));
          _state=PS_SELECT;
          continue;
        }
        if (queued<(BUF_SIZE+12)) {
          _state=PS_SELECT;
          continue;
        }
	do {
	  ret=::readv(IN_FD, rtp_buf, 2);
	} while (ret<0 && errno==EINTR);
	// TODO More generic loss module. 
	if (ret<HDR_SIZE) {
	  syslog(LOG_ERR, "PS_RX_DATA: recv() error: %s", 
		 strerror(errno));
	  length=0;
	  _state=PS_TX_DATA;
	  continue;
	}
	// Process the packet.
	RTPHdr* hdr=(RTPHdr*)rtp_buf[0].iov_base;
	sn=ntohs(hdr->_sn);
        // FIXME
        sn_stream << sn << endl;
        ts=ntohl(hdr->_ts);
	_rtp_session._ssrc=ntohl(hdr->_ssrc);
	struct timeval last_sr;
	double arrival;
	u_int32_t transit, d;
	switch (hdr->_pt) {
	case PT_DATA:
          {
            if (gettimeofday(&now, NULL)!=0) 
              syslog(LOG_ERR, "gettimeofday: %s", strerror(errno));
            last_sr.tv_sec=_rtp_session._r._ntp_sec;
            last_sr.tv_usec=_rtp_session._r._ntp_frac;
            timersub(&now, &last_sr, &diff);
            ddiff=diff.tv_sec+diff.tv_usec/1e6;
            // Convert to units of rtp_ts.
            ddiff/=rtp_ts_interval;
            // Add offset.
            ddiff+=_rtp_session._r._rtp_ts;
            arrival=ddiff;
            transit = (u_int32_t)(arrival - ts);
            d = transit - _rtp_session._r._transit;
            if (d<0) d = -d;
            _rtp_session._r._jitter += (1./16.)*((double)d - 
                                                 _rtp_session._r._jitter);   
            length=ret-HDR_SIZE;
            total_bytes_rcvd+=ret;
            data_bytes_rcvd+=length;
            // No retransmissions.
            _rtp_session._r._max_seq=sn; 
            length=ret-HDR_SIZE;                
            good_total_bytes_rcvd+=ret;            
            good_data_bytes_rcvd+=length;
          }
	  break;
	default:
	  syslog(LOG_ERR, "PS_RX_DATA: Received unknown packet");
          length=0;
	  break;
	}
	// print_pkt_hdr(sbcb_vec[0]._sb);
	if (length!=_t_spec._mtu)
	  syslog(LOG_ERR, "PS_RX_DATA: Short read: %s==%d %d", 
		 "<ret length>", ret, length);
#ifdef RTP_SEQ_UPDATE
	// TODO: incorporate configurable lossy + lossless 
	// updating into here.
	if (_rtp_session.update_seq(sn)==1) {
	  syslog(LOG_DEBUG, "Accept packet: sn==%d", sn);
	} else {
	  syslog(LOG_DEBUG, "Discard packet: sn==%d", sn);
	}
#endif
	_state=PS_TX_DATA;
      }
      // FIXME This is fast, but don't forget that it's here.
      break;
    case PS_TX_DATA:
      {
        ret=0;
	if (_tvg) {
	  ret=length;
	} else {
          if (rtp_buf) {
            do {
              ret=::write(OUT_FD, (char*)rtp_buf[1].iov_base, length);
            } while (ret<0 && errno==EINTR);
          } else {
            syslog(LOG_ERR, "Invalid rtp_buf!");
            exit(1);
          }
        }
        if (ret<0) {
	  syslog(LOG_ERR, "PS_TX_DATA: Send error %s", 
		 strerror(errno));
	} else {
	  total_bytes_sent+=ret; // Valid length read.
	}
	_state=PS_SELECT;
      }
      break;
    case PS_RX_RTCP:
      {
	do {
	  ret=::readv(CTRL_FD, rtcp_buf, 1);
	} while (ret<0 && errno==EINTR);
	RTCPCommon* h=(RTCPCommon*)rtcp_buf[0].iov_base;
	RTCPPacket* hh=NULL;
	u_int32_t ntp_sec=0, ntp_frac=0;
	switch (h->_pt) {
	case RTCP_SR:         
          // Set watchdog flag. It not set within WATCHDOG_INTERVAL
          // watchdog thread will terminate process.
          RCVD_RTCP=true;
	  if (gettimeofday(&_rtp_session._r._lsrr, NULL)!=0)
	    syslog(LOG_ERR, "PS_RX_RTCP: gettimeofday error: %s", 
		   strerror(errno));
	  hh=(RTCPPacket*)rtcp_buf[0].iov_base;
	  _rtp_session._ssrc=ntohl(hh->r.sr._ssrc);
	  ntp_sec=ntohl(hh->r.sr._ntp_sec);
	  ntp_frac=ntohl(hh->r.sr._ntp_frac);
	  _rtp_session._r._ntp_sec=ntp_sec;
	  _rtp_session._r._ntp_frac=ntp_frac;
	  _rtp_session._r._rtp_ts=ntohl(hh->r.sr._rtp_ts);
	  _rtp_session._s._psent=ntohl(hh->r.sr._psent);
	  _rtp_session._s._osent=ntohl(hh->r.sr._osent);
	  _rtp_session._r._lsr=(ntp_sec<<16)+(ntp_frac>>16);
	  _rtp_session.print_sr((u_int8_t*)rtcp_buf[0].iov_base);
	  break;
	case RTCP_SDES:
	  syslog(LOG_NOTICE, "Received SDES!");
	  break;
	case RTCP_BYE:
	  syslog(LOG_NOTICE, "Received BYE!");
	  _state=PS_BYE;
	  continue;
	case RTCP_APP:
	  syslog(LOG_NOTICE, "Received APP!");
	  break;
	default:
	  syslog(LOG_NOTICE, "Received unknown RTCP packet!");
	  break;
	}
	_state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
        unsigned int count=1;
	// <Ref: RFC3550:Appendix A.3.>
	u_int32_t extended_max=_rtp_session._r._cycles+
	  _rtp_session._r._max_seq;
	u_int32_t expected=extended_max-_rtp_session._r._base_seq+1;
	int32_t lost=expected-_rtp_session._r._received;
	lost=(lost>0x7fffff)?0x7fffff:lost;
	lost=(lost<0)?0x800000:lost;
	_rtp_session._r._lost=lost;
	u_int32_t expected_interval=expected-
	  _rtp_session._r._expected_prior;
	_rtp_session._r._expected_prior=expected;
	u_int32_t received_interval=_rtp_session._r._received-
	  _rtp_session._r._received_prior;
	_rtp_session._r._received_prior=_rtp_session._r._received;
	u_int32_t lost_interval=expected_interval-received_interval;
	if (expected_interval==0 || lost_interval<=0) 
	  _rtp_session._r._fraction=0;
	else 
	  _rtp_session._r._fraction=(lost_interval<<8)/
	    expected_interval;
	syslog(LOG_DEBUG, "FRACTION: %u", _rtp_session._r._fraction);
	syslog(LOG_DEBUG, "LOST_INTERVAL: %u", lost_interval);
	syslog(LOG_DEBUG, "EXPECTED_INTERVAL: %u", expected_interval);
	// </Ref: RFC3550:Appendix A.3.>
	if (gettimeofday(&now, NULL)!=0) 
	  syslog(LOG_ERR, "PS_TX_RTCP: gettimeofday error: %s", 
		 strerror(errno));
	timersub(&now, &_rtp_session._r._lsrr, &diff);
	ddiff=diff.tv_sec+(diff.tv_usec/1e6); 
	ddiff=ddiff*65536;
	_rtp_session._r._dlsr=int(ddiff);
	RTCPPacket* hh;
	hh=(RTCPPacket*)_rtp_session.build_rr(
					      (u_int8_t*)rtcp_buf[0].iov_base, 
					      rtcp_buf[0].iov_len,
					      count, 
					      _rtp_session._ssrc, 
					      _rtp_session._r._fraction, 
					      _rtp_session._r._lost, 
					      extended_max,
					      (u_int32_t)_rtp_session._r._jitter, 
					      _rtp_session._r._lsr, 
					      _rtp_session._r._dlsr);
	const u_int16_t rr=0;
	const u_int16_t pid=_rtp_session._r._max_seq;
	const u_int16_t blp=0;
	const u_int16_t blp_sz=1;
	_rtp_session.build_ack((u_int8_t*)hh, 
			       rtcp_buf[0].iov_len,
			       _rtp_session._ssrc,
			       _rtp_session._ssrc,
			       &rr,
			       &pid,
			       &blp,
			       blp_sz);
	_rtp_session.print_rr((u_int8_t*)rtcp_buf[0].iov_base);
	_rtp_session.print_ack((u_int8_t*)hh);
	unsigned int r;
	do {
	  r=::writev(CTRL_FD, rtcp_buf, 1);
	} while (r<0 && errno==EINTR);
	// Update average packet size.
	_rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
	_rtp_session.report(stdout);
	_state=PS_SELECT;
      }
      break;
    case PS_BYE:
      {
	syslog(LOG_NOTICE, "---- PS_BYE ----");
	syslog(LOG_NOTICE, "STATS:\tdiscard_packets\t\t%d", 
	       (int)_rtp_session._r._lost);
	syslog(LOG_NOTICE, "STATS:\tdiscard_bytes\t\t%d", 
	       (int)_rtp_session._r._lost*_t_spec._mtu);
	duration_timer.stop();
	const double RX_TIME=duration_timer.elapsed();
	stop_clock=times(&stop_tms);


	printf("<STATS>\n");
	printf("    <duration> %f </duration>\n", RX_TIME);
	printf("    <total_bytes_rcvd> %lld </total_bytes_rcvd>\n",
	       total_bytes_rcvd);
	printf("    <data_bytes_rcvd>  %lld </data_bytes_rcvd>\n",
	       data_bytes_rcvd);	
        printf("    <good_total_bytes_rcvd> %lld </good_total_bytes_rcvd>\n",
	       good_total_bytes_rcvd);
	printf("    <good_data_bytes_rcvd>  %lld </good_data_bytes_rcvd>\n",
	       good_data_bytes_rcvd);
	printf("    <discards> %d </discards>\n", 
	       _rtp_session._r._lost);
	printf("    <average_throughput> %f </average_throughput>\n",
	       8.0*double(total_bytes_rcvd)/(RX_TIME*1e6));	
        printf("    <average_goodput> %f </average_goodput>\n",
	       8.0*double(good_total_bytes_rcvd)/(RX_TIME*1e6));
	const clock_t d_clock=stop_clock-start_clock;
	const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
	const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
	const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
	const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
	printf("    <duration_clocks> %ld </duration_clocks>\n",
	       d_clock);
	printf("    <tms_utime> %ld </tms_utime>\n", d_utime);
	printf("    <tms_stime> %ld </tms_stime>\n", d_stime);
	printf("    <tms_cutime> %ld </tms_cutime>\n", d_cutime);
	printf("    <tms_cstime> %ld </tms_cstime>\n", d_cstime);
	printf("    <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",
	       sysconf(_SC_CLK_TCK));
	const double cpu_utilization = double(d_utime+d_stime)/
	  double(d_clock);
	printf("    <cpu_utilization> %f </cpu_utilization>\n", 
	       cpu_utilization) ;
	printf("</STATS>\n");

	_state=PS_APP_BYE;
	// Get resource usage.
	print_rusage();
	::close(IN_FD);
	::close(OUT_FD);
	::close(CTRL_FD);
	return;
      }
      break;
    default:
      syslog(LOG_NOTICE, "Unknown state.");
      break;
    }
  }
  // FIXME
  sn_stream.close();
}

VRClientSocket::VRClientSocket(const TSpec& t):
  VRSocket(t)
{
  _state=PS_CONN_INIT;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _t_spec._peak_rate=DEFAULT_PEAK_RATE;
  _tvg=false;
  _tvg_duration=0;
}

VRClientSocket::VRClientSocket(const TSpec& t, const string& ip, 
			       const int& uport, const int& tport,
			       const int& cport, const bool& tvg, 
                               const double& tvg_duration,
			       const u_int32_t& sampling_frequency, 
                               const u_int32_t& packet_size,
			       const u_int32_t& bits_per_sample,
			       const string& output_file_name,
			       const ScanVector& scan_vector,
                               const u_int16_t& n):
  VRSocket(t, ip, uport, tport, cport, tvg, tvg_duration, scan_vector, n)
{
  _state=PS_CONN_INIT;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _rtp_session._sampling_frequency=sampling_frequency;
  _rtp_session._packet_size=packet_size;
  _rtp_session._bits_per_sample=bits_per_sample;
  _output_file_name=output_file_name;
}

VRClientSocket::VRClientSocket(const VRClientSocket &s):
  VRSocket(s._t_spec)
{
  _state=s._state;
  _t_spec=s._t_spec;
  _udp_sock=s._udp_sock;
  _tcp_sock=s._tcp_sock;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _tvg=s._tvg;
  _tvg_duration=s._tvg_duration;
  _rtp_session=s._rtp_session;
  _output_file_name=s._output_file_name;
  _scan_vector=s._scan_vector;
  _max_window=s._max_window;
}

VRClientSocket::~VRClientSocket()
{
  // This is an important feature of the VRClientSocket class. Especially
  // for copying of VRClientSockets. VRClientSockets do *not* close their
  // sockets when they are destroyed.
}

           
