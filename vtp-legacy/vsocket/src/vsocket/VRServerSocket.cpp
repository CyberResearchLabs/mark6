/*
 *    VRServerSocket.cpp
 *    vrtp
 *
 *    Created by David Lapsley on Mon Feb 23 2004.
 *    Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <VRServerSocket.h>
#include <Utils.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/times.h>                // For times().
#include <sys/resource.h>        // For getrusage().
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <sys/uio.h>
#include <unistd.h>                        // For getrusage().
#include <signal.h>


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

void VRServerSocket::run() 
{
  if (_tvg) {
    run_tvg();
  } else {
    run1();
  }
}

void VRServerSocket::run_tvg()
{
  struct timeval to;
  unsigned long long total_bytes_sent=0, total_bytes_rcvd=0, data_bytes_sent=0;
  to.tv_sec=30; to.tv_usec=0;
  int ret=0;
  Timer send_timer, duration_timer, tvg_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int length=0;
  u_int8_t pt=PT_DATA;
  struct timespec ipd_req, ipd_rem;
  int IN_FD=0, OUT_FD=0, CTRL_FD=0;
  int RBUF_SIZE=_t_spec._mtu;
  u_int8_t* rtp_dbuf=NULL, *rtp_hbuf=NULL, *rtcp_buf=NULL;
  Iovec *rtp_buf;
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  for (int i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval zero_to;
  zero_to.tv_sec=0;
  zero_to.tv_usec=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  // Rate parameters.
  int bucket=_max_bucket;
  unsigned long long scan_size=0;
  struct timeval myto;
  const bool RETRANSMISSION=true;
  // TODO const double IDLE_TIMEOUT=10;
  sleep_idx=0;

  // Install watchdog handler.
#ifdef WATCHDOG
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
        int policy=SCHED_FIFO;
        struct sched_param param;
        param.sched_priority=99;
        int ret=pthread_setschedparam(pthread_self(), policy, &param);
        if (ret!=0) { 
          syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
        }
        syslog(LOG_DEBUG, "---- PS_CONN_INIT ----\n");
        _tcp_sock.set_so_rcvbuf(4000000);
        _udp_sock.set_so_sndbuf(4000000);
        _tcp_sock.set_so_rcvlowat(_t_spec._mtu);
        _ctrl_sock.set_so_rcvlowat(_t_spec._mtu);

        // Connection setup.
        // 1. Setup control port for receiving incoming connections.
        cout << "Binding to 0.0.0.0/" << _ctrl_port << endl;
        _tcp_accept_sock.bind("0.0.0.0", _ctrl_port);
        _tcp_accept_sock.listen();
        cout << "Accepting incoming ctrl connections\n";
        // 2. Accept incoming control connections.        
        int sd=_tcp_accept_sock.accept();
        if (sd<=0) {
          syslog(LOG_ERR, "Invalid accept\n");
          return;
        }
        cout << "Got incoming ctrl connection\n";
        _ctrl_sock=TCPSocket(sd, _t_spec._mtu);
        // 3. Connect to remote data port.
        cout << "Connecting to " << _remote_ip.c_str() << "/"
             <<    _udp_port << " data port\n";
        _udp_sock.connect(_remote_ip, _udp_port);
        // 4. CONNECTION ESTABLISHED.

        cout << "Test Vector Generator enabled\n";
        IN_FD=-1;
        tvg_timer.start();
        OUT_FD=_udp_sock.get_sockd();
        CTRL_FD=_ctrl_sock.get_sockd();
        cout << "IN_FD:       " << IN_FD << endl;
        cout << "OUT_FD:      " << OUT_FD << endl;
        cout << "CTRL_FD:     " << CTRL_FD << endl;
        cout << "MTU:         " << _t_spec._mtu << endl;
        cout << "MAX_WINDOW:  " << _max_window << endl;
        cout << "MAX_BUCKET:  " << _max_bucket << endl;
        RBUF_SIZE=_t_spec._mtu;
        send_timer.start();
        rtcp_timer.start();
        _rtp_session._ssrc=3;
        rtcp_buf=new u_int8_t[RBUF_SIZE];
        current_scan=_scan_vector.begin();
        // Rate parameters.
        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd-ipd_req.tv_sec));
        ipd_rem.tv_sec=0;
        ipd_rem.tv_nsec=0;
        double ipd=ipd_req.tv_sec+ipd_req.tv_nsec/1e9;
        myto.tv_sec=ipd_req.tv_sec*_max_bucket;
        myto.tv_usec=int(ipd_req.tv_nsec/1000.0*_max_bucket);
        double mytopd=myto.tv_sec+myto.tv_usec/1e6;
        cout << "IPD:         " << ipd << endl;
        cout << "MYTO:        " << mytopd << endl;
        _state=PS_SELECT;
      }
      break;
    case PS_SELECT:
      {
        syslog(LOG_DEBUG, "---- PS_SELECT ----\n");
        fd_set rfd;
        FD_ZERO(&rfd);
        if (CTRL_FD>0)
          FD_SET(CTRL_FD, &rfd);
        int n=CTRL_FD+1;
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
        _state=PS_RX_DATA;
        continue;
      }     
      break; 
    case PS_RX_DATA:
      {
        syslog(LOG_DEBUG, "---- PS_RX_DATA ----\n");
        syslog(LOG_DEBUG, "PS_RX_DATA: <tb> <sl> : %lld %lld\n", 
               total_bytes_rcvd, 
               scan_size);
        tvg_timer.stop();
        if (tvg_timer.elapsed()>_tvg_duration)        {
          rtp_dbuf=NULL;
          length=0;
          if (RETRANSMISSION)
            _state=PS_TX_DATA;
          else
            _state=PS_BYE;
          continue;      
        } 
        // Check to see if we have exhausted our window.
        u_int16_t outstanding=_rtp_session._s._sn_max-
          _rtp_session._s._sn_min;
        if (RETRANSMISSION) {
          if (outstanding>=_max_window) {
            // Yes we have. Start retransmitting.
            syslog(LOG_DEBUG, 
                   "PS_RX_DATA: %s==%d %d",
                   "Exhausted window: <_sn_max _sn_min>",
                   _rtp_session._s._sn_max, 
                   _rtp_session._s._sn_min);
            rtp_dbuf=NULL;
            length=0;
            _state=PS_TX_DATA;
            continue;
          }
        }
        rtp_dbuf=new u_int8_t[_t_spec._mtu];
        length=_t_spec._mtu;
        // Valid read.
        total_bytes_rcvd+=length;
        syslog(LOG_DEBUG, "RX_DATA: total bytes rcvd==%lld",
               total_bytes_rcvd);
        _state=PS_TX_DATA;
      }
      break;
    case PS_TX_DATA:
      {
        syslog(LOG_DEBUG, "---- PS_TX_DATA ----\n");
        // We have nothing new to send.
        if (length<=0) {
          if (RETRANSMISSION)
            _state=PS_RETX_DATA;
          else
            _state=PS_SELECT;
          continue;
        }

        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd
                                 -ipd_req.tv_sec));
        mysleep(ipd_req, bucket);

        // We have something to send.
        rtp_hbuf=new u_int8_t[HDR_SIZE];
        if (_rtp_session.build_data_hdr(rtp_hbuf, 
                                        pt, 
                                        _rtp_session._s._sn_max, 
                                        _rtp_session._s._rtp_ts,
                                        _rtp_session._ssrc, HDR_SIZE, 
                                        false)!=0) {
          syslog(LOG_ERR, "PS_TX_DATA: Unable to build data header");
          _state=PS_SELECT;
          continue;
        }
        // Create new iovec buffer to contain data and header.
        // Used to store packet in rtp_map for any retransmissions.
        rtp_buf=new Iovec[2];
        rtp_buf[0].iov_base=(u_int8_t*)rtp_hbuf;
        rtp_buf[0].iov_len=HDR_SIZE;
        rtp_buf[1].iov_base=(u_int8_t*)rtp_dbuf;
        rtp_buf[1].iov_len=length;
        // Transmit the packet.
        int ret;
        do {
          ret=::writev(OUT_FD, rtp_buf, 2);
          syslog(LOG_DEBUG, "PS_TX_DATA: In send loop.");
        } while (ret<0 && errno==EINTR);
        // Store buffer in case retransmission required.
        // But first CHECK to make sure that anohher packet with the
        // same sequence number is stored in the rtp_map.This should
        // NOT occur, however, if it does, then we need to report the
        // error, delete any duplicate packets and then store the new
        // one. If there are duplicate packets, then it indicates that
        // the rtp_map is not being cleared properly in response to
        // acknowledgements from the destination (this is done in the
        // PS_RX_RTCP state.
        if (RETRANSMISSION) {
          if (rtp_map[_rtp_session._s._sn_max]) {
            syslog(LOG_ERR, "PS_TX_DATA: packet already in rtp_map, sn==%d\n", 
                   _rtp_session._s._sn_max);
            delete (u_int8_t*)(rtp_map[_rtp_session._s._sn_max])[0].iov_base;               
            delete (u_int8_t*)(rtp_map[_rtp_session._s._sn_max])[1].iov_base;
            delete rtp_map[_rtp_session._s._sn_max];
            rtp_map[_rtp_session._s._sn_max]=NULL;
          }
          // Insert iovec buffer into the rtp_map until it has been 
          // acknowledged. Packet will    be retransmitted during any 
          // intervening retransmission cycles.
          rtp_map[_rtp_session._s._sn_max]=rtp_buf;
        } else {
          delete [] (char*)(rtp_buf[0].iov_base);
          delete [] (char*)(rtp_buf[1].iov_base);
          delete [] rtp_buf;
          rtp_buf=NULL;
        }
        // Start the duration timer for measuring stats if this is
        // the first time through the loop.
        if (first) {
          duration_timer.start();
          start_clock=times(&start_tms);
          first=false;
        }
        // Check return codes from the writev() call above.
        if (ret<0) {
          syslog(LOG_ERR, "PS_TX_DATA: %s", strerror(errno));
          _state=PS_BYE;
          continue;
        } else if (ret<(length+HDR_SIZE)) {
          syslog(LOG_ERR, "TX_DATA: Short send(): ret==%d", ret);
          _state=PS_SELECT;
          continue;
        }
        // Valid send.
        total_bytes_sent+=ret;
        ++_rtp_session._s._rtp_ts;
        ++_rtp_session._s._psent;
        _rtp_session._s._osent+=(ret-HDR_SIZE);
        data_bytes_sent+=ret-HDR_SIZE;
        syslog(LOG_DEBUG, "PS_TX_DATA: sn_min sn_max==%d %d\n",         
               _rtp_session._s._sn_min,
               _rtp_session._s._sn_max);
        // Update the _sn_max state.
        ++_rtp_session._s._sn_max;
        _state=PS_SELECT;
      }
      break;
    case PS_RETX_DATA:
      {
        syslog(LOG_DEBUG, "---- PS_RETX_DATA ----");
        if (!RETRANSMISSION) {
          _state=PS_SELECT;
          continue;
        }
        if (_rtp_session._s._sn_max==_rtp_session._s._sn_min) {
          syslog(LOG_NOTICE, "PS_RETX_DATA: COMPLETED SCAN.\n");
          _state=PS_BYE;
          continue;
        }
        // Start retransmitting.
        u_int16_t sn=_rtp_session._s._sn_min;
        u_int16_t s2=_rtp_session._s._sn_max-_rtp_session._s._sn_min;
        int i;
        for (i=0; i<65537; i++) {
          rtp_buf=rtp_map[sn];
          u_int16_t s1=sn-_rtp_session._s._sn_min;
          if (!rtp_map[sn]) {
            syslog(LOG_ERR,
                   "PS_RETX_DATA: reached end of retransmission map: %s==%d %d %d\n",
                   "sn sn_min sn_max",
                   sn, _rtp_session._s._sn_min,
                   _rtp_session._s._sn_max);
            break;
          } else {
            sn++;
          }
          if (s1>=s2) {
            syslog(LOG_ERR, 
                   "PS_RETX_DATA: sn out of bounds: %s==%d %d %d\n", 
                   "sn sn_min sn_max",
                   sn, _rtp_session._s._sn_min, 
                   _rtp_session._s._sn_max);
            break;
          }
          RTPHdr* p = (RTPHdr*)rtp_buf[0].iov_base;
          u_int16_t s = ntohs(p->_sn);
          syslog(LOG_DEBUG, "PS_RETX_DATA: Retransmitting Packet: sn==%d", s);;
          do {
            ret=::writev(OUT_FD, rtp_buf, 2);
          } while (ret<0 && errno==EINTR);
          if (ret<0) {
            syslog(LOG_ERR, "PS_RETX_DATA: writev error: %s", strerror(errno));
            _state=PS_BYE;
            continue;
          } else if (ret<(length+HDR_SIZE)) {
            syslog(LOG_ERR, "PS_RETX_DATA: Short send(): ret==%d", ret);
          }
          ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
          ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd-ipd_req.tv_sec));
          mysleep(ipd_req, bucket);
        }          

        if (i>=65536) {
          cerr << "Maxed out retranmsit loop" << endl;
          cerr << "i" << i << endl;
          cerr << "sn_min " << _rtp_session._s._sn_min << endl;
          cerr << "sn_max " << _rtp_session._s._sn_max << endl;
          exit(1);
        }
        _state=PS_SELECT;
        continue;
      }
      break;
    case PS_RX_RTCP:
      {     
        syslog(LOG_DEBUG, "---- PS_RX_RTCP ----");
        // rtcp_buf init-ed in PS_CONN_INIT.
        do {
          ret=::read(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (ret<0 && errno==EINTR);
        if (ret<0) {
          syslog(LOG_ERR, "PS_RTCP: read error: %s\n", 
                 strerror(errno));
        }
        // Parse received packet.
        RTCPCommon* h=(RTCPCommon*)rtcp_buf;
        RTCPPacket* hh=(RTCPPacket*)rtcp_buf;
        struct timeval now, lsr, diff;
        // Currently number of RTCP packets within a compound
        // packet is hardcoded.
        // TODO: make this dynamic.
        const int num_packets=2;
        u_int8_t* p=NULL;
        for (int i=0; i<num_packets; ++i) {
          switch (h->_pt) {
          case RTCP_RR:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received Receiver Report");
            // Set watchdog flag. It not set within WATCHDOG_INTERVAL
            // watchdog thread will terminate process.
            RCVD_RTCP=true;

            if (gettimeofday(&now, NULL)!=0)
              syslog(LOG_ERR, "gettimeofday: %s\n", strerror(errno));
            _rtp_session.print_rr(rtcp_buf);
            _rtp_session._r._fraction=hh->r.rr._rr[0]._fraction;
            _rtp_session._r._lost=ntohl(hh->r.rr._rr[0]._lost<<8);
            _rtp_session._r._last_seq=ntohl(hh->r.rr._rr[0]._last_seq);
            _rtp_session._r._jitter=ntohl(hh->r.rr._rr[0]._jitter);
            _rtp_session._r._lsr=ntohl(hh->r.rr._rr[0]._lsr);
            _rtp_session._r._dlsr=ntohl(hh->r.rr._rr[0]._dlsr);
            lsr.tv_sec=(_rtp_session._r._lsr>>16);
            lsr.tv_usec=(_rtp_session._r._lsr&0x0000ffff);
            printf("lsr.tv_sec: %u\n", (u_int32_t)lsr.tv_sec);
            printf("lsr.tv_usec: %u\n", (u_int32_t)lsr.tv_usec);
            now.tv_sec&=0x0000ffff;                // Lose msbs.
            now.tv_usec&=0xffff0000;        // Lose lsbs.
            timersub(&now, &lsr, &diff);
            _rtp_session._s._rtt=diff.tv_sec+(diff.tv_usec/1e6);
            _rtp_session._s._rtt-=double(_rtp_session._r._dlsr)/65536;
            _rtp_session.report(stdout);
            p=(u_int8_t*)hh;
            p+=28;
            h=(RTCPCommon*)p;
            hh=(RTCPPacket*)p;
            break;
          case RTCP_SDES:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received SDES!\n");
            break;
          case RTCP_BYE:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received BYE!\n");
            break;
          case RTCP_APP:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received APP!\n");
            break;
          case RTCP_FB:
            {
              printf("PS_RX_RTCP: Received FB!\n");
              _rtp_session.print_ack((u_int8_t*)hh);
              // TODO Common processing.
              // hh->r._fb._sender_ssrc;
              // hh->r._fb._src_ssrc;
              // Obtain the Reqest Number from the ack packet.
              u_int16_t rn=ntohs(hh->r._fb._fci[0]._ack._pid);
              // TODO Advanced ACK processing.
              // hh->r._fb._fci[0]._ack._blp;
              // hh->r._fb._fci[0]._ack._r;
              // Clean up rtp_map.
              u_int16_t sn_min=_rtp_session._s._sn_min;
              u_int16_t sn_max=_rtp_session._s._sn_max;
              syslog(LOG_DEBUG, 
                     "PS_RX_RTCP: %s==%d %d %d\n",
                     "<sn_min sn_max rn>",
                     sn_min, sn_max, rn); 
              // These variables for modulo 2^16 arithmetic.
              u_int16_t res1=rn-sn_min;
              u_int16_t res2=sn_max-sn_min;
              // Check that Request Number is in the current
              // window(defined by sn_min and sn_max).
              if (res1<=res2) {
                u_int16_t i=sn_min;
                // These variables have changed meaning but
                // are re-used for module 2^16 arithmetic.
                // Have to be CAREFUL here. Automatic compiler
                // casts can really ruin your day/night!
                res1=i-sn_min;
                res2=rn-sn_min;
                // Clean out all packets that have been 
                // implicitly acknowledged by the Request
                // Number in the RTCP ack packet.
                if (RETRANSMISSION) {
                  while (res1<res2) {
                    Iovec* cur=rtp_map[i];
                    delete [] (char*)((cur)[0].iov_base);
                    delete [] (char*)((cur)[1].iov_base);
                    delete [] (cur);
                    rtp_map[i++]=NULL;
                    res1=i-sn_min;
                  }
                }
                // Update sn_min to point to the first
                // unacknowledged packet stored in rtp_map.
                _rtp_session._s._sn_min=rn;
              } else {
                syslog(LOG_ERR, "PS_RX_RTCP: %s==%d",
                       "Received out of range rn",
                       rn);
              }
              // TODO NACK Processing.
              // hh->r._fb._fci[0]._sender_ssrc;
              // hh->r._fb._fci[0]._src_ssrc;
              // hh->r._fb._fci[0]._nack._pid;
              // hh->r._fb._fci[0]._nack._blp;
            }
            break;
          case RTCP_PSFB:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received PSFB!");
            break;
          default:                    
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received unknown RTCP packet!");
            break;
          }
        }
        _state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
        syslog(LOG_DEBUG, "---- PS_TX_RTCP ----");
        syslog(LOG_DEBUG, "PS_TX_RTCP: rtp_map.size()==%d", 
               RTP_MAP_SZ);
        unsigned int count=1;
        struct timeval ntp_ts;
        if (gettimeofday(&ntp_ts, NULL)!=0)
          syslog(LOG_ERR, "PS_TX_RTCP: Error reading NTP timestamp\n");
        syslog(LOG_DEBUG, "About to send SR");
        unsigned int ntp_sec=ntp_ts.tv_sec;
        unsigned int ntp_frac=ntp_ts.tv_usec;
        _rtp_session.build_sr(rtcp_buf, RBUF_SIZE,
                              count,
                              _rtp_session._ssrc,
                              ntp_sec,
                              ntp_frac,
                              _rtp_session._s._rtp_ts,
                              _rtp_session._s._psent,
                              _rtp_session._s._osent);
        _rtp_session.print_sr(rtcp_buf);
        // Transmit RTCP packet.
        unsigned int r;
        do {
          r=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (r<0 && errno==EINTR);
        // Update average RTCP packet size.
        _rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
        _rtp_session.update_rate();
        syslog(LOG_DEBUG, "PS_TX_RTCP: bytes_sent==%d\n", r);
        rtcp_timer.start();
        _state=PS_SELECT;
        continue;
      }             
      break;
    case PS_BYE:
      {
        syslog(LOG_NOTICE, "---- PS_BYE ----");
        duration_timer.stop();
        const double TX_TIME=duration_timer.elapsed();
        stop_clock=times(&stop_tms);
        syslog(LOG_NOTICE, "PS_BYE: BYE Transmission");
        u_int8_t source_count=1;
        u_int32_t ssrcs[1] = { _rtp_session._ssrc };
        _rtp_session.build_bye(rtcp_buf, RBUF_SIZE, source_count, ssrcs);
        ret=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        if (ret<0) {
          syslog(LOG_ERR, "PS_BYE: BYE: failed send: %s",
                 strerror(errno));
        } else if (ret!=(int)(_t_spec._mtu+HDR_SIZE)) {
          // Successfully transmitted (at least) part of the packet.
          syslog(LOG_ERR, "FIN: Short send: ret==%d", ret);
        } else {
          syslog(LOG_INFO, "PS_BYE: BYE packet sent");
        }
        total_bytes_sent+=ret;
        const double average_bytes_rate=total_bytes_sent/(TX_TIME*1e6);
        const double average_rate=average_bytes_rate*8;
        printf("<STATS>\n");
        printf("        <total_bytes_sent> %lld </total_bytes_sent>\n", 
               total_bytes_sent);
        printf("        <data_bytes_sent>    %lld </data_bytes_sent>\n", 
               data_bytes_sent);
        printf("        <TX_TIME> %f </TX_TIME>\n", TX_TIME);
        printf("        <average_rate> %f </average_rate>\n", 
               average_rate);
        const clock_t d_clock=stop_clock-start_clock;
        const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
        const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
        const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
        const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
        printf("        <duration_clocks> %ld </duration_clocks>\n", 
               d_clock);
        printf("        <tms_utime> %ld </tms_utime>\n", d_utime);
        printf("        <tms_stime> %ld </tms_stime>\n", d_stime);
        printf("        <tms_cutime> %ld </tms_cutime>\n", d_cutime);
        printf("        <tms_cstime> %ld </tms_cstime>\n", d_cstime);
        printf("        <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",         
               sysconf(_SC_CLK_TCK));
        const double cpu_utilization = double(d_utime+d_stime)/
          double(d_clock);
        printf("        <cpu_utilization>\t%f </cpu_utilization>\n", cpu_utilization) ;
        printf("</STATS>\n");
        _state=PS_APP_BYE;
        // Get resource usage.
        print_rusage();
        ofstream sleep_stream("sleep.dat");
        for (int j=0; j<65535; ++j)
          sleep_stream << sleep_times[j] << endl;
        sleep_stream.close();
        ::close(IN_FD);
        ::close(OUT_FD);
        ::close(CTRL_FD);
        return;
      }
      break;
    default:
      syslog(LOG_ERR, "send_proc(): unknown state");
      break;
    }
  }
}


void VRServerSocket::run1()
{
  struct timeval to;
  unsigned long long total_bytes_sent=0, total_bytes_rcvd=0, data_bytes_sent=0;
  to.tv_sec=30; to.tv_usec=0;
  int ret=0;
  Timer send_timer, duration_timer, tvg_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int length=0;
  u_int8_t pt=PT_DATA;
  struct timespec ipd_req, ipd_rem;
  int IN_FD=0, OUT_FD=0, CTRL_FD=0;
  int RBUF_SIZE=_t_spec._mtu;
  u_int8_t* rtp_dbuf=NULL, *rtp_hbuf=NULL, *rtcp_buf=NULL;
  Iovec *rtp_buf;
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  for (int i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval zero_to;
  zero_to.tv_sec=0;
  zero_to.tv_usec=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  // Rate parameters.
  int bucket=_max_bucket;
  unsigned long long scan_size=0;
  struct timeval myto;
  const bool RETRANSMISSION=true;
  // TODO const double IDLE_TIMEOUT=10;

  // Install watchdog handler.
#ifdef WATCHDOG
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
        int policy=SCHED_FIFO;
        struct sched_param param;
        param.sched_priority=99;
        int ret=pthread_setschedparam(pthread_self(), policy, &param);
        if (ret!=0) { 
          syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
        }
        syslog(LOG_DEBUG, "---- PS_CONN_INIT ----\n");
        _tcp_sock.set_so_rcvbuf(4000000);
        _udp_sock.set_so_sndbuf(4000000);
        _tcp_sock.set_so_rcvlowat(_t_spec._mtu);
        _ctrl_sock.set_so_rcvlowat(_t_spec._mtu);

        // Connection setup.
        // 1. Setup control port for receiving incoming connections.
        cout << "Binding to 0.0.0.0/" << _ctrl_port << endl;
        _tcp_accept_sock.bind("0.0.0.0", _ctrl_port);
        _tcp_accept_sock.listen();
        cout << "Accepting incoming ctrl connections\n";
        // 2. Accept incoming control connections.        
        int sd=_tcp_accept_sock.accept();
        if (sd<=0) {
          syslog(LOG_ERR, "Invalid accept\n");
          return;
        }
        cout << "Got incoming ctrl connection\n";
        _ctrl_sock=TCPSocket(sd, _t_spec._mtu);
        // 3. Connect to remote data port.
        cout << "Connecting to " << _remote_ip.c_str() << "/"
             <<    _udp_port << " data port\n";
        _udp_sock.connect(_remote_ip, _udp_port);
        // 4. CONNECTION ESTABLISHED.

        if (_tvg) {
          printf("Test Vector Generator enabled\n");;
          IN_FD=-1;
          tvg_timer.start();
        } else if (_scan_vector.size()<=0) {
          /* Socket source */
          cout << "Binding to 0.0.0.0/" << _tcp_port << endl;
          _tcp_accept_sock.bind("0.0.0.0", _tcp_port);
          cout << "Listening\n";
          _tcp_accept_sock.listen();
          cout << "Accepting incoming data connection\n";
          int sd=_tcp_accept_sock.accept();
          if (sd<=0) {
            syslog(LOG_ERR, "Invalid accept\n");
            return;
          }
          _tcp_sock=TCPSocket(sd, _t_spec._mtu);
          IN_FD=_tcp_sock.get_sockd();
        }
        OUT_FD=_udp_sock.get_sockd();
        CTRL_FD=_ctrl_sock.get_sockd();
        cout << "IN_FD:     " << IN_FD << endl;
        cout << "OUT_FD:    " << OUT_FD << endl;
        cout << "CTRL_FD: " << CTRL_FD << endl;
        cout << "MTU:         " << _t_spec._mtu << endl;
        cout << "MAX_WINDOW:         " << _max_window << endl;
        cout << "MAX_BUCKET:         " << _max_bucket << endl;
        RBUF_SIZE=_t_spec._mtu;
        send_timer.start();
        rtcp_timer.start();
        _rtp_session._ssrc=3;
        rtcp_buf=new u_int8_t[RBUF_SIZE];
        current_scan=_scan_vector.begin();
        // Rate parameters.
        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd-ipd_req.tv_sec));
        ipd_rem.tv_sec=0;
        ipd_rem.tv_nsec=0;
        double ipd=ipd_req.tv_sec+ipd_req.tv_nsec/1e9;
        myto.tv_sec=ipd_req.tv_sec*_max_bucket;
        myto.tv_usec=int(ipd_req.tv_nsec/1000.0*_max_bucket);
        double mytopd=myto.tv_sec+myto.tv_usec/1e6;
        cout << "IPD:     " << ipd << endl;
        cout << "MYTO:     " << mytopd << endl;
        _state=PS_SCAN_INIT;
      }
      break;
    case PS_SCAN_INIT:
      {
        if (_tvg) {
          _state=PS_SELECT;
          break;
        }
        if (IN_FD>0) {
          _state=PS_SELECT;
          break;
        }
        if (current_scan!=_scan_vector.begin())
          ++current_scan;
        if (current_scan->_scan_file.size()) {
          /* File source */
          FILE* fin=fopen(current_scan->_scan_file.c_str(), "r");
          syslog(LOG_DEBUG, "PS_SCAN_INIT: scan_name=%s\n",
                 current_scan->_scan_file.c_str()); 
          if (fin==NULL) {
            syslog(LOG_ERR, "PS_SCAN_INIT: Unable to open input file");
            return;
          } else {
            syslog(LOG_NOTICE, "PS_SCAN_INIT: Opened Input scan: %s",
                   current_scan->_scan_file.c_str());
            scan_size=current_scan->_size;
            syslog(LOG_DEBUG, "PS_SCAN_INIT: scan_size==%lld\n", scan_size);
          }
          IN_FD=fileno(fin);
        } else {
          syslog(LOG_ERR, "PS_SCAN_INIT: zero length input file name\n");
        }
        _state=PS_SELECT;
      }
      break;
    case PS_SELECT:
      {
#if SYSLOG
        syslog(LOG_NOTICE, "---- PS_RX_DATA ----\n");
#endif        
        fd_set rfd;
        int n=0;
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
        if (_tvg) { // use tvg data.
          _state=PS_RX_DATA;
          continue;
        }
        // TODO Add idle timer later.
      }     
      break; 
    case PS_RX_DATA:
      {
#ifdef SYSLOG
        syslog(LOG_NOTICE, "---- PS_RX_DATA ----\n");
        syslog(LOG_DEBUG, "PS_RX_DATA: <tb> <sl> : %lld %lld\n", 
               total_bytes_rcvd, 
               scan_size);
#endif
        if (_tvg) {
          tvg_timer.stop();
          if (tvg_timer.elapsed()>_tvg_duration)        {
            rtp_dbuf=NULL;
            length=0;
            if (RETRANSMISSION)
              _state=PS_TX_DATA;
            else
              _state=PS_BYE;
            continue;
          }        
        } else if (total_bytes_rcvd>=scan_size) {       
          rtp_dbuf=NULL;
          length=0;
          _state=PS_TX_DATA;
          continue;
        }
        // Check to see if we have exhausted our window.
        u_int16_t outstanding=_rtp_session._s._sn_max-
          _rtp_session._s._sn_min;
        if (RETRANSMISSION) {
          if (outstanding>=_max_window) {
            // Yes we have. Start retransmitting.
            syslog(LOG_DEBUG, 
                   "PS_RX_DATA: %s==%d %d",
                   "Exhausted window: <_sn_max _sn_min>",
                   _rtp_session._s._sn_max, 
                   _rtp_session._s._sn_min);
            rtp_dbuf=NULL;
            length=0;
            _state=PS_TX_DATA;
            continue;
          }
        }
        rtp_dbuf=new u_int8_t[_t_spec._mtu];
        if (_tvg) {
          length=_t_spec._mtu;
        } else {
          do {
            length=::read(IN_FD, rtp_dbuf, RBUF_SIZE);
          } while (length<0 && errno==EINTR);
        }
        if (length==0) {
          syslog(LOG_ERR, "RX_DATA: Zero byte recv().");
          delete [] rtp_dbuf;
          rtp_dbuf=NULL;
        } else if (length<0) {
          syslog(LOG_ERR, "RX_DATA: Error recv(): %s", 
                 strerror(errno));
          delete [] rtp_dbuf;
          rtp_dbuf=NULL;
          length=0;
        } else if (length<(int)_t_spec._mtu) {
          syslog(LOG_ERR, "RX_DATA: Short recv(): ret==%d", ret);
          total_bytes_rcvd+=length;
        } else {
          // Valid read.
          total_bytes_rcvd+=length;
#ifdef SYSLOG
          syslog(LOG_NOTICE, "RX_DATA: total bytes rcvd==%lld",
                 total_bytes_rcvd);
#endif        
        }
        _state=PS_TX_DATA;
      }
      break;
    case PS_TX_DATA:
      {
#ifdef SYSLOG
        syslog(LOG_NOTICE, "---- PS_TX_DATA ----\n");
#endif        
        // We have nothing new to send.
        if (length<=0) {
          if (RETRANSMISSION)
            _state=PS_RETX_DATA;
          else
            _state=PS_SELECT;
          continue;
        }

        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd
                                 -ipd_req.tv_sec));
        mysleep(ipd_req, bucket);

        // We have something to send.
        rtp_hbuf=new u_int8_t[HDR_SIZE];
        if (_rtp_session.build_data_hdr(rtp_hbuf, 
                                        pt, 
                                        _rtp_session._s._sn_max, 
                                        _rtp_session._s._rtp_ts,
                                        _rtp_session._ssrc, HDR_SIZE, 
                                        false)!=0) {
          syslog(LOG_ERR, "PS_TX_DATA: Unable to build data header");
          _state=PS_SELECT;
          continue;
        }
        // Create new iovec buffer to contain data and header.
        // Used to store packet in rtp_map for any retransmissions.
        rtp_buf=new Iovec[2];
        rtp_buf[0].iov_base=(u_int8_t*)rtp_hbuf;
        rtp_buf[0].iov_len=HDR_SIZE;
        rtp_buf[1].iov_base=(u_int8_t*)rtp_dbuf;
        rtp_buf[1].iov_len=length;
#ifdef SYSLOG
        syslog(LOG_DEBUG, "PS_TX_DATA: Packet to be sent");
#endif        
        // Transmit the packet.
        int ret;
        do {
          ret=::writev(OUT_FD, rtp_buf, 2);
#ifdef SYSLOG          
          syslog(LOG_NOTICE, "PS_TX_DATA: In send loop.");
#endif
        } while (ret<0 && errno==EINTR);
        // Store buffer in case retransmission required.
        // But first CHECK to make sure that anohher packet with the
        // same sequence number is stored in the rtp_map.This should
        // NOT occur, however, if it does, then we need to report the
        // error, delete any duplicate packets and then store the new
        // one. If there are duplicate packets, then it indicates that
        // the rtp_map is not being cleared properly in response to
        // acknowledgements from the destination (this is done in the
        // PS_RX_RTCP state.
        if (RETRANSMISSION) {
          if (rtp_map[_rtp_session._s._sn_max]) {
            syslog(LOG_DEBUG, "PS_TX_DATA: packet already in rtp_map, sn==%d\n", 
                   _rtp_session._s._sn_max);
            delete (u_int8_t*)(rtp_map[_rtp_session._s._sn_max])[0].iov_base;               
            delete (u_int8_t*)(rtp_map[_rtp_session._s._sn_max])[1].iov_base;
            delete rtp_map[_rtp_session._s._sn_max];
            rtp_map[_rtp_session._s._sn_max]=NULL;
          }
          // Insert iovec buffer into the rtp_map until it has been 
          // acknowledged. Packet will    be retransmitted during any 
          // intervening retransmission cycles.
          rtp_map[_rtp_session._s._sn_max]=rtp_buf;
        } else {
          delete [] (char*)(rtp_buf[0].iov_base);
          delete [] (char*)(rtp_buf[1].iov_base);
          delete [] rtp_buf;
          rtp_buf=NULL;
        }
        // Start the duration timer for measuring stats if this is
        // the first time through the loop.
        if (first) {
          duration_timer.start();
          start_clock=times(&start_tms);
          first=false;
        }
        // Check return codes from the writev() call above.
        if (ret<0) {
          syslog(LOG_ERR, "PS_TX_DATA: %s", strerror(errno));
          printf("PS_TX_DATA: %s", strerror(errno));
          _state=PS_SELECT;
          continue;
        } else if (ret<(length+HDR_SIZE)) {
          syslog(LOG_ERR, "TX_DATA: Short send(): ret==%d", ret);
          _state=PS_SELECT;
          continue;
        }
        // Valid send.
        total_bytes_sent+=ret;
        ++_rtp_session._s._rtp_ts;
        ++_rtp_session._s._psent;
        _rtp_session._s._osent+=(ret-HDR_SIZE);
        data_bytes_sent+=ret-HDR_SIZE;
#ifdef SYSLOG
        syslog(LOG_DEBUG, "PS_TX_DATA: sn_min sn_max==%d %d\n",         
               _rtp_session._s._sn_min,
               _rtp_session._s._sn_max);
#endif
        // Update the _sn_max state.
        ++_rtp_session._s._sn_max;
        _state=PS_SELECT;
      }
      break;
    case PS_RETX_DATA:
      {
#ifdef SYSLOG
        syslog(LOG_NOTICE, "---- PS_RETX_DATA ----");
#endif        
        if (!RETRANSMISSION) {
          _state=PS_SELECT;
          continue;
        }
        if (_rtp_session._s._sn_max==_rtp_session._s._sn_min) {
          syslog(LOG_NOTICE, "PS_RETX_DATA: COMPLETED SCAN.\n");
          _state=PS_BYE;
          continue;
        }
        // Start retransmitting.
        u_int16_t sn=_rtp_session._s._sn_min;
        u_int16_t s2=_rtp_session._s._sn_max-_rtp_session._s._sn_min;
        int i;
        for (i=0; i<65537; i++) {
          rtp_buf=rtp_map[sn];
          u_int16_t s1=sn-_rtp_session._s._sn_min;
          if (!rtp_map[sn]) {
            syslog(LOG_ERR,
                   "PS_RETX_DATA: sn out of bounds: %s==%d %d %d\n",
                   "sn sn_min sn_max",
                   sn, _rtp_session._s._sn_min,
                   _rtp_session._s._sn_max);
            break;
          } else {
            sn++;
          }
          if (s1>=s2) {
            syslog(LOG_ERR, 
                   "PS_RETX_DATA: sn out of bounds: %s==%d %d %d\n", 
                   "sn sn_min sn_max",
                   sn, _rtp_session._s._sn_min, 
                   _rtp_session._s._sn_max);
            break;
          }
          RTPHdr* p = (RTPHdr*)rtp_buf[0].iov_base;
          u_int16_t s = ntohs(p->_sn);
#ifdef SYSLOG
          syslog(LOG_DEBUG, "PS_RETX_DATA: Retransmitting Packet: sn==%d", s);
          syslog(LOG_DEBUG,
                 "PS_RETX_DATA: Retransmitting Packet: sn==%d\n", 
                 s);
#endif          
          int ret;        
          do {
            ret=::writev(OUT_FD, rtp_buf, 2);
          } while (ret<0 && errno==EINTR);
          ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
          ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd-ipd_req.tv_sec));
          mysleep(ipd_req, bucket);
          if (ret<0) {
            syslog(LOG_ERR, "PS_RETX_DATA: %s", strerror(errno));
          } else if (ret<(length+HDR_SIZE)) {
            syslog(LOG_ERR, "PS_RETX_DATA: Short send(): ret==%d", ret);
          }
        }
        if (i>=65536) {
          cerr << "Maxed out retranmsit loop" << endl;
          cerr << "i" << i << endl;
          cerr << "sn_min " << _rtp_session._s._sn_min << endl;
          cerr << "sn_max " << _rtp_session._s._sn_max << endl;
          exit(1);
        }
        _state=PS_SELECT;
        continue;
      }
      break;
    case PS_RX_RTCP:
      {     
        syslog(LOG_DEBUG, "---- PS_RX_RTCP ----");
        // rtcp_buf init-ed in PS_CONN_INIT.
        do {
          ret=::read(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (ret<0 && errno==EINTR);
        if (ret<0) {
          syslog(LOG_ERR, "PS_RTCP: read error: %s\n", 
                 strerror(errno));
        }
        // Parse received packet.
        RTCPCommon* h=(RTCPCommon*)rtcp_buf;
        RTCPPacket* hh=(RTCPPacket*)rtcp_buf;
        struct timeval now, lsr, diff;
        // Currently number of RTCP packets within a compound
        // packet is hardcoded.
        // TODO: make this dynamic.
        const int num_packets=2;
        u_int8_t* p=NULL;
        for (int i=0; i<num_packets; ++i) {
          switch (h->_pt) {
          case RTCP_RR:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received Receiver Report");
            // Set watchdog flag. It not set within WATCHDOG_INTERVAL
            // watchdog thread will terminate process.
            RCVD_RTCP=true;

            if (gettimeofday(&now, NULL)!=0)
              syslog(LOG_ERR, "gettimeofday: %s\n", strerror(errno));
            _rtp_session.print_rr(rtcp_buf);
            _rtp_session._r._fraction=hh->r.rr._rr[0]._fraction;
            _rtp_session._r._lost=ntohl(hh->r.rr._rr[0]._lost<<8);
            _rtp_session._r._last_seq=ntohl(hh->r.rr._rr[0]._last_seq);
            _rtp_session._r._jitter=ntohl(hh->r.rr._rr[0]._jitter);
            _rtp_session._r._lsr=ntohl(hh->r.rr._rr[0]._lsr);
            _rtp_session._r._dlsr=ntohl(hh->r.rr._rr[0]._dlsr);
            lsr.tv_sec=(_rtp_session._r._lsr>>16);
            lsr.tv_usec=(_rtp_session._r._lsr&0x0000ffff);
            printf("lsr.tv_sec: %u\n", (u_int32_t)lsr.tv_sec);
            printf("lsr.tv_usec: %u\n", (u_int32_t)lsr.tv_usec);
            now.tv_sec&=0x0000ffff;                // Lose msbs.
            now.tv_usec&=0xffff0000;        // Lose lsbs.
            timersub(&now, &lsr, &diff);
            _rtp_session._s._rtt=diff.tv_sec+(diff.tv_usec/1e6);
            _rtp_session._s._rtt-=double(_rtp_session._r._dlsr)/65536;
            _rtp_session.report(stdout);
            p=(u_int8_t*)hh;
            p+=28;
            h=(RTCPCommon*)p;
            hh=(RTCPPacket*)p;
            break;
          case RTCP_SDES:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received SDES!\n");
            break;
          case RTCP_BYE:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received BYE!\n");
            break;
          case RTCP_APP:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received APP!\n");
            break;
          case RTCP_FB:
            {
              printf("PS_RX_RTCP: Received FB!\n");
              _rtp_session.print_ack((u_int8_t*)hh);
              // TODO Common processing.
              // hh->r._fb._sender_ssrc;
              // hh->r._fb._src_ssrc;
              // Obtain the Reqest Number from the ack packet.
              u_int16_t rn=ntohs(hh->r._fb._fci[0]._ack._pid);
              // TODO Advanced ACK processing.
              // hh->r._fb._fci[0]._ack._blp;
              // hh->r._fb._fci[0]._ack._r;
              // Clean up rtp_map.
              u_int16_t sn_min=_rtp_session._s._sn_min;
              u_int16_t sn_max=_rtp_session._s._sn_max;
              syslog(LOG_DEBUG, 
                     "PS_RX_RTCP: %s==%d %d %d\n",
                     "<sn_min sn_max rn>",
                     sn_min, sn_max, rn); 
              // These variables for modulo 2^16 arithmetic.
              u_int16_t res1=rn-sn_min;
              u_int16_t res2=sn_max-sn_min;
              // Check that Request Number is in the current
              // window(defined by sn_min and sn_max).
              if (res1<=res2) {
                u_int16_t i=sn_min;
                // These variables have changed meaning but
                // are re-used for module 2^16 arithmetic.
                // Have to be CAREFUL here. Automatic compiler
                // casts can really ruin your day/night!
                res1=i-sn_min;
                res2=rn-sn_min;
                // Clean out all packets that have been 
                // implicitly acknowledged by the Request
                // Number in the RTCP ack packet.
                if (RETRANSMISSION) {
                  while (res1<res2) {
                    Iovec* cur=rtp_map[i];
                    delete [] (char*)((cur)[0].iov_base);
                    delete [] (char*)((cur)[1].iov_base);
                    delete [] (cur);
                    rtp_map[i++]=NULL;
                    res1=i-sn_min;
                  }
                }
                // Update sn_min to point to the first
                // unacknowledged packet stored in rtp_map.
                _rtp_session._s._sn_min=rn;
              } else {
                syslog(LOG_ERR, "PS_RX_RTCP: %s==%d",
                       "Received out of range rn",
                       rn);
              }
              // TODO NACK Processing.
              // hh->r._fb._fci[0]._sender_ssrc;
              // hh->r._fb._fci[0]._src_ssrc;
              // hh->r._fb._fci[0]._nack._pid;
              // hh->r._fb._fci[0]._nack._blp;
            }
            break;
          case RTCP_PSFB:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received PSFB!");
            break;
          default:                    
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received unknown RTCP packet!");
            break;
          }
        }
        _state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
        syslog(LOG_DEBUG, "---- PS_TX_RTCP ----");
        syslog(LOG_DEBUG, "PS_TX_RTCP: rtp_map.size()==%d", 
               RTP_MAP_SZ);
        unsigned int count=1;
        struct timeval ntp_ts;
        if (gettimeofday(&ntp_ts, NULL)!=0)
          syslog(LOG_ERR, "PS_TX_RTCP: Error reading NTP timestamp\n");
        syslog(LOG_DEBUG, "About to send SR");
        unsigned int ntp_sec=ntp_ts.tv_sec;
        unsigned int ntp_frac=ntp_ts.tv_usec;
        _rtp_session.build_sr(rtcp_buf, RBUF_SIZE,
                              count,
                              _rtp_session._ssrc,
                              ntp_sec,
                              ntp_frac,
                              _rtp_session._s._rtp_ts,
                              _rtp_session._s._psent,
                              _rtp_session._s._osent);
        _rtp_session.print_sr(rtcp_buf);
        // Transmit RTCP packet.
        unsigned int r;
        do {
          r=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (r<0 && errno==EINTR);
        // Update average RTCP packet size.
        _rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
        _rtp_session.update_rate();
        syslog(LOG_DEBUG, "PS_TX_RTCP: bytes_sent==%d\n", r);
        rtcp_timer.start();
        _state=PS_SELECT;
        continue;
      }             
      break;
    case PS_BYE:
      {
        syslog(LOG_NOTICE, "---- PS_BYE ----");
        duration_timer.stop();
        const double TX_TIME=duration_timer.elapsed();
        stop_clock=times(&stop_tms);
        syslog(LOG_NOTICE, "PS_BYE: BYE Transmission");
        u_int8_t source_count=1;
        u_int32_t ssrcs[1] = { _rtp_session._ssrc };
        _rtp_session.build_bye(rtcp_buf, RBUF_SIZE, source_count, ssrcs);
        ret=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        if (ret<0) {
          syslog(LOG_ERR, "PS_BYE: BYE: failed send: %s",
                 strerror(errno));
        } else if (ret!=(int)(_t_spec._mtu+HDR_SIZE)) {
          // Successfully transmitted (at least) part of the packet.
          syslog(LOG_ERR, "FIN: Short send: ret==%d", ret);
        } else {
          syslog(LOG_INFO, "PS_BYE: BYE packet sent");
        }
        total_bytes_sent+=ret;
        const double average_bytes_rate=total_bytes_sent/(TX_TIME*1e6);
        const double average_rate=average_bytes_rate*8;
        printf("<STATS>\n");
        printf("        <total_bytes_sent> %lld </total_bytes_sent>\n", 
               total_bytes_sent);
        printf("        <data_bytes_sent>    %lld </data_bytes_sent>\n", 
               data_bytes_sent);
        printf("        <TX_TIME> %f </TX_TIME>\n", TX_TIME);
        printf("        <average_rate> %f </average_rate>\n", 
               average_rate);
        const clock_t d_clock=stop_clock-start_clock;
        const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
        const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
        const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
        const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
        printf("        <duration_clocks> %ld </duration_clocks>\n", 
               d_clock);
        printf("        <tms_utime> %ld </tms_utime>\n", d_utime);
        printf("        <tms_stime> %ld </tms_stime>\n", d_stime);
        printf("        <tms_cutime> %ld </tms_cutime>\n", d_cutime);
        printf("        <tms_cstime> %ld </tms_cstime>\n", d_cstime);
        printf("        <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",         
               sysconf(_SC_CLK_TCK));
        const double cpu_utilization = double(d_utime+d_stime)/
          double(d_clock);
        printf("        <cpu_utilization>\t%f </cpu_utilization>\n", cpu_utilization) ;
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
      syslog(LOG_ERR, "send_proc(): unknown state");
      break;
    }
  }
}

void VRServerSocket::run2()
{
  struct timeval to;
  unsigned long long total_bytes_sent=0, total_bytes_rcvd=0, data_bytes_sent=0;
  to.tv_sec=30; to.tv_usec=0;
  int ret=0;
  Timer send_timer, duration_timer, tvg_timer, rtcp_timer;
  struct tms start_tms, stop_tms;
  clock_t start_clock=0, stop_clock=0;
  int length=0;
  u_int8_t pt=PT_DATA;
  struct timespec ipd_req, ipd_rem;
  int IN_FD=0, OUT_FD=0, CTRL_FD=0;
  int RBUF_SIZE=_t_spec._mtu;
  u_int8_t* rtp_dbuf=NULL, *rtp_hbuf=NULL, *rtcp_buf=NULL, *big_buf=NULL;
  int BIG_BUF_SIZE=0;
  Iovec *rtp_buf;
  const int RTP_MAP_SZ=SEQ_SPACE_SIZE;
  IovecMap rtp_map[RTP_MAP_SZ];
  int i=0;
  for (i=0; i<RTP_MAP_SZ; ++i) 
    rtp_map[i]=NULL;
  bool first=true;
  const double RTCP_TIMEOUT=1;
  struct timeval zero_to;
  zero_to.tv_sec=0;
  zero_to.tv_usec=0;
  Timer idle_timer;
  ScanVectorIter current_scan;
  // Rate parameters.
  int bucket=_max_bucket;
  unsigned long long scan_size=0;
  struct timeval myto;
  sleep_idx=0;
  // TODO const double IDLE_TIMEOUT=10;

  // Install watchdog handler.
#ifdef WATCHDOG
  if (signal(SIGALRM, sig_alrm)==SIG_ERR) {
    syslog(LOG_ERR, "Unable to install watchdog handler.");
    exit(1);
  }
  alarm(WATCHDOG_INTERVAL);
#endif

  while (true) {
    switch (_state) {
    case PS_CONN_INIT:
      {
        int policy=SCHED_FIFO;
        struct sched_param param;
        param.sched_priority=99;
        int ret=pthread_setschedparam(pthread_self(), policy, &param);
        if (ret!=0) { 
          syslog(LOG_ERR, "setschedparam: %s", strerror(errno));
        }
        syslog(LOG_DEBUG, "---- PS_CONN_INIT ----\n");
        _tcp_sock.set_so_rcvbuf(4000000);
        _udp_sock.set_so_sndbuf(40000000);
        _tcp_sock.set_so_rcvlowat(_t_spec._mtu);
        _ctrl_sock.set_so_rcvlowat(_t_spec._mtu);

        // Connection setup.
        // 1. Setup control port for receiving incoming connections.
        cout << "Binding to 0.0.0.0/" << _ctrl_port << endl;
        _tcp_accept_sock.bind("0.0.0.0", _ctrl_port);
        _tcp_accept_sock.listen();
        cout << "Accepting incoming ctrl connections\n";
        // 2. Accept incoming control connections.        
        int sd=_tcp_accept_sock.accept();
        if (sd<=0) {
          syslog(LOG_ERR, "Invalid accept\n");
          return;
        }
        cout << "Got incoming ctrl connection\n";
        _ctrl_sock=TCPSocket(sd, _t_spec._mtu);
        // 3. Connect to remote data port.
        cout << "Connecting to " << _remote_ip.c_str() << "/"
             <<    _udp_port << " data port\n";
        _udp_sock.connect(_remote_ip, _udp_port);
        // 4. CONNECTION ESTABLISHED.

        if (_tvg) {
          printf("Test Vector Generator enabled\n");;
          IN_FD=-1;
          tvg_timer.start();
        } else if (_scan_vector.size()<=0) {
          /* Socket source */
          cout << "Binding to 0.0.0.0/" << _tcp_port << endl;
          _tcp_accept_sock.bind("0.0.0.0", _tcp_port);
          cout << "Listening\n";
          _tcp_accept_sock.listen();
          cout << "Accepting incoming data connection\n";
          int sd=_tcp_accept_sock.accept();
          if (sd<=0) {
            syslog(LOG_ERR, "Invalid accept\n");
            return;
          }
          _tcp_sock=TCPSocket(sd, _t_spec._mtu);
          IN_FD=_tcp_sock.get_sockd();
        }
        OUT_FD=_udp_sock.get_sockd();
        CTRL_FD=_ctrl_sock.get_sockd();
        cout << "IN_FD:     " << IN_FD << endl;
        cout << "OUT_FD:    " << OUT_FD << endl;
        cout << "CTRL_FD: " << CTRL_FD << endl;
        cout << "MTU:         " << _t_spec._mtu << endl;
        cout << "MAX_WINDOW:         " << _max_window << endl;
        cout << "MAX_BUCKET:         " << _max_bucket << endl;
        cout << "IPD:                " << _rtp_session._c._tx_ipd << endl;
        RBUF_SIZE=_t_spec._mtu;
        send_timer.start();
        rtcp_timer.start();
        _rtp_session._ssrc=3;
        rtcp_buf=new u_int8_t[RBUF_SIZE];
        current_scan=_scan_vector.begin();
        // Rate parameters.
        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd-ipd_req.tv_sec));
        ipd_rem.tv_sec=0;
        ipd_rem.tv_nsec=0;
        double ipd=ipd_req.tv_sec+ipd_req.tv_nsec/1e9;
        myto.tv_sec=ipd_req.tv_sec*_max_bucket;
        myto.tv_usec=int(ipd_req.tv_nsec/1000.0*_max_bucket);
        double mytopd=myto.tv_sec+myto.tv_usec/1e6;
        cout << "IPD:     " << ipd << endl;
        cout << "MYTO:     " << mytopd << endl;
        _state=PS_SELECT;
        rtp_buf=NULL;
        BIG_BUF_SIZE=(HDR_SIZE+RBUF_SIZE);
        big_buf=new u_int8_t[BIG_BUF_SIZE];
      }
      break;
    case PS_SELECT:
      {
#if SYSLOG
        syslog(LOG_NOTICE, "---- PS_RX_DATA ----\n");
#endif                
        if (_tvg) {
          tvg_timer.stop();
          if (tvg_timer.elapsed()>_tvg_duration)        {
            _state=PS_BYE;
            continue;
          }
        }
        fd_set rfd;
        if (CTRL_FD>0)
          FD_SET(CTRL_FD, &rfd);
        int n=CTRL_FD+1;
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
        _state=PS_TX_DATA;
        // TODO Add idle timer later.
      }     
      break; 
    case PS_TX_DATA:
      {
        ipd_req.tv_sec=int(_rtp_session._c._tx_ipd);
        ipd_req.tv_nsec=int(1e9*(_rtp_session._c._tx_ipd
                                 -ipd_req.tv_sec));
        mysleep(ipd_req, bucket);

        // We have something to send.
        if (_rtp_session.build_data_hdr(big_buf, 
                                        pt, 
                                        _rtp_session._s._sn_max, 
                                        _rtp_session._s._rtp_ts,
                                        _rtp_session._ssrc, 
                                        HDR_SIZE, 
                                        false)!=0) {
          syslog(LOG_ERR, "PS_TX_DATA: Unable to build data header");
          _state=PS_BYE;
          continue;
        }
        // Create new iovec buffer to contain data and header.
        // Used to store packet in rtp_map for any retransmissions.

        // Transmit the packet.
        int ret;
        do {
          ret=::write(OUT_FD, big_buf, BIG_BUF_SIZE);
        } while (ret<0 && errno==EINTR);
        if (ret<0) {
          syslog(LOG_ERR, "TX_DATA: Write error: %s", strerror(errno));
          _state=PS_BYE;
          continue;
        }

        // Start the duration timer for measuring stats if this is
        // the first time through the loop.
        if (first) {
          duration_timer.start();
          start_clock=times(&start_tms);
          first=false;
        }
        // Check return codes from the write() call above.
        if (ret<(length+HDR_SIZE)) {
          syslog(LOG_ERR, "TX_DATA: Short send(): ret==%d", ret);
          _state=PS_SELECT;
          continue;
        }
        // Valid send.
        total_bytes_sent+=ret;
        ++_rtp_session._s._rtp_ts;
        ++_rtp_session._s._psent;
        _rtp_session._s._osent+=(ret-HDR_SIZE);
        data_bytes_sent+=ret-HDR_SIZE;
        // Update the _sn_max state.
        ++_rtp_session._s._sn_max;
        _state=PS_SELECT;
      }
      break;
    case PS_RX_RTCP:
      {     
        do {
          ret=::read(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (ret<0 && errno==EINTR);
        if (ret<0) {
          syslog(LOG_ERR, "PS_RTCP: read error: %s\n", 
                 strerror(errno));
        }
        // Parse received packet.
        RTCPCommon* h=(RTCPCommon*)rtcp_buf;
        RTCPPacket* hh=(RTCPPacket*)rtcp_buf;
        struct timeval now, lsr, diff;
        // Currently number of RTCP packets within a compound
        // packet is hardcoded.
        // TODO: make this dynamic.
        const int num_packets=2;
        u_int8_t* p=NULL;
        for (int i=0; i<num_packets; ++i) {
          switch (h->_pt) {
          case RTCP_RR:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received Receiver Report");
            // Set watchdog flag. It not set within WATCHDOG_INTERVAL
            // watchdog thread will terminate process.
            RCVD_RTCP=true;

            if (gettimeofday(&now, NULL)!=0)
              syslog(LOG_ERR, "gettimeofday: %s\n", strerror(errno));
            _rtp_session.print_rr(rtcp_buf);
            _rtp_session._r._fraction=hh->r.rr._rr[0]._fraction;
            _rtp_session._r._lost=ntohl(hh->r.rr._rr[0]._lost<<8);
            _rtp_session._r._last_seq=ntohl(hh->r.rr._rr[0]._last_seq);
            _rtp_session._r._jitter=ntohl(hh->r.rr._rr[0]._jitter);
            _rtp_session._r._lsr=ntohl(hh->r.rr._rr[0]._lsr);
            _rtp_session._r._dlsr=ntohl(hh->r.rr._rr[0]._dlsr);
            lsr.tv_sec=(_rtp_session._r._lsr>>16);
            lsr.tv_usec=(_rtp_session._r._lsr&0x0000ffff);
            now.tv_sec&=0x0000ffff;                // Lose msbs.
            now.tv_usec&=0xffff0000;        // Lose lsbs.
            timersub(&now, &lsr, &diff);
            _rtp_session._s._rtt=diff.tv_sec+(diff.tv_usec/1e6);
            _rtp_session._s._rtt-=double(_rtp_session._r._dlsr)/65536;
            // FIXME
            // _rtp_session.report(stdout);
            p=(u_int8_t*)hh;
            p+=28;
            h=(RTCPCommon*)p;
            hh=(RTCPPacket*)p;
            break;
          case RTCP_SDES:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received SDES!\n");
            break;
          case RTCP_BYE:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received BYE!\n");
            break;
          case RTCP_APP:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received APP!\n");
            break;
          case RTCP_FB:
            {
              _rtp_session.print_ack((u_int8_t*)hh);
              // TODO Common processing.
              // hh->r._fb._sender_ssrc;
              // hh->r._fb._src_ssrc;
              // Obtain the Reqest Number from the ack packet.
              u_int16_t rn=ntohs(hh->r._fb._fci[0]._ack._pid);
              // TODO Advanced ACK processing.
              // hh->r._fb._fci[0]._ack._blp;
              // hh->r._fb._fci[0]._ack._r;
              // Clean up rtp_map.
              u_int16_t sn_min=_rtp_session._s._sn_min;
              u_int16_t sn_max=_rtp_session._s._sn_max; 
              // These variables for modulo 2^16 arithmetic.
              u_int16_t res1=rn-sn_min;
              u_int16_t res2=sn_max-sn_min;
              // Check that Request Number is in the current
              // window(defined by sn_min and sn_max).
              // TODO NACK Processing.
              // hh->r._fb._fci[0]._sender_ssrc;
              // hh->r._fb._fci[0]._src_ssrc;
              // hh->r._fb._fci[0]._nack._pid;
              // hh->r._fb._fci[0]._nack._blp;
            }
            break;
          case RTCP_PSFB:
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received PSFB!");
            break;
          default:                    
            syslog(LOG_NOTICE, "PS_RX_RTCP: Received unknown RTCP packet!");
            break;
          }
        }
        _state=PS_SELECT;
      }
      break;
    case PS_TX_RTCP:
      {
        unsigned int count=1;
        struct timeval ntp_ts;
        if (gettimeofday(&ntp_ts, NULL)!=0)
          syslog(LOG_ERR, "PS_TX_RTCP: Error reading NTP timestamp\n");
        unsigned int ntp_sec=ntp_ts.tv_sec;
        unsigned int ntp_frac=ntp_ts.tv_usec;
        _rtp_session.build_sr(rtcp_buf, RBUF_SIZE,
                              count,
                              _rtp_session._ssrc,
                              ntp_sec,
                              ntp_frac,
                              _rtp_session._s._rtp_ts,
                              _rtp_session._s._psent,
                              _rtp_session._s._osent);
        _rtp_session.print_sr(rtcp_buf);
        // Transmit RTCP packet.
        unsigned int r;
        do {
          r=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        } while (r<0 && errno==EINTR);
        // Update average RTCP packet size.
        _rtp_session._r._avg_rtcp_size = (1/16)*(ret+8+20)+                                                                                                
          (15/16) * _rtp_session._r._avg_rtcp_size;
        _rtp_session.update_rate();
        rtcp_timer.start();
        _state=PS_SELECT;
        continue;
      }             
      break;
    case PS_BYE:
      {
        syslog(LOG_NOTICE, "---- PS_BYE ----");
        duration_timer.stop();
        const double TX_TIME=duration_timer.elapsed();
        stop_clock=times(&stop_tms);
        syslog(LOG_NOTICE, "PS_BYE: BYE Transmission");
        u_int8_t source_count=1;
        u_int32_t ssrcs[1] = { _rtp_session._ssrc };
        _rtp_session.build_bye(rtcp_buf, RBUF_SIZE, source_count, ssrcs);
        ret=::write(CTRL_FD, rtcp_buf, RBUF_SIZE);
        if (ret<0) {
          syslog(LOG_ERR, "PS_BYE: BYE: failed send: %s",
                 strerror(errno));
        } else if (ret!=(int)(_t_spec._mtu+HDR_SIZE)) {
          // Successfully transmitted (at least) part of the packet.
          syslog(LOG_ERR, "FIN: Short send: ret==%d", ret);
        } else {
          syslog(LOG_INFO, "PS_BYE: BYE packet sent");
        }
        total_bytes_sent+=ret;
        const double average_bytes_rate=total_bytes_sent/(TX_TIME*1e6);
        const double average_rate=average_bytes_rate*8;
        printf("<STATS>\n");
        printf("        <total_bytes_sent> %lld </total_bytes_sent>\n", 
               total_bytes_sent);
        printf("        <data_bytes_sent>    %lld </data_bytes_sent>\n", 
               data_bytes_sent);
        printf("        <TX_TIME> %f </TX_TIME>\n", TX_TIME);
        printf("        <average_rate> %f </average_rate>\n", 
               average_rate);
        const clock_t d_clock=stop_clock-start_clock;
        const clock_t d_utime=stop_tms.tms_utime-start_tms.tms_utime;
        const clock_t d_stime=stop_tms.tms_stime-start_tms.tms_stime;
        const clock_t d_cutime=stop_tms.tms_cutime-start_tms.tms_cutime;
        const clock_t d_cstime=stop_tms.tms_cstime-start_tms.tms_cstime;
        printf("        <duration_clocks> %ld </duration_clocks>\n", 
               d_clock);
        printf("        <tms_utime> %ld </tms_utime>\n", d_utime);
        printf("        <tms_stime> %ld </tms_stime>\n", d_stime);
        printf("        <tms_cutime> %ld </tms_cutime>\n", d_cutime);
        printf("        <tms_cstime> %ld </tms_cstime>\n", d_cstime);
        printf("        <_SC_CLK_TCK> %ld </_SC_CLK_TCK>\n",         
               sysconf(_SC_CLK_TCK));
        const double cpu_utilization = double(d_utime+d_stime)/
          double(d_clock);
        printf("        <cpu_utilization>\t%f </cpu_utilization>\n", cpu_utilization) ;
        printf("</STATS>\n");
        _state=PS_APP_BYE;
        // Get resource usage.
        print_rusage();
        // FIXME.
        ofstream sleep_stream("sleep.dat");
        for (int j=0; j<65535; ++j)
          sleep_stream << sleep_times[j] << endl;
        sleep_stream.close();
        ::close(IN_FD);
        ::close(OUT_FD);
        ::close(CTRL_FD);
        return;
      }
      break;
    default:
      syslog(LOG_ERR, "send_proc(): unknown state");
      break;
    }
  }
}

VRServerSocket::VRServerSocket(const TSpec& t, const string& ip, 
                               const int& uport, const int& tport,
                               const int& cport, const bool& tvg, 
                               const double& tvg_duration,
                               const u_int32_t& freq, 
                               const u_int32_t& packet_size,
                               const u_int32_t& bits_per_sample,
                               const string& in_file_name,
                               const ScanVector& scan_vector,
                               const u_int16_t& max_window,        
                               const u_int32_t& max_bucket):
  VRSocket(t, ip, uport, tport, cport, tvg, tvg_duration, scan_vector, max_window),
_max_bucket(max_bucket)
{
  _state=PS_CONN_INIT;
  _udp_sock.set_so_reuseaddr(1);
  _tcp_sock.set_so_reuseaddr(1);
  _tcp_accept_sock.set_so_reuseaddr(1);
  _rtp_session._sampling_frequency=freq;
  _rtp_session._packet_size=packet_size;
  _rtp_session._bits_per_sample=bits_per_sample;
  _in_file_name=in_file_name;
}

VRServerSocket::VRServerSocket(const VRServerSocket &s):
VRSocket(s._t_spec), _max_bucket(s._max_bucket)
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
  _in_file_name=s._in_file_name;
  _max_window=s._max_window;
}

VRServerSocket::~VRServerSocket()
{
  // This is an important feature of the VRServerSocket class. Especially
  // for copying of VRServerSockets. VRServerSockets do *not* close their
  // sockets when they are destroyed.
}

    
    
                                                     
                                                     
    
