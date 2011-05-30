/*
 *  RTPSession.cpp
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

#include <math.h>
#include <RTPSession.h>


RTPSession::RTPSession(const TSpec& t)
{
  _ssrc=1;
  _sampling_frequency=0;
  _packet_size=1024;
  _bits_per_sample=1;
  _t_spec=t;
  _r._tp.tv_sec=0;
  _r._tp.tv_usec=0;
  _r._tc.tv_sec=0;
  _r._tc.tv_usec=0;
  _r._tn.tv_sec=0;
  _r._tn.tv_usec=0;
  _r._pmembers=2;
  _r._senders=1;
  _r._rtcp_bw=0;
  _r._we_sent=false;
  _r._avg_rtcp_size=0;
  _r._initial=true;
  _r._lost=0;
  _r._fraction=0;
  _r._lsr=0; 
  _r._dlsr=0; 
  _r._lsrr.tv_sec=0; 
  _r._lsrr.tv_usec=0; 
  _r._ldata.tv_sec=0; 
  _r._ldata.tv_usec=0; 

  _r._max_seq=0;	/* highest seq. number seen */
  _r._cycles=0;	 /* shifted count of seq. number cycles */
  _r._base_seq=0;	   /* base seq number */
  _r._bad_seq=RTP_SEQ_MOD+1;	/* last 'bad' seq number + 1 */
  _r._probation=2;	  /* sequ. packets till source is valid */
  _r._received=0;	   /* packets received */
  _r._expected_prior=0; /* packet expected at last interval */
  _r._received_prior=0; /* packet received at last interval */
  _r._transit=0;	/* relative trans time for prev pkt */
  _r._jitter=0;	 /* estimated jitter */

  _r._ntp_sec=0;
  _r._ntp_frac=0;
  _r._rtp_ts=0;
  _r._d=0;
  _r._seq_init=0;

  _s._rtp_ts=0;
  _s._psent=0;
  _s._osent=0;
  _s._rtt=0;
  _s._sn_max=0;
  _s._sn_min=0;

  _c._tx_rate=_t_spec._peak_rate;
  _c._tx_ipd=_t_spec._mtu*8.0/_c._tx_rate;
}

void RTPSession::report(FILE* s) {
  const double rtp_ts_interval=double(_packet_size)/
    (double(_bits_per_sample)
     *double(_sampling_frequency*1e3));
  struct timeval now;
  if (gettimeofday(&now, NULL)!=0)
    fprintf(s, "ERROR: Unable to gettimeofday()\n");
  double n=now.tv_sec+now.tv_usec/1e6;
  fprintf(s, "<RTPSESSION>\n");
  fprintf(s, "	<TIME>\n");
  fprintf(s, "	    <now> %f </now>\n", n);
  fprintf(s, "	</TIME>\n");
  fprintf(s, "	<CALC>\n");
  fprintf(s, "	    <_rtp_ts_int> %f </_rtp_ts_int>\n", rtp_ts_interval);
  fprintf(s, "	</CALC>\n");
  fprintf(s, "	<GENERAL>\n");
  fprintf(s, "	    <_sampling_frequency> %u </_sampling_frequency>\n", (u_int32_t)_sampling_frequency);
  fprintf(s, "	    <_packet_size>	%u <_packet_size>\n", 
          (u_int32_t)_packet_size);
  fprintf(s, "	    <_bits_per_sample> %u <_bits_per_sample>\n", 
          (u_int32_t)_bits_per_sample);
  fprintf(s, "	</GENERAL>\n");
  fprintf(s, "	<TSPEC>\n");
  fprintf(s, "	    <_cc> %u </_cc>\n", _t_spec._cc);
  fprintf(s, "	    <_mtu> %u </_mtu>\n", _t_spec._mtu);
  fprintf(s, "	    <_peak_rate> %u </_peak_rate>\n", _t_spec._peak_rate);
  fprintf(s, "	    <_min_rate> %u </_min_rate>\n", _t_spec._min_rate);
  fprintf(s, "	</TSPEC>\n");
  fprintf(s, "	<RECEIVER>\n");
  fprintf(s, "	    <_tp.tv_sec> %u </_tp.tv_sec>\n", 
          (u_int32_t)_r._tp.tv_sec);
  fprintf(s, "	    <_tp.tv_usec> %u </_tp.tv_usec>\n", 
          (u_int32_t)_r._tp.tv_usec);
  fprintf(s, "	    <_tc.tv_sec> %u </_tc.tv_sec>\n", 
          (u_int32_t)_r._tc.tv_sec);
  fprintf(s, "	    <_tc.tv_usec> %u </_tc.tv_usec>\n", 
          (u_int32_t)_r._tc.tv_usec);
  fprintf(s, "	    <_tn.tv_sec> %u </_tn.tv_sec>\n", 
          (u_int32_t)_r._tn.tv_sec);
  fprintf(s, "	    <_tn.tv_usec> %u </_tn.tv_usec>\n", 
          (u_int32_t)_r._tn.tv_usec);
  fprintf(s, "	    <_pmembers> %u </_pmembers>\n", _r._pmembers);
  fprintf(s, "	    <_senders> %u </_senders>\n", _r._senders);
  fprintf(s, "	    <_rtcp_bw> %u </_rtcp_bw>\n", _r._rtcp_bw);
  fprintf(s, "	    <_we_sent> %u </_we_sent>\n", _r._we_sent);
  fprintf(s, "	    <_avg_rtcp_size> %u </_avg_rtcp_size> \n", _r._avg_rtcp_size);
  fprintf(s, "	    <_initial> %u </_initial>\n", _r._initial);
  fprintf(s, "	    <_lost> %u </_lost>\n", _r._lost);
  fprintf(s, "	    <_fraction> %u </_fraction>\n", _r._fraction);
  fprintf(s, "	    <_lsr> %u </_lsr>\n", _r._lsr); 
  fprintf(s, "	    <_dlsr> %u </_dlsr>\n", _r._dlsr); 
  fprintf(s, "	    <_lsrr.tv_sec> %u </_lsrr.tv_sec>\n", 
          (u_int32_t)_r._lsrr.tv_sec); 
  fprintf(s, "	    <_lsrr.tv_usec> %u </_lsrr.tv_usec>\n", 
          (u_int32_t)_r._lsrr.tv_usec); 
  fprintf(s, "	    <_ldata.tv_sec> %u </_ldata.tv_sec>\n", 
          (u_int32_t)_r._ldata.tv_sec); 
  fprintf(s, "	    <_ldata.tv_usec> %u </_ldata.tv_usec>\n", 
          (u_int32_t)_r._ldata.tv_usec); 
  fprintf(s, "	    <_max_seq> %u </_max_seq>\n", (u_int32_t)_r._max_seq);
  fprintf(s, "	    <_cycles> %u </_cycles>\n", (u_int32_t)_r._cycles);
  fprintf(s, "	    <_base_seq> %u </_base_seq>\n", (u_int32_t)_r._base_seq);
  fprintf(s, "	    <_bad_seq> %u </_bad_seq>\n", (u_int32_t)_r._bad_seq);
  fprintf(s, "	    <_probation> %u </_probation>\n", 
          (u_int32_t)_r._probation);
  fprintf(s, "	    <_received> %u </_received>\n", (u_int32_t)_r._received);
  fprintf(s, "	    <_expected_prior> %u </_expected_prior>\n", 
          (u_int32_t)_r._expected_prior);
  fprintf(s, "	    <_received_prior> %u </_received_prior>\n", 
          (u_int32_t)_r._received_prior);
  fprintf(s, "	    <_transit> %u </_transit>\n", (u_int32_t)_r._transit);
  fprintf(s, "	    <_jitter> %u </_jitter>\n", (u_int32_t)_r._jitter);
  fprintf(s, "	    <_rtp_ts> %u </_rtp_ts>\n", _r._rtp_ts);
  fprintf(s, "	    <_ntp_sec> %u </_ntp_sec>\n", _r._ntp_sec);
  fprintf(s, "	    <_ntp_frac> %u </_ntp_frac>\n", _r._ntp_frac);
  fprintf(s, "	    <_d> %f </_d>\n", _r._d);
  fprintf(s, "	    <_seq_init> %u </_seq_init>\n", _r._seq_init);
  fprintf(s, "	</RECEIVER>\n");
  fprintf(s, "	<SENDER>\n");
  fprintf(s, "	    <_rtp_ts> %u </_rtp_ts>\n", _s._rtp_ts);
  fprintf(s, "	    <_psent> %u </_psent>\n", _s._psent);
  fprintf(s, "	    <_osent> %u </_osent>\n", _s._osent);
  fprintf(s, "	    <_rtt> %f </_rtt>\n", _s._rtt);
  fprintf(s, "	    <_sn_max> %u </_sn_max>\n", _s._sn_max);
  fprintf(s, "	    <_sn_min> %u </_sn_min>\n", _s._sn_min);
  fprintf(s, "	</SENDER>\n");
  fprintf(s, "	<CONGESTION>\n");
  fprintf(s, "	    <_tx_rate> %f </_tx_rate>\n", _c._tx_rate);
  fprintf(s, "	    <_tx_ipd> %f </_tx_ipd>\n", _c._tx_ipd);
  fprintf(s, "	</CONGESTION>\n");
  fprintf(s, "</RTPSESSION>\n");
}

void RTPSession::init_seq(u_int16_t seq)
{
  _r._base_seq = seq;
  _r._max_seq = seq;
  _r._bad_seq = RTP_SEQ_MOD + 1;   /* so seq == bad_seq is false */
  _r._cycles = 0;
  _r._received = 0;
  _r._received_prior = 0;
  _r._expected_prior = 0;
  printf("init_seq");
  ++_r._seq_init;
}

int RTPSession::update_seq(u_int16_t seq)
{
  u_int16_t udelta = seq - _r._max_seq;
  // FIXME
  // const int MAX_DROPOUT = 3000;
  const int MAX_DROPOUT = 2;
  const int MAX_MISORDER = 100;
  const int MIN_SEQUENTIAL = 2;

  /*
   * Source is not valid until MIN_SEQUENTIAL packets with
   * sequential sequence numbers have been received.
   */
  if (_r._probation) {
    /* packet is in sequence */
    if (seq == _r._max_seq + 1) {
      _r._probation--;
      _r._max_seq = seq;
      if (_r._probation == 0) {
        init_seq(seq);
        _r._received++;
        return 1;
      }
    } else {
      _r._probation = MIN_SEQUENTIAL - 1;
      _r._max_seq = seq;
    }
    return 0;
  } else if (udelta < MAX_DROPOUT) {
    /* in order, with permissible gap */
    if (seq < _r._max_seq) {
      /*
       * Sequence number wrapped - count another 64K cycle.
       */
      _r._cycles += RTP_SEQ_MOD;
    }
    _r._max_seq = seq;
  } else if (udelta <= RTP_SEQ_MOD - MAX_MISORDER) {
    printf("update_seq: udelta>MAX_DROPOUT: %d\n", udelta);
    /* the sequence number made a very large jump */
    if (seq == _r._bad_seq) {
      /*
       * Two sequential packets -- assume that the other side
       * restarted without telling us so just re-sync
       * (i.e., pretend this was the first packet).
       */
      init_seq(seq);
      // FIXME
      return 0;
    } else {
      _r._bad_seq = (seq + 1) & (RTP_SEQ_MOD-1);
      return 0;
    }
  } else {
    /* duplicate or reordered packet */
    printf("update_seq: dup or re-ordered packet\n");
  }
  _r._received++;
  return 1;
}

void RTPSession::update_rate()
{
  switch (_t_spec._cc) {
  case RCC_OPENLOOP:
    {
      double loss=_r._fraction/256.;
      _c._tx_rate=_t_spec._peak_rate;
      _c._tx_ipd=_t_spec._mtu*8.0/_c._tx_rate;
      printf("OPEN: tx_rate %f\n", _c._tx_rate);
      printf("OPEN: tx_ipd %f\n", _c._tx_ipd);
      printf("OPEN: loss %f\n", loss);
    }
    break;
  case RCC_TFRC:
    {
      /** Reference: 
       * http://www.psc.edu/networking/papers/tcp_friendly.html
       * bw = 1.3 * MTU/(RTT*sqrt(Loss))
       */
      double loss=_r._fraction/256.;
      printf("TFRC: loss %f\n", loss);
      if (loss>0) {
        _c._tx_rate=1.3*_t_spec._mtu/(_s._rtt*sqrt(loss));
      } else {
        _c._tx_rate*=2;
      }
      _c._tx_rate=_c._tx_rate>_t_spec._peak_rate?
        _t_spec._peak_rate:_c._tx_rate;
      _c._tx_rate=_c._tx_rate<_t_spec._min_rate?
        _t_spec._min_rate:_c._tx_rate;
      printf("TFRC: tx_rate %f\n", _c._tx_rate);
      _c._tx_ipd=1./_c._tx_rate;
      printf("TFRC: tx_ipd %f\n", _c._tx_ipd);
    }
    break;
  case RCC_DLRC:
    {
      double loss=_r._fraction/256.;
      double delay=_s._rtt;
      printf("DLRC: loss %f\n", loss);
      printf("DLRC: delay %f\n", delay);
      if (loss>0) {
        _c._tx_rate=1.3*_t_spec._mtu/(_s._rtt*sqrt(loss));
      } else {
        _c._tx_rate*=2;
      }
      _c._tx_rate=_c._tx_rate>_t_spec._peak_rate?
        _t_spec._peak_rate:_c._tx_rate;
      _c._tx_rate=_c._tx_rate<_t_spec._min_rate?
        _t_spec._min_rate:_c._tx_rate;
      printf("DLRC: tx_rate %f\n", _c._tx_rate);
      _c._tx_ipd=1./_c._tx_rate;
      printf("DLRC: tx_ipd %f\n", _c._tx_ipd);
    }
    break;
  }
}

void RTPSession::print_sr(uint8_t* sb)
{
  struct RTCPPacket* h=(RTCPPacket*)&(sb[0]);

  uint8_t pt=h->h._common._pt;
  u_int16_t length=ntohs(h->h._common._length);
  u_int32_t ssrc=ntohl(h->r.sr._ssrc);
  u_int32_t ntp_sec=ntohl(h->r.sr._ntp_sec);
  u_int32_t ntp_frac=ntohl(h->r.sr._ntp_frac);
  u_int32_t rtp_ts=ntohl(h->r.sr._rtp_ts);	
  u_int32_t psent=ntohl(h->r.sr._psent);	
  u_int32_t osent=ntohl(h->r.sr._osent);	
  printf("<sender_report>\n");
  printf("    <pt> %x </pt>\n", pt);
  printf("    <length> %u </length>\n", length);
  printf("    <ssrc> %u </ssrc>\n", ssrc);
  printf("    <ntp_sec> %u </ntp_sec>\n", ntp_sec);
  printf("    <ntp_frac> %u </ntp_frac>\n", ntp_frac);
  printf("    <rtp_ts> %u </rtp_ts>\n", rtp_ts);
  printf("    <psent> %u </psent>\n", psent);
  printf("    <osent> %u </osent>\n", osent);
  printf("</sender_report>\n");
}

void RTPSession::print_rr(uint8_t* sb)
{
  struct RTCPPacket* h=(RTCPPacket*)&(sb[0]);

  uint8_t pt=h->h._common._pt;
  u_int16_t length=ntohs(h->h._common._length);
  u_int32_t ssrc=ntohl(h->r.rr._rr[0]._ssrc);
  uint8_t fraction=h->r.rr._rr[0]._fraction;
  u_int32_t lost=ntohl(h->r.rr._rr[0]._lost);
  u_int32_t last_seq=ntohl(h->r.rr._rr[0]._last_seq);	
  u_int32_t jitter=ntohl(h->r.rr._rr[0]._jitter);	
  u_int32_t lsr=ntohl(h->r.rr._rr[0]._lsr);	
  u_int32_t dlsr=ntohl(h->r.rr._rr[0]._dlsr);	
  printf("<receiver_report>\n");
  printf("    <pt> %x </pt>\n", pt);
  printf("    <length> %u </length>\n", length);
  printf("    <ssrc> %u </ssrc>\n", ssrc);
  printf("    <fraction> %u </fraction>\n", fraction);
  printf("    <lost> %u </lost>\n", lost);
  printf("    <last_seq> %u <last_seq>\n", last_seq);
  printf("    <jitter> %u </jitter>\n", jitter);
  printf("    <lsr> %u </lsr>\n", lsr);
  printf("    <dlsr> %u </dlsr>\n", dlsr);
  printf("</receiver_report>\n");
}

void RTPSession::print_nack(uint8_t* sb)
{
  struct RTCPPacket* h=(RTCPPacket*)&(sb[0]);

  uint8_t pt=h->h._common._pt;
  uint8_t fmt=h->h._common._count;
  u_int16_t length=ntohs(h->h._common._length);
  u_int32_t sender_ssrc=ntohl(h->r._fb._sender_ssrc);
  u_int32_t src_ssrc=ntohl(h->r._fb._src_ssrc);

  printf("<nack>\n");
  printf("    <pt> %x </pt>\n", pt);
  printf("    <fmt> %x </fmt>\n", fmt);
  printf("    <length> %u </length>\n", length);
  printf("    <sender_ssrc> %u </sender_ssrc>\n", sender_ssrc);
  printf("    <src_ssrc> %u </src_ssrc>\n", src_ssrc);
  printf("    <feedback>\n");
  for (int i=0; i<(length-3); ++i) {
    u_int16_t pid=ntohs(h->r._fb._fci[i]._nack._pid);
    u_int8_t blp=ntohs(h->r._fb._fci[i]._nack._blp);
    printf("    <pid> %u </pid>\n", pid);
    printf("    <blp> %u </blp>\n", blp);
  }	
  printf("    </feedback>\n");
  printf("</nack>\n");
}

void RTPSession::print_ack(uint8_t* sb)
{
  struct RTCPPacket* h=(RTCPPacket*)&(sb[0]);

  uint8_t pt=h->h._common._pt;
  uint8_t fmt=h->h._common._count;
  u_int16_t length=ntohs(h->h._common._length);
  u_int32_t sender_ssrc=ntohl(h->r._fb._sender_ssrc);
  u_int32_t src_ssrc=ntohl(h->r._fb._src_ssrc);

  printf("<ack>\n");
  printf("    <pt> %x </pt>\n", pt);
  printf("    <fmt> %x </fmt>\n", fmt);
  printf("    <length> %u </length>\n", length);
  printf("    <sender_ssrc> %u </sender_ssrc>\n", sender_ssrc);
  printf("    <src_ssrc> %u </src_ssrc>\n", src_ssrc);
  printf("    <feedback>\n");
  for (int i=0; i<(length-3); ++i) {
    u_int16_t pid=ntohs(h->r._fb._fci[i]._ack._pid);
    u_int8_t r=ntohs(h->r._fb._fci[i]._ack._r);
    u_int8_t blp=ntohs(h->r._fb._fci[i]._ack._blp);
    printf("        <pid> %u </pid>\n", pid);
    printf("        <r> %u </r>\n", r);
    printf("        <blp> %u </blp>\n", blp);
  }
  printf("    </feedback>\n");
  printf("</ack>\n");
}


