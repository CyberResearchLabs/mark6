/*
 *  RTPSession.h
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

#ifndef RTCPSESSION_H
#define RTCPSESSION_H

#include <common.h>
#include <Socket.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <RTP.h>

/** RTPSession class
  */

struct RTPSession
{
	/** Common parameter
	  * This determines the traffic parameters for this RTP session.
	  * It specified the mtu size, peak rate, the type of congestioon 
	  * control to be used by by the update_rate() method to update the 
	  * _c._tx_rate etc.
	  */
	TSpec _t_spec;
	/** Common parameter
	  * Synchronization source.
	  */
	u_int32_t _ssrc;
	/** Common parameter
	  * Sampling frequency.
	  */
	u_int32_t _sampling_frequency;
	/** Common parameter.
	  * packet size in bytes.
	  */
	u_int32_t _packet_size;
	/** Common parameter.
	  * Number of bits per sample.
	  */
	u_int32_t _bits_per_sample;
	struct {
		/** Time
	  	  * The last time an RTCP packet was transmitted.
	  	  */
		struct timeval _tp;
		/** Time
	  	  * The currnt time.
	  	  */
		struct timeval _tc;
		/** Time
	  	  * The next schedule transmission time of an RTCP packet.
	  	  */
		struct timeval _tn;
		/** Session state.
	  	  * The extimated number of session members at the time tn was
	  	  * last recomputed.
	  	  */
		u_int32_t _pmembers;
		/** Session state.
	  	  * The most current estimate for the number of senders in the session.
	  	  */
		u_int32_t _senders;
		/** Session state.
	  	  * The target RTCP bandwidth.
	  	  */
		u_int32_t _rtcp_bw;
		/** Session state.
	  	 * Flag that is true if application has sent data since the 2nd previous
	  	 * RTCP report was transmitted.
	  	 */
		bool _we_sent;
		/** Session state.
	  	 * Average computed RTCP packet size in octets. Includes lower layer
	  	 * tranport and network protocol headers.
	  	 */
		u_int32_t _avg_rtcp_size;
		/** Session state.
	  	 * Flag that is true if application has not yet sent an RTCP packet.
	  	 */
		bool _initial;
		/** Session state. 
		  * The fraction of RTP data packets from source SSRC_n lost since the
      	  * previous SR or RR packet was sent, expressed as a fixed point
      	  * number with the binary point at the left edge of the field.  (That
      	  * is equivalent to taking the integer part after multiplying the
      	  * loss fraction by 256.)  This fraction is defined to be the number
      	  * of packets lost divided by the number of packets expected.
		  * (RFC3550).
		  */
    	u_int32_t _fraction;
		/** Session state.
		  * The total number of RTP data packets from source SSRC_n that have
      	  * been lost since the beginning of reception.  This number is
      	  * defined to be the number of packets expected less the number of
      	  * packets actually received, where the number of packets received
      	  * includes any which are late or duplicates.  Thus, packets that
      	  * arrive late are not counted as lost, and the loss may be negative
      	  * if there are duplicates.  The number of packets expected is
      	  * defined to be the extended last sequence number received, as
      	  * defined next, less the initial sequence number received.
		  * (RFC3550).
		  */
    	int32_t _lost;
		/** Session state.
		  * The low 16 bits contain the highest sequence number received in an
      	  * RTP data packet from source SSRC_n, and the most significant 16
      	  * bits extend that sequence number with the corresponding count of
      	  * sequence number cycles
		  * (RFC3550).
	 	  */
    	u_int32_t _last_seq;
		/** Session state.
		  * An estimate of the statistical variance of the RTP data packet
      	  * interarrival time, measured in timestamp units and expressed as an
      	  * unsigned integer.  The interarrival jitter J is defined to be the
      	  * mean deviation (smoothed absolute value) of the difference D in
      	  * packet spacing at the receiver compared to the sender for a pair
      	  * of packets.  As shown in the equation below, this is equivalent to
      	  * the difference in the "relative transit time" for the two packets;
      	  * the relative transit time is the difference between a packet's RTP
      	  * timestamp and the receiver's clock at the time of arrival,
      	  * measured in the same units.
	  	  * 
      	  * If Si is the RTP timestamp from packet i, and Ri is the time of
      	  * arrival in RTP timestamp units for packet i, then for two packets
      	  * i and j, D may be expressed as
	  	  * 
          * D(i,j) = (Rj - Ri) - (Sj - Si) = (Rj - Sj) - (Ri - Si)
	  	  * 
      	  * The interarrival jitter SHOULD be calculated continuously as each
      	  * data packet i is received from source SSRC_n, using this
      	  * difference D for that packet and the previous packet i-1 in order
      	  * of arrival (not necessarily in sequence), according to the formula
	  	  * 
          * J(i) = J(i-1) + (|D(i-1,i)| - J(i-1))/16
	  	  * 
      	  * Whenever a reception report is issued, the current value of J is
      	  * sampled.
		  * (RFC3550).
	 	  */
    	double _jitter;
		/** Session state.
		  * The middle 32 bits out of 64 in the NTP timestamp (as explained in
      	  * Section 4) received as part of the most recent RTCP sender report
      	  * (SR) packet from source SSRC_n.  If no SR has been received yet,
      	  * the field is set to zero.
  	 	  * (RFC3550)
  		  */
    	u_int32_t _lsr; 
		/** Session state.
		  * The delay, expressed in units of 1/65536 seconds, between
      	  * receiving the last SR packet from source SSRC_n and sending this
      	  * reception report block.  If no SR packet has been received yet
      	  * from SSRC_n, the DLSR field is set to zero.
  	 	  * (RFC3550)
  		  */
    	u_int32_t _dlsr; 
		/** Session state.
		  * The time at which the last receiver report was received.
  		  */
		struct timeval _lsrr;	
		/** Session state.
		  * The time at which the last data packet was received.
  		  */
		struct timeval _ldata;	
		/** Source state. 
		  * highest seq. number seen
		  * RFC3550.
		  */
		u_int16_t _max_seq;        
		/** Source state. 
		  * shifted count of seq. number cycles
		  * RFC3550.
		  */
		u_int32_t _cycles;         
		/** Source state. 
		  * base seq number
		  * RFC3550.
		  */
		u_int32_t _base_seq;       
		/** Source state. 
		  * last 'bad' seq number + 1
		  * RFC3550.
		  */
		u_int32_t _bad_seq;        
		/** Source state. 
		  * sequ. packets till source is valid
		  * RFC3550.
		  */
		u_int32_t _probation;      
		/** Source state. 
		  * packets received
		  * RFC3550.
		  */
		u_int32_t _received;       
		/** Source state. 
 		  * packet expected at last interval
		  * RFC3550.
		  */
		u_int32_t _expected_prior;
		/** Source state. 
		  * packet received at last interval
		  * RFC3550.
		  */
		u_int32_t _received_prior; 
		/** Source state. 
		  * relative trans time for prev pkt
		  * RFC3550.
		  */
		u_int32_t _transit;        
		/** Receiver stat.
	      * NTP timestamp MSW.
		  */
		u_int32_t _ntp_sec;
		/** Receiver stat.
	      * NTP timestamp LSW.
		  */
		u_int32_t _ntp_frac;
		/** Receiver state.
	      * RTP Timestamp. This is used in combination with the NTP
	  	  * timestamp above to syncrhonize the RTP and NTP timescales.
		  */
		u_int32_t _rtp_ts;
		/** Receiver state.
		  * Difference in packet spacing 
		  */
		double _d;					
		/** Receiver state.
		  * Number of times MAX_DROPOUT exceeded and sequence numbers
	 	  * re-initialized.
		  */
		int _seq_init;
	} _r;

	struct {
		/** Source stat.
	      * RTP Timestamp.
		  */
		u_int32_t _rtp_ts;
		/** Source stat.
	      * Packets sent.
		  */
		u_int32_t _psent;
		/** Source stat.
	      * Octect sent.
		  */
		u_int32_t _osent;
		/** Source stat.
		  * Round trip time to receiver.
		  */
		double _rtt;
		/** Sequence number state */
		uint16_t _sn_max;
		/** Sequence number state */
		uint16_t _sn_min;
	} _s;
	
	struct { 
		/** Congestion state */
		double _tx_rate;
		/** Congestion state */
		double _tx_ipd;
	} _c;

	/** Constructor.
	  */
	RTPSession(const TSpec& t);
	/** Utility function.
	  * Generate a report of all session sratistics.
	  * @param s pointer to output stream.
	  */
	void report(FILE* s);
	/** Utility function.
	  * Initialize the sequence number state machine.
	  * @param seq starting sequence number.
	  */
	void init_seq(u_int16_t seq);
	/** Utility function.
  	  * After a source is considered valid, the sequence number is considered
  	  * valid if it is no more than MAX_DROPOUT ahead of s->max_seq nor more
  	  * than MAX_MISORDER behind.  If the new sequence number is ahead of
  	  * max_seq modulo the RTP sequence number range (16 bits), but is
  	  * smaller than max_seq, it has wrapped around and the (shifted) count
  	  * of sequence number cycles is incremented.  A value of one is returned
  	  * to indicate a valid sequence number.
  	  *
  	  * Otherwise, the value zero is returned to indicate that the validation
  	  * failed, and the bad sequence number plus 1 is stored.  If the next
  	  * packet received carries the next higher sequence number, it is
  	  * considered the valid start of a new packet sequence presumably caused
  	  * by an extended dropout or a source restart.  Since multiple complete
  	  * sequence number cycles may have been missed, the packet loss
  	  * statistics are reset.
  	  *
  	  * Typical values for the parameters are shown, based on a maximum
  	  * misordering time of 2 seconds at 50 packets/second and a maximum
  	  * dropout of 1 minute.  The dropout parameter MAX_DROPOUT should be a
  	  * small fraction of the 16-bit sequence number space to give a
  	  * reasonable probability that new sequence numbers after a restart will
  	  * not fall in the acceptable range for sequence numbers from before the
  	  * restart.
	  * 
	  * @param seq sequence number to be used to update sequence state machine.
	  * @return 0: invalid sequence number, 1: valid sequence number.
  	  */
	int update_seq(u_int16_t seq);
	/** Utility functon.
	  * This will update the congestion control statistics based on
	  * the current receive/send statistics stored in this struct.
	  */
	void update_rate();
	/**  Debug function.
	  *  Prints an rr packet to debug stream.
	  */
	void print_rr(uint8_t* sb);
	/**  Debug function.
	  *  Prints an sr packet to debug stream.
	  */
	void print_sr(uint8_t* sb);
	/**  Debug function.
	  *  Prints a nack packet to debug stream.
	  */
	void print_nack(uint8_t* sb);
	/**  Debug function.
	  *  Prints a ack packet to debug stream.
	  */
	void print_ack(uint8_t* sb);
	/** Transport function.
	  * Builds a DATA packet header and stores it in the supplied SocketBuffer
	  * @param sb array to store data header in.
	  * @param pt Payload type.
	  * @param sn Sequence Number.
	  * @param ts Timestamp.
	  * @param ssrc Syncrhonization Source.
	  * @param sz Size of the header.
	  * @param padding indicates whether or not padding is required
	  * @return 0: on success, -1 on error.
	  */
	inline int build_data_hdr(uint8_t* sb, const uint32_t& pt, 
                            const uint16_t& sn, const uint32_t& ts,
                            const uint32_t& ssrc, const int& sz, 
							bool padding) {
    	if (sz<HDR_SIZE)
        	return(-1);
    	RTPHdr* h=(RTPHdr*)sb;
    	h->_v=0x2;
    	h->_pt=pt;
    	h->_sn=htons(sn);
    	h->_ts=htonl(ts);
    	h->_ssrc=htonl(ssrc);
    	h->_p=padding?1:0;
    	return(0);
	}
	/** Transport function.
	  * Builds a FIN packet header and stores it in the supplied SocketBuffer
	  * @param sb array to store data header in.
	  * @param ssrc Syncrhonization Source.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	inline int build_fin_hdr(uint8_t* sb, const uint32_t& ssrc, const int& sz) {
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
	/** Transport function.
	  * Builds an RTCP receiver report packet.
	  * @param sb array to store data header in.
	  * @param sz size of sb array.
	  * @param count count of receiver reports contained in this RR.
	  * @param ssrc synchronization source.
	  * @param fraction  fraction of packets lost since last SR received.
	  * @param lost total number of packets lost since last SR received.
	  * @param last_seq sequence number of last data packet received.
	  * @param jitter  running average jitter.
	  * @param lsr last sender report.
	  * @param dlsr delay since last sender report.
	  * @return pointer to one past end of buffer.
	  */
	inline uint8_t* build_rr(uint8_t* sb, const int& sz, const u_int8_t count, 
				  	 const uint32_t& ssrc, const u_int32_t& fraction, 
					 const u_int32_t& lost, const u_int32_t& last_seq, 
					 const u_int32_t& jitter, const u_int32_t& lsr, 
					 const u_int32_t& dlsr) {
    	RTCPPacket* h=(RTCPPacket*)sb;
    	h->h._common._v=0x2;
    	h->h._common._p=0;
    	h->h._common._pt=RTCP_RR;
    	h->h._common._length=htons(6);
    	h->r.rr._rr[0]._ssrc=htonl(ssrc);
    	h->r.rr._rr[0]._fraction=0xff&fraction;
    	h->r.rr._rr[0]._lost=htonl(0xffffff&lost)>>8;
    	h->r.rr._rr[0]._last_seq=htonl(last_seq);
    	h->r.rr._rr[0]._jitter=htonl(jitter);
    	h->r.rr._rr[0]._lsr=htonl(lsr);
    	h->r.rr._rr[0]._dlsr=htonl(dlsr);
    	sb+=28;
    	return(sb);
	}
	/** Transport function.
	  * Builds an RTCP sender report packet.
	  * @param sb array to store data header in.
	  * @param sz size of sb array.
	  * @param count count of receiver reports contained in this RR.
	  * @param ssrc synchronization source.
	  * @param ntp_sec NTP timestamp value.
	  * @param ntp_frac NTP timestamp value.
	  * @param rtp_ts RTP Timestamp.
	  * @param psent packets sent.
	  * @param osent octets sent.
	  * @return pointer to one past end of buffer.
	  */
	inline uint8_t* build_sr(uint8_t* sb, const int& sz, const u_int8_t count, 
				  	 const uint32_t& ssrc, const u_int32_t& ntp_sec, 
					 const u_int32_t& ntp_frac, const u_int32_t& rtp_ts, 
					 const u_int32_t& psent, const u_int32_t& osent) {
    	RTCPPacket* h=(RTCPPacket*)sb;
    	h->h._common._v=0x2;
    	h->h._common._p=0;
    	h->h._common._pt=RTCP_SR;
    	h->h._common._length=htons(7);
    	h->r.sr._ssrc=htonl(ssrc);
    	h->r.sr._ntp_sec=htonl(ntp_sec);
    	h->r.sr._ntp_frac=htonl(ntp_frac);
    	h->r.sr._rtp_ts=htonl(rtp_ts);
    	h->r.sr._psent=htonl(psent);
    	h->r.sr._osent=htonl(osent);
    	sb+=28;
    	return(sb);
	}

	/** Transport function.
	  * Builds an RTCP feedback packet.
	  * @param sb array to store data header in.
	  * @param sz size of sb array.
	  * @param sender_ssrc SSRC of packet sender.
	  * @param src_ssrc SSRC of media source.
	  * @param pid Packet ID. This identifies the packet that is being NACK-ed.
	  * @param Bit-mask of following lost packets. 
      * bitmask of following lost packets (BLP): 16 bits  
      * The BLP allows for reporting losses of any of the 16 RTP 
      * packets immediately following the RTP packet indicated by the 
      * PID.  The BLP's definition is identical to that given in [6].  
      * Denoting the BLP's least significant bit as bit 1, and its most 
      * significant bit as bit 16, then bit i of the bit mask is set to 
      * 1 if the receiver has not received RTP packet number (PID+i) 
      * (modulo 2^16) and indicates this packet is lost; bit i is set 
      * to 0 otherwise.  Note that the sender MUST NOT assume that a 
      * receiver has received a packet because its bit mask was set to 
      * 0.   For example, the least significant bit of the BLP would be 
      * set to 1 if the packet corresponding to the PID and the 
      * following packet have been lost.  However, the sender cannot 
      * infer that packets PID+2 through PID+16 have been received 
      * simply because bits 2 through 15 of the BLP are 0; all the 
      * sender knows is that the receiver has not reported them as lost 
      * at this time(from draft-ietf-avt-rtcp-feedback-08.txt).
      * @param blp_sz Number of blps.
	  */
	inline uint8_t* build_nack(uint8_t* sb, const int& sz,
             			const u_int32_t& sender_ssrc,
                        const u_int32_t& src_ssrc,
                        const u_int16_t* pid,
                        const u_int16_t* blp,
						int blp_sz) {
    	RTCPPacket* h=(RTCPPacket*)sb;
    	h->h._common._v=0x2;
    	h->h._common._p=0;
    	h->h._common._pt=RTCP_FB;
    	h->h._common._count=FB_NACK;
    	h->h._common._length=htons(3+blp_sz);
    	h->r._fb._sender_ssrc=htonl(sender_ssrc);
    	h->r._fb._src_ssrc=htonl(src_ssrc);
    	for (int i=0; i<blp_sz; ++i) {
        	h->r._fb._fci[i]._ack._pid=htons(pid[i]);
        	h->r._fb._fci[i]._ack._blp=htons(blp[i]);
    	}
    	sb+=(3+blp_sz)*4;
    	return(sb);
	}
	/** Transport function.
	  * Builds an RTCP feedback packet.
	  * @param sb array to store data header in.
	  * @param sz size of sb array.
	  * @param sender_ssrc SSRC of packet sender.
	  * @param src_ssrc SSRC of media source.
	  * @param pid Packet ID of packet being ACK-ed.
	  * @param r Range of ACKs. 
	  * @param blp  Bit mask of Lost Packets/# Packets
      * @param blp_sz Number of blps.
	  */
	uint8_t* build_ack(uint8_t* sb, const int& sz,
             		   const u_int32_t& sender_ssrc,
                       const u_int32_t& src_ssrc,
                       const u_int16_t* r,
                       const u_int16_t* pid,
                       const u_int16_t* blp,
					int blp_sz) {
    	RTCPPacket* h=(RTCPPacket*)sb;
    	h->h._common._v=0x2;
    	h->h._common._p=0;
    	h->h._common._pt=RTCP_FB;
    	h->h._common._count=FB_ACK;
    	h->h._common._length=htons(3+blp_sz);
    	h->r._fb._sender_ssrc=htonl(sender_ssrc);
    	h->r._fb._src_ssrc=htonl(src_ssrc);
    	for (int i=0; i<blp_sz; ++i) {
        	h->r._fb._fci[i]._ack._r=htons(r[i]);
        	h->r._fb._fci[i]._ack._pid=htons(pid[i]);
        	h->r._fb._fci[i]._ack._blp=htons(blp[i]);
    	}
    	sb+=(3+blp_sz)*4;
    	return(sb);
	}
	/** Transport function.
	  * Builds an RTCP BYE packet.
	  * @param sb array to store data header in.
	  * @param sz size of sb array.
	  * @param src_count number of sources included in this packet.
	  * @param ssrcs array of source ssrc's.
	  */
	inline uint8_t* build_bye(uint8_t* sb, const int& sz,
                       const u_int8_t src_count,
                       const u_int32_t* ssrcs) {
	    RTCPPacket* h=(RTCPPacket*)sb;
    	h->h._common._v=0x2;
    	h->h._common._p=0;
    	h->h._common._pt=RTCP_BYE;
    	h->h._common._count=src_count;
    	h->h._common._length=htons(src_count);
    	for (int i=0; i<src_count; ++i) {
        	h->r.bye._src[i]=htonl(ssrcs[i]);
    	}
    	sb+=(src_count+1)*4;
    	return(sb);
	}
};
#endif

