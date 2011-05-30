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

#ifndef RTP_H
#define RTP_H

/** RTP module.
  */

/** RTCP Constant */
const int RTCP_TIMEOUT=1;
/** RTP Constant */
const int HDR_SIZE=12;
/** RTCP Constant */
const int RR_SIZE=28;

/** RTP Constant */
const int RTP_VERSION=12;

/** RTP Constant */
const int RTP_SEQ_MOD=(1<<16);
/** RTP Constant */
const int RTP_MAX_SDES=255;      /* maximum text length for SDES */

/** RTCP constants.  */
const int MAX_DROPOUT=3000;
/** RTCP constants.  */
const int MAX_MISORDER=100;
/** RTCP constants.  */
const int MIN_SEQUENTIAL=2;

/** RTP Constant
  * Packet types.
  */
enum RTCPType{
    RTCP_SR   = 200,
    RTCP_RR   = 201,
    RTCP_SDES = 202,
    RTCP_BYE  = 203,
    RTCP_APP  = 204,
	RTCP_FB = 205,  /* RTP Feedback. Denotes Transport layer feedback message. */
	RTCP_PSFB = 206 /* RTP Feedback. Denotes Payload-specific feedback message.*/
};  

/** RTCP Constant
  * Definition of RTCP SDES types.
  */
enum RTCPSdesType {
    RTCP_SDES_END   = 0,
    RTCP_SDES_CNAME = 1,
    RTCP_SDES_NAME  = 2,
    RTCP_SDES_EMAIL = 3,
    RTCP_SDES_PHONE = 4,
    RTCP_SDES_LOC   = 5,
    RTCP_SDES_TOOL  = 6,
    RTCP_SDES_NOTE  = 7,
    RTCP_SDES_PRIV  = 8
};


/** RTCP Constant.
  * Definition for NACK constant in FMT field of FB message.
  */

const int FB_NACK=1;
/** RTCP Constant.
  * Definition for ACK constant in FMT field of FB message.
  */
const int FB_ACK=2;


/** RTP Constant
  * Definition of Congestion Control Types
  */
typedef enum { RCC_OPENLOOP, RCC_TFRC, RCC_DLRC } RTPCongestionControlType;
   
/** Traffic Specification.
  * This defines the trasffic specification for an RTP session.
  */ 
struct TSpec {
    u_int32_t _peak_rate;
    u_int32_t _min_rate;
	u_int32_t _mtu;
	RTPCongestionControlType _cc;
};
    

#ifdef LINUX

/** Definition of packet types */
typedef enum { 
	PT_DATA=0x1, 
	PT_ACK=0x2, 
	PT_FIN=0x3 
} RTPPacketType;

/** RTP Header.
  * @doc This class provides the format for the RTP header.
  */
// RTP data packet header format.
//    0                   1                   2                   3
//  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |V=2|P|X|  CC   |M|     PT      |       sequence number         |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |                           timestamp                           |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |           synchronization source (SSRC) identifier            |
// +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
// |            contributing source (CSRC) identifiers             |
// |                             ....                              |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
struct RTPHdr {
        u_int8_t _cc:4;        	// CSRC count 
        u_int8_t _x:1;         	// header extension flag 
        u_int8_t _p:1;         	// padding flag 
        u_int8_t _v:2;   		// protocol version 
        u_int8_t _pt:7;        	// payload type
        u_int8_t _m:1;         	// marker bit
        u_int16_t _sn;      		// sequence number
        u_int32_t _ts;           // timestamp
        u_int32_t _ssrc;         // synchronization source
        u_int32_t _csrc;         // optional CSRC list
};      

/** RTP Packet.
  * @doc Provides general format for RTP data packet.
  */
struct RTPPacket {
	struct RTPHdr* _h;
	u_int8_t* _data;
};

// Output from TCPdump 
// 21:40:39.177421 voyager.32813 > argus.egaedata: udp 1356 (DF) 
// (ttl 64, id 41284, len 1384)
// 4500 0568 a144 4000 4011 0d61 c0a8 02c7
// c0a8 02c8 802d bf68 0554 e4fa 8001 2b78
//								 ^  ^  ^
//					v/p/x/cc   --/  |  |
//					m/pt     -------/  |
//					sn	     ----------/
// 0000 2b78 0000 0003 4c2e 1000 1c00 0000
// --------- --------- -------------------
//     ^         ^              ^
//     |         |              |
// ts--/         |              |
// ssrc ---------/              |
// data ------------------------/

/** RTCP Packet.
  * RTCP common header word
 */
struct RTCPCommon {
    u_int8_t _count:5;     /* varies by packet type */
    u_int8_t _p:1;         /* padding flag */
    u_int8_t _v:2;   /* protocol version */
    u_int8_t _pt:8;        /* RTCP packet type */
    u_int16_t _length;           /* pkt len in words, w/o this word */
};  

/** RTCP Packet.
  * Reception report block
  */
struct RTCPRr {
    u_int32_t _ssrc;             /* data source being reported */
    u_int32_t _lost:24;              /* cumul. no. pkts lost (signed!) */
    u_int32_t _fraction:8;  /* fraction lost since last SR/RR */
    u_int32_t _last_seq;         /* extended last seq. no. received */
    u_int32_t _jitter;           /* interarrival jitter */
    u_int32_t _lsr;              /* last SR packet from this source */
    u_int32_t _dlsr;             /* delay since last SR packet */
};

/** RTCP Packet.
  * SDES item
  */
struct RTCPSdesItem {
    u_int8_t _type;              /* type of item (rtcp_sdes_type_t) */
    u_int8_t _length;            /* length of item (in octets) */
    char data[1];             /* text, not null-terminated */
};


/** RTCP Packet.
 * One RTCP packet
 */
struct RTCPPacket {
	union {
    	RTCPCommon _common;     	/* common header */
	} h;
    union {
        /** Sender Report (SR) */
        struct {
            u_int32_t _ssrc;     /* sender generating this report */
            u_int32_t _ntp_sec;  /* NTP timestamp */
            u_int32_t _ntp_frac;
            u_int32_t _rtp_ts;   /* RTP timestamp */
            u_int32_t _psent;    /* packets sent */
            u_int32_t _osent;    /* octets sent */
            RTCPRr _rr[1];  /* variable-length list */
        } sr;
        /** Reception Report (RR) */
        struct {
            u_int32_t _ssrc;     /* receiver generating this report */
            RTCPRr _rr[1];  /* variable-length list */
        } rr;
        /** Source Description (SDES) */
        struct {
            u_int32_t _src;      /* first SSRC/CSRC */
            RTCPSdesItem _item[1]; /* list of SDES items */
        } sdes;
        /** BYE */
        struct {
            u_int32_t _src[1];   /* list of sources */
            /* can't express trailing text for reason */
        } bye;
		/** FB Message */
		struct {
			u_int32_t _sender_ssrc;
			u_int32_t _src_ssrc;
			union {
				struct {
					u_int16_t _pid;	/* Packet ID */
					u_int16_t _blp; /* Bitmask of following Lost Packets */
				} _nack;
				struct {
					u_int16_t _pid;	/* Packet ID */
					u_int16_t _blp:15;	/* Bitmask of Lost Packets/#packets */
					u_int16_t _r:1;
				} _ack;
			} _fci[1];
		} _fb;
    } r;
};



#else
/* BIGENDIAN Definitions */
#endif

#endif // RTP_H


