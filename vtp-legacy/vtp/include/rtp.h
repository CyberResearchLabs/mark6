/*
 * Copyright (c) 2003 MIT, Haystack Observatory
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction,including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 */

/*
 * $Source: /usr/local/cvsroot/mit/vtp/include/rtp.h,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/18 01:48:45 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: rtp.h,v $
 * Revision 1.2  2003/09/18 01:48:45  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:46  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.11  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.10  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.9  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.8  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.7  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.6  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.5  2003/09/04 12:39:40  davidlapsley
 * Daily release.
 *
 * Revision 1.4  2003/09/03 19:57:23  davidlapsley
 * Added new rtcp module that implements the RTCP session functionality.
 * All RTCP state is stored in this module and state management functions
 * are also implemented in this module.
 *
 * RTP module is now mainly for defining RTP/RTCP data types (e.g. packet
 * formats etc.). Most functinality has moved into rtcp module.
 *
 * Currently code compiles but does not link. Need to implement module (interfaces
 * have been done).
 *
 * Revision 1.3  2003/09/03 18:05:39  davidlapsley
 * Added and tested schedule functionality.
 *
 * Revision 1.2  2003/09/03 12:54:02  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <sys/types.h>
#include <glib.h>
#include <stdio.h>

#ifndef _RTP_H_
#define _RTP_H_

/*
 * The type definitions below are valid for 32-bit architectures and
 * may have to be adjusted for 16- or 64-bit architectures.
 */
#if __ORIG__
typedef unsigned char  u_int8_t;
typedef unsigned short u_int16_t;
typedef unsigned int   u_int32_t;
typedef          short int16;
#endif

/*
 * Current protocol version.
 */
#define RTP_VERSION    2

#define RTP_SEQ_MOD (1<<16)
#define RTP_MAX_SDES 255      /* maximum text length for SDES */

/**
* RTCP sequence constants.
 */
#define MAX_DROPOUT 	3000
#define MAX_MISORDER 	100
#define MIN_SEQUENTIAL 	2


typedef enum {
    RTCP_SR   = 200,
    RTCP_RR   = 201,
    RTCP_SDES = 202,
    RTCP_BYE  = 203,
    RTCP_APP  = 204
} rtcp_type_t;

typedef enum {
    RTCP_SDES_END   = 0,
    RTCP_SDES_CNAME = 1,
    RTCP_SDES_NAME  = 2,
    RTCP_SDES_EMAIL = 3,
    RTCP_SDES_PHONE = 4,
    RTCP_SDES_LOC   = 5,
    RTCP_SDES_TOOL  = 6,
    RTCP_SDES_NOTE  = 7,
    RTCP_SDES_PRIV  = 8
} rtcp_sdes_type_t;

/*
 * RTP data header
 */
typedef struct {
    unsigned int version:2;   /* protocol version */
    unsigned int p:1;         /* padding flag */
    unsigned int x:1;         /* header extension flag */
    unsigned int cc:4;        /* CSRC count */
    unsigned int m:1;         /* marker bit */
    unsigned int pt:7;        /* payload type */
    unsigned int seq:16;      /* sequence number */
    u_int32_t ts;               /* timestamp */
    u_int32_t ssrc;             /* synchronization source */
    u_int32_t csrc[1];          /* optional CSRC list */
} rtp_hdr_t;

/*
 * RTCP common header word
 */
typedef struct {
    unsigned int version:2;   /* protocol version */
    unsigned int p:1;         /* padding flag */
    unsigned int count:5;     /* varies by packet type */
    unsigned int pt:8;        /* RTCP packet type */
    u_int16_t length;           /* pkt len in words, w/o this word */
} rtcp_common_t;

/*
 * Big-endian mask for version, padding bit and packet type pair
 */
/*
#define RTCP_VALID_MASK (0xc000 | 0x2000 | 0xfe)
#define RTCP_VALID_VALUE ((RTP_VERSION << 14) | RTCP_SR)
*/
#define RTCP_VALID_MASK		(0xc0000000 | 0x0f000000)
#define RTCP_VALID_VALUE 	(0x80000000)
#define RTCP_VALID_SR		(0x80000000 | (RTCP_SR<<16))
#define RTCP_VALID_RR		(0x80000000 | (RTCP_RR<<16))
#define RTCP_VALID_BYE		(0x80000000 | (RTCP_BYE<<16))

/*
 * Reception report block
 */
typedef struct {
    u_int32_t ssrc;             /* data source being reported */
    unsigned int fraction:8;  /* fraction lost since last SR/RR */
    int lost:24;              /* cumul. no. pkts lost (signed!) */
    u_int32_t last_seq;         /* extended last seq. no. received */
    u_int32_t jitter;           /* interarrival jitter */
    u_int32_t lsr;              /* last SR packet from this source */
    u_int32_t dlsr;             /* delay since last SR packet */
} rtcp_rr_t;

/*
 * SDES item
 */
typedef struct {
    u_int8_t type;              /* type of item (rtcp_sdes_type_t) */
    u_int8_t length;            /* length of item (in octets) */
    char data[1];             /* text, not null-terminated */
} rtcp_sdes_item_t;

/*
 * One RTCP packet
 */
typedef struct {
    rtcp_common_t common;     /* common header */
    union {
        /* sender report (SR) */
        struct {
            u_int32_t ssrc;     /* sender generating this report */
            u_int32_t ntp_sec;  /* NTP timestamp */
            u_int32_t ntp_frac;
            u_int32_t rtp_ts;   /* RTP timestamp */
            u_int32_t psent;    /* packets sent */
            u_int32_t osent;    /* octets sent */
            rtcp_rr_t rr[1];  /* variable-length list */
        } sr;

        /* reception report (RR) */
        struct {
            u_int32_t ssrc;     /* receiver generating this report */
            rtcp_rr_t rr[1];  /* variable-length list */
        } rr;

        /* source description (SDES) */
        struct rtcp_sdes {
            u_int32_t src;      /* first SSRC/CSRC */
            rtcp_sdes_item_t item[1]; /* list of SDES items */
        } sdes;

        /* BYE */
        struct {
            u_int32_t src[1];   /* list of sources */
            /* can't express trailing text for reason */
        } bye;
    } r;
} rtcp_t;

typedef struct {
    u_int8_t* buf;
    u_int32_t length;
} rtcp_packet_t;

typedef struct rtcp_sdes rtcp_sdes_t;

/*
 * Per-source state information
 */
typedef struct {
    u_int16_t max_seq;        /* highest seq. number seen */
    u_int32_t cycles;         /* shifted count of seq. number cycles */
    u_int32_t base_seq;       /* base seq number */
    u_int32_t bad_seq;        /* last 'bad' seq number + 1 */
    u_int32_t probation;      /* sequ. packets till source is valid */
    u_int32_t received;       /* packets received */
    u_int32_t expected_prior; /* packet expected at last interval */
    u_int32_t received_prior; /* packet received at last interval */
    u_int32_t transit;        /* relative trans time for prev pkt */
    u_int32_t jitter;         /* estimated jitter */
    /* ... */
} source;

/* Scheduling types */
typedef enum {
    EVENT_BYE,
    EVENT_REPORT
} rtcp_event_type_t;

typedef struct {
    rtcp_event_type_t etype;
    u_int32_t sent_packet_size;
} rtcp_event_t;

typedef enum {
    PACKET_RTCP_REPORT,
    PACKET_RTP,
    PACKET_BYE,
    PACKET_INVALID
} rtcp_packet_type_t;


#endif