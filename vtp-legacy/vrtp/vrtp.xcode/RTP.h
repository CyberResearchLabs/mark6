/*
 *  RTP.h
 *  vrtp
 *
 *  Created by David Lapsley on Wed Feb 25 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#include <sys/types.h>
#include <MyException.h>

#ifndef _RTP_H_
#define _RTP_H_


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


enum RTCPType{
    RTCP_SR   = 200,
    RTCP_RR   = 201,
    RTCP_SDES = 202,
    RTCP_BYE  = 203,
    RTCP_APP  = 204
};

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

// RTPPacket //
class RTPException {
private:
    int _n;
    string _message;
public:
        RTPException(int n) { 
            _message="RTPException: ";
            _n=n; 
        }
    RTPException(int n, string s) {
        _n=n;
        _message="RTPException: " + s;
    }
    ~RTPException() { }
    const char* what() {
        const char* s=(char*)_message.data();
        return (s);
    }
};

class RTPPacket {
private:
    struct _hdr {
        unsigned int version:2;   /* protocol version */
        unsigned int p:1;         /* padding flag */
        unsigned int x:1;         /* header extension flag */
        unsigned int cc:4;        /* CSRC count */
        unsigned int m:1;         /* marker bit */
        unsigned int pt:7;        /* payload type */
        unsigned int seq:16;      /* sequence number */
        u_int32_t ts;               /* timestamp */
        u_int32_t ssrc;             /* synchronization source */
        u_int32_t csrc;          /* optional CSRC list */        
    };
    _hdr* _hdrp;
    u_int8_t* _buf;
    u_int32_t _buf_size;
public:
        RTPPacket(unsigned int buf_size);
        RTPPacket(unsigned int buf_size, 
                  unsigned int v,
                  unsigned int p,
                  unsigned int x,
                  unsigned int cc,
                  unsigned int m,
                  unsigned int pt,
                  unsigned int seq,
                  unsigned int ts,
                  unsigned int ssrc,
                  u_int32_t* csrc,
                  unsigned char data[],
                  u_int32_t data_size);
        ~RTPPacket();
        u_int8_t* get_buf();
        u_int32_t get_version();
        u_int32_t get_p();
        u_int32_t get_cc();
        u_int32_t get_m();
        u_int32_t get_pt();
        u_int32_t get_seq();
        u_int32_t get_ts();
        u_int32_t get_ssrc();
        u_int32_t* get_csrc();
        u_int8_t* get_data();
        void dump(ostream& out);
};

/*
 * RTCP common header word
 */
struct RTCPCommon {
    unsigned int version:2;   /* protocol version */
    unsigned int p:1;         /* padding flag */
    unsigned int count:5;     /* varies by packet type */
    unsigned int pt:8;        /* RTCP packet type */
    u_int16_t length;           /* pkt len in words, w/o this word */
};

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
struct RTCPRr {
    u_int32_t ssrc;             /* data source being reported */
    unsigned int fraction:8;  /* fraction lost since last SR/RR */
    int lost:24;              /* cumul. no. pkts lost (signed!) */
    u_int32_t last_seq;         /* extended last seq. no. received */
    u_int32_t jitter;           /* interarrival jitter */
    u_int32_t lsr;              /* last SR packet from this source */
    u_int32_t dlsr;             /* delay since last SR packet */
};

/*
 * SDES item
 */
struct RTCPSdesItem {
    u_int8_t type;              /* type of item (rtcp_sdes_type_t) */
    u_int8_t length;            /* length of item (in octets) */
    char data[1];             /* text, not null-terminated */
};

/*
 * One RTCP packet
 */
struct RTCP {
    struct RTCPCommon common;     /* common header */
    union r {
        /* sender report (SR) */
        struct sr {
            u_int32_t ssrc;     /* sender generating this report */
            u_int32_t ntp_sec;  /* NTP timestamp */
            u_int32_t ntp_frac;
            u_int32_t rtp_ts;   /* RTP timestamp */
            u_int32_t psent;    /* packets sent */
            u_int32_t osent;    /* octets sent */
            RTCPRr rr[1];  /* variable-length list */
        };
        
        /* reception report (RR) */
        struct rr {
            u_int32_t ssrc;     /* receiver generating this report */
            RTCPRr rr[1];  /* variable-length list */
        };
        
        /* source description (SDES) */
        struct RTCPSdes {
            u_int32_t src;      /* first SSRC/CSRC */
            RTCPSdesItem item[1]; /* list of SDES items */
        };
        
        /* BYE */
        struct BYE {
            u_int32_t src[1];   /* list of sources */
            /* can't express trailing text for reason */
        };
    };
};


/*
 * Per-source state information
 */
struct Source {
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
};

/* Scheduling types */
enum RTCPEventType {
    EVENT_BYE,
    EVENT_REPORT
};

struct RTCPEvent {
    RTCPEventType etype;
    u_int32_t sent_packet_size;
};

enum RTCPPacketType {
    PACKET_RTCP_REPORT,
    PACKET_RTP,
    PACKET_BYE,
    PACKET_INVALID
};

#endif