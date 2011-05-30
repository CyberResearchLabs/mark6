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
 * $Source: /usr/local/cvsroot/mit/vtp/src/rtp.c,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/18 01:48:47 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: rtp.c,v $
 * Revision 1.2  2003/09/18 01:48:47  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:49  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.9  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.8  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.7  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.6  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.5  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.4  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.3  2003/09/03 19:57:23  davidlapsley
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
 * Revision 1.2  2003/09/03 12:54:02  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include "rtp.h"


#if __ORIG__
/* For random number generation */
#include <sys/types.h>   /* u_long */
#include <sys/time.h>    /* gettimeofday() */
#include <unistd.h>      /* get..() */
#include <stdio.h>       /* printf() */
#include <time.h>        /* clock() */
#include <sys/utsname.h> /* uname() */
/* #include "global.h"      /* from RFC 1321 */
#include <openssl/md5.h> /* from RFC 1321 */

#define MD_CTX MD5_CTX
#define MDInit MD5Init
#define MDUpdate MD5Update
#define MDFinal MD5Final

/*
 The routine update_seq shown below ensures that a source is declared
 valid only after MIN_SEQUENTIAL packets have been received in
 sequence.  It also validates the sequence number seq of a newly
 received packet and updates the sequence state for the packet's
 source in the structure to which s points.

 When a new source is heard for the first time, that is, its SSRC
 identifier is not in the table (see Section 8.2), and the per-source
 state is allocated for it, s->probation is set to the number of
 sequential packets required before declaring a source valid
 (parameter MIN_SEQUENTIAL) and other variables are initialized:

 init_seq(s, seq);
 s->max_seq = seq - 1;
 s->probation = MIN_SEQUENTIAL;

 A non-zero s->probation marks the source as not yet valid so the
 state may be discarded after a short timeout rather than a long one,
 as discussed in Section 6.2.1.

 After a source is considered valid, the sequence number is considered
 valid if it is no more than MAX_DROPOUT ahead of s->max_seq nor more
 than MAX_MISORDER behind.  If the new sequence number is ahead of
 max_seq modulo the RTP sequence number range (16 bits), but is
 smaller than max_seq, it has wrapped around and the (shifted) count
 of sequence number cycles is incremented.  A value of one is returned
 to indicate a valid sequence number.

 Otherwise, the value zero is returned to indicate that the validation
 failed, and the bad sequence number plus 1 is stored.  If the next
 packet received carries the next higher sequence number, it is
 considered the valid start of a new packet sequence presumably caused
 by an extended dropout or a source restart.  Since multiple complete
 sequence number cycles may have been missed, the packet loss
 statistics are reset.

 Typical values for the parameters are shown, based on a maximum
 misordering time of 2 seconds at 50 packets/second and a maximum
 dropout of 1 minute.  The dropout parameter MAX_DROPOUT should be a
 small fraction of the 16-bit sequence number space to give a
 reasonable probability that new sequence numbers after a restart will
 not fall in the acceptable range for sequence numbers from before the
 restart.
 */

void init_seq(source *s, u_int16_t seq)
{
    s->base_seq = seq;
    s->max_seq = seq;
    s->bad_seq = RTP_SEQ_MOD + 1;   /* so seq == bad_seq is false */
    s->cycles = 0;
    s->received = 0;
    s->received_prior = 0;
    s->expected_prior = 0;
    /* other initialization */
}

int update_seq(source *s, u_int16_t seq)
{
    u_int16_t udelta = seq - s->max_seq;
    const int MAX_DROPOUT = 3000;
    const int MAX_MISORDER = 100;
    const int MIN_SEQUENTIAL = 2;

    /*
     * Source is not valid until MIN_SEQUENTIAL packets with
     * sequential sequence numbers have been received.
     */
    if (s->probation) {
        /* packet is in sequence */
        if (seq == s->max_seq + 1) {
            s->probation--;
            s->max_seq = seq;
            if (s->probation == 0) {
                init_seq(s, seq);
                s->received++;
                return 1;
            }
        } else {
            s->probation = MIN_SEQUENTIAL - 1;
            s->max_seq = seq;
        }
        return 0;
    } else if (udelta < MAX_DROPOUT) {
        /* in order, with permissible gap */
        if (seq < s->max_seq) {
            /*
             * Sequence number wrapped - count another 64K cycle.
             */
            s->cycles += RTP_SEQ_MOD;
        }
        s->max_seq = seq;
    } else if (udelta <= RTP_SEQ_MOD - MAX_MISORDER) {
        /* the sequence number made a very large jump */
        if (seq == s->bad_seq) {
            /*
             * Two sequential packets -- assume that the other side
             * restarted without telling us so just re-sync
             * (i.e., pretend this was the first packet).
             */
            init_seq(s, seq);
        }
        else {
            s->bad_seq = (seq + 1) & (RTP_SEQ_MOD-1);
            return 0;
        }
    } else {
        /* duplicate or reordered packet */
    }
    s->received++;
    return 1;
}



/*
 A.4 Generating RTCP SDES Packets

 This function builds one SDES chunk into buffer b composed of argc
 items supplied in arrays type, value and length.  It returns a
 pointer to the next available location within b.
 */
char *rtp_write_sdes(char *b, u_int32_t src, int argc,
                     rtcp_sdes_type_t type[], char *value[],
                     int length[])
{
    rtcp_sdes_t *s = (rtcp_sdes_t *)b;
    rtcp_sdes_item_t *rsp;
    int i;
    int len;
    int pad;

    /* SSRC header */
    s->src = src;
    rsp = &s->item[0];

    /* SDES items */
    for (i = 0; i < argc; i++) {
        rsp->type = type[i];
        len = length[i];
        if (len > RTP_MAX_SDES) {
            /* invalid length, may want to take other action */
            len = RTP_MAX_SDES;
        }
        rsp->length = len;
        memcpy(rsp->data, value[i], len);
        rsp = (rtcp_sdes_item_t *)&rsp->data[len];
    }

    /* terminate with end marker and pad to next 4-octet boundary */
    len = ((char *) rsp) - b;
    pad = 4 - (len & 0x3);
    b = (char *) rsp;
    while (pad--) *b++ = RTCP_SDES_END;

    return b;
}

/* A.5 Parsing RTCP SDES Packets

This function parses an SDES packet, calling functions find_member()
to find a pointer to the information for a session member given the
SSRC identifier and member_sdes() to store the new SDES information
for that member.  This function expects a pointer to the header of
the RTCP packet.
*/
void rtp_read_sdes(rtcp_t *r)
{
    int count = r->common.count;
    rtcp_sdes_t *sd = &r->r.sdes;
    rtcp_sdes_item_t *rsp, *rspn;
    rtcp_sdes_item_t *end = (rtcp_sdes_item_t *)
        ((u_int32_t *)r + r->common.length + 1);
    source *s;

    while (--count >= 0) {
        rsp = &sd->item[0];
        if (rsp >= end) break;
        s = find_member(sd->src);

        for (; rsp->type; rsp = rspn ) {
            rspn = (rtcp_sdes_item_t *)((char*)rsp+rsp->length+2);
            if (rspn >= end) {
                rsp = rspn;
                break;
            }
            member_sdes(s, rsp->type, rsp->data, rsp->length);
        }
        sd = (rtcp_sdes_t *)
            ((u_int32_t *)sd + (((char *)rsp - (char *)sd) >> 2)+1);
    }
    if (count >= 0) {
        /* invalid packet format */
    }
}

/*
 A.7 Computing the RTCP Transmission Interval

 The following functions implement the RTCP transmission and reception
 rules described in Section 6.2.  These rules are coded in several
 functions:

 o  rtcp_interval() computes the deterministic calculated interval,
 measured in seconds.  The parameters are defined in Section 6.3.
 o  OnExpire() is called when the RTCP transmission timer expires.
 o  OnReceive() is called whenever an RTCP packet is received.

 Both OnExpire() and OnReceive() have event e as an argument.  This is
 the next scheduled event for that participant, either an RTCP report
 or a BYE packet.  It is assumed that the following functions are
 available:

 o  Schedule(time t, event e) schedules an event e to occur at time t.
 When time t arrives, the function OnExpire is called with e as an
 argument.
 o  Reschedule(time t, event e) reschedules a previously scheduled
 event e for time t.
 o  SendRTCPReport(event e) sends an RTCP report.
 o  SendBYEPacket(event e) sends a BYE packet.
 o  TypeOfEvent(event e) returns EVENT_BYE if the event being
 processed is for a BYE packet to be sent, else it returns
 EVENT_REPORT.
 o  PacketType(p) returns PACKET_RTCP_REPORT if packet p is an RTCP
 report (not BYE), PACKET_BYE if its a BYE RTCP packet, and
 PACKET_RTP if its a regular RTP data packet.
 o  ReceivedPacketSize() and SentPacketSize() return the size of the
 referenced packet in octets.
 o  NewMember(p) returns a 1 if the participant who sent packet p is
 not currently in the member list, 0 otherwise.  Note this function
 is not sufficient for a complete implementation because each CSRC
 identifier in an RTP packet and each SSRC in a BYE packet should
 be processed.
 o  NewSender(p) returns a 1 if the participant who sent packet p is
 not currently in the sender sublist of the member list, 0
 otherwise.
 o  AddMember() and RemoveMember() to add and remove participants from
 the member list.
 o  AddSender() and RemoveSender() to add and remove participants from
 the sender sublist of the member list.

 These functions would have to be extended for an implementation that
 allows the RTCP bandwidth fractions for senders and non-senders to be
 specified as explicit parameters rather than fixed values of 25% and
 75%.  The extended implementation of rtcp_interval() would need to
 avoid division by zero if one of the parameters was zero.
 */
  

#if __ORIG__

        /* Checkign RTCP packet validity */

        u_int32_t len;        /* length of compound RTCP packet in words */
        rtcp_t *r;          /* RTCP header */
        rtcp_t *end;        /* end of compound RTCP packet */

        if ((*(u_int16_t *)r & RTCP_VALID_MASK) != RTCP_VALID_VALUE) {
            /* something wrong with packet format */
        }
        end = (rtcp_t *)((u_int32_t *)r + len);

        do r = (rtcp_t *)((u_int32_t *)r + r->common.length + 1);
        while (r < end && r->common.version == 2);
        if (r != end) {
            /* something wrong with packet format */
        }



        A.8 Estimating the Interarrival Jitter

        The code fragments below implement the algorithm given in Section
        6.4.1 for calculating an estimate of the statistical variance of the
        RTP data interarrival time to be inserted in the interarrival jitter
        field of reception reports.  The inputs are r->ts, the timestamp from
        the incoming packet, and arrival, the current time in the same units.
        Here s points to state for the source; s->transit holds the relative
        transit time for the previous packet, and s->jitter holds the
        estimated jitter.  The jitter field of the reception report is
        measured in timestamp units and expressed as an unsigned integer, but
        the jitter estimate is kept in a floating point.  As each data packet
        arrives, the jitter estimate is updated:

        int transit = arrival - r->ts;
        int d = transit - s->transit;
        s->transit = transit;
        if (d < 0) d = -d;
        s->jitter += (1./16.) * ((double)d - s->jitter);

        When a reception report block (to which rr points) is generated for
        this member, the current jitter estimate is returned:

        rr->jitter = (u_int32_t) s->jitter;

        Alternatively, the jitter estimate can be kept as an integer, but
        scaled to reduce round-off error.  The calculation is the same except
        for the last line:

        s->jitter += d - ((s->jitter + 8) >> 4);

        In this case, the estimate is sampled for the reception report as:

        rr->jitter = s->jitter >> 4;

#endif

#endif
        