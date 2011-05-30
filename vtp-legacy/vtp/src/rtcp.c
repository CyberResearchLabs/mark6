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
 * all copies or subst
 antial portions of the Software.
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
 * $Source: /usr/local/cvsroot/mit/vtp/src/rtcp.c,v $
 * $Revision: 1.3 $
 * $Date: 2003/09/18 01:48:47 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: rtcp.c,v $
 * Revision 1.3  2003/09/18 01:48:47  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:49  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.7  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.6  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.5  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.4  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
                                                     * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.3  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.2  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.1  2003/09/03 19:57:23  davidlapsley
 * Added new rtcp module that implements the RTCP session functionality.
 * All RTCP state is stored in this module and state management functions
 * are also implemented in this module.
 *
 * RTP module is now mainly for defining RTP/RTCP data types (e.g. packet
 * formats etc.). Most functinality has moved into rtcp module.
 *
 * Currently code compiles but does not link. Need to implement module
 * (interfaces have been done).
 *
 *
 *
 *
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <mysock.h>
#include <syslog.h>
#include <stdarg.h>

#include <random.h>

#include "rtcp.h"

/* Session initiation/termination */
/**
*	Initializes a new rtcp_session_t object.
 *	@param s A pointer to a rtcp_session_t object (already created).
 *	@param rtcp_bw Target RTCP bandwidth (Bps).
 *	@param avg_rtcp_size Average RTCP packet size (B).
 *	@param ssrc 32 bit identifier used to identify this host when it is
 *			transmitting data.
 *	@param sampling_frequency Source sampling frequency in KSamples/s.
 *	@param samples_per_packet Total number of samples per packet.
 *	@param bits_per_sample Number of bits per sample.
 *	@param sz_ctrl_rcv_buf Size of control receive buffer.
 *	@param sz_ctrl_snd_buf Size of control send buffer.
 *	@return	1: if successful, 0: if not successful.
 */
int rtcp_init(rtcp_session_t* s,
              int ctrl_sock,
              int data_sock,
              double rtcp_bw,
              double avg_rtcp_size,
              u_int32_t ssrc,
              u_int32_t sampling_frequency,
              u_int32_t samples_per_packet,
              u_int32_t bits_per_sample,
              u_int32_t sz_ctrl_rcv_buf,
              u_int32_t sz_ctrl_snd_buf)
{
    assert(s!=NULL);

    s->ctrl_sock=ctrl_sock;
    s->data_sock=data_sock;
    s->members=1;
    s->senders=0;
    s->pmembers=1;
    s->rtcp_bw=rtcp_bw;
    s->we_sent=0;
    s->initial=1;
    assert(memset(&(s->curr_event), '\0', sizeof(s->curr_event)));
    s->tc=0;
    s->tp=0;
    s->p=NULL;
    assert(gettimeofday(&s->t0, NULL)==0);
    s->rtp_ts0=random32(0);
    s->ntp_sec0=s->t0.tv_sec;
    s->ntp_frac0=(s->t0.tv_usec)/1e6*4294967296;
    s->sampling_frequency=sampling_frequency;
    s->samples_per_packet=samples_per_packet;
    s->bits_per_sample=bits_per_sample;
    s->psent=0;
    s->osent=0;
    s->avg_rtcp_size=avg_rtcp_size;
    s->tn = rtcp_interval(s,
                          s->members,
                          s->senders,
                          s->rtcp_bw,
                          s->we_sent,
                          s->avg_rtcp_size,
                          s->initial);

    s->evt_sched=malloc(sizeof(schedule_t));
    if (schedule_init(s->evt_sched)==0)
        return(0);

    /* Schedule first event */
    rtcp_event_t e;
    e.etype=EVENT_REPORT;
    rtcp_schedule(s, s->tn, e);

    /* Allocate space for control buffers */
    assert(s->ctrl_rcv_buf=malloc(sz_ctrl_rcv_buf*sizeof(u_int8_t)));
    assert(s->ctrl_snd_buf=malloc(sz_ctrl_snd_buf*sizeof(u_int8_t)));

    /* Add self to list of members */
    s->ssrc=ssrc;
    gpointer ssrcp=malloc(sizeof(u_int32_t));
    memcpy(ssrcp, &ssrc, sizeof(ssrc));
    member_t* me=NULL;
    me=malloc(sizeof(member_t));
    assert(me);
    return(1);
}

/**
 *  Destroy the rtcp_session_t and deallocate memory associated with its
 *	data structures.
 *	@param	s A pointer to the rtcp_session_t to be destroyed.
 *	@return 1: on success.
 */
int rtcp_destroy(rtcp_session_t* s)
{
    assert(s);
    assert(s->evt_sched);
    assert(s->p);
    assert(s->ctrl_rcv_buf);
    assert(s->ctrl_snd_buf);
    if (s->p) {
        free(s->p);
    }
    member_destroy(&s->remote);
    member_destroy(&s->local);
    schedule_destroy(s->evt_sched);
    free(s->evt_sched);
    free(s->ctrl_rcv_buf);
    free(s->ctrl_snd_buf);
    return(1);
}

/* Packet transmission */
int rtcp_send_rtcp_report(rtcp_session_t *s, rtcp_type_t type)
{
    assert(s);
    rtcp_t* p=(rtcp_t*)s->ctrl_snd_buf;
    struct timeval t;
    u_int32_t sz_buf=0;

    /* Common portion of the header */
    p->common.version=2;
    p->common.p=0;
    p->common.count=1;

    /* Size variables */
    const int bytes_per_word=4;
    int sz_sr=0;								/* in bytes */
    int sz_rr=0;								/* in bytes */

    if (type==RTCP_SR) {
        /* We transmitted data in the last interval, so transmit a sender
        * report.
        */
        p->common.pt=RTCP_SR;
        p->r.sr.ssrc=s->ssrc;

        assert(gettimeofday(&t, NULL)==0);
        p->r.sr.ntp_sec=t.tv_sec;
        p->r.sr.ntp_frac=(t.tv_usec/1e6)*4294967296;
        struct timeval diff;
        timersub(&s->t0, &t, &diff);
        double time_per_packet=(s->samples_per_packet/s->sampling_frequency);
        double elapsed_time=diff.tv_sec+diff.tv_usec/1e6;
        double packets=elapsed_time/time_per_packet;
        u_int32_t ipackets=packets;
        p->r.sr.rtp_ts+=ipackets; 	/* XXXX Does this automatically wrap? */
        p->r.sr.psent=s->psent;
        p->r.sr.osent=s->osent;
        member_t* m=rtcp_get_sender(s, &s->ssrc);
        p->r.sr.rr[0].ssrc=m->rr.ssrc;
        p->r.sr.rr[0].fraction=m->rr.fraction;
        p->r.sr.rr[0].lost=m->rr.lost;
        p->r.sr.rr[0].last_seq=m->rr.last_seq;
        p->r.sr.rr[0].jitter=m->rr.jitter;
        p->r.sr.rr[0].lsr=m->rr.lsr;
        p->r.sr.rr[0].dlsr=m->rr.dlsr;

        sz_sr=6*4;							/* in bytes */
        sz_rr=sizeof(rtcp_rr_t);			/* in bytes */
    } else if (type==RTCP_RR) {
        /* We did not transmit any data in the last interval, so transmit a
        * receiver report.
        */
        p->common.pt=RTCP_RR;
        p->r.sr.ssrc=s->ssrc;
        member_t* m=rtcp_get_sender(s, &s->ssrc);
        assert(m);
        p->r.sr.rr[0].ssrc=m->rr.ssrc;
        p->r.sr.rr[0].fraction=m->rr.fraction;
        p->r.sr.rr[0].lost=m->rr.lost;
        p->r.sr.rr[0].last_seq=m->rr.last_seq;
        p->r.sr.rr[0].jitter=m->rr.jitter;
        p->r.sr.rr[0].lsr=m->rr.lsr;
        p->r.sr.rr[0].dlsr=m->rr.dlsr;
        sz_sr=0;							/* in bytes */
        sz_rr=sizeof(rtcp_rr_t);			/* in bytes */
    } else {
        syslog(LOG_WARNING, "rtcp_send_rtcp_report: unknown type.");
    }

    int sz_words=(sz_sr+sz_rr)/4;
    sz_buf=sz_words*bytes_per_word;			/* words * bytes_per_word */
    p->common.length=sz_words; 				/* length in words, w/o this word */
    int ret=my_send(s->ctrl_sock, s->ctrl_snd_buf, sz_buf, 0);
    return(ret>0?ret:0);
}

int rtcp_send_bye_packet(rtcp_session_t *s)
{
    assert(s);
    /* XXX NOT thread safe */
    rtcp_t* p=(rtcp_t*)s->ctrl_snd_buf;
    p->common.version=2;
    p->common.p=0;
    p->common.count=1;
    p->common.pt=RTCP_BYE;
    p->common.length=1; 				/* length in words, w/o this word */
    p->r.bye.src[0]=s->ssrc;
    u_int32_t num_words=2;
    u_int32_t bytes_per_word=4;
    u_int32_t sz_buf=num_words*bytes_per_word;	/* words * bytes_per_word */
    int ret=my_send(s->ctrl_sock, s->ctrl_snd_buf, sz_buf, 0);
    return(ret>0?ret:0);
}

/* Search functions */
/**
*	Determines if a member is in the member list or not.
 *	@param m A pointer to the RTCP session.
 *	@param p A pointer to a packet received from the member.
 *	@return 1: if the participant is not currently in the members list,
 *			0: otherwise.
 */
int rtcp_new_member(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);
    assert(p->buf);
    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (s->remote.ssrc==ssrc)
        return(0);
    return(1);
}

/**
*	Determines if a sender is in the member list or not.
 *	@param m A pointer to the member tree.
 *	@param s A pointer to the ssrc that identifies the member to look for.
 *	@return 1: if the participant who sent packet p is not currently in
 *				the sender "subtree" of the member tree, 0: otherwise.
 */
int rtcp_new_sender(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);
    assert(p->buf);
    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (s->remote.ssrc!=ssrc)
        return(0);
    if (s->remote.sender)
        return(0);
    return(1);
}

/* Session state */
/**
*	Adds the member that sent packet p to the member tree.
 *	@param s A pointer to the RTCP session.
 *  @param p A pointer to packet sent by new member.
 *	@return 1: on success, 0: otherwise.
 */
int rtcp_add_member(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);
    assert(p->buf);
    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (s->remote.ssrc==ssrc)
        return(0);
    member_destroy(&s->remote);
    member_init(&s->remote, ssrc);
    return(1);
}

/**
*	Removes the member that sent packet p from the member tree.
 *	@param m A pointer to the member tree.
 *	@param s A pointer to packet sent by the member.
 *	@return 1: on success, 0: otherwise.
 */
int rtcp_remove_member(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);

    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (ssrc==0)
        return(0);
    if (s->remote.ssrc!=ssrc)
        return(1);
    member_destroy(&s->remote);
    return(1);
}

/**
*	Add a new sender to the sender "sublist" of the member tree. This function
 *  simply sets the sender flag for a member that is already in the list.
 *	@param m A pointer to the member tree to add to.
 *	@param p A pointer to a packet received from this sender.
 *	@return 1: on success, 0: on failure.
 */
int rtcp_add_sender(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);
    rtcp_packet_type_t pt=rtcp_packet_type(p);
    if (pt==PACKET_INVALID)
        return(0);

    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (ssrc==0) {
        /* Invalid SSRC - XXXX */
        return(0);
    }
    if (s->remote.ssrc!=ssrc)
        return(0);
    s->remote.sender=1;
    return(1);
}

int rtcp_remove_sender(rtcp_session_t *s, rtcp_packet_t* p)
{
    assert(s);
    assert(p);
    rtcp_packet_type_t pt=rtcp_packet_type(p);
    if (pt==PACKET_INVALID)
        return(0);

    u_int32_t ssrc=rtcp_packet_ssrc(p);
    if (ssrc==0) {
        /* Invalid SSRC - XXXX */
        return(0);
    }
    if (s->remote.ssrc!=ssrc)
        return(0);
    s->remote.sender=0;
    return(1);
}

static member_t* rtcp_get_sender(rtcp_session_t *s, const u_int32_t* ssrc)
{
    assert(s);
    assert(s);
    return((member_t*)&s->remote);
}

/**
 *	Called whenever an RTCP packet is received.
 *	@param s a pointer to the rtcp session block.
 *	@param p a pointer to the received packet.
 * 	@see on_receive()
 */
extern void rtcp_on_receive(rtcp_session_t *s,
                            rtcp_packet_t *p)
{
    struct timeval tc;
    gettimeofday(&tc, NULL);
    s->tc=tc.tv_sec+tc.tv_usec/1e6;
    
    /* What we do depends on whether we have left the group, and are
        * waiting to send a BYE (rtcp_type_of_event(e) == EVENT_BYE) or an RTCP
        * report.  p represents the packet that was just received.  */
    if (rtcp_packet_type(p) == PACKET_RTCP_REPORT) {
        u_int32_t ssrc=rtcp_packet_ssrc(p);
        if (rtcp_new_member(s, p)) {
            s->members += 1;
        }
        s->avg_rtcp_size = (1./16.)*rtcp_received_packet_size(p) +
        (15./16.)*(s->avg_rtcp_size);
    } else if (rtcp_packet_type(p) == PACKET_RTP) {
        u_int32_t ssrc=rtcp_packet_ssrc(p);
        u_int16_t seq=rtcp_packet_seq(p);
        u_int8_t added_sender=0;
        member_t *src=NULL;
        if (rtcp_new_member(s, p)) {
            syslog(LOG_WARNING, "Adding member.");
            rtcp_add_member(s, p);
            s->members += 1;
        }
        if (rtcp_new_sender(s, p)) {
            /* New sender */
            syslog(LOG_WARNING, "Adding sender.");
            rtcp_add_sender(s, p);
            s->senders += 1;
            if (ssrc==0) {
                syslog(LOG_WARNING, "Invalid ssrc.");
                return;
            } else {
                src=rtcp_get_sender(s, &ssrc);
                if (src==NULL) {
                    syslog(LOG_WARNING, "Unable to find sender with ssrc: %d",
                           ssrc);
                    member_init(&s->remote, ssrc);
                }
                init_seq(&src->src, seq);
                src->src.max_seq = seq - 1;
                src->src.probation = MIN_SEQUENTIAL;
            }
        } else {
            /* Existing sender */
            src=rtcp_get_sender(s, &ssrc);
            if (src) {
                update_seq(&src->src, seq);
            } else {
                syslog(LOG_WARNING, "Unable to find sender with ssrc: %d",
                       ssrc);
            }
        }
    } else if (rtcp_packet_type(p) == PACKET_BYE) {
        s->avg_rtcp_size = (1./16.)*rtcp_received_packet_size(p) +
        (15./16.)*(s->avg_rtcp_size);

        if (rtcp_new_sender(s, p) == FALSE) {
            rtcp_remove_sender(s, p);
            s->senders -= 1;
        }
        if (rtcp_new_member(s, p) == FALSE) {
            rtcp_remove_member(s, p);
            s->members -= 1;
        }
    }
}

/*
 * The routine update_seq shown below ensures that a source is declared
 * valid only after MIN_SEQUENTIAL packets have been received in
 * sequence.  It also validates the sequence number seq of a newly
 * received packet and updates the sequence state for the packet's
 * source in the structure to which s points.
 *
 * When a new source is heard for the first time, that is, its SSRC
 * identifier is not in the table (see Section 8.2), and the per-source
 * state is allocated for it, s->probation is set to the number of
 * sequential packets required before declaring a source valid
 * (parameter MIN_SEQUENTIAL) and other variables are initialized:
 *
 * init_seq(s, seq);
 * s->max_seq = seq - 1;
 * s->probation = MIN_SEQUENTIAL;
 *
 * A non-zero s->probation marks the source as not yet valid so the
 * state may be discarded after a short timeout rather than a long one,
 * as discussed in Section 6.2.1.
 *
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
 *  considered the valid start of a new packet sequence presumably caused
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
 */
static void init_seq(source *s, u_int16_t seq)
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

static int update_seq(source *s, u_int16_t seq)
{
    u_int16_t udelta = seq - s->max_seq;


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

/**
*	Computes the deterministic calculated interval, measured in seconds.
 *	@param s a pointer to the rtcp session block.
 *	@param members estimated number of members in the current session.
 *	@param senders estimated number of senders in the current session.
 *	@param rtcp_bw The target RTCP bandwidth, i.e., the total bandwidth.
 *	that will be used for RTCP packets by all members of this session,
 *	in octets per second.  This will be a specified fraction of the
 *	"session bandwidth" parameter supplied to the application at
 *	startup.
 *	@param we_sent true if application has sent data since the second previous
 *	RTCP report was transmitted.
 *	@param avg_rtcp_size The average compound RTCP packet size, in octets,
 *	over all RTCP packets sent and received by this participant.  The
 *	size includes lower-layer transport and network protocol headers
 *	(e.g., UDP and IP).
 *	@param initial flag that is true if the application has not yet sent
 *	an RTCP packet.
 *	@return current rctp interval (in seconds).
 */
static double rtcp_interval(rtcp_session_t* s,
                            int members,
                            int senders,
                            double rtcp_bw,
                            int we_sent,
                            double avg_rtcp_size,
                            int initial)
{
    /*
     * Minimum average time between RTCP packets from this site (in
                                                                 * seconds).  This time prevents the reports from `clumping' when
     * sessions are small and the law of large numbers isn't helping
     * to smooth out the traffic.  It also keeps the report interval
     * from becoming ridiculously small during transient outages like
     * a network partition.
     */
    double const RTCP_MIN_TIME = 5.;
    /*
     * Fraction of the RTCP bandwidth to be shared among active
     * senders.  (This fraction was chosen so that in a typical
                  * session with one or two active senders, the computed report
                  * time would be roughly equal to the minimum report time so that
                  * we don't unnecessarily slow down receiver reports.)  The
     * receiver fraction must be 1 - the sender fraction.
     */
    double const RTCP_SENDER_BW_FRACTION = 0.25;
    double const RTCP_RCVR_BW_FRACTION = (1-RTCP_SENDER_BW_FRACTION);
    /*
     /* To compensate for "timer reconsideration" converging to a
     * value below the intended average.
     */
     double const COMPENSATION = 2.71828 - 1.5;

     double t;                   /* interval */
     double rtcp_min_time = RTCP_MIN_TIME;
     int n;                      /* no. of members for computation */

     /*
      * Very first call at application start-up uses half the min
      * delay for quicker notification while still allowing some time
      * before reporting for randomization and to learn about other
      * sources so the report interval will converge to the correct
      * interval more quickly.
      */
     if (initial) {
         rtcp_min_time /= 2;
     }
     /*
      * Dedicate a fraction of the RTCP bandwidth to senders unless
      * the number of senders is large enough that their share is
      * more than that fraction.
      */
     n = members;
     if (senders <= members * RTCP_SENDER_BW_FRACTION) {
         if (we_sent) {
             rtcp_bw *= RTCP_SENDER_BW_FRACTION;
             n = senders;
         } else {
             rtcp_bw *= RTCP_RCVR_BW_FRACTION;
             n -= senders;
         }
     }

     /*
      * The effective number of sites times the average packet size is
      * the total number of octets sent when each site sends a report.
      * Dividing this by the effective bandwidth gives the time
      * interval over which those packets must be sent in order to
      * meet the bandwidth target, with a minimum enforced.  In that
      * time interval we send one report so this time is also our
      * average time between reports.
      */
     t = avg_rtcp_size * n / rtcp_bw;
     if (t < rtcp_min_time) t = rtcp_min_time;

     /*
      * To avoid traffic bursts from unintended synchronization with
      * other sites, we then pick our actual next report interval as a
      * random number uniformly distributed between 0.5*t and 1.5*t.
      */
     t = t * (drand48() + 0.5);
     t = t / COMPENSATION;
     return t;
}

/**
*	Determines the size of a received packet.
 *	@param s 	A pointer to the packet to be parsed.
 *	@return 	The size of the received packet.
 */
static int rtcp_received_packet_size(rtcp_packet_t *p)
{
    assert(p);
    assert(p->buf);
    return(p->length);
}

/**
*	Determines the size of packet generated and sent by event e.
 *	@param s 	The event that generated the packet.
 *	@return 	The size of the sent packet.
 */
static int rtcp_sent_packet_size(rtcp_event_t e)
{
    return(e.sent_packet_size);
}

/**
*	Determines the type of an event.
 *	@param e 	The event we are interested in.
 *	@return 	The type of the event. Valid types include:
 *				EVENT_BYE, EVENT_REPORT.
 */
static rtcp_event_type_t rtcp_type_of_event(rtcp_event_t e)
{
    return(e.etype);
}

/* Scheduling */
/**
*	Retrieves a pointer to the earliest event in the event queue and removes
 *	the event from the queue.
 *	@param s a pointer to the rtcp session block
 *	@param t a pointer that will contain the time of the event after
 *	the function returns. This must point to pre-allocated memory.
 *	@return a pointer to the type of the event.
 */
extern gpointer rtcp_earliest_event(rtcp_session_t *s, gpointer t)
{
    assert(s);
    assert(t);
    double k;
    gpointer ret=schedule_earliest_event(s->evt_sched, &k);
    memcpy(t, &k, sizeof(k));
    return(ret);
}

static void rtcp_schedule(rtcp_session_t *s, double t, rtcp_event_t e)
{
    assert(s);
    double* nt=malloc(sizeof(double));
    assert(nt);
    rtcp_event_t* ne=malloc(sizeof(rtcp_event_t));
    assert(ne);
    memcpy(nt, &t, sizeof(t));
    memcpy(ne, &e, sizeof(e));
    schedule_schedule(s->evt_sched, nt, ne);
    double k=0;
    ne=schedule_earliest_event_cp(s->evt_sched, &k);
    if (ne) {
        memcpy(&s->curr_event, ne, sizeof(rtcp_event_t));
        free(ne);
    }
}

static void rtcp_reschedule(rtcp_session_t *s, double t, rtcp_event_t e)
{
    assert(s);
    /* XXXX Needs to be implemented */
}

/* Misc */
/**
*	Determines the type of a packet. Note:
 *	Only weak validity checks are possible on an RTP/RTCP packet from a
 *	source that has not been heard before:
 *	o  RTP version field must equal 2.
 *	o  The payload type must be known.
 *	@param s 	A pointer to the packet to be parsed.
 *	@return 	the packet type of the packet. Packet type is one of:
 *				PACKET_RTCP_REPORT, PACKET_RTP, PACKET_BYE, PACKET_INVALID.
 *	@see		rtcp_packet_type_t.
 */
extern rtcp_packet_type_t rtcp_packet_type(rtcp_packet_t *p)
{
    rtcp_packet_type_t ret=PACKET_INVALID;
    if (p==NULL)
        return(PACKET_INVALID);
    if (p->length<4)
        return(PACKET_INVALID);
    if (p->buf==NULL)
        return(PACKET_INVALID);
    u_int32_t* hdr=&(((u_int32_t*)p->buf)[0]);
    u_int32_t masked_hdr=*hdr & RTCP_VALID_MASK;
    switch (masked_hdr) {
        case RTCP_VALID_SR:
            ret=PACKET_RTCP_REPORT;
            break;
        case RTCP_VALID_RR:
            ret=PACKET_RTCP_REPORT;
            break;
        case RTCP_VALID_BYE:
            ret=PACKET_BYE;
            break;
        default:
            /* XXXX This needs to be looked at */
            ret=PACKET_RTP;
            break;
    }
    return(ret);
}

/**
 *	Returns the SSRC of the packet supplied. Performs the appropriate checks to
 *	ensure the packet has an SSRC (i.e. the packet is one of: PACKET_RTCP_REPORT,
 *	or PACKET_RTP).
 *	@param s 	A pointer to the packet to be parsed.
 *	@return 	the SSRC of the packet. 0 is used to indicate an error.
 */
extern u_int32_t rtcp_packet_ssrc(rtcp_packet_t *p)
{
    if (p==NULL)
        return(0);
    if (p->buf==NULL)
        return(0);

    u_int32_t* hdr=NULL;
    rtcp_packet_type_t pt=rtcp_packet_type(p);

    switch (pt) {
        case PACKET_INVALID:
            return(0);
            break;
        case PACKET_RTCP_REPORT:
            hdr=&(((u_int32_t*)p->buf)[1]);
            break;
        case PACKET_RTP:
            hdr=&(((u_int32_t*)p->buf)[2]);
            break;
        case PACKET_BYE:
            hdr=&(((u_int32_t*)p->buf)[2]);
            break;
        default:
            return(0);
            break;
    };
    return(ntohl(*hdr));
}

/**
 *	Returns the SSRC of the packet supplied. Performs the appropriate checks to
 *	ensure the packet has an SSRC (i.e. the packet is one of: PACKET_RTCP_REPORT,
 *	or PACKET_RTP).
 *	@param s 	A pointer to the packet to be parsed.
 *	@return 	the seq of the packet. 0 is used to indicate an error.
 */
static u_int32_t rtcp_packet_seq(rtcp_packet_t *p)
{
    if (p==NULL)
        return(0);
    if (p->buf==NULL)
        return(0);

    u_int16_t* hdr=NULL;
    rtcp_packet_type_t pt=rtcp_packet_type(p);

    switch (pt) {
        case PACKET_INVALID:
            return(0);
            break;
        case PACKET_RTCP_REPORT:
            return(0);
            break;
        case PACKET_RTP:
            hdr=&(((u_int16_t*)p->buf)[1]);
            break;
        default:
            return(0);
            break;
    };
    return(ntohl(*hdr));
}

/**
 *	Takes as input a packet buffer and gives the caller access to
 * 	the data portion of the packet.
 *	@param p 	A pointer to the packet to be parsed.
 *	@param sz 	A pointer to a u_int32_t that will contain the size of
 *				the returned data buffer.
 *	@return 	NULL on error, A pointer to the packet data on success.
 */
extern u_int8_t* rtcp_packet_data(rtcp_packet_t *p, u_int32_t* sz)
{
    if (p==NULL)
        return(NULL);
    if (p->buf==NULL)
        return(NULL);
    if (p->length<(sizeof(rtp_hdr_t)-4))
        return(NULL);
    
    rtp_hdr_t* hdr=NULL;
    rtcp_packet_type_t pt=rtcp_packet_type(p);

    switch (pt) {
        case PACKET_INVALID:
            return(NULL);
            break;
        case PACKET_RTCP_REPORT:
            return(NULL);
            break;
        case PACKET_RTP:
            hdr=(rtp_hdr_t*)p->buf;
            *sz=(p->length-sizeof(rtp_hdr_t)+4);	/* Size needs to be adjusted
                                                     * for empty csrc field.
                                                     */
            break;
        default:
            return(NULL);
            break;
    };
    
    /* Since csrc field is empty, data will begin here. */
    return((u_int8_t*)hdr->csrc);
}

/**
*	Prints the supplied data to the sender's output file.
 *	@param m A pointer to the member tree.
 *	@param ssrc The ssrc of the sender to print.
 *	@param buf The buffer to print.
 *	@param sz The size of the buffer to print.
 *	@return 0: on eof/error, Number of bytes written: on success.
 */
extern int rtcp_member_write(rtcp_session_t* s,
                             u_int32_t ssrc,
                             u_int8_t* buf,
                             u_int32_t sz)
{
    assert(s);

    if (buf==NULL || sz==0)
        return(0);
    if (s->remote.ssrc!=ssrc)
        return(0);
    return(member_write(&s->remote, buf, sz));
}

