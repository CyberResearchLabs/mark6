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
 * $Source: /usr/local/cvsroot/mit/vtp/include/rtcp.h,v $
 * $Revision: 1.3 $
 * $Date: 2003/09/18 01:48:45 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: rtcp.h,v $
 * Revision 1.3  2003/09/18 01:48:45  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.2  2003/09/10 06:19:26  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:46  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.8  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.7  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.6  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.5  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.4  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.3  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.2  2003/09/04 12:39:40  davidlapsley
 * Daily release.
 *
 * Revision 1.1  2003/09/03 19:57:23  davidlapsley
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
 *
 *
 *
 */

#include <sys/time.h>
#include <glib.h>
#include <rtp.h>
#include <schedule.h>
#include <member.h>

#ifndef _RTCP_H_
#define _RTCP_H_

/**
 *	This struct stores all of the RTCP session state.
 */
typedef struct {
    /* Stores member state */
    member_t remote;
    member_t local;
    
    /* For storing socket descriptors */
    int ctrl_sock;
    int data_sock;

    /* For calcuation of rtcp_interval */
    int members;
    int senders;
    int pmembers;
    double rtcp_bw;
    double avg_rtcp_size;

    /* Flags */
    int we_sent;
    int initial;

    /* Next event to occur */
    rtcp_event_t curr_event;

    /* Time parameters */
    double tc;
    double tp;
    rtcp_packet_t *p;
    struct timeval t0;
    double tn;
    u_int32_t rtp_ts0;
    u_int32_t ntp_sec0;
    u_int32_t ntp_frac0;
    u_int32_t sampling_frequency;
    u_int32_t samples_per_packet;
    u_int32_t bits_per_sample;

    /* Sender report stats */
    u_int32_t psent;
    u_int32_t osent;

    /* Stores event queue */
    schedule_t* evt_sched;

    /* Control buffers */
    u_int8_t *ctrl_rcv_buf;
    u_int8_t *ctrl_snd_buf;

    u_int32_t ssrc;
} rtcp_session_t;

/* Session initiation/termination */
extern int rtcp_init(rtcp_session_t* s,
                     int ctrl_sock,
                     int data_sock,
                     double rtcp_bw,
                     double avg_rtcp_size,
                     u_int32_t ssrc,
                     u_int32_t sampling_frequency,
                     u_int32_t samples_per_packet,
                     u_int32_t bits_per_sample,
                     u_int32_t sz_ctrl_rcv_buf,
                     u_int32_t sz_ctrl_snd_buf);

extern int rtcp_destroy(rtcp_session_t* s);

/* Packet transmission */
extern int rtcp_send_rtcp_report(rtcp_session_t *s, rtcp_type_t r);
extern int rtcp_send_bye_packet(rtcp_session_t *s);

/* Search functions */
static int rtcp_new_member(rtcp_session_t *s, rtcp_packet_t* p);
static int rtcp_new_sender(rtcp_session_t *s, rtcp_packet_t* p);

/* Session state */
static int rtcp_add_member(rtcp_session_t *s, rtcp_packet_t* p);
static int rtcp_remove_member(rtcp_session_t *s, rtcp_packet_t* p);
static int rtcp_add_sender(rtcp_session_t *s, rtcp_packet_t* p);
static int rtcp_remove_sender(rtcp_session_t *s, rtcp_packet_t* p);
static member_t* rtcp_get_sender(rtcp_session_t *s, const u_int32_t* ssrc);
static void init_seq(source *s, u_int16_t seq);
static int update_seq(source *s, u_int16_t seq);


extern void rtcp_on_receive(rtcp_session_t *s,
                            rtcp_packet_t *p);


static void on_receive(rtcp_session_t *s,
                       rtcp_packet_t *p,
                       rtcp_event_t e,
                       int *members,
                       int *pmembers,
                       int *senders,
                       double *avg_rtcp_size,
                       double *tp,
                       double tc,
                       double tn);

static double rtcp_interval(rtcp_session_t *s,
                            int members,
                            int senders,
                            double rtcp_bw,
                            int we_sent,
                            double avg_rtcp_size,
                            int initial);

/* Scheduling */

/**
*	Retrieves a pointer to the earliest event in the event queue.
 *	@param s a pointer to the rtcp sessin block
 *	@param t a pointer that will contain the time of the event after
 *	the function returns.
 *	@return a pointer to the type of the event.
 */
extern gpointer rtcp_earliest_event(rtcp_session_t *s, gpointer t);

static void rtcp_schedule(rtcp_session_t *s,
                          double t,
                          rtcp_event_t e);

static void rtcp_reschedule(rtcp_session_t *s,
                            double t,
                            rtcp_event_t e);


/* Misc */
static int rtcp_received_packet_size(rtcp_packet_t *p);
static int rtcp_sent_packet_size(rtcp_event_t e);
static rtcp_event_type_t rtcp_type_of_event(rtcp_event_t e);
static rtcp_packet_type_t rtcp_packet_type(rtcp_packet_t *p);
extern u_int32_t rtcp_packet_ssrc(rtcp_packet_t *p);
static u_int32_t rtcp_packet_seq(rtcp_packet_t *p);
extern u_int8_t* rtcp_packet_data(rtcp_packet_t *p, u_int32_t* sz);
extern int rtcp_member_write(rtcp_session_t* s,
                             u_int32_t ssrc,
                             u_int8_t* buf,
                             u_int32_t sz);

#endif

