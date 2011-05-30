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
 * $Source: /usr/local/cvsroot/mit/vtp/include/vtp.h,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/10 00:32:46 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: vtp.h,v $
 * Revision 1.1  2003/09/10 00:32:46  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.13  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.12  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.11  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.10  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.9  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.8  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.7  2003/09/03 12:54:02  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/types.h>
#include <time.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <string.h>
#include <stdio.h>
#include <glib.h>

#ifndef _VTP_H_
#define _VTP_H_


typedef enum { CLIENT, SERVER, TEST } vtp_mode_t;

typedef struct {
    u_int32_t ctrl_port;
    u_int32_t data_port;
    struct in_addr ip;
    vtp_mode_t mode;
    char *file_name;
    char *config_file_name;
    u_int32_t rtcp_bw;
    u_int32_t avg_rtcp_size;
    u_int32_t ssrc;
    u_int32_t sampling_frequency;
    u_int32_t samples_per_packet;
    u_int32_t bits_per_sample;
    u_int32_t sz_ctrl_buf;
} vtp_options_t;

/* Thread parameter passing */
typedef struct {
    int i;
} vtp_rtp_init_t;

typedef struct {
    int i;
} vtp_rtcp_init_t;


/* Arguments processing */
extern int vtp_foption(int argc, const char * argv[], vtp_options_t *options);
extern void vtp_usage(FILE* out);

/* Message types */
#define	VTP_VERSION		31415
#define	MAX_WINDOW_SIZE	1000;

typedef enum {
    CTRL_START = 0x1,
    CTRL_STOP = 0x2,
    CTRL_ACK = 0x4,
    CTRL_PROBE = 0x8,
    DATA = 0x10
} vtp_pt_t;

typedef struct {
    u_int32_t version;
    u_int32_t pt;
    u_int32_t seq;
    u_int32_t padding;
    u_int32_t orig_timestamp;
    u_int32_t dest_timestamp;
    u_int32_t rtt;
    u_int32_t packets_lost;
    u_int32_t lost_seq_length;
    u_int32_t *lost_seq;
} vtp_ctrl_hdr_t;

typedef struct {
    u_int32_t version;
    u_int32_t pt;
    u_int32_t seq;
    u_int32_t padding;
    u_int32_t data_length;
    u_int8_t *data;
} vtp_data_hdr_t;

typedef enum {
    INIT = 0,
    CONNECTED = 1,
    ACKING = 2,
    PROBING = 3,
    CLOSING = 4
} vtp_state_t;

/* Connection */
extern int vtp_init(char* remote_ip,
                    u_int16_t ctrl_port,
                    u_int16_t data_port);


/* Message processing */
#ifdef COMMENT
extern int vtp_check_ctrl(int ctrl_connect_sock,
                          u_int8_t *ctrl_rcv_buf, int sz_ctrl_rcv_buf,
                          struct timeval *timeout,
                          vtp_ctrl_hdr_t *ctrl_packet);
#else
extern int vtp_check_ctrl(int ctrl_connect_sock,
                          u_int8_t *ctrl_rcv_buf, int sz_ctrl_rcv_buf,
                          struct timeval *timeout);
#endif

extern int vtp_send_ctrl(int ctrl_connect_sock,
                         u_int8_t* ctrl_snd_buf, int sz_ctrl_snd_buf,
                         vtp_pt_t type);
#ifdef COMMENT
extern int vtp_check_data(int data_sock,
                          u_int8_t* data_rcv_buf, int sz_data_rcv_buf,
                          u_int8_t* data_buf, int sz_data_buf,
                          int* data_bytes_to_write,
                          struct timeval *timeout);
#else
extern int vtp_check_data(int data_sock,
                          u_int8_t* data_rcv_buf, int sz_data_rcv_buf,
                          struct timeval *timeout);
#endif

#if __ORIG__
extern int vtp_send_data(int data_sock,
                         u_int8_t* data_snd_buf, int sz_data_snd_buf,
                         u_int8_t* data_to_snd,	int sz_data_to_snd,
                         int mtu);
#else
extern int vtp_send_data(int data_sock,
                         u_int8_t* hdr_buf,
                         int sz_hdr_buf,
                         u_int8_t* data_to_snd,
                         int sz_data_to_snd);
#endif
/* Data operations */
extern int vtp_read_data(FILE* inf, u_int8_t* data_buf, int sz_data_buf);

extern int vtp_write_data(FILE* outf, u_int8_t* data_buf, int sz_data_buf);

/* Signal processing */
int vtp_signal(int signo, void (*func)());

#endif /* _VTP_H_ */

