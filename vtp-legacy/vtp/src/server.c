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
 * $Source: /usr/local/cvsroot/mit/vtp/src/server.c,v $
 * $Revision: 1.4 $
 * $Date: 2003/09/18 01:48:47 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: server.c,v $
 * Revision 1.4  2003/09/18 01:48:47  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.3  2003/09/18 00:29:43  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:50  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.14  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.13  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.12  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.11  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
                                                     * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.10  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.9  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.8  2003/09/03 12:54:01  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <assert.h>
#include <syslog.h>
#include <stdarg.h>
#include <pthread.h>
#include <rtp.h>
#include <rtcp.h>
#include <math.h>
#include "mysock.h"
#include "vtp.h"
#include "server.h"

/* "Module" data */
static int RUN_THREAD = 1;
static int ctrl_listen_sock = -1, ctrl_connect_sock = -1;
static int data_sock = -1;
static int yes = 1;
static struct sockaddr_in ctrl_local_addr, ctrl_remote_addr;
static struct sockaddr_in data_local_addr, data_remote_addr;
static u_int8_t ctrl_rcv_buf[MAX_RCV_BUF];
static u_int8_t ctrl_snd_buf[MAX_SND_BUF];
static u_int8_t data_rcv_buf[MAX_RCV_BUF];
static u_int8_t data_snd_buf[MAX_SND_BUF];
static u_int8_t data_buf[MAX_DATA_BUF];

static rtcp_session_t r;

/* State variable */
vtp_state_t curr_server_state = INIT;

/* RTCP Receive function */
static void* rtcp_server_rcv_proc(void* arg)
{
    syslog(LOG_WARNING, "RTCP Rcv Proc.");
    u_int32_t read_bytes=0;
    rtcp_packet_t* p=malloc(sizeof(rtcp_packet_t));
    while (RUN_THREAD) {
        read_bytes=vtp_check_ctrl(ctrl_connect_sock,
                                  ctrl_rcv_buf,
                                  MAX_RCV_BUF,
                                  NULL);
        if (read_bytes>0) {
            syslog(LOG_WARNING, "Received RTCP message");
            p->buf=ctrl_rcv_buf;
            p->length=read_bytes;
            rtcp_on_receive(&r, p);
        }
    }
    syslog(LOG_WARNING, "Exiting Rcv RTCP Proc...");
    return(NULL);
}

/* RTP Receive function */
static void* rtp_server_rcv_proc(void* arg)
{
    syslog(LOG_WARNING, "RTP Rcv Proc.");
    u_int32_t data_bytes_rcvd=0;
    u_int32_t ssrc=0;
    u_int32_t data_bytes_to_write=0;
    u_int32_t data_bytes_written=0;
    u_int8_t* data=NULL;
    rtcp_packet_t* p=malloc(sizeof(rtcp_packet_t));
    while (RUN_THREAD) {
        data_bytes_rcvd=vtp_check_data(data_sock,
                                       data_rcv_buf,
                                       MAX_RCV_BUF,
                                       NULL);
        if (data_bytes_rcvd>0) {
            syslog(LOG_WARNING, "    Received RTP message");
            /* Check consistency */
            p->buf=data_rcv_buf;
            p->length=data_bytes_rcvd;
            rtcp_on_receive(&r, p);
            data=rtcp_packet_data(p, &data_bytes_to_write);
            ssrc=rtcp_packet_ssrc(p);
            if (ssrc) {
                syslog(LOG_WARNING, "    %d bytes to write for ssrc %u.",
                       data_bytes_to_write, ssrc);
                data_bytes_written=rtcp_member_write(&r, ssrc, data,
                                                     data_bytes_to_write);
                syslog(LOG_WARNING, "    %d bytes written for ssrc %u.",
                      data_bytes_written, ssrc);
            } else {
                syslog(LOG_WARNING, "    Invalid ssrc.");
            }
        } else {
            syslog(LOG_WARNING, "    No RTP data received");
        }
    }
    free(p);
    syslog(LOG_WARNING, "Exiting RTP Rcv Snd Proc...");
    return(NULL);
}

/* Thread initialization function */
int init_server_threads(vtp_rtcp_init_t* rtcp_init,
                        vtp_rtp_init_t* rtp_init)
{
    pthread_t new_thread;
    if (pthread_create(&new_thread, NULL, rtcp_server_rcv_proc, (void*)rtcp_init) != 0) {
        perror("RTCP rcv thread init.");
        return(-1);
    }
    if (pthread_create(&new_thread, NULL, rtp_server_rcv_proc, (void*)rtcp_init) != 0) {
        perror("RTCP rcv thread init.");
        return(-1);
    }
    return(0);
}

static void vtp_client_sig_handler(int signo)
{
    struct timeval t;
    gettimeofday(&t, NULL);
    switch (signo) {
        case SIGUSR1:
            printf("%f: Received SIGUSR1\n", t.tv_sec+t.tv_usec/1e6 );
            /* Close files */
            member_close(&r.remote);
            member_close(&r.local);
            break;
        default:
            break;
    }
}

int vtp_run_server(vtp_options_t* options)
{
    /* Initialize signal handlers */
    struct sigaction act, old_act;
    act.sa_handler=vtp_client_sig_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags=0;
    assert(sigaction(SIGUSR1, &act, &old_act)==0);
    
    /* Initialize data structures */
    assert( memset(&ctrl_local_addr, '\0', sizeof(ctrl_local_addr)) );
    assert( memset(&ctrl_remote_addr, '\0', sizeof(ctrl_remote_addr)) );
    assert( memset(&data_local_addr, '\0', sizeof(data_local_addr)) );
    assert( memset(&data_remote_addr, '\0', sizeof(data_remote_addr)) );

    assert( memset(ctrl_rcv_buf, '\0', MAX_RCV_BUF) );
    assert( memset(ctrl_snd_buf, '\0', MAX_SND_BUF) );
    assert( memset(data_rcv_buf, '\0', MAX_RCV_BUF) );
    assert( memset(data_snd_buf, '\0', MAX_SND_BUF) );
    assert( memset(data_buf, '\0', MAX_DATA_BUF) );

    /* Setup control socket address */
    ctrl_listen_sock = my_socket(AF_INET, SOCK_STREAM, 0);
    ctrl_local_addr.sin_family = AF_INET;
    ctrl_local_addr.sin_port = htonl(options->ctrl_port);
    ctrl_local_addr.sin_addr.s_addr = htonl(options->ip.s_addr);

    /* Set socket options */
    my_setsockopt(ctrl_listen_sock, SOL_SOCKET, SO_REUSEADDR, &yes,
                  sizeof(int));

    /* Setup data socket address */
    data_sock = my_socket(AF_INET, SOCK_DGRAM, 0);
    data_local_addr.sin_family = AF_INET;
    data_local_addr.sin_port = htonl(options->data_port);
    data_local_addr.sin_addr.s_addr = htonl(options->ip.s_addr);

    /* Bind sockets */
    my_bind(ctrl_listen_sock, (struct sockaddr*)&ctrl_local_addr,
            sizeof(struct sockaddr_in));
    my_bind(data_sock, (struct sockaddr*)&data_local_addr,
            sizeof(struct sockaddr_in));

    my_listen(ctrl_listen_sock, MAX_BACKLOG);

#if __ORIG__
    /* Open data file for writing */
    output_file = fopen(options->file_name, "w");
    if (output_file == NULL) {
        perror("Unable to open output file");
        exit(1);
    }
#endif

    /* Connection processing loop */
    const int sz_ctrl_remote_addr = sizeof(ctrl_remote_addr);
    ctrl_connect_sock = accept(ctrl_listen_sock,
                               (struct sockaddr*)&ctrl_remote_addr,
                               (int*)&sz_ctrl_remote_addr);

    syslog(LOG_WARNING, "Rcvd control connection from %s\n",
           inet_ntoa(ctrl_remote_addr.sin_addr));

    /* Initialize RTCP session */
    if (rtcp_init(&r,
                  ctrl_connect_sock,
                  data_sock,
                  options->rtcp_bw,
                  options->avg_rtcp_size,
                  options->ssrc,
                  options->sampling_frequency,
                  options->samples_per_packet,
                  options->bits_per_sample,
                  options->sz_ctrl_buf,
                  options->sz_ctrl_buf)==0) {
        syslog(LOG_WARNING, "Server unable to init RTCP session.");
        return(0);
    }
    
    /* Initialize threads */
    RUN_THREAD = 1;
    if (init_server_threads(NULL, NULL)!=0) {
        syslog(LOG_WARNING, "Unable to init threads.");
        exit(1);
    }

    /* Get pid() */
    pid_t mypid=getpid();
    syslog(LOG_WARNING, "server pid: %d\n", mypid);
    
    while (RUN_THREAD) {
        syslog(LOG_WARNING, "Server: sending RTCP_RR.");
        rtcp_send_rtcp_report(&r, RTCP_RR);
        usleep(5000000);
    }
    RUN_THREAD=0;
    /* Break threads out of blocked system calls */
    kill(mypid, SIGUSR1);
    usleep(100000);
    rtcp_destroy(&r);
    closelog();
    return(1);
}



