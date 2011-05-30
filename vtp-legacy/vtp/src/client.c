/*
 * $Source: /usr/local/cvsroot/mit/vtp/src/client.c,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/10 06:19:29 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: client.c,v $
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:49  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.17  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.16  2003/09/09 22:30:14  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.15  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.14  2003/09/09 05:57:41  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
                                                     * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.13  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.12  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.11  2003/09/03 12:54:01  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <assert.h>
#include <unistd.h>
#include <signal.h>
#include <syslog.h>
#include <stdarg.h>
#include <pthread.h>
#include <math.h>
#ifndef _OSX_
#include <sys/siginfo.h>
#endif

#include "mysock.h"
#include <rtcp.h>
#include "client.h"

/* "Module" data */
static int RUN_THREAD = 1;
static struct sockaddr_in ctrl_local_addr, ctrl_remote_addr;
static struct sockaddr_in data_local_addr, data_remote_addr;
static u_int8_t ctrl_rcv_buf[MAX_RCV_BUF];
static u_int8_t ctrl_snd_buf[MAX_SND_BUF];
static u_int8_t data_rcv_buf[MAX_RCV_BUF];
static u_int8_t data_snd_buf[MAX_SND_BUF];
static u_int8_t data_buf[MAX_DATA_BUF];
static FILE* input_file = NULL;
static int ctrl_connect_sock = -1;
static int data_sock = -1;
static int sock_opt_val = 0;
static struct timeval sock_timeval;

/* RTCP Session information */
rtcp_session_t r;

/* RTCP Receive function */
static void* rtcp_client_rcv_proc(void* arg)
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
    free(p);
    syslog(LOG_WARNING, "Exiting Rcv RTCP Proc...");
    return(NULL);
}

/* RTP Receive function */
static void* rtp_client_rcv_proc(void* arg)
{
    syslog(LOG_WARNING, "RTP Rcv Proc.");
    u_int32_t read_bytes=0;
    u_int32_t ssrc=0;
    rtcp_packet_t* p=malloc(sizeof(rtcp_packet_t));
    while (RUN_THREAD) {
        read_bytes=vtp_check_data(data_sock,
                                  data_rcv_buf,
                                  MAX_RCV_BUF,
                                  NULL);
        if (read_bytes>0) {
            syslog(LOG_WARNING, "    Received RTP message");
            p->buf=data_rcv_buf;
            p->length=read_bytes;
            rtcp_on_receive(&r, p);
            ssrc=rtcp_packet_ssrc(p);
            if (ssrc==0) {
                syslog(LOG_WARNING, "    Invalid SSRC");
                continue;
            }

        }
    }
    free(p);
    syslog(LOG_WARNING, "Exiting RTP Rcv Snd Proc...");
    return(NULL);
}

/* RTP Send function */
static void* rtp_client_snd_proc(void* arg)
{
    int data_bytes_read = 0;
    int data_bytes_sent = 0;
    rtp_hdr_t h;
    const int mtu=1400;
    int i=0;

    /* Initialize packet header */
    h.version=2;
    h.p=0;
    h.x=0;
    h.cc=0;
    h.m=0;
    h.pt=1;
    h.seq=0;		/* Needs to be changed XXXX */
    h.ts=0;			/* Needs to be changed XXXX */
    h.ssrc=r.ssrc;

    syslog(LOG_WARNING, "RTP Proc.");
    while (RUN_THREAD) {
        syslog(LOG_WARNING, "\tRTP Proc: sending data");

        /* Send data channel */
        data_bytes_read = vtp_read_data(input_file, data_buf, MAX_DATA_BUF);
        syslog(LOG_WARNING, "\tReading from file: read bytes: %d.\n",
               data_bytes_read);
        u_int32_t data_length=0;

        if (data_bytes_read==0) {
            syslog(LOG_WARNING, "\tReached end of file on input.\n");
            rtcp_send_bye_packet(&r);
            break;
        }

        for (i=0;i<data_bytes_read;i+=mtu)
        {
            if ( (i+mtu)<data_bytes_read) {
                data_length=mtu;
            } else {
                data_length=data_bytes_read-i;
            }
            /* Send data */
            data_bytes_sent = vtp_send_data(data_sock,
                                            (u_int8_t*)&h,
                                            sizeof(h)-4,
                                            /* Need to exclude
                                            * CSRC field
                                            */
                                            &data_buf[i],
                                            data_length);
            /* Update header */
            h.seq++;
            h.ts++;
            /* Update RTCP session info */
            r.psent++;
            r.osent+=data_length;
            r.we_sent=1;
            /* Print diagnostic messages */
            syslog(LOG_WARNING, "\tData packet: Packets sent: %d.\n",
                   r.psent);
            syslog(LOG_WARNING, "\tData packet: read bytes: %d.\n",
                   data_bytes_read);
            if (data_bytes_sent > 0) {
                syslog(LOG_WARNING, "\tData packet: sent bytes: %d.\n",
                       data_bytes_sent);
            }
            usleep(100000);
        }
    }
    syslog(LOG_WARNING, "Exiting RTP Proc...");
    return(NULL);
}

/* Thread initialization function */
int init_client_threads(vtp_rtp_init_t* rtcp_init,
                        vtp_rtcp_init_t* rtp_init)
{
    pthread_t new_thread;
    if (pthread_create(&new_thread, NULL, rtcp_client_rcv_proc, (void*)rtcp_init) != 0) {
        perror("RTCP rcv thread init.");
        return(-1);
    }
    if (pthread_create(&new_thread, NULL, rtp_client_rcv_proc, (void*)rtcp_init) != 0) {
        perror("RTCP rcv thread init.");
        return(-1);
    }
    if (pthread_create(&new_thread, NULL, rtp_client_snd_proc, (void*)rtp_init) !=0) {
        perror("RTP snd thread init.");
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
            break;
        default:
            break;
    }
}

int vtp_run_client(vtp_options_t* options)
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

    /* Open data file for reading */
    input_file = fopen(options->file_name, "r");
    if (input_file == NULL) {
        syslog(LOG_WARNING, "Input file: %s", options->file_name);
        perror("Unable to open input file");
        exit(1);
    }

    /* Setup control socket address */
    ctrl_connect_sock = my_socket(AF_INET, SOCK_STREAM, 0);
    ctrl_remote_addr.sin_family = AF_INET;
    ctrl_remote_addr.sin_port = htonl(options->ctrl_port);
    ctrl_remote_addr.sin_addr.s_addr = htonl(options->ip.s_addr);

    /* Set socket options */
    sock_opt_val = 5;
    my_setsockopt(ctrl_connect_sock, SOL_SOCKET, SO_SNDLOWAT, &sock_opt_val,
                  sizeof(int));
    sock_timeval.tv_sec = 1;
    sock_timeval.tv_usec = 0;
    my_setsockopt(ctrl_connect_sock, SOL_SOCKET, SO_SNDTIMEO, &sock_timeval,
                  sizeof(sock_timeval));

    /* Setup data socket address */
    data_sock = my_socket(AF_INET, SOCK_DGRAM, 0);
    data_remote_addr.sin_family = AF_INET;
    data_remote_addr.sin_port = htonl(options->data_port);
    data_remote_addr.sin_addr.s_addr = htonl(options->ip.s_addr);

    /* Connect to server */
    my_connect(ctrl_connect_sock, (struct sockaddr*) &ctrl_remote_addr,
               sizeof(struct sockaddr_in));
    my_connect(data_sock, (struct sockaddr*) &data_remote_addr,
               sizeof(struct sockaddr_in));

    syslog(LOG_WARNING, "Connected to %s\n",
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
        syslog(LOG_WARNING, "Client unable to init RTCP session.");
        return(0);
    }
    
    /* Initialize threads */
    RUN_THREAD = 1;
    if (init_client_threads(NULL, NULL)!=0) {
        syslog(LOG_WARNING, "Unable to init threads.");
        exit(1);
    }

    /* Get pid() */
    pid_t mypid=getpid();
    syslog(LOG_WARNING, "client pid: %d\n", mypid);

    while (RUN_THREAD) {
        syslog(LOG_WARNING, "Server: sending RTCP_SR.");
        rtcp_send_rtcp_report(&r, RTCP_SR);
        usleep(5000000);
    }
    RUN_THREAD=0;
    /* Break threads out of blocked system calls */
    kill(mypid, SIGUSR1);
    usleep(100000);
    closelog();
    rtcp_destroy(&r);
    return(1);
}
