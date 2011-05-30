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
 * $Source: /usr/local/cvsroot/mit/vtp/src/vtp.c,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/10 00:32:50 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: vtp.c,v $
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
 * Revision 1.8  2003/09/03 12:54:02  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <syslog.h>
#include <stdarg.h>
#include <arpa/inet.h>
#include <assert.h>
#include <sys/uio.h>
#include <xml.h>

#include "mysock.h"
#include "vtp.h"

void vtp_usage(FILE* out)
{
#if __ORIG__
    fprintf(out, "Usage: vtp -[cst] -p <control port> -P <data port>\n");
    fprintf(out, "           -I <ip> -F <file name> -C <config file>\n");
#else
    fprintf(out, "Usage: vtp -F <xml config file>\n");
#endif
}

int vtp_foption(int argc, const char * argv[], vtp_options_t *options)
{
    char opt;
    opterr = 0;
#if __ORIG__
    while ((opt=getopt(argc, (char*const*)argv, "cstp:P:I:F:C:b:a:r:f:e:S:l:")) != -1){
        switch (opt){
            case 'c':
                /* We are in client mode */
                options->mode = CLIENT;
                break;
            case 's':
                /* We are in server mode */
                options->mode = SERVER;
                break;
            case 't':
                /* We are in test mode */
                options->mode = TEST;
                break;
            case 'p':
                /* Control port */
                options->ctrl_port = atoi(optarg);
                break;
            case 'P':
                /* Data port */
                options->data_port = atoi(optarg);
                break;
            case 'I':
                /* IP */
                my_inet_aton( optarg, &options->ip );
                break;
            case 'F':
                /* File name */
                assert( optarg );
                options->file_name = malloc(strlen(optarg)+1);
                assert( strncpy( options->file_name, optarg, strlen(optarg)) );
                break;
            case 'C':
                /* File name */
                assert( optarg );
                options->config_file_name = malloc(strlen(optarg)+1);
                assert( strncpy( options->config_file_name, optarg,
                                 strlen(optarg)) );
                break;
            case 'b':
                /* RTCP Bandwidth */
                options->rtcp_bw = atol(optarg);
                break;
            case 'a':
                /* Avg RTCP Bandwidth */
                options->avg_rtcp_size = atol(optarg);
                break;
            case 'r':
                /* SSRC */
                options->ssrc = atol(optarg);
                break;
            case 'f':
                /* Sampling frequency */
                options->sampling_frequency = atol(optarg);
                break;
            case 'e':
                /* Samples per packet */
                options->samples_per_packet = atol(optarg);
                break;
            case 'S':
                /* Bits per sample */
                options->bits_per_sample = atol(optarg);
                break;
            case 'l':
                /* Control buffer size */
                options->sz_ctrl_buf = atol(optarg);
                break;
            case '?':
                syslog(LOG_WARNING, "Unknown option %c\n", opt);
                exit(1);
        }
    }
#else
    char* xml_file_name=NULL;
    while ((opt=getopt(argc, (char*const*)argv, "F:")) != -1){
        switch (opt){
            case 'F':
                /* File name */
                assert( optarg );
                xml_file_name = malloc(strlen(optarg)+1);
                assert( strncpy( xml_file_name, optarg, strlen(optarg)) );
                break;
            case '?':
                syslog(LOG_WARNING, "Unknown option %c\n", opt);
                exit(1);
        }
    }
    if (xml_file_name) {
        xml_options_t o;
        if (xml_parse(&o, xml_file_name)!=0) {
            syslog(LOG_WARNING, "Unable to parse XML file.");
            return(0);
        }
        memcpy(options, &o.options, sizeof(vtp_options_t));
    } else {
        syslog(LOG_WARNING, "No XML file");
    }
#endif
    return(1);
}

extern int vtp_check_ctrl(int ctrl_connect_sock,
                          u_int8_t *ctrl_rcv_buf, int sz_ctrl_rcv_buf,
                          struct timeval *timeout)
{
    int ret = 0;
    int nfds = 0;
    fd_set readfds;

    assert(ctrl_rcv_buf);

    FD_ZERO(&readfds);
    FD_SET(ctrl_connect_sock, &readfds);
    nfds = ctrl_connect_sock + 1;

    /* Check control channel */
    if ( (ret = my_select(nfds, &readfds, NULL, NULL, timeout)) > 0 ) {
        if (FD_ISSET(ctrl_connect_sock, &readfds)) {
            ret = my_recv(ctrl_connect_sock, ctrl_rcv_buf, sz_ctrl_rcv_buf, 0);
#if __ORIG__
            ctrl_packet->version = ntohl(((u_int32_t*)ctrl_rcv_buf)[0]);
            ctrl_packet->pt = ntohl(((u_int32_t*)ctrl_rcv_buf)[1]);
            ctrl_packet->orig_timestamp = ntohl(((u_int32_t*)ctrl_rcv_buf)[2]);
            ctrl_packet->dest_timestamp = ntohl(((u_int32_t*)ctrl_rcv_buf)[3]);
#endif
            syslog(LOG_WARNING, "Rcvd control data");
        }
    }
    return(ret);
}

#ifdef COMMENT
int vtp_check_data(int data_sock, u_int8_t* data_rcv_buf, int sz_data_rcv_buf,
                   u_int8_t* data_buf, int sz_data_buf,
                   int* data_bytes_to_write,
                   struct timeval *timeout)
#else
int vtp_check_data(int data_sock, u_int8_t* data_rcv_buf, int sz_data_rcv_buf,
                   struct timeval *timeout)
#endif
{
    int ret = 0;
    int nfds = 0;
    fd_set readfds;
#ifdef COMMENT
    vtp_data_hdr_t data_packet;
    int i=0;
#endif

    assert( data_rcv_buf );
#ifdef COMMENT
    assert( data_buf );
    assert( data_bytes_to_write );
#endif

    FD_ZERO( &readfds );
    FD_SET( data_sock, &readfds );
    nfds = data_sock + 1;

    /* Check data channel */
    if ( (ret = my_select(nfds, &readfds, NULL, NULL, timeout)) > 0 ) {
        if (FD_ISSET(data_sock, &readfds)) {
            ret = my_recv(data_sock, data_rcv_buf, sz_data_rcv_buf, 0);
            syslog(LOG_WARNING, "Rcvd data: %d bytes received\n", ret);
#ifdef COMMENT
            data_packet.version = ntohl(((u_int32_t*)data_rcv_buf)[0]);
            data_packet.pt = ntohl(((u_int32_t*)data_rcv_buf)[1]);
            data_packet.seq = ntohl(((u_int32_t*)data_rcv_buf)[2]);
            data_packet.padding = ntohl(((u_int32_t*)data_rcv_buf)[3]);
            data_packet.data_length = ntohl(((u_int32_t*)data_rcv_buf)[4]);

            for (i=0; i<data_packet.data_length; i++)
            {
                if (i > sz_data_buf) {
                    syslog(LOG_WARNING, "Received data > data_buf.\n");
                    break;
                }
                if ( (20+i) < ret )
                    data_buf[i] = ntohl(data_rcv_buf[20+i]);
                else {
                    syslog(LOG_WARNING, "Partial data packet received.\n");
                    break;
                }
            }
            *data_bytes_to_write = i;
            syslog(LOG_WARNING, "Rcvd data: length = %d", data_packet.data_length+20);
#endif
        }
    }
    return(ret);
}

int vtp_send_ctrl(int ctrl_connect_sock,
                  u_int8_t* ctrl_snd_buf, int sz_ctrl_snd_buf,
                  vtp_pt_t type)
{
    int ret = 0;
    int bytes_to_send = 0;

    assert( ctrl_snd_buf );

    vtp_ctrl_hdr_t p;
    int i = 0;

    p.version = VTP_VERSION;
    p.pt = type;
    p.seq = 0;
    p.padding = 0;
    p.orig_timestamp = 0;
    p.dest_timestamp = 1;
    p.rtt = 0;
    p.packets_lost = 0;
    p.lost_seq_length = 0;
    p.lost_seq = NULL;

    ((u_int32_t*)ctrl_snd_buf)[0] = htonl(p.version);
    ((u_int32_t*)ctrl_snd_buf)[1] = htonl(p.pt);
    ((u_int32_t*)ctrl_snd_buf)[2] = htonl(p.seq);
    ((u_int32_t*)ctrl_snd_buf)[3] = htonl(p.seq);
    ((u_int32_t*)ctrl_snd_buf)[4] = htonl(p.orig_timestamp);
    ((u_int32_t*)ctrl_snd_buf)[5] = htonl(p.dest_timestamp);
    ((u_int32_t*)ctrl_snd_buf)[6] = htonl(p.rtt);
    ((u_int32_t*)ctrl_snd_buf)[7] = htonl(p.packets_lost);
    ((u_int32_t*)ctrl_snd_buf)[8] = htonl(p.lost_seq_length);

    for (i=0; i< p.lost_seq_length; i+=4)
    {
        if (!(i+36)<=sz_ctrl_snd_buf)
            break;
        if (p.lost_seq[i])
            ((u_int32_t*)ctrl_snd_buf)[9+i] = htonl(p.lost_seq[i]);
        else {
            syslog(LOG_WARNING, "No lost sequence numbers to send\n");
            break;
        }
    }
    bytes_to_send = (9+i)*4;
    if (!(bytes_to_send<=sz_ctrl_snd_buf)) {
        syslog(LOG_WARNING, "Packet larger than ctrl_snd_buf.\n");
        return(-1);
    }

    ret = my_send(ctrl_connect_sock, ctrl_snd_buf, bytes_to_send, 0);
    return(ret);
}

int vtp_send_data(int data_sock,
                  u_int8_t* hdr_buf,
                  int sz_hdr_buf,
                  u_int8_t* data_to_snd,
                  int sz_data_to_snd)
{
    int ret = 0;
    struct iovec iovectors[2];
    iovectors[0].iov_base = (void*)hdr_buf;
    iovectors[0].iov_len = sz_hdr_buf;
    iovectors[1].iov_base = (void*)data_to_snd;
    iovectors[1].iov_len = sz_data_to_snd;

    if ( ret += my_writev(data_sock, iovectors, 2) ) {
        syslog(LOG_WARNING, "Sent data packet\n");
    } else {
        syslog(LOG_WARNING, "Unable to send data packet\n");
    }
    return(ret);
}

int vtp_read_data(FILE* inf, u_int8_t* data_buf, int sz_data_buf)
{
    int ret = 0;
    assert( inf );
    assert( data_buf );

    if ( feof(inf) )
        return(0);
    if ( ferror(inf) ) {
        syslog(LOG_WARNING, "Error on input data file stream.\n");
        return(-1);
    }

    ret = fread(data_buf, 1, sz_data_buf, inf);
    return(ret);
}

int vtp_write_data(FILE* outf, u_int8_t* data_buf, int sz_data_buf)
{
    int ret = 0;
    assert( outf );
    assert( data_buf );

    if ( feof(outf) )
        return(0);
    if ( ferror(outf) ) {
        syslog(LOG_WARNING, "Error on output data file stream.\n");
        return(-1);
    }

    ret = fwrite(data_buf, 1, sz_data_buf, outf);
    return(ret);
}

int vtp_signal(int signo, void (*func)())
{
    struct sigaction act, oact;

    act.sa_handler = func;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (signo == SIGALRM) {
#ifdef SA_INTERRUPT
        act.sa_flags |= SA_INTERRUPT;	/* SunOS 4.x */
#endif
    } else {
        act.sa_flags |= SA_RESTART;		/* SVR4, 4.4BSD */
    }
    if (sigaction(signo, &act, &oact) < 0) {
        perror("sigaction :-(");
        return(-1);/* return (SIG_ERR); */
    }
    /* return(oact.sa_handler); */
    return(0);
}

