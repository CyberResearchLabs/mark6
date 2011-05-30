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
 * $Source: /usr/local/cvsroot/mit/vtp/src/mysock.c,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/10 00:32:49 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: mysock.c,v $
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
 * Revision 1.7  2003/09/09 05:57:41  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.6  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.5  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.4  2003/09/03 12:54:01  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */

#include <sys/types.h>
#include <sys/uio.h>
#include <errno.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "mysock.h"

/* Sockets wrapper */
int my_socket(int domain, int type, int protocol)
{
    int retfd = 0;
    if ((retfd = socket(domain, type, protocol)) == -1) {
        perror("socket");
        exit(1);
    }
    return(retfd);
}

int my_setsockopt(int s, int level, int optname, const void *optval, int optlen)
{
    int ret = 0;
    if ((ret = setsockopt(s, level, optname, optval, optlen)) == -1) {
        perror("setsockopt");
        exit(1);
    }
    return(ret);
}

int my_bind(int s, const struct sockaddr *name, int namelen)
{
    int ret = 0;
    if ((ret = bind(s, name, namelen)) == -1) {
        perror("bind");
        exit(1);
    }
    return(ret);
}

int my_listen(int s, int backlog)
{
    int ret = 0;
    if ((ret = listen(s, backlog)) == -1) {
        perror("listen");
        exit(1);
    }
    return(ret);
}

int my_accept(int s, struct sockaddr *addr, int *addrlen)
{
    int retsock = 0;
    if ((retsock = accept(s, addr, addrlen)) == -1) {
        perror("accept");
        exit(1);
    }
    return(retsock);
}

int my_connect(int s, const struct sockaddr *name, int namelen)
{
    int ret = 0;
    if ((ret = connect(s, name, namelen)) == -1) {
        perror("connect");
        exit(1);
    }
    return(ret);
}

int my_recv(int s, void *buf, size_t len, int flags)
{
    int ret = 0;
    if ((ret = recv(s, buf, len, flags)) == -1) {
        perror("recv");
        exit(1);
    }
    return(ret);
}

int my_send(int s, const void *msg, size_t len, int flags)
{
    int ret = 0;
    if ((ret = send(s, msg, len, flags)) == -1) {
        perror("send");
        exit(1);
    }
    return(ret);
}

int my_sendto(int s, const void *msg, size_t len, int flags, const struct sockaddr *to, int tolen)
{
    int ret = 0;
    if ((ret = sendto(s, msg, len, flags, to, tolen)) == -1) {
        perror("sendto");
        exit(1);
    }
    return(ret);
}

int my_recvfrom(int s, void *buf, size_t len, int flags, struct sockaddr *from, int *fromlen)
{
    int ret = 0;
    if ((ret = recvfrom(s, buf, len, flags, from, fromlen)) == -1) {
        perror("recvfrom");
        exit(1);
    }
    return(ret);
}

int my_inet_aton(const char* cp, struct in_addr *pin)
{
    int ret = 0;
    if ((ret = inet_aton(cp, pin)) == 0) {
        perror("inet_aton");
        exit(1);
    }
    return(ret);
}

int my_select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout)
{
    int ret = 0;
    if ((ret = select(nfds, readfds, writefds, exceptfds, timeout)) == -1) {
        perror("select");
        exit(1);
    }
    return(ret);
}

int my_readv(int filedes, const struct iovec *iov, int iovcnt)
{
    int ret=0;
    if ((ret=readv(filedes, iov, iovcnt)) == -1) {
        perror("readv");
        exit(1);
    }
    return(ret);
}

int my_writev(int filedes, const struct iovec *iov, int iovcnt)
{
    int ret=0;
    if ((ret=writev(filedes, iov, iovcnt)) == -1) {
        perror("writev");
        exit(1);
    }
    return(ret);
}



