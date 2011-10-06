/*
 * Created by Geoff Crewe.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * $Id: bmr_listener.c 526 2010-07-23 17:00:55Z gbc $
 *
 * Listener code for server.
 */

#include <errno.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "bmr_server.h"

static fd_set	*rfds_ptr = 0, *wfds_ptr = 0;
static Handler	**cmd_pptr = 0, **mon_pptr = 0;

static ssize_t just_in_case(void)
{
    fputs("Ghost\n", stderr);
    return(0);
}

/*
 * Routines to clean up after an attached client.
 * It matters little whether the client was read or write.
 */
void bmr_sub_client(int client)
{
    static Handler *just_in_case_ptr = &just_in_case;
    if (bss.cl_type[client] == BMR_CLIENT_TYPE_COMMAND)
	FD_CLR(bss.cl_sock[client], rfds_ptr);
    if (bss.cl_type[client] == BMR_CLIENT_TYPE_MONITOR)
	FD_CLR(bss.cl_sock[client], wfds_ptr);
    if (bss.cl_sock[client] == bss.nfds) bss.nfds --;
    close(bss.cl_sock[client]);
    if (errno>0) {
	perror("bmr_sub_client");
	errno = 0;
    }
    bss.cl_sock[client] = -1;
    bss.cl_type[client] = BMR_CLIENT_TYPE_NOT_DEF;
    bss.cl_func[client] = &just_in_case_ptr;
    if (client+1 == bss.clients) bss.clients--;
    if (bss.verb>0) fprintf(stderr, "Purged client slot %d\n", client);
}

/*
 * Handler for SIGPIPE on client close
 */
static void bmr_sigpipe_handler(int signal)
{
    /* doing nothing is equivalent to SIG_IGN */
    if (bss.verb>0) fprintf(stderr, "Ignoring SIGPIPE\n");
}

/*
 * Handler for client acceptance.
 * Normally connects accepted clients up to bss structure.
 *
 * It is not clear why a dup of cls is required, but writing
 * to cls apparently goes off into oblivion.  (FIXME: bug?)
 */
ssize_t bmr_accept(void)
{
    struct sockaddr from;
    struct in_addr  addr;
    unsigned int    len = sizeof(from);
    int		    cls, dls;

    if (bss.verb>0) fputs("Accepting client...\n", stderr);

    if (!rfds_ptr || !wfds_ptr || !cmd_pptr || !mon_pptr)
	return(fputs("Unable to accept clients\n", stderr),-1);

    /* accept the client */
    cls = accept(bss.cmd_sock, &from, &len);
    if (cls < 0) return(perror("accept"),-2);

    /* dup it for logical separation of read and write */
    dls = dup(cls);
    if (dls < 0) return(perror("dup"),-3);

    /* connect the client */
    addr = ((struct sockaddr_in *)&from)->sin_addr;
    bmr_add_read_client(cls, BMR_CLIENT_TYPE_COMMAND, cmd_pptr, rfds_ptr);
    bmr_add_write_client(dls, BMR_CLIENT_TYPE_MONITOR, mon_pptr, wfds_ptr);

    if (bss.verb>0) fprintf(stderr, "Accepted client %s\n", inet_ntoa(addr));
    return(0);
}

/*
 * Routine to setup tcp server socket and related support.
 * Returns server socket on success, negative on error.
 */
int bmr_listener(fd_set *rfds, fd_set *wfds, Handler **cmd, Handler **mon)
{
    struct sockaddr_in	sa;
    int			opt = 1, sock;
    struct linger	lo;

    if (bss.verb>0) fprintf(stderr, "SRVR TCP port %d\n", bss.tcp_port);
    signal(SIGPIPE, bmr_sigpipe_handler);

    memset(&sa, 0, sizeof(sa));
    sa.sin_family = PF_INET;		/* internet protocol family */
    sa.sin_port = htons(bss.tcp_port);
    sa.sin_addr.s_addr = INADDR_ANY;	/* talk to anyone who calls */

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) return(perror("bmr_listener_socket"),-1);

    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt)))
	return(perror("bmr_listener_reuseaddr"),-2);

    lo.l_onoff = 0;
    lo.l_linger = 0;
    if (setsockopt(sock, SOL_SOCKET, SO_LINGER, (char *)&lo, sizeof(lo)))
	return(perror("bmr_listener_linger"),-3);

    if (bind(sock, (struct sockaddr *)&sa, sizeof(sa)))
	return(perror("bmr_listener_bind"),-4);

    if (listen(sock, 5))
	return(perror("bmr_listener_listen"),-5);

    /* save these later in bmr_accept() */
    rfds_ptr = rfds;
    wfds_ptr = wfds;
    cmd_pptr = cmd;
    mon_pptr = mon;
    if (bss.verb>0) fprintf(stderr, "SRVR listening on %d\n", sock);
    return(sock);
}

/*
 * eof
 */
