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
 * $Id: bmr_server.c 536 2010-07-26 16:07:30Z gbc $
 *
 * The bmr_buffer task which combines GRAB + PUSH into a single server.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "bmr_server.h"
#include "grab_task.h"
#include "push_task.h"

#include "buffer_server.h"

/*
 * SRVR public data
 */
BMRServer   bss;

/*
 * SRVR private data
 *
 * We'll process commands and responses through these buffers.
 * bss.cl_io is the implicit argument of bmr_command() -- it
 * cannot explictly passed as bmr_command() is of type Handler.
 * bss.cl_io is the index of the active client in the bss
 * client structures and the following message arrays:
 */
static char	    cl_msgs[BMR_MAX_CLIENT_SOCKETS][BMR_MAX_MESSAGE];

/*
 * This is invoked to handle a command from the incoming client
 * The return values is the number of bytes in a response.
 * 0 means no response
 * -1 implies an error (FIXME: error handling; log it? close it?).
 */
static ssize_t bmr_command(void)
{
    int	nb, rb = 0;
    if (bss.verb>3) fprintf(stderr, "bmr_command[%d]\n", bss.cl_io);
    if (bss.cl_io < 0 || bss.cl_io >= bss.clients) return(-1);
    cl_msgs[bss.cl_io][0] = 0;		    /* never to repeat */
    nb = read(bss.cl_sock[bss.cl_io], cl_msgs[bss.cl_io], BMR_MAX_MESSAGE);
    if (nb <= 0) {
	if (bss.verb>3) fprintf(stderr, "bmr_sub_client[%d]\n", bss.cl_io);
	bmr_sub_client(bss.cl_io+1);
	bmr_sub_client(bss.cl_io);
	return(0);
    }
    if (nb > 0) {
	if (bss.verb>3) fprintf(stderr, "bmr_response[%d]\n", bss.cl_io);
	rb = bmr_response(cl_msgs[bss.cl_io], cl_msgs[bss.cl_io+1]);
	cl_msgs[bss.cl_io][0] = 0;	    /* never to repeat */
	if (rb < 0) return(-2);
    }
    if (bss.verb>3) fprintf(stderr, "bmr_command[%d] %d %d\n",
	bss.cl_io, nb, rb);
    return(rb);
}

/*
 * This is invoked to send a response to a command (if one is available).
 * It returns the number of bytes written.
 * It returns negative on error (FIXME: error handling; log it? close it?).
 */
static ssize_t bmr_monitor(void)
{
    int nb = 0, len;
    if (bss.verb>3) fprintf(stderr, "bmr_monitor[%d]\n", bss.cl_io);
    if (bss.cl_io < 0 || bss.cl_io >= bss.clients) return(-1);
    len = strlen(cl_msgs[bss.cl_io]);
    if (len > 0) nb = write(bss.cl_sock[bss.cl_io], cl_msgs[bss.cl_io], len);
    cl_msgs[bss.cl_io][0] = 0;		    /* never to repeat */
    if (bss.verb>3) fprintf(stderr, "bmr_monitor %d %d > %d\n", nb, len,
	bss.cl_sock[bss.cl_io]);
    return(nb);
}

/*
 * Safe copy of internals.
 */
BMRServer *get_bmr_server_state(void)
{
    static BMRServer	mine;
    mine = bss;
    return(&mine);
}

/*
 * A brutal shutdown of the listener socket and any clients.
 */
static void bmr_server_yank(void)
{
    int	cc;
    if (bss.verb>0) fprintf(stderr, "bmr_server_yank(%d)\n", bss.cmd_sock);
    if (bss.cmd_sock >= 0) {
	if (shutdown(bss.cmd_sock, SHUT_RD)) perror("shutdown");
	close(bss.cmd_sock);
    }
    for (cc = 0; cc < bss.clients; cc++)
	if (bss.cl_sock[cc] >= 0) close(bss.cl_sock[cc]);
}

/*
 * Shut down the server (cmd_sock -> -1) and impose new parameters.
 * With the present dispatch loop, this is the end of the road.  ;-)
 */
int set_bmr_server_state(BMRServer *bs)
{
    static int	again = 0;
    int	cc;
    if (again++) bmr_server_yank();
    bss = *bs;
    bss.cmd_sock = -1;
    bss.clients = 0;
    for (cc = 0; cc < BMR_MAX_CLIENT_SOCKETS; cc++) {
	bss.cl_sock[cc] = -1;
	bss.cl_type[cc] = BMR_CLIENT_TYPE_NOT_DEF;
	bss.cl_func[cc] = 0;
    }
    if (bss.verb>0) fputs("set_bmr_server_state() done.\n", stderr);
    return(0);
}

/*
 * A debugging function.
 */
char *show_bmr_bits(fd_set *msk, char *buf, char hit)
{
    int	ndx;
    for (ndx = 0; ndx < bss.clients; ndx++)
	buf[ndx] = (bss.cl_sock[ndx] >= 0) && FD_ISSET(bss.cl_sock[ndx], msk)
		 ? hit : '-';
    buf[ndx] = 0;
    return(buf);
}

/*
 * Add a client, which typically has only input or output.
 * There is a potential race condition on the welcoming
 * message (bss.motd) if two write clients are added in
 * rapid succession, but that's not likely with typical
 * coding of the dispatch loop.
 */
int bmr_add_read_client(int ifd, int type, Handler **ioh, fd_set *rfds)
{
    static char buf[BMR_MAX_MESSAGE];
    if (ifd >= 0) {
	if (bss.clients >= bss.max_clients)
	    return(fputs("Too many clients\n", stderr));
	bss.cl_sock[bss.clients] = ifd;
	bss.cl_type[bss.clients] = type;
	bss.cl_func[bss.clients] = ioh;
	if (ifd >= bss.nfds) bss.nfds = ifd + 1;
	FD_SET(ifd, rfds);
	bss.clients++;
	if (bss.verb>0) fprintf(stderr,
	    "SRVR read client (%d) %s\n", ifd, show_bmr_bits(rfds, buf, 'R'));
    }
    return(0);
}
int bmr_add_write_client(int ofd, int type, Handler **ioh, fd_set *wfds)
{
    static char buf[BMR_MAX_MESSAGE];
    if (ofd >= 0) {
	if (bss.clients >= bss.max_clients)
	    return(fputs("Too many clients\n", stderr));
	bss.cl_sock[bss.clients] = ofd;
	bss.cl_type[bss.clients] = type;
	bss.cl_func[bss.clients] = ioh;
	if (ofd >= bss.nfds) bss.nfds = ofd + 1;
	FD_SET(ofd, wfds);
	if (bss.motd && (type == BMR_CLIENT_TYPE_MONITOR))
	    strncpy(cl_msgs[bss.cl_io = bss.clients],
		bss.motd, BMR_MAX_MESSAGE);
	bss.clients++;
	if (bss.verb>0) fprintf(stderr,
	    "SRVR write client (%d) %s\n", ofd, show_bmr_bits(wfds, buf, 'W'));
    }
    return(0);
}

/*
 * Some things to get started (as enabled):
 *  a command/monitor client on the stdin/stdout
 *  a listener for new clients
 *
 * Conventionally we'll add COMMAND/MONITORs in
 * adjacent pairs, which simplifies things a bit.
 *
 * The Handler pointers here are so that we have the
 * level of indirection requried by the add routines.
 */
int bmr_server_prep(fd_set *rfds, fd_set *wfds)
{
    static Handler *bmr_command_ptr = &bmr_command;
    static Handler *bmr_monitor_ptr = &bmr_monitor;
    static Handler *bmr_accept_ptr = &bmr_accept;
    if (bss.verb>0) fputs("SRVR prepping for buffer.\n", stderr);
    FD_ZERO(rfds);	    /* empty slate */
    FD_ZERO(wfds);	    /* empty slate */
    if (bss.std_input && (
	bmr_add_read_client(fileno(stdin), BMR_CLIENT_TYPE_COMMAND,
	    &bmr_command_ptr, rfds) ||
	bmr_add_write_client(fileno(stdout), BMR_CLIENT_TYPE_MONITOR,
	    &bmr_monitor_ptr, wfds)) )
	    return(fputs("Problem adding stdio/stdout client\n", stderr));
    if (bss.tcp_port &&
	((bss.cmd_sock =
	  bmr_listener(rfds,wfds,&bmr_command_ptr,&bmr_monitor_ptr))>0) && (
	bmr_add_read_client(bss.cmd_sock, BMR_CLIENT_TYPE_LISTEN,
	    &bmr_accept_ptr, rfds) ||
	bmr_add_write_client(fileno(stderr), BMR_CLIENT_TYPE_MONITOR,
	    &bmr_monitor_ptr, wfds)) )
	    return(fputs("Problem adding listener\n", stderr));
    if (bss.verb>0) fputs("SRVR completed for buffer.\n", stderr);
    return(0);
}

/*
 * eof
 */
