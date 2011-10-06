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
 * $Id: replay_server.c 592 2010-08-09 15:54:42Z gbc $
 *
 * The bmr_replay task which combines LOAD + PUSH into a single server.
 */

#include <errno.h>
#include <stdio.h>

#include "bmr_server.h"
#include "load_task.h"
#include "push_task.h"

#include "replay_server.h"

/*
 * Preparation to go into service to replay the packets.
 * This includes the general server startup and nothing else.
 */
static int replay_prep(fd_set *rfds, fd_set *wfds)
{
    LoadState	*lsp = get_load_state();
    int	xx = 0, lf;
    if (bss.verb>0) fputs("Starting replay_prep()\n", stderr);
    /* general server setup */
    xx += bmr_server_prep(rfds, wfds);
    /* UDP PUSH setup */
    xx += bmr_add_write_client( *current_push_sock_ptr,
	BMR_CLIENT_TYPE_UDP_OUT, current_push_send_ptr, wfds);
    /* file LOAD setup */
    for (lf = 0; lf < lsp->num_files; lf++)
	xx += bmr_add_read_client( lsp->filed[lf],
	    BMR_CLIENT_TYPE_PKTLOAD, current_load_reader_ptr, rfds);
    if (bss.verb>0) fprintf(stderr, "done with replay_prep(%d)\n", xx);
    return(xx);
}

/*
 * Take a short nap.
 */
static void nap_time(void)
{
    struct timeval punt;
    punt.tv_sec = 0;
    punt.tv_usec = 250000;
    select (0, NULL, NULL, NULL, &punt);
}

/*
 * MACROS for post-select processing introduced for readability.
 *
 * BMR_SELECT_ERROR() handles the select-was-interrupted issue.
 * BMR_RECORD_READS() handles sockets that need a read.
 * BMR_RECORD_WRITZ() handles sockets that need a write.
 * BMR_RECORD_STATE() toggles the selection of fds based on
 *                    the internal state machines of LOAD & PUSH.
 *
 * VFD_ISSET(N,F) is required since we flag disconnected clients
 * with an illegal file descriptor (-1, typically).
 *
 * The read/write macros could be augmented to give priority to the
 * packet reading/pushing operations at the expense of command & monitoring.
 *
 * The BMR_DEBUG_*() Macros are for development only.
 */
#define BMR_SELECT_ERROR(ND,RV) do {				\
    if (ND < 0) {						\
	if (errno == EINTR) { errno = 0; continue; }		\
	perror("select");					\
	RV = fputs("bmr_*_dispatch\n", stderr);			\
	ok = 0;							\
    }								\
} while(0)

#define VFD_ISSET(N,F) (N>=0 && FD_ISSET(N,F))

#define BMR_RECORD_READS(ND,SRD,AWD) do {			\
    int ndx, nbs;						\
    for (ndx = 0; ND>0 && ndx<bss.clients; ndx++) {		\
	if (VFD_ISSET(bss.cl_sock[ndx], &SRD)) {		\
	    ND --;						\
	    *current_load_fd_ptr = bss.cl_sock[ndx];		\
	    nbs = (**(bss.cl_func[bss.cl_io = ndx]))();		\
	    if (nbs < 0) { perror("BMR_RECORD_READS"); ok = 0; }\
	    if (nbs == 0) { nap_time(); } else { dbg = 6; }	\
	    if (bss.cl_type[ndx] == BMR_CLIENT_TYPE_COMMAND)	\
		FD_SET(bss.cl_sock[ndx+1], &AWD);		\
	}							\
    }								\
} while(0)

#define BMR_RECORD_WRITZ(ND,SWD,AWD) do {			\
    int ndx, nbs;						\
    for (ndx = 0; ND>0 && ndx<bss.clients; ndx++) {		\
	if (VFD_ISSET(bss.cl_sock[ndx], &SWD)) {		\
	    ND --;						\
	    nbs = (**(bss.cl_func[bss.cl_io = ndx]))();		\
	    if (nbs < 0) { perror("BMR_RECORD_WRITZ"); ok = 0;}	\
	    if (nbs == 0) { nap_time(); } else { dbg = 6; }	\
	    if (bss.cl_type[ndx] == BMR_CLIENT_TYPE_MONITOR)	\
		FD_CLR(bss.cl_sock[ndx], &AWD);			\
	}							\
    }								\
} while(0)

#define BMR_RECORD_STATE(ARD,AWD,SRD,SWD) do {			\
    sel_to = bss.sel_to;					\
    SRD = ARD;							\
    if (check_load_state()) {					\
	LoadState	*lsp = get_load_state();		\
	int		lf;					\
	for (lf = 0; lf < lsp->num_files; lf++)			\
	    FD_CLR(lsp->filed[lf], &SRD);			\
    }								\
    SWD = AWD;							\
    if (check_push_state()) {					\
	FD_CLR(*current_push_sock_ptr, &SWD);			\
    }								\
} while(0)


#define BMR_DEBUG_SELECT(RD,WD,R,W,CC,NN) if (bdp && dbg) do {	\
    int ndx;							\
    TimeSpec now;						\
    (void)clock_gettime(CLOCK_REALTIME, &now);			\
    fprintf(bdp, PRI_TS " T/O " PRI_TV,				\
	now.tv_sec, now.tv_nsec, sel_to.tv_sec,sel_to.tv_usec);	\
    for (fputs("  Read:",bdp),ndx=0; ndx<bss.clients; ndx++)	\
	fputs(VFD_ISSET(bss.cl_sock[ndx],&RD)?R:"-",bdp);	\
    for (fputs(" Write:",bdp),ndx=0; ndx<bss.clients; ndx++)	\
	fputs(VFD_ISSET(bss.cl_sock[ndx],&WD)?W:"-",bdp);	\
    for (fputs("    fd:",bdp),ndx=0; ndx<bss.clients; ndx++)	\
	fprintf(bdp, "%d", bss.cl_sock[ndx]);			\
    fprintf(bdp, " %d %d %d\n", CC, NN, dbg--);			\
    fflush(bdp);						\
} while(0)

#define BMR_DEBUG_SERVER() if (bdp && (bss.verb>5)) do {	\
    static char buf[BMR_MAX_MESSAGE];				\
    desc_bmr_server_state(&bss, buf);				\
    fputs(buf, bdp);						\
    fflush(bdp);						\
} while (0)

#define LOAD_DEBUG() if (bdp && (bss.verb>5)) do {		\
    static char buf[BMR_MAX_MESSAGE];				\
    desc_load_info(get_load_info(), buf);			\
    fputs(buf, bdp);						\
    fflush(bdp);						\
} while (0)

#define PUSH_DEBUG() if (bdp && (bss.verb>5)) do {		\
    static char buf[BMR_MAX_MESSAGE];				\
    desc_push_info(get_push_info(), buf);			\
    fputs(buf, bdp);						\
    fflush(bdp);						\
} while (0)

/*
 * Dispatch server for LOAD + PUSH activities.
 *
 * We should be ready to respond to:
 *  a command on stdin
 *    (but only if bss.std_input)
 *  a new client on the listener socket
 *    (only if bss.tcp_port >= 0 and bss.cmd_sock >= 0)
 *  commands from the client(s) (if bss.clients > 0)
 * and one of:
 *  an outgoing UDP data packet (as are available)
 *  data packets read from one or more files (as can be read)
 *
 * MACROS are used here to insert large blocks of code both for
 * readability and to allow code reuse among similar dispatch loops.
 * These will issue a break or a continue as appropriate.
 * In addition, they adjust the active descriptors in rfds and wfds.
 */
int bmr_replay_dispatch(void)
{
    fd_set	all_r, all_w, sel_r, sel_w;
    TimeVal	sel_to;
    int		ofds, nfds = 0, rv = 0;
    int		lcnt = 0, ok = 1, dbg = 6;
    FILE	*bdp = 0;
    
    if (bss.dfile) {
	bdp = fopen(bss.dfile, "w");
	if (!bdp) return(perror("dispatch"),1);
    }

    sel_to.tv_sec = sel_to.tv_usec = 0;
    if (replay_prep(&all_r, &all_w)) return(1);

    BMR_DEBUG_SELECT(all_r,all_w,"+","+", lcnt, nfds);

    if (bss.verb>0) fprintf(stderr, "Dispatch Loop Count %d\n", lcnt);
    while (ok) {
	/* toggles descriptors on/off */
	BMR_RECORD_STATE(all_r, all_w, sel_r, sel_w);
	BMR_DEBUG_SELECT(sel_r,sel_w,"R","W", lcnt, nfds);

	ofds =
	nfds = select(bss.nfds, &sel_r, &sel_w, NULL, &sel_to);

	/* respond to the nfds descriptors */
	BMR_SELECT_ERROR(nfds, rv);
	BMR_RECORD_READS(nfds, sel_r, all_w);
	BMR_RECORD_WRITZ(nfds, sel_w, all_w);

	BMR_DEBUG_SERVER();
	LOAD_DEBUG();
	PUSH_DEBUG();

	BMR_DEBUG_SELECT(sel_r,sel_w,"r","w", lcnt, ofds);
	lcnt ++;
    }

    if (bss.verb>0) fprintf(stderr, "Dispatch Loop Count %d\n", lcnt);

    if (bdp) fclose(bdp);
    rv += set_bmr_server_state(&bss);
    return(rv);
}

/*
 * eof
 */
