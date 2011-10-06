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
 * $Id: push_task.c 984 2011-02-08 17:02:31Z gbc $
 *
 * Implementation of the PUSH task.
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <netdb.h>
#include <arpa/inet.h>

#include "bmr_util.h"
#if BMR_SERVER == BMR_SERVER_BUFFER
# include "grab_task.h"
#elif BMR_SERVER == BMR_SERVER_REPLAY
# include "load_task.h"
#else /* BMR_SERVER */
# error "Unsupported BMR_SERVER TYPE"
#endif /* BMR_SERVER */
# include "push_task.h"

#define USE_CONNECT 0

/*
 * PUSH private data
 */
static PushState    pss;
static PushInfo     pis;

/*
 * PUSH Public (read-only) Data
 */
int const * const   current_push_sock_ptr = &pss.push_sock;
Sender **           current_push_send_ptr = &pss.push_send;

/*
 * Check if sending is worthwhile.
 * Returns 0 if calling send() is a good idea.
 ****
 * We have nothing to send if:
 *  the read pointer points to the write pointer
 *   (nothing valid to read)
 *  the read pointer points to an invalid sequence number/packet
 *   (implies source hasn't written here yet)
 *  the read pointer points to a number less than our last
 *   (implies something is wrong--corrective action?)
 *
 * Figure out if it is time to send.
 * Returns 0 when it is time to send.
 * Comparison is between now and pss.lastsnd, noting that
 * pss.pktwait is guaranteed to be less than one second.
 *
 * Depending on how much time remains we may sleep a bit here.
 * If pss.overlap != 0, sleeping is forbidden (otherwise packets drop).
 * If pss.overlap == 0, we sleep briefly and return 1.
 *
 * The logic is horribly contorted to avoid unnecessary calculations.
 */
static int must_not_send(void)
{
    struct timespec now, del;

    /* for is-it-time-to-send checks */
    (void)clock_gettime(CLOCK_REALTIME, &now);

    /* something-to-send checks */
#if BMR_SERVER == BMR_SERVER_BUFFER
    if (pss.mem_read == *current_grab_write_ptr) return(pis.tosn.rw++,1);
#elif BMR_SERVER == BMR_SERVER_REPLAY
    if (pss.mem_read == *current_load_write_ptr) return(pis.tosn.rw++,1);
#else /* BMR_SERVER */
# error "Unsupported BMR_SERVER TYPE"
#endif /* BMR_SERVER */
    if (pss.mem_type == BMR_MEM_SEQN &&
        *(int64_t*)pss.mem_read == BMR_SEQN_INVALID) return(pis.tosn.inv++,2);
    if (pss.mem_type == BMR_MEM_EDVS &&
        *(int64_t*)pss.mem_read == BMR_PKT_NOTVALID) return(pis.tosn.inv++,2);
    if (*(int64_t*)pss.mem_read <= pss.last_seqn) return(pis.tosn.rl++,3);

    /* this is the most common case */
    del.tv_nsec = now.tv_nsec - pss.lastsnd.tv_nsec;
    if (del.tv_nsec > pss.pktwait.tv_nsec) return(pss.lastsnd = now,0);

    /* once a second, we have a carry */
    del.tv_sec = now.tv_sec - pss.lastsnd.tv_sec;
    if (del.tv_sec == 1) del.tv_nsec += PUSH_ONENANOSECOND;
    if (del.tv_nsec > pss.pktwait.tv_nsec) return(pss.lastsnd = now,0);

    /* seriously overdue */
    if (del.tv_sec > 1) return(pss.lastsnd = now,0);
    /* del.tv_nsec <= pss.pktwait.tv_nsec */

    /* do the sleep ourselves */
    now.tv_nsec += del.tv_nsec;
    if (now.tv_nsec > PUSH_ONENANOSECOND) {
	now.tv_sec += 1;
	now.tv_nsec -= PUSH_ONENANOSECOND;
    }
    (void)clock_nanosleep(CLOCK_REALTIME, TIMER_ABSTIME, &now, 0);
    return(pis.tosn.slp++,pss.lastsnd = now,0);
}

/*
 * This does the real work--it sends the next packet,
 * and afterwards moves forward (no matter what).
 */
static ssize_t send_seqn(void)
{
    int	ns;

    pis.sending.called ++;
    if (must_not_send()) return(0);

    pss.last_seqn = *(int64_t *)pss.mem_read;
#if USE_CONNECT
    ns = send(pss.push_sock, pss.mem_read + sizeof(int64_t),
	pss.push_size, pss.flags);
#else /* USE_CONNECT */
    ns = sendto(pss.push_sock, pss.mem_read + sizeof(int64_t),
	pss.push_size, pss.flags,
	(struct sockaddr *)&pss.dname, sizeof(struct sockaddr_in));
#endif /* USE_CONNECT */
    if (errno != 0) { pis.sending.syserr ++; errno = 0; }
    /* update counters */
    if (ns == pss.push_size) {
	pis.sending.bytes += ns;
	pis.sending.pkts ++;
    } else {
	pis.sending.sizerr ++;
    }
    /* advance to the next packet */
    pss.mem_read += pss.mem_chunk;
    if (pss.mem_read - pss.mem_start >= pss.mem_size)
	pss.mem_read = pss.mem_start;
    return(ns);
}

/*
 * Same as send_seqn() but with embedded seqn
 */
static ssize_t send_edvs(void)
{
    int	ns;

    pis.sending.called ++;
    if (must_not_send()) return(0);

    pss.last_seqn = BMR_SEQN_UNPACK( ((int64_t *)pss.mem_read)[2] );
#if USE_CONNECT
    ns = send(pss.push_sock, pss.mem_read, pss.push_size, pss.flags);
#else /* USE_CONNECT */
    ns = sendto(pss.push_sock, pss.mem_read, pss.push_size, pss.flags,
	(struct sockaddr *)&pss.dname, sizeof(struct sockaddr_in));
#endif /* USE_CONNECT */
    if (errno != 0) { pis.sending.syserr ++; errno = 0; }
    /* update counters */
    if (ns == pss.push_size) {
	pis.sending.bytes += ns;
	pis.sending.pkts ++;
    } else {
	pis.sending.sizerr ++;
    }
    /* advance to the next packet */
    pss.mem_read += pss.mem_chunk;
    if (pss.mem_read - pss.mem_start >= pss.mem_size)
	pss.mem_read = pss.mem_start;
    return(ns);
}

/*
 * This is a no-op.
 */
static ssize_t send_idler(void)
{
    pis.idling.called ++;
    return(0);
}

/*
 * For the descriptive tasks, this is more useful than a pointer.
 */
char *push_sender_name(void *send)
{
    if (send == send_seqn) return("seqn");
    if (send == send_edvs) return("edvs");
    if (send == send_idler) return("idler");
    return("fault");
}

/*
 * Capture the exact times of entry/exit from sending mode.
 * On entry, capture the time in pis.sending_entry,
 * on exit, increase the total in pis.sending_time.
 */
static void push_sending_timing(int entry)
{
    TimeSpec	exit_time;
    if (entry) {
	clock_gettime(CLOCK_REALTIME, &pis.sending_entry);
    } else if (pis.sending_entry.tv_sec > 0) {
	clock_gettime(CLOCK_REALTIME, &exit_time);
	time_increment(&pis.sending_time,
	    time_decrement(&exit_time, &pis.sending_entry));
	pis.sending_entry.tv_sec = 0;	/* accumulate once! */
    }
}

/*
 * Assign the handler based on the action.
 */
static void set_send_action(int action, PushState *ps)
{
    switch (action) {
    case PUSH_ACT_IDLE:
	if (ps->verb>0 && action != ps->action)
	    fputs("Idle pusher\n", stderr);
	ps->push_send = send_idler;
	break;
    case PUSH_ACT_INIT:
	if (ps->verb>0) fputs("Init pusher\n", stderr);
	ps->push_send = send_idler;
	break;
    case PUSH_ACT_NONE:
	if (ps->verb>0) fputs("None pusher\n", stderr);
	ps->push_send = send_idler;
	push_sending_timing(0);
	break;
    case PUSH_ACT_SEND:
	if (ps->mem_type == BMR_MEM_SEQN) {
	    if (ps->verb>0) fputs("Send pusher\n", stderr);
	    ps->push_send = send_seqn;
	} else {
	    if (ps->verb>0) fputs("Sent pusher\n", stderr);
	    ps->push_send = send_edvs;
	}
	push_sending_timing(1);
	break;
    default:
	if (ps->verb>0) fprintf(stderr,
	    "Illegal sender action %d\n", action);
	action = PUSH_ACT_IDLE;
    }
    ps->action = action;
}

/*
 * May wish to impose some checking
 * FIXME -- more elaborate push-net checking?
 * FIXME -- do we have the right sendto() flags?
 */
static int chk_push_net(PushState *ps)
{
    double  gbps = (8e-9 * ps->push_size * ps->push_rate);
    if (ps->push_size < 64 || ps->push_size > 9000)
	return(fprintf(stderr, "Illegal push UDP size %d\n", ps->push_size));
    if (ps->push_rate <= 1.0)
	return(fprintf(stderr, "Illegal push pkt rate %ld\n", ps->push_rate));
    if (ps->push_rate < ps->rate_lim)
	return(fprintf(stderr, "Push rate %ld < %ld pkts/s required\n",
	    ps->push_rate, ps->rate_lim));
    if (gbps > 10.0)
	return(fprintf(stderr, "Illegal push bit rate %g\n", gbps));
    ps->flags = MSG_DONTWAIT;
    if (ps->verb>0) fprintf(stderr, "Target Push rate is %g (Gbps)\n", gbps);
    return(0);
}

/*
 * Set up the network infrastructure.  Failure leaves ps->push_sock < 0
 * FIXME: Still need to add the TCP flavor of pushing.
 */
void push_network_prep(PushState *ps)
{
    struct hostent      *hostenp;
    struct in_addr      addr;
    char		*peer;

    if (ps->verb>0) fputs("Entering push_network_prep\n", stderr);
    ps->push_sock = -1;
    if (chk_push_net(ps)) return;
    if (ps->verb>0) fprintf(stderr,
	"Prepping Host %s : %d\n", ps->push_addr, ps->push_port);

    /* preparation for the eventual sendto */
    ps->dname.sin_family = AF_INET;
    ps->dname.sin_port = htons(ps->push_port);
    ps->dname.sin_addr.s_addr = (hostenp = gethostbyname(ps->push_addr))
	? *((in_addr_t *) (hostenp->h_addr_list[0]))
	: inet_addr(ps->push_addr);
    if (ps->dname.sin_addr.s_addr == INADDR_NONE) {
	fprintf(stderr, "Host:port %s:%d is unintelligible.\n",
	    ps->push_addr, ps->push_port);
	return;
    }

    /* get a socket */
    if (ps->push_type == PUSH_PKT_TYPE_UDP)
	ps->push_sock = socket(PF_INET, SOCK_DGRAM, 0);
    else /* FIXME: TCP flavor */
	ps->push_sock = -1;
    if (ps->push_sock < 0) { perror("socket"); return; }

    /* provide a diagnostic */
    addr.s_addr = ps->dname.sin_addr.s_addr;
    peer = inet_ntoa(addr);
    if (ps->verb>0) fprintf(stderr,
	"PUSHing Packets to %s:%d (%s:%d)\n", 
                ps->push_addr, ps->push_port,
		peer ? peer : "dunno", ntohs(ps->dname.sin_port));
#if USE_CONNECT
    /* connect the socket */
    if (connect(ps->push_sock,
	(struct sockaddr *)&ps->dname, sizeof(ps->dname))) {
	perror("connect");
	close(ps->push_sock);
	ps->push_sock = -1;
	return;
    }
#else /* USE_CONNECT */
#endif /* USE_CONNECT */
    if (ps->verb>1) fprintf(stderr, "PUSH socket %d\n", ps->push_sock);
}

/*
 * Back to initial state--not talking to nobody, nohow.
 */
void push_network_yank(void)
{
    static int again = 0;
    if (pss.verb>0) fputs("Shut down PUSH socket.\n", stderr);
    if (again++ && (pss.push_sock >= 0)) close(pss.push_sock);
    pss.push_sock = -1;
    pss.push_type = PUSH_PKT_TYPE_OFF;
    memset(&pss.dname, 0, sizeof(pss.dname));
    set_send_action(PUSH_ACT_IDLE, &pss);
}

/*
 * Set up the timing....  push_rate > 1.0 so tv_sec == 0
 * and we just need to worry about the ns part.
 * FIXME: push_rate needs to fit into grab cycle.
 * FIXME: push_timing makes no allowance for overhead.
 */
static int push_timing_prep(PushState *ps)
{
    struct timespec res;
    ps->pktwait.tv_sec = 0;
    ps->pktwait.tv_nsec = floor(1e9 / (double)ps->push_rate);
    if (ps->pktwait.tv_nsec > PUSH_ONENANOSECOND)
	return(fputs("Push rate < 1 packet/sec not allowed\n", stderr));
    if (clock_getres(CLOCK_REALTIME, &res))
	return(fputs("Unable to get CLOCK_REALTIME resolution\n", stderr));
    ps->res_ns = res.tv_nsec;
    if (clock_gettime(CLOCK_REALTIME, &ps->lastsnd))
	return(fputs("Unable to get CLOCK_REALTIME time\n", stderr));
    return(0);
}

/*
 * Check the time and, as required, switch between the
 * various (sending/faking) states (if not IDLE).  The
 * rep counter decrements after the NONE state, and
 * we shift to the idle state when it hits zero.
 *
 * We care most about performance in the SEND state
 * (which actually has to push packets).
 *
 * INIT -> NONE -> SEND -> NONE ( -> IDLE finally )
 *
 * The active/passive logic mirrors that of GRAB.  However, if overlap
 * is allowed, we will arrange things so that we never fall passive.
 *
 * Return 0 indicates we want to send for packets,
 * Return 1 indicates the packet socket should be ignored.
 *
 * FIXME: overlap not yet coded....
 */
#if BMR_SERVER == BMR_SERVER_BUFFER
static void advance_push_cycle(void)
{
    GrabState	*gsp = get_grab_state();
    pss.push_to_us = pss.pktwait.tv_nsec / 2000;
    pss.push_period = gsp->grab_period;
    pss.push_active = gsp->grab_start + gsp->grab_secs;
    pss.push_passive = gsp->grab_start + gsp->grab_period;
    pss.push_reps = gsp->grab_cycles;
}
#elif BMR_SERVER == BMR_SERVER_REPLAY
static void advance_push_cycle(void)
{
    LoadState	*lsp = get_load_state();
    pss.push_to_us = pss.pktwait.tv_nsec / 2000;
    pss.push_period = lsp->load_period;
    pss.push_active = lsp->load_start + lsp->load_secs;
    pss.push_passive = lsp->load_start + lsp->load_period;
    pss.push_reps = lsp->load_cycles;
}
#else /* BMR_SERVER */
# error "Unsupported BMR_SERVER TYPE"
#endif /* BMR_SERVER */
int check_push_state(void)
{
    time_t	now = time(0);
    switch (pss.action) {

    case PUSH_ACT_SEND:
	if (now < pss.push_passive) return(0);
	pss.push_passive += pss.push_period;
	set_send_action(PUSH_ACT_NONE, &pss);
	return(1);

    case PUSH_ACT_NONE:
	if (now < pss.push_active) return(1);
	pss.push_active += pss.push_period;
	if (pss.push_reps-- > 0) {
	    set_send_action(PUSH_ACT_SEND, &pss);
	    return(0);
	}
	/* fall through to IDLE */
    default:
    case PUSH_ACT_IDLE:
	set_send_action(PUSH_ACT_IDLE, &pss);
	break;

    case PUSH_ACT_INIT:
	advance_push_cycle();
	set_send_action(PUSH_ACT_NONE, &pss);
	break;

    }
    return(1);
}

#if BMR_SERVER == BMR_SERVER_BUFFER
/*
 * When GRAB is our source....
 * FIXME: overlap will flush any captured, but not delivered data; ok?
 * FIXME: this rate limit assumes no sending during flush (not true).
 */
static int push_buffer_prep(PushState *ps)
{
    GrabState	*gsp = get_grab_state();
    if (ps->source != PUSH_SOURCE_GRAB)
	return(fprintf(stderr, "Illegal source %d\n", ps->source));
    ps->push_size = gsp->udp_size;
    ps->mem_start = gsp->mem_start;
    if (!ps->mem_start) return(fputs("No Push Buffer\n", stderr));
    ps->mem_size  = gsp->mem_size;
    ps->mem_type  = gsp->mem_type;
    ps->mem_chunk = gsp->mem_chunk;
    ps->last_seqn = gsp->next_seqn - 1;
    if (ps->overlap) {
	ps->mem_read = gsp->mem_write;
	ps->rate_lim = gsp->udp_rate * gsp->grab_secs
		     / ( gsp->grab_period );
    } else {
	ps->mem_read = gsp->mem_write;
	ps->rate_lim = gsp->udp_rate * gsp->grab_secs
		     / ( gsp->grab_idle - gsp->grab_flush );
    }
    return(0);
}
#elif BMR_SERVER == BMR_SERVER_REPLAY
/*
 * When LOAD is our source....FIXME
 * FIXME: overlap?
 */
static int push_buffer_prep(PushState *ps)
{
    LoadState	*lsp = get_load_state();
    if (ps->source != PUSH_SOURCE_LOAD)
	return(fprintf(stderr, "Illegal source %d\n", ps->source));
    ps->push_size = lsp->load_size;
    ps->mem_start = lsp->mem_start;
    if (!ps->mem_start) return(fputs("No Push Buffer\n", stderr));
    ps->mem_size  = lsp->mem_size;
    ps->mem_type  = lsp->mem_type;
    ps->mem_chunk = lsp->mem_chunk;
    ps->last_seqn = lsp->next_seqn - 1;
    if (ps->overlap) {
	ps->mem_read = lsp->mem_write;
	ps->rate_lim = lsp->load_rate * lsp->load_secs
		     / ( lsp->load_period );
    } else {
	ps->mem_read = lsp->mem_write;
	ps->rate_lim = lsp->load_rate * lsp->load_secs
		     / ( lsp->load_idle );
    }
    return(0);
}
#elif BMR_SERVER == BMR_SERVER_PHAKER
/*
 * When FAKE is our source....FIXME
 */
#else /* BMR_SERVER */
# error "Unsupported BMR_SERVER TYPE"
#endif /* BMR_SERVER */

/*
 * Nuking the PushInfo structure is a good start.
 */
static int push_stats_prep(PushState *ps)
{
    memset(&pis, 0, sizeof(pis));
    return(0);
}

/*
 * last_seqn is initialized from GRAB by push_buffer_prep_*().
 * FIXME: what do we want to do if it jumps too much?
 */
static void update_push_state(PushState *ps)
{
    int64_t	old_seqn;
    old_seqn = pss.last_seqn;
    pss = *ps;	    /* make it so, #1 */
    /* FIXME: pss.last_seqn <> old_seqn; */
    check_push_state();
}

/*
 * Shut down any pushing activity in progress and initialize a new activity.
 * In the case of PUSH_ACT_IDLE, we're still configuring.
 */
int set_push_state(PushState *ps)
{
    push_network_yank();

    if (push_stats_prep(ps))
	return(fputs("Failed to initialize push stats.\n", stderr));

    if (push_buffer_prep(ps))
	return(fputs("Failed to initialize push buffer.\n", stderr));

    if (push_timing_prep(ps))
	return(fputs("Failed to initialize push timing.\n", stderr));

    push_network_prep(ps);
    if (ps->push_sock < 0)
	return(fputs("Failed to initialize push socket.\n", stderr));

    update_push_state(ps);
    return(0);
}

/*
 * Methods to return safe copies of our internals.
 */
PushInfo *get_push_info(void)
{
    static PushInfo	mine;
    clock_gettime(CLOCK_REALTIME, &pis.push_time);
    pis.push_state = pss;
    mine = pis;
    return(&mine);
}
PushState *get_push_state(void)
{
    static PushState	mine;
    mine = pss;
    return(&mine);
}

/*
 * eof
 */
