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
 * $Id: push_task.h 984 2011-02-08 17:02:31Z gbc $
 *
 * External interface for the PUSH task.
 *
 * The core routines are in push_task.c,
 * commanding/config routines are in push_parser.c, and
 * various diagnostics are in push_info.c.
 */

#ifndef push_task_h
#define push_task_h

#include "bmr_common.h"

/*
 * PUSH task working variables.
 */
typedef struct push_state {
    char	push_addr[128];	/* network address of UDP/TCP */
    int		push_port;	/* the Mark5C uses 2650 */
    int		push_size;	/* must be less than 9000 */
    long	push_rate;	/* packets per second */
    int		push_type;	/* UDP or TCP protocol */

    int64_t	last_seqn;	/* last seqn sent */
    void	*mem_start;	/* start of a buffer */
    ssize_t	mem_size;	/* of some size */
    void	*mem_read;	/* next packet to send */
    int		mem_chunk;	/* bytes read per packet */
    int		mem_type;	/* how the packet is stored */

    int		overlap;	/* permission to run simultaneously */
    int		source;		/* who is loading mem? GRAB or LOAD */
    int		push_period;	/* a copy of grab's value */

    time_t	push_active;	/* next time to emit packets */
    time_t	push_passive;	/* next time to fall silent */
    int		push_reps;	/* countdown on number of cycles */
    int		push_sock;	/* destination (e.g. Mark5C) socket */
    Sender	*push_send;	/* packet pusher handler */
    int		push_to_us;	/* requested wait before next packet */

    int		action;		/* allow for variations */
    int		verb;		/* verbosity */
    int		flags;		/* send flags */
    long	res_ns;		/* clock resolution */
    long	rate_lim;	/* based on data input */
    SockAddr	dname;		/* sendto(to) recipient */
    TimeSpec	pktwait;	/* time between sends */
    TimeSpec	lastsnd;	/* time of last send */
} PushState;

/*
 * The push_type member controls whether we're pushing UDP to a Mark5C
 * (or equivalent) or pushing packets on TCP in a daemon mode.
 */
#define PUSH_PKT_TYPE_OFF   -1
#define PUSH_PKT_TYPE_UDP   0
#define PUSH_PKT_TYPE_TCP   1

/*
 * The memory might have been set up via GRAB or LOAD (or FAKE)
 */
#define PUSH_SOURCE_GRAB    0
#define PUSH_SOURCE_LOAD    1
#define PUSH_SOURCE_FAKE    2

/*
 * The action member allows for some variations on the basic theme:
 * The PUSH_ACT_IDLE state is for configuration of the push task,
 * the PUSH_ACT_INIT state is for launching into service,
 * the PUSH_ACT_NONE state is for times when sending isn't allowed,
 * the PUSH_ACT_SEND state is for normal delivery of packets,
 *
 * Implicitly a set_push_state() with action PUSH_ACT_INIT is
 * required to start the PUSH task for normal activity.
 */
#define PUSH_ACT_IDLE       0
#define PUSH_ACT_INIT       1
#define PUSH_ACT_NONE       2
#define PUSH_ACT_SEND       3

/*
 * Switch between active and passive states based on time.
 * Returns zero if the outgoing socket should be monitored
 * for sendability, and nonzero if it should be ignored.
 */
extern int check_push_state(void);

/*  
 * In order to push at some metered rate, we may need to sleep
 * until time to send.  This limits the time we can be stuck
 * sleeping and doing nothing (1 us .. 250 ms).
 */
#define PUSH_NANOSLEEP_MAX    25000000L
#define PUSH_NANOSLEEP_MIN        1000L
#define PUSH_ONENANOSECOND  1000000000L

/*
 * Functions for updating the PUSH task.  get_push_state() returns
 * a pointer to a (modifyable) copy of the current push parameters
 * which may be updated as desired.  Then set_push_state() can be called
 * to (abort the current PUSH activity, if any) and update the PUSH task.
 * set_push_state() returns nonzero upon an error.
 */
extern PushState *get_push_state(void);
extern int set_push_state(PushState *state);

/*
 * For some of PUSH's variables, we may want immediate gratification.
 */
extern int const * const current_push_sock_ptr;
extern Sender ** current_push_send_ptr;

/*
 * Structure for monitoring PUSH activity.
 */
typedef struct send_total {
    int64_t	bytes;		/* bytes sent */
    int64_t	pkts;		/* packets sent */
    int64_t	sizerr;		/* short packets */
    int64_t	syserr;		/* errno != 0 */
    int64_t	called;		/* how many times called */
} SendTotal;
typedef struct too_soon {
    int64_t	rw;		/* send caught up */
    int64_t	inv;		/* same thing */
    int64_t	rl;		/* anomalous */
    int64_t	slp;		/* sleep count */
} TooSoon;
typedef struct push_info {
    PushState	push_state;	/* a copy of the private state */
    SendTotal	sending;	/* counters of pushed packets */
    SendTotal	idling;		/* counters when idling */
    TooSoon	tosn;		/* too soon to send counters */
    TimeSpec	sending_entry;	/* time of last entry */
    TimeSpec	sending_time;	/* cumulative sending time */
    TimeSpec	push_time;	/* time of report */
} PushInfo;

/*
 * A function to get the current status--this returns a pointer to
 * a temporary private data area.
 */
extern PushInfo *get_push_info(void);

/*
 * Some configuration commands, returns nonzero if cmd is in error.
 */
extern int push_config(char *cmd, PushState *gs, char *buf);
extern void set_default_push_state(PushState *ps);
extern void push_dump(FILE *fp, PushState *gs);
extern int push_help(char *buf);

/*
 * Various routines for reporting push activities.
 */
extern char *push_action_name(int action);
extern char *push_sender_name(void *send);
extern void desc_push_state(PushState *ps, char *buf);
extern void desc_push_info(PushInfo *pi, char *buf);
extern void desc_push_total(char *label, SendTotal *st, char *buf);
extern void desc_push_tosn(char *label, TooSoon *ts, char *buf);
extern void desc_push_rate(char *label, PushInfo *pi, char *buf);

#endif /* push_task_h */

/*
 * eof
 */
