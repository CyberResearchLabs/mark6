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
 * $Id: grab_task.h 984 2011-02-08 17:02:31Z gbc $
 *
 * External interface for the GRAB task.
 *
 * The core routines are in grab_task.c,
 * commanding/config routines are in grab_parser.c, and
 * various diagnostics are in grab_info.c.
 */

#ifndef grab_task_h
#define grab_task_h

#include "bmr_common.h"

/*
 * GRAB task working variables.
 */
typedef struct grab_state {
    char	udp_addr[128];	/* binding address/device of UDP */
    int		udp_port;	/* Mark5C:2650; BMR:420? */
    int		udp_size;	/* must be less than 9000 */
    long	udp_rate;	/* in packets per second */
    int		udp_hlen;	/* raw mode header preceding UDP data */

    int64_t	next_seqn;	/* a sequence counter */
    void	*mem_start;	/* start of a buffer */
    ssize_t	mem_size;	/* of some size */
    void	*mem_write;	/* place for next packet */
    int		mem_chunk;	/* bytes written per packet */
    int		mem_type;	/* how the packet is stored */

    time_t	grab_start;	/* UNIX time to start */
    time_t	grab_stop;	/* UNIX time to stop */
    int		grab_secs;	/* grabbing duration */
    int		grab_idle;	/* secs between grabs */
    int		grab_flush;	/* secs before grab */
    int		grab_period;	/* grab_secs+grab_idle */
    int		grab_cycles;	/* how many rep times */

    time_t	grab_passive;	/* next time to ignore packets */
    time_t	grab_active;	/* next time to capture packets */
    time_t	grab_punting;	/* next time to be punting packets */
    int		grab_reps;	/* countdown on number of cycles */
    int		grab_sock;	/* input socket from DDS */
    Receiver	*grab_recv;	/* packet grabber handler */

    int		action;		/* state machine variable */
    int		verb;		/* verbosity */
    int		flags;		/* recv flags */
    int		tognet;		/* create/destroy socket each cycle */
} GrabState;

/*
 * The action member allows for some variations on the basic theme.
 * In the initial implementation:
 *
 * The GRAB_ACT_IDLE state is for configuration of the grab task,
 * the GRAB_ACT_INIT state is for launching into service,
 * the GRAB_ACT_PUNT state is for ignoring packets altogether
 * the GRAB_ACT_NONE state is for capturing packets but ignoring them,
 * the GRAB_ACT_BUFF state is for doing the job as specified.
 *
 * Other states can be added for modified functionality.
 *
 * The nominal mode is to configure the task in GRAB_ACT_IDLE mode
 * and then alternate between GRAB_ACT_NONE and GRAB_ACT_BUFF by time.
 * Implicitly, a set_grab_state() with action GRAB_ACT_INIT is
 * required to start the GRAB task for normal activity.
 */
#define GRAB_ACT_IDLE       0
#define GRAB_ACT_INIT       1
#define GRAB_ACT_PUNT       2
#define GRAB_ACT_NONE       3
#define GRAB_ACT_BUFF       4

/*
 * Switch between active and passive states based on time.
 * Returns zero if the incoming socket should be monitored for
 * reception of packets, and nonzero if it should be ignored.
 */
extern int check_grab_state(void);

/*
 * Functions for updating the GRAB task.  get_grab_state() returns
 * a pointer to a (modifyable) copy of the current grab parameters
 * which may be updated as desired.  Then set_grab_state() can be called
 * to (abort the current GRAB activity, if any) and update the GRAB task.
 * set_grab_state() returns nonzero upon an error.
 */
extern GrabState *get_grab_state(void);
extern int set_grab_state(GrabState *state);

/*
 * For several of GRAB's variables, we may want immediate gratification
 */
extern void const ** current_grab_write_ptr;
extern int const * const current_grab_sock_ptr;
extern Receiver ** current_grab_recv_ptr;

/*
 * Structure for monitoring GRAB activity.
 */
typedef struct recv_total {
    int64_t	bytes;		/* bytes recv()'d */
    int64_t	pkts;		/* packets recv()'d */
    int64_t	sizerr;		/* short packets */
    int64_t	syserr;		/* errno != 0 */
    int64_t	called;		/* how many times called */
} RecvTotal;
typedef struct grab_info {
    GrabState	grab_state;	/* a copy of the private state */
    RecvTotal	active;		/* counters of saved packets */
    RecvTotal	passive;	/* counters of collected */
    RecvTotal	idle;		/* counters when idling */
    TimeSpec	active_entry;	/* time of last entry */
    TimeSpec	active_time;	/* cumulative active time */
    TimeSpec	grab_time;	/* time of report */
} GrabInfo;

/*
 * A function to get the current status--this returns a pointer to
 * a temporary private data area.
 */
extern GrabInfo *get_grab_info(void);

/*
 * Some configuration commands, returns nonzero if cmd is in error.
 */
extern int grab_config(char *cmd, GrabState *gs, char *buf);
extern void set_default_grab_state(GrabState *gs);
extern void grab_dump(FILE *fp, GrabState *gs);
extern int grab_help(char *buf);

/*
 * Various routines for reporting grab activities.
 */
extern char *grab_action_name(int action);
extern char *grab_recver_name(void *grab);
extern void desc_grab_state(GrabState *gs, char *buf);
extern void desc_grab_info(GrabInfo *gi, char *buf);
extern void desc_grab_total(char *label, RecvTotal *rt, char *buf);
extern void desc_grab_rate(char *label, GrabInfo *gi, char *buf);

/*
 * Internal support routine
 */
extern void grab_network_prep(GrabState *gs);

#endif /* grab_task_h */

/*
 * eof
 */
