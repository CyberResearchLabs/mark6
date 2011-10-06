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
 * $Id: save_task.h 1298 2011-08-01 21:20:02Z gbc $
 *
 * External interface for the SAVE task.
 *
 * The core routines are in save_task.c,
 * commanding/config routines are in save_parser.c, and
 * various diagnostics are in save_info.c.
 */

#ifndef save_task_h
#define save_task_h

#include "bmr_common.h"

/*
 * SAVE task working variables.
 */
typedef struct save_state {
    int		save_type;	/* RAID or multiple files */
    char	*scan_label;	/* a label for the saved files */
    int		save_size;	/* bytes written per packet */
    long	save_rate;	/* desired packet storage rate (pps) */
    int		num_files;	/* number of files/disks to use */
    char	*path[BMR_MAX_FILES];
    int		filed[BMR_MAX_FILES];
    int         save_anal;      /* number of packets to analyze */

    int64_t	last_seqn;	/* last seqn sent */
    void	*mem_start;	/* start of a buffer */
    ssize_t	mem_size;	/* of some size */
    void	*mem_read;	/* next packet to send */
    int		mem_chunk;	/* bytes read per packet */
    int		mem_type;	/* a copy of grab's value */

    int		overlap;	/* permission to run simultaneously */
    int		ppgroup;	/* max # of packets per group-write */
    int		max_cycles;	/* maximum cyles per file group */
    int		file_index;	/* sequential file counter */
    int		save_period;	/* a copy of grab's value */

    time_t	save_active;	/* next time to write packets */
    time_t	save_passive;	/* next time to do nothing */
    int		save_reps;	/* countdown on number of cycles */
    Writer	*save_writer;	/* packet write handler */
    int		save_fd;	/* next fd to write */

    int		action;		/* allow for variations */
    int		verb;		/* verbosity */
    int		file_fmt;	/* packet reformatting on write */
    int		station;	/* output station bits if >0 */
    long	rate_lim;	/* based on data input */
} SaveState;

/*
 * The action member allows for some variations on the basic theme:
 * The SAVE_ACT_IDLE state is for configuration of the save task,
 * the SAVE_ACT_INIT state is for launching into service,
 * the SAVE_ACT_NONE state is for times when saving isn't allowed,
 * the SAVE_ACT_FILE state is for normal storage of packets.
 *
 * Implicitly a set_save_state() with action SAVE_ACT_INIT is
 * required to start the SAVE task for normal activity.
 */
#define SAVE_ACT_IDLE       0
#define SAVE_ACT_INIT       1
#define SAVE_ACT_NONE       2
#define SAVE_ACT_FILE       3

/*
 * Switch between active and passive states based on time.
 * Returns zero if the outgoing socket should be monitored
 * for sendability, and nonzero if it should be ignored.
 */
extern int check_save_state(void);

/*
 * Functions for updating the SAVE task.  get_save_state() returns
 * a pointer to a (modifyable) copy of the current save parameters
 * which may be updated as desired.  Then set_save_state() can be called
 * to (abort the current SAVE activity, if any) specify the parameters
 * that will take effect with the next SAVE cycle.  set_save_state()
 * returns nonzero upon an error.
 */
extern SaveState *get_save_state(void);
extern int set_save_state(SaveState *state);

/*
 * For some of SAVE's variables, we may want immediate gratification.
 */
extern Writer ** current_save_writer_ptr;
extern int * const current_save_fd_ptr;

/*
 * Structures for monitoring SAVE activity.
 */
typedef struct save_total {
    int64_t	bytes;		/* bytes written */
    int64_t	pkts;		/* packets written */
    int64_t	sizerr;		/* short packets */
    int64_t	syserr;		/* errno != 0 */
    int64_t	called;		/* how many times called */
} SaveTotal;
typedef struct no_write {
    int64_t	rw;		/* write caught up */
    int64_t	inv;		/* same thing */
    int64_t	rl;		/* anomalous */
    int64_t	ok;		/* writing */
} NoWrite;
typedef struct save_info {
    SaveState	save_state;	/* a copy of the private state */
    SaveTotal	writing;	/* counters of written packets */
    SaveTotal	idling;		/* counters when idling */
    NoWrite	notw;		/* nothing to write */
    TimeSpec	writing_entry;	/* time of last entry */
    TimeSpec	writing_time;	/* cumulative writing time */
    TimeSpec	save_time;	/* time of report */
} SaveInfo;

/*
 * A function to get the current status--this returns a pointer to
 * a temporary private data area.
 */
extern SaveInfo *get_save_info(void);

/*
 * Some configuration commands, returns nonzero if cmd is in error.
 */
extern int save_config(char *cmd, SaveState *gs, char *buf);
extern void set_default_save_state(SaveState *ps);
extern void save_dump(FILE *fp, SaveState *gs);
extern int save_help(char *buf);

/*
 * Various routines for reporting save activities.
 */
extern char *save_action_name(int action);
extern char *save_writer_name(void *writer);
extern void desc_save_state(SaveState *ss, char *buf);
extern void desc_save_info(SaveInfo *si, char *buf);
extern void desc_save_total(char *label, SaveTotal *st, char *buf);
extern void desc_save_notw(char *label, NoWrite *nw, char *buf);
extern void desc_save_rate(char *label, SaveInfo *si, char *buf);

#endif /* save_task_h */

/*
 * eof
 */
