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
 * $Id: load_task.h 984 2011-02-08 17:02:31Z gbc $
 *
 * External interface for the LOAD task.
 *
 * The core routines are in load_task.c,
 * commanding/config routines are in load_parser.c, and
 * various diagnostics are in load_info.c.
 */

#ifndef load_task_h
#define load_task_h

#include "bmr_common.h"

/*
 * LOAD task working variables.
 */
typedef struct load_state {
    int		load_type;	/* RAID or multiple files */
    char	*scan_label;	/* a label for the saved files */
    int		load_size;	/* bytes read per packet */
    long	load_rate;	/* desired data loading rate (pps) */
    int		num_files;	/* number of files/disks to use */
    int		file_index;	/* index of next file to open */
    char	*path[BMR_MAX_FILES];
    int		filed[BMR_MAX_FILES];

    int64_t	next_seqn;	/* a sequence counter */
    void	*mem_start;	/* start of a buffer */
    ssize_t	mem_size;	/* of some size */
    void	*mem_write;	/* place for next packet */
    int		mem_chunk;	/* bytes written per packet */
    int		mem_type;	/* how packet is stored */

    time_t	load_start;	/* UNIX time to start */
    time_t	load_stop;	/* UNIX time to stop */
    int		load_secs;	/* loading duration */
    int		load_idle;	/* secs between loads */
    int		load_period;	/* load_secs+load_idle */
    int		load_cycles;	/* how many rep times */

    time_t	load_passive;	/* next time to do nothing */
    time_t	load_active;	/* next time to be reading packets */
    int		load_reps;	/* countdown on number of cycles */
    Receiver	*load_reader;	/* packet reader handler */
    int		load_fd;	/* next fd to read */

    int		action;		/* state machine variable */
    int		verb;		/* verbosity */
    int		file_fmt;	/* packet reformatting on read */
    long	rate_lim;	/* based on input data */
} LoadState;

/*
 * The action member allows for some variations on the basic theme.
 * In the initial implementation:
 *
 * The LOAD_ACT_IDLE state is for configuration of the load task,
 * the LOAD_ACT_INIT state is for launching into service,
 * the LOAD_ACT_NONE state is for when reading isn't allowed,
 * the LOAD_ACT_READ state is for nominal reading of packets.
 *
 * Other states can be added for modified functionality.
 *
 * The nominal mode is to configure the task in LOAD_ACT_IDLE mode
 * and then alternate between LOAD_ACT_READ and LOAD_ACT_NONE by time.
 * Implicitly, a set_load_state() with action LOAD_ACT_INIT is
 * required to start the LOAD task for normal activity.
 */
#define LOAD_ACT_IDLE       0
#define LOAD_ACT_INIT       1
#define LOAD_ACT_NONE       3
#define LOAD_ACT_READ       4

/*
 * Switch between active and passive states based on time.
 * Returns zero if the readable file descriptors should be
 * monitored, and nonzero if they should be ignored.
 */
extern int check_load_state(void);

/*
 * Functions for updating the LOAD task.  get_load_state() returns
 * a pointer to a (modifyable) copy of the current load parameters
 * which may be updated as desired.  Then set_load_state() can be called
 * to specify the parameters that will take effect with the next LOAD
 * cycle.
 */
extern LoadState *get_load_state(void);
extern int set_load_state(LoadState *state);

/*
 * For some of LOAD's variables we may want immediate gratification.
 */
extern void const ** current_load_write_ptr;
extern Reader ** current_load_reader_ptr;
extern int * const current_load_fd_ptr;

/*
 * Structures for monitoring LOAD activity.
 */
typedef struct load_total {
    int64_t	bytes;		/* bytes written */
    int64_t	pkts;		/* packets written */
    int64_t	sizerr;		/* short packets */
    int64_t	syserr;		/* errno != 0 */
    int64_t	called;		/* how many times called */
} LoadTotal;
typedef struct no_read {
    int64_t	rw;		/* read caught up */
    int64_t	inv;		/* same thing */
    int64_t	rl;		/* anomalous */
    int64_t	ok;		/* reading */
} NoRead;
typedef struct load_info {
    LoadState	load_state;	/* a copy of the private state */
    LoadTotal	reading;	/* counters of read packets */
    LoadTotal	idling;		/* counters when idling */
    NoRead	notr;		/* nothing to read */
    TimeSpec	reading_entry;	/* time of last entry */
    TimeSpec	reading_time;	/* cumulative reading time */
    TimeSpec	load_time;	/* time of report */
} LoadInfo;

/*
 * A function to get the current status--this returns a pointer to
 * a temporary private data area.
 */
extern LoadInfo *get_load_info(void);

/*
 * Some configuration commands, returns nonzero if cmd is in error.
 */
extern int load_config(char *cmd, LoadState *ls, char *buf);
extern void set_default_load_state(LoadState *ls);
extern void load_dump(FILE *fp, LoadState *ls);
extern int load_help(char *buf);

/*
 * Various routines for reporting save activities.
 */
extern char *load_action_name(int action);
extern char *load_reader_name(void *reader);
extern void desc_load_state(LoadState *ls, char *buf);
extern void desc_load_info(LoadInfo *li, char *buf);
extern void desc_load_total(char *label, LoadTotal *lt, char *buf);
extern void desc_load_notr(char *label, NoRead *nr, char *buf);
extern void desc_load_rate(char *label, LoadInfo *li, char *buf);

#endif /* load_task_h */

/*
 * eof
 */
