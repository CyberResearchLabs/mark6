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
 * $Id: load_info.c 984 2011-02-08 17:02:31Z gbc $
 *
 * LOAD configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_util.h"
#include "load_task.h"

/*
 * For readability
 */
char *load_action_name(int action)
{
    switch (action) {
    case LOAD_ACT_IDLE: return("IDLE");
    case LOAD_ACT_INIT: return("INIT");
    case LOAD_ACT_NONE: return("NONE");
    case LOAD_ACT_READ: return("READ");
    }
    return("????");
}

/*
 * Generate a list of loaded files.
 */
void desc_load_files(LoadState *ls, char *buf)
{
    int lf, nb, left = BMR_MAX_MESSAGE;
    for (lf = 0; left > 0 && lf < ls->num_files; lf++) {
	nb = snprintf(buf, left, "  file[%d] = '%s' (%d)\n",
	    lf, ls->path[lf], ls->filed[lf]);
	left += nb;
	buf += nb;
    }
}

/*
 * Show the current state
 */
void desc_load_state(LoadState *ls, char *buf)
{
    char sstimes[2][TIME_BUFFER_SIZE];
    char *list = malloc(BMR_MAX_MESSAGE);
    desc_load_files(ls, list);
    timefmtTT(ls->load_start, sstimes[0], TIMESTYLE_VEX);
    timefmtTT(ls->load_stop, sstimes[1], TIMESTYLE_VEX);
    snprintf(buf, BMR_MAX_MESSAGE, "LOAD state:\n"
	"  load_type=%d scan_label=%s num_files=%d rate=%ld (%g Gbps)\n"
	"  start=%ld stop=%ld secs=%d idle=%d period=%d cycles=%d\n"
	"  start_vex:%s stop_vex:%s\n"
	"  file_index=%d\n"
	"  mem[%d]: %p <= %p < %p (%d MiB) %d pkts\n"
	"  active:%ld passive:%ld reps:%d\n"
	"  load_fd:%d read:%s verb:%d file_fmt:%d rate_lim:%ld\n"
	"%s"
	"  next_seqn: %016" PRIX64 " action:%s\n"
	,
	ls->load_type, ls->scan_label, ls->num_files, ls->load_rate,
	ls->load_rate * 8.0 * ls->load_size / 1000000000.0,
	ls->load_start, ls->load_stop, ls->load_secs,
	ls->load_idle, ls->load_period, ls->load_cycles,
	sstimes[0], sstimes[1],
	ls->file_index, ls->mem_type,
	ls->mem_start, ls->mem_write, ls->mem_start + ls->mem_size,
	(int)(ls->mem_size / (1024*1024)),
	(int)(ls->mem_chunk ? ls->mem_size / ls->mem_chunk : 0),
	ls->load_active, ls->load_passive, ls->load_reps,
	ls->load_fd, load_reader_name(ls->load_reader),
	ls->verb, ls->file_fmt, ls->rate_lim, list,
	ls->next_seqn, load_action_name(ls->action)
    );
    free(list);
}

/*
 * Show some packet statistics
 */
void desc_load_total(char *label, LoadTotal *lt, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "B %" PRId64 "P %" PRId64 "z "
	"%" PRId64 "e %" PRId64 "c\n",
	label, lt->bytes, lt->pkts, lt->sizerr, lt->syserr, lt->called);
}

void desc_load_notr(char *label, NoRead *nr, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "rw %" PRId64 "inv %" PRId64 "rl %" PRId64 "ok\n",
	label, nr->rw, nr->inv, nr->rl, nr->ok);
}

/*
 * Show the current LOAD rate.
 */
void desc_load_rate(char *label, LoadInfo *li, char *buf)
{
    TimeSpec	total;
    double	secs, rate;
    if (li->load_state.action == LOAD_ACT_READ) {
	clock_gettime(CLOCK_REALTIME, &total);
	time_increment(
	    time_decrement(&total, &li->reading_entry), &li->reading_time);
    } else {
	total = li->reading_time;
    }
    secs = total.tv_sec + 1e-9 * total.tv_nsec;
    rate = (secs>0) ? 8e-09 * li->reading.bytes / secs : 0.0;
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %g Gbps (%.9f s)\n", label, rate, secs);
}

/*
 * Show the current info
 */
void desc_load_info(LoadInfo *li, char *buf)
{
    static char bbb[5][BMR_MAX_MESSAGE];
    desc_load_rate(" ReadRT", li, bbb[0]);
    desc_load_total("Reading", &li->reading, bbb[1]);
    if (li->load_state.verb>0) {
	desc_load_total(" Idling", &li->idling, bbb[2]);
	desc_load_notr(" NoRead", &li->notr, bbb[3]);
	desc_load_state(&li->load_state, bbb[4]);
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s%s%s%s",
	    bbb[0], bbb[1], bbb[2], bbb[3], bbb[4]);
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s", bbb[0], bbb[1]);
    }
}

/*
 * eof
 */
