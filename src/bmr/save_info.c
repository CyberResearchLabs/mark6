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
 * $Id: save_info.c 1298 2011-08-01 21:20:02Z gbc $
 *
 * SAVE configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_util.h"
#include "save_task.h"
#include "anal_util.h"

/*
 * For readability
 */
char *save_action_name(int action)
{
    switch (action) {
    case SAVE_ACT_IDLE: return("IDLE");
    case SAVE_ACT_INIT: return("INIT");
    case SAVE_ACT_NONE: return("NONE");
    case SAVE_ACT_FILE: return("FILE");
    }
    return("????");
}

/*
 * Generate a list of saved files.
 */
void desc_save_files(SaveState *ss, char *buf)
{
    int sf, nb, left = BMR_MAX_MESSAGE;
    for (sf = 0; left > 0 && sf < ss->num_files; sf++) {
	nb = snprintf(buf, left, "  file[%d] = '%s' (%d)\n",
	    sf, ss->path[sf], ss->filed[sf]);
	left += nb;
	buf += nb;
    }
}

/*
 * Show the current state
 */
void desc_save_state(SaveState *ss, char *buf)
{
    char *list = malloc(BMR_MAX_MESSAGE);
    char sstimes[2][TIME_BUFFER_SIZE];
    desc_save_files(ss, list);
    timefmtTT(ss->save_active, sstimes[0], TIMESTYLE_VEX);
    timefmtTT(ss->save_passive, sstimes[1], TIMESTYLE_VEX);
    snprintf(buf, BMR_MAX_MESSAGE, "SAVE state:\n"
	"  save_type=%d scan_label=%s num_files=%d rate=%ld (%g Gbps)\n"
	"  ppgroup=%d save_anal=%d\n"
	"  mem[%d]: %p <= %p < %p\n"
	"  (%d MiB) %d pkts\n"
	"  active:%ld passive:%ld reps:%d\n"
	"  active_vex:%s passive_vex:%s\n"
	"  overlap:%s max_cycles:%d file_index:%d\n"
	"  save_fd:%d send:%s verb:%d file_fmt:%d rate_lim:%ld\n"
	"  station:%02X\n"
	"%s"
	"  last_seqn: %016" PRIX64 " action:%s\n"
	,
	ss->save_type, ss->scan_label, ss->num_files, ss->save_rate,
	ss->save_rate * 8.0 * ss->save_size / 1000000000.0,
	ss->ppgroup, ss->save_anal, ss->mem_type,
	ss->mem_start, ss->mem_read, ss->mem_start + ss->mem_size,
	(int)(ss->mem_size / (1024*1024)),
	(int)(ss->mem_chunk ? ss->mem_size / ss->mem_chunk : 0),
	ss->save_active, ss->save_passive, ss->save_reps,
	sstimes[0], sstimes[1],
	ss->overlap ? "allowed" : "disabled",
	ss->max_cycles, ss->file_index,
	ss->save_fd, save_writer_name(ss->save_writer),
	ss->verb, ss->file_fmt, ss->rate_lim,
	ss->station,
	list,
	ss->last_seqn, save_action_name(ss->action)
    );
    free(list);
}

/*
 * Show some packet statistics
 */
void desc_save_total(char *label, SaveTotal *st, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "B %" PRId64 "P %" PRId64 "z "
	"%" PRId64 "e %" PRId64 "c\n",
	label, st->bytes, st->pkts, st->sizerr, st->syserr, st->called);
}

void desc_save_notw(char *label, NoWrite *nw, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "rw %" PRId64 "inv %" PRId64 "rl %" PRId64 "ok\n",
	label, nw->rw, nw->inv, nw->rl, nw->ok);
}

/*
 * Show the current SAVE rate.
 */
void desc_save_rate(char *label, SaveInfo *si, char *buf)
{
    TimeSpec	total;
    double	secs, rate;
    if (si->save_state.action == SAVE_ACT_FILE) {
	clock_gettime(CLOCK_REALTIME, &total);
	time_increment(
	    time_decrement(&total, &si->writing_entry), &si->writing_time);
    } else {
	total = si->writing_time;
    }
    secs = total.tv_sec + 1e-9 * total.tv_nsec;
    rate = (secs>0) ? 8e-09 * si->writing.bytes / secs : 0.0;
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %g Gbps (%.9f s)\n", label, rate, secs);
}

/*
 * Show the current info
 */
void desc_save_info(SaveInfo *si, char *buf)
{
    static char bbb[6][BMR_MAX_MESSAGE];
    desc_save_rate("WriteRT", si, bbb[0]);
    desc_save_total("Writing", &si->writing, bbb[1]);
    if (si->save_state.verb>0) {
	desc_save_total(" Idling", &si->idling, bbb[2]);
	desc_save_notw("NoWrite", &si->notw, bbb[3]);
	desc_save_state(&si->save_state, bbb[4]);
	anal_save_result(bbb[5]);
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s%s%s%s%s",
	    bbb[0], bbb[1], bbb[2], bbb[3], bbb[4], bbb[5]);
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s", bbb[0], bbb[1]);
    }
}

/*
 * eof
 */
