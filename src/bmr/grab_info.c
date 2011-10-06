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
 * $Id: grab_info.c 1298 2011-08-01 21:20:02Z gbc $
 *
 * GRAB configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_util.h"
#include "grab_task.h"

/*
 * For readability
 */
char *grab_action_name(int action)
{
    switch (action) {
    case GRAB_ACT_IDLE:	return("IDLE");
    case GRAB_ACT_INIT:	return("INIT");
    case GRAB_ACT_NONE:	return("NONE");
    case GRAB_ACT_PUNT:	return("PUNT");
    case GRAB_ACT_BUFF:	return("BUFF");
    }
    return("????");
}

/*
 * Show the current state
 */
void desc_grab_state(GrabState *gs, char *buf)
{
    char sstimes[2][TIME_BUFFER_SIZE];
    timefmtTT(gs->grab_start, sstimes[0], TIMESTYLE_VEX);
    timefmtTT(gs->grab_stop, sstimes[1], TIMESTYLE_VEX);
    snprintf(buf, BMR_MAX_MESSAGE, "GRAB state:\n"
	"  addr=%s port=%d size=%d rate=%ld (%g Gbps)\n"
	"  secs=%d flush=%d idle=%d period=%d cycles=%d\n"
	"  start=%ld stop=%ld tognet=%d\n"
	"  start_vex:%s stop_vex:%s\n"
	"  mem[%d]: %p <= %p < %p\n"
	"  (%d MiB) %d pkts\n"
	"  punting:%ld active:%ld passive:%ld reps:%d\n"
	"  sock:%d recv:%s verb:%d flags:%X hlen:%d\n"
	"  next_seqn: %016" PRIX64 " action:%s\n"
	,
	gs->udp_addr, gs->udp_port, gs->udp_size, gs->udp_rate,
	gs->udp_rate * 8.0 * gs->udp_size / 1000000000.0,
	gs->grab_secs, gs->grab_flush,
	gs->grab_idle, gs->grab_period, gs->grab_cycles,
	gs->grab_start, gs->grab_stop, gs->tognet,
	sstimes[0], sstimes[1], gs->mem_type,
	gs->mem_start, gs->mem_write, gs->mem_start + gs->mem_size,
	(int)(gs->mem_size / (1024*1024)),
	(int)(gs->mem_chunk ? gs->mem_size / gs->mem_chunk : 0),
	gs->grab_punting, gs->grab_active, gs->grab_passive, gs->grab_reps,
	gs->grab_sock, grab_recver_name(gs->grab_recv),
	gs->verb, gs->flags, gs->udp_hlen,
	gs->next_seqn, grab_action_name(gs->action)
    );
}

/*
 * Show some packet statistics
 */
void desc_grab_total(char *label, RecvTotal *rt, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "B %" PRId64 "P %" PRId64 "z "
	"%" PRId64 "e %" PRId64 "c\n",
	label, rt->bytes, rt->pkts, rt->sizerr, rt->syserr, rt->called);
}

/*
 * Show the current GRAB rate.
 */
void desc_grab_rate(char *label, GrabInfo *gi, char *buf)
{
    TimeSpec	total;
    double	secs, rate;
    if (gi->grab_state.action == GRAB_ACT_BUFF) {
	clock_gettime(CLOCK_REALTIME, &total);
	time_increment(
	    time_decrement(&total, &gi->active_entry), &gi->active_time);
    } else {
	total = gi->active_time;
    }
    secs = total.tv_sec + 1e-9 * total.tv_nsec;
    rate = (secs>0) ? 8e-09 * gi->active.bytes / secs : 0.0;
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %g Gbps (%.9f s)\n", label, rate, secs);
}

/*
 * Show the current info
 */
void desc_grab_info(GrabInfo *gi, char *buf)
{
    static char bbb[5][BMR_MAX_MESSAGE];
    desc_grab_rate(" GrabRT", gi, bbb[0]);
    desc_grab_total(" Active", &gi->active, bbb[1]);
    if (gi->grab_state.verb>0) {
	desc_grab_total("Passive", &gi->passive, bbb[2]);
	desc_grab_total("   Idle", &gi->idle, bbb[3]);
	desc_grab_state(&gi->grab_state, bbb[4]);
	snprintf(buf, BMR_MAX_MESSAGE,
	    "%s%s%s%s%s", bbb[0], bbb[1], bbb[2], bbb[3], bbb[4]);
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s", bbb[0], bbb[1]);
    }
}

/*
 * eof
 */
