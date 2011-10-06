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
 * $Id: push_info.c 984 2011-02-08 17:02:31Z gbc $
 *
 * PUSH configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_util.h"
#include "push_task.h"

/*
 * For readability
 */
char *push_action_name(int action)
{
    switch (action) {
    case PUSH_ACT_IDLE: return("IDLE");
    case PUSH_ACT_INIT: return("INIT");
    case PUSH_ACT_NONE: return("NONE");
    case PUSH_ACT_SEND: return("SEND");
    }
    return("????");
}

/*
 * Show the current state
 */
void desc_push_state(PushState *ps, char *buf)
{
    unsigned char	*b = (unsigned char*)&ps->dname;
    char sstimes[2][TIME_BUFFER_SIZE];
    timefmtTT(ps->push_active, sstimes[0], TIMESTYLE_VEX);
    timefmtTT(ps->push_passive, sstimes[1], TIMESTYLE_VEX);
    snprintf(buf, BMR_MAX_MESSAGE, "PUSH state:\n"
	"  addr=%s port=%d size=%d rate=%ld (%g Gbps)\n"
	"  mem[%d]: %p <= %p < %p (%d MiB) %d pkts\n"
	"  active:%ld passive:%ld reps:%d\n"
	"  active_vex:%s passive_vex:%s\n"
	"  type:%d overlap:%s source:%d rate_lim:%ld\n"
	"  sock:%d send:%s verb:%d flags:%d res_ns:%ld\n"
	"  pktwait:" PRI_TS " lastsnd:" PRI_TS "\n"
	"  dname:%02X.%02X.%02X.%02X.%02X.%02X.%02X.%02X to_us:%d\n"
	"  last_seqn: %016" PRIX64 " action:%s\n"
	,
	ps->push_addr, ps->push_port, ps->push_size, ps->push_rate,
	ps->push_rate * 8.0 * ps->push_size / 1000000000.0, ps->mem_type,
	ps->mem_start, ps->mem_read, ps->mem_start + ps->mem_size,
	(int)(ps->mem_size / (1024*1024)),
	(int)(ps->mem_chunk ? ps->mem_size / ps->mem_chunk : 0),
	ps->push_active, ps->push_passive, ps->push_reps,
	sstimes[0], sstimes[1],
	ps->push_type, ps->overlap ? "allowed" : "disabled",
	ps->source, ps->rate_lim,
	ps->push_sock, push_sender_name(ps->push_send),
	ps->verb, ps->flags, ps->res_ns,
	ps->pktwait.tv_sec, ps->pktwait.tv_nsec,
	ps->lastsnd.tv_sec, ps->lastsnd.tv_nsec,
	b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
	ps->push_to_us,
	ps->last_seqn, push_action_name(ps->action)
    );
}

/*
 * Show some packet statistics
 */
void desc_push_total(char *label, SendTotal *st, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "B %" PRId64 "P %" PRId64 "z "
	"%" PRId64 "e %" PRId64 "c\n",
	label, st->bytes, st->pkts, st->sizerr, st->syserr, st->called);
}

void desc_push_tosn(char *label, TooSoon *ts, char *buf)
{
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %" PRId64 "rw %" PRId64 "inv %" PRId64 "rl %" PRId64 "slp\n",
	label, ts->rw, ts->inv, ts->rl, ts->slp);
}


/*
 * Show the current PUSH rate.
 */
void desc_push_rate(char *label, PushInfo *pi, char *buf)
{
    TimeSpec	total;
    double	secs, rate;
    if (pi->push_state.action == PUSH_ACT_SEND) {
	clock_gettime(CLOCK_REALTIME, &total);
	time_increment(
	    time_decrement(&total, &pi->sending_entry), &pi->sending_time);
    } else {
	total = pi->sending_time;
    }
    secs = total.tv_sec + 1e-9 * total.tv_nsec;
    rate = (secs>0) ? 8e-09 * pi->sending.bytes / secs : 0.0;
    snprintf(buf, BMR_MAX_MESSAGE,
	"%s: %g Gbps (%.9f s)\n", label, rate, secs);
}

/*
 * Show the current info
 */
void desc_push_info(PushInfo *pi, char *buf)
{
    static char bbb[5][BMR_MAX_MESSAGE];
    desc_push_rate(" SendRT", pi, bbb[0]);
    desc_push_total("Sending", &pi->sending, bbb[1]);
    if (pi->push_state.verb>0) {
	desc_push_total(" Idling", &pi->idling, bbb[2]);
	desc_push_tosn("TooSoon", &pi->tosn, bbb[3]);
	desc_push_state(&pi->push_state, bbb[4]);
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s%s%s%s",
	    bbb[0], bbb[1], bbb[2], bbb[3], bbb[4]);
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "%s%s", bbb[0], bbb[1]);
    }
}

/*
 * eof
 */
