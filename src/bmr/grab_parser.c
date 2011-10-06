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
 * $Id: grab_parser.c 988 2011-02-08 22:07:00Z gbc $
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
 * Set some (harmless) defaults
 */
void set_default_grab_state(GrabState *gs)
{
    if (gs->verb>0) fputs("GRAB default state\n", stderr);
    strncpy(gs->udp_addr, "127.0.0.1", sizeof(gs->udp_addr));
    gs->udp_port = 4200;
    gs->udp_size = 8224;
    gs->udp_rate = 2;

    gs->mem_type = BMR_MEM_SEQN;
    gs->next_seqn = BMR_SEQN_ATBIRTH;
    gs->grab_start = time(0) + 15;
    gs->grab_secs = 5;
    gs->grab_flush = 1;
    gs->grab_period = 15;
    gs->grab_stop = gs->grab_start + 4 * gs->grab_period;

    gs->action = GRAB_ACT_INIT;
    gs->tognet = 1;
}

/*
 * Dump the current state as a set of commands (for possible reuse)
 */
void grab_dump(FILE *fp, GrabState *gs)
{
    fputs("##\n## GRAB commands\n##\n", fp);
    fprintf(fp, "grab:addr=%s\n", gs->udp_addr);
    fprintf(fp, "grab:port=%d\n", gs->udp_port);
    fprintf(fp, "grab:size=%d\n", gs->udp_size);
    fprintf(fp, "grab:rate=%ld\n", gs->udp_rate);
    fprintf(fp, "grab:mbps=%g\n", gs->udp_rate*gs->udp_size*8.0e-6);
    fprintf(fp, "grab:gbps=%g\n", gs->udp_rate*gs->udp_size*8.0e-9);
    fprintf(fp, "grab:type=%ld\n", gs->mem_type);
    fprintf(fp, "grab:start=%ld\n", gs->grab_start);
    fprintf(fp, "grab:stop=%ld\n", gs->grab_stop);
    fprintf(fp, "grab:secs=%d\n", gs->grab_secs);
    fprintf(fp, "grab:flush=%d\n", gs->grab_flush);
    fprintf(fp, "grab:period=%d\n", gs->grab_period);
    fprintf(fp, "grab:tognet=%d\n", gs->tognet);
}

/*
 * Provide some help
 */
int grab_help(char *buf)
{
    return(snprintf(buf, BMR_MAX_MESSAGE,
	"The GRAB configuration commands are:\n"
	"\tgrab:addr=<addr>  # specify receiving IP addr or ETH device\n"
	"\tgrab:port=<int>   # specify the UDP port receiving packets\n"
	"\tgrab:size=<int>   # specify size of all UDP packets\n"
	"\tgrab:rate=<int>   # specify packet rate (packets per sec)\n"
	"\tgrab:mbps=<float> # specify packet rate (Mbps)\n"
	"\tgrab:gbps=<float> # specify packet rate (Gbps)\n"
	"\tgrab:type=<int>   # specify packet memory model\n"
	"\tgrab:start=<time> # specify starting time\n"
	"\tgrab:stop=<time>  # specify stopping time\n"
	"\tgrab:secs=<int>   # specify capturing duration (secs)\n"
	"\tgrab:flush=<int>  # specify packet flushing (secs)\n"
	"\tgrab:period=<int> # specify cycle duration (secs)\n"
	"\tgrab:tognet=<int> # specify data socket toggling\n"
	"\n"
	"\tgrab:help         # provides this message\n"
	"\tgrab:show         # displays GRAB state\n"
	"\tgrab:default      # resets to a default state\n"
	"\tgrab:apply        # applies all commands\n"
	"\tgrab:quit         # provokes a fatal error\n"
    ));
}

/*
 * A rather primitive command parser that adjusts the state structure
 */
int grab_config(char *cmd, GrabState *gs, char *buf)
{
    double  temp;

    *buf = 0;
    if (!(cmd = skip_leading_space(cmd))) return(0);

    if        (!strncmp(cmd, "show", 4)) {
	desc_grab_state(gs, buf);
    } else if (!strncmp(cmd, "help", 4)) {
	grab_help(buf);
    } else if (!strncmp(cmd, "default", 7)) {
	set_default_grab_state(gs);
    } else if (!strncmp(cmd, "apply", 5)) {
	return(set_grab_state(gs));
    } else if (!strncmp(cmd, "quit", 4)) {
	return(-1);

    } else if (!strncmp(cmd, "addr=", 5)) {
	strncpy(gs->udp_addr, cmd+5, sizeof(gs->udp_addr));
    } else if (!strncmp(cmd, "port=", 5)) {
	gs->udp_port = atoi(cmd+5);
    } else if (!strncmp(cmd, "size=", 5)) {
	gs->udp_size = atoi(cmd+5);
    } else if (!strncmp(cmd, "rate=", 5)) {
	gs->udp_rate = atol(cmd+5);
    } else if (!strncmp(cmd, "mbps=", 5)) {
	if (gs->udp_size == 0) return(2);
	temp = atof(cmd+5) * 1000000.0 / 8.0;		/* Bps */
	gs->udp_rate = floor(temp / (gs->udp_size));	/* Pps */
    } else if (!strncmp(cmd, "gbps=", 5)) {
	if (gs->udp_size == 0) return(2);
	temp = atof(cmd+5) * 1000000000.0 / 8.0;	/* Bps */
	gs->udp_rate = floor(temp / (gs->udp_size));	/* Pps */
    } else if (!strncmp(cmd, "type=", 5)) {
	gs->mem_type = atoi(cmd+5);
    } else if (!strncmp(cmd, "start=", 6)) {
	gs->grab_start = timeis(cmd+6);
    } else if (!strncmp(cmd, "stop=", 5)) {
	gs->grab_stop = timeis(cmd+5);
    } else if (!strncmp(cmd, "secs=", 5)) {
	gs->grab_secs = atoi(cmd+5);
    } else if (!strncmp(cmd, "flush=", 6)) {
	gs->grab_flush = atoi(cmd+6);
    } else if (!strncmp(cmd, "period=", 7)) {
	gs->grab_period = atoi(cmd+7);
    } else if (!strncmp(cmd, "action=init", 11)) {
	gs->action = GRAB_ACT_INIT;
    } else if (!strncmp(cmd, "action=idle", 11)) {
	gs->action = GRAB_ACT_IDLE;
    } else if (!strncmp(cmd, "tognet=", 7)) {
	gs->tognet = atoi(cmd+7);
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "Unparsed GRAB command %s\n", cmd);
	return(1);
    }
    return(0);
}

/*
 * eof
 */
