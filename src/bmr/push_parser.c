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
 * $Id: push_parser.c 601 2010-08-09 20:25:28Z gbc $
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
 * Set some (harmless) defaults
 */
void set_default_push_state(PushState *ps)
{
    if (ps->verb>0) fputs("PUSH default state\n", stderr);
    ps->push_type = PUSH_PKT_TYPE_UDP;
    strncpy(ps->push_addr, "127.0.0.1", sizeof(ps->push_addr));
    ps->push_port = 2652;
    ps->push_size = 5008;
    ps->push_rate = 4;

    ps->overlap = 0;
#if BMR_SERVER == BMR_SERVER_BUFFER
    ps->source = PUSH_SOURCE_GRAB;
#elif BMR_SERVER == BMR_SERVER_REPLAY
    ps->source = PUSH_SOURCE_LOAD;
#elif BMR_SERVER == BMR_SERVER_PHAKER
# error "BMR_SERVER_PHAKER not implemented"
#else /* BMR_SERVER */
# error "Unsupported BMR_SERVER TYPE"
#endif /* BMR_SERVER */


    ps->action = PUSH_ACT_INIT;
}

/*
 * Dump the current state as a set of commands (for possible reuse)
 */
void push_dump(FILE *fp, PushState *ps)
{
    fputs("##\n## PUSH commands\n##\n", fp);
    fprintf(fp, "push:addr=%s\n", ps->push_addr);
    fprintf(fp, "push:port=%d\n", ps->push_port);
    fprintf(fp, "push:size=%d\n", ps->push_size);
    fprintf(fp, "push:rate=%ld\n", ps->push_rate);
    fprintf(fp, "push:mbps=%g\n", ps->push_rate*ps->push_size*8.0e-6);
    fprintf(fp, "push:gbps=%g\n", ps->push_rate*ps->push_size*8.0e-9);
}

/*
 * Provide some help
 */
int push_help(char *buf)
{
    return(snprintf(buf, BMR_MAX_MESSAGE,
	"The PUSH configuration commands are:\n"
	"\tpush:addr=<addr>  # specify IP address to be given packets\n"
	"\tpush:port=<int>   # specify UDP port to be given packets\n"
	"\tpush:size=<int>   # specify size all UDP packets\n"
	"\tpush:rate=<int>   # specify packet rate (packets per sec)\n"
	"\tpush:mbps=<float> # specify packet rate (Mbps)\n"
	"\tpush:gbps=<float> # specify packet rate (Gbps)\n"
	"\n"
	"\tpush:help         # provides this message\n"
	"\tpush:show         # displays PUSH state\n"
	"\tpush:default      # resets to a default state\n"
	"\tpush:apply        # applies all commands\n"
	"\tpush:quit         # provokes a fatal error\n"
    ));
}

/*
 * A rather primitive parser
 */
int push_config(char *cmd, PushState *ps, char *buf)
{
    double  temp;

    *buf = 0;
    if (!(cmd = skip_leading_space(cmd))) return(0);

    if        (!strncmp(cmd, "show", 4)) {
	desc_push_state(ps, buf);
    } else if (!strncmp(cmd, "help", 4)) {
	push_help(buf);
    } else if (!strncmp(cmd, "default", 7)) {
	set_default_push_state(ps);
    } else if (!strncmp(cmd, "apply", 5)) {
	return(set_push_state(ps));
    } else if (!strncmp(cmd, "quit", 4)) {
	return(-1);

    } else if (!strncmp(cmd, "addr=", 5)) {
	strncpy(ps->push_addr, cmd+5, sizeof(ps->push_addr));
    } else if (!strncmp(cmd, "port=", 5)) {
	ps->push_port = atoi(cmd+5);
    } else if (!strncmp(cmd, "size=", 5)) {
	ps->push_size = atoi(cmd+5);
    } else if (!strncmp(cmd, "rate=", 5)) {
	ps->push_rate = atol(cmd+5);
    } else if (!strncmp(cmd, "mbps=", 5)) {
	if (ps->push_size == 0) return(2);
	temp = atof(cmd+5) * 1000000.0 / 8.0;		/* Bps */
	ps->push_rate = floor(temp / (ps->push_size));	/* Pps */
    } else if (!strncmp(cmd, "gbps=", 5)) {
	if (ps->push_size == 0) return(2);
	temp = atof(cmd+5) * 1000000000.0 / 8.0;	/* Bps */
	ps->push_rate = floor(temp / (ps->push_size));	/* Pps */
    } else if (!strncmp(cmd, "action=init", 11)) {
	ps->action = PUSH_ACT_INIT;
    } else if (!strncmp(cmd, "action=idle", 11)) {
	ps->action = PUSH_ACT_IDLE;
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "Unparsed PUSH command %s\n", cmd);
	return(1);
    }
    return(0);
}

/*
 * eof
 */
