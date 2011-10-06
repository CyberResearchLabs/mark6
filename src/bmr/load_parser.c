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
 * $Id: load_parser.c 984 2011-02-08 17:02:31Z gbc $
 *
 * LOAD configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_files.h"
#include "bmr_util.h"
#include "bmr_scan.h"
#include "load_task.h"

/*
 * Set some (harmless) defaults
 * FIXME: tune this up to 8Gbps burst.
 */
void set_default_load_state(LoadState *ls)
{
    if (ls->verb>0) fputs("LOAD default state\n", stderr);
    ls->load_type = BMR_TYPE_RAID0;

    curr_expr_name("EXPR");
    curr_stn_name("STN");
    curr_scan_name("scan");
    curr_scan_suffix("");
    ls->scan_label = curr_scan_label(0);

    ls->load_size = 8224;
    ls->load_rate = 4;

    ls->num_files = 1;
    ls->file_index = 0;
    if (!ls->path[0]) ls->path[0] = malloc(8);
    if (ls->path[0]) snprintf(ls->path[0], 8, ".");

    ls->next_seqn = BMR_SEQN_ATBIRTH;
    ls->mem_type = BMR_MEM_EDVS;
    ls->load_start = time(0) + 15;
    ls->load_secs = 5;
    ls->load_period = 15;
    ls->load_stop = ls->load_start + 4 * ls->load_period;

    ls->action = LOAD_ACT_INIT;
    ls->file_fmt = BMR_FILE_RAW;
}

/*
 * Dump the current state as a set of commands (for possible reuse)
 */
void load_dump(FILE *fp, LoadState *ls)
{
    int	lf;
    char *slsh;
    fputs("##\n## LOAD commands\n##\n", fp);
    fprintf(fp, "load:load_type=%d\n", ls->load_type);
    fprintf(fp, "load:expr_name=%s\n", curr_expr_name(0));
    fprintf(fp, "load:stn_name=%s\n", curr_stn_name(0));
    fprintf(fp, "load:scan_name=%s\n", curr_scan_name(0));
    fprintf(fp, "load:scan_suffix=%s\n", curr_scan_suffix(0));
    fprintf(fp, "load:scan_label=%s\n", curr_scan_label(0));
    fprintf(fp, "load:num_files=%d\n", ls->num_files);
    fprintf(fp, "load:file_index=%d\n", ls->file_index);
    for (lf = 0; lf < ls->num_files; lf++) {
	slsh = strrchr(ls->path[lf], '/');
	if (slsh) *slsh = 0;
	fprintf(fp, "load:path[%d]=%s\n", lf, ls->path[lf]);
	if (slsh) *slsh = '/';
    }
    fprintf(fp, "load:rate=%ld\n", ls->load_rate);
    fprintf(fp, "load:mbss=%g\n", ls->load_rate*ls->load_size*8.0e-6);
    fprintf(fp, "load:gbss=%g\n", ls->load_rate*ls->load_size*8.0e-9);
    fprintf(fp, "load:type=%d\n", ls->mem_type);
    fprintf(fp, "load:start=%ld\n", ls->load_start);
    fprintf(fp, "load:stop=%ld\n", ls->load_stop);
    fprintf(fp, "load:secs=%d\n", ls->load_secs);
    fprintf(fp, "load:period=%d\n", ls->load_period);
    fprintf(fp, "load:file_fmt=%d\n:", ls->file_fmt);
}

/*
 * Provide some help
 */
int load_help(char *buf)
{
    return(snprintf(buf, BMR_MAX_MESSAGE,
	"The LOAD configuration commands are:\n"
	"\tload:load_type=<int>     # (0|1|2) for (Files|Raid0|Raid5)\n"
	"\tload:scan_label=<string> # name of the scan (for files)\n"
	"\tload:expr_name=<string>  # name of the experiment\n"
	"\tload:stn_name=<string>   # name of the station (files/vdif)\n"
	"\tload:scan_name=<string>  # name of the scan (for files)\n"
	"\tload:scan_suffix=<string># optional suffix to scan label\n"
	"\tload:num_files=<int>     # number of output files to use\n"
	"\tload:file_index=<int>    # first file index to read\n"
	"\tload:path[N]=<string>    # path to the Nth directory\n"
	"\tload:rate=<long>         # specify packet rate (pps)\n"
	"\tload:mbps=<float>        # specify packet rate (Mbps)\n"
	"\tload:gbps=<float>        # specify packet rate (Gbps)\n"
	"\tload:type=<int>          # memory storage plan\n"
	"\tload:start=<time>        # specify starting time\n"
	"\tload:stop=<time>         # specify stopping time\n"
	"\tload:secs=<int>          # specify reading duration (secs)\n"
	"\tload:period=<int>        # specify cycle duration (secs)\n"
	"\tload:file_fmt=<int>      # 0,1 ...\n"
	"\n"
	"\tload:help                # provides this message\n"
	"\tload:show                # displays LOAD state\n"
	"\tload:default             # resets to a default state\n"
	"\tload:apply               # applies all commands\n"
	"\tload:quit                # provokes a fatal error\n"
    ));
}

/*
 * A rather primitive parser
 */
int load_config(char *cmd, LoadState *ls, char *buf)
{
    double  temp;
    int	    np;
    char    *pt;

    *buf = 0;
    if (!(cmd = skip_leading_space(cmd))) return(0);

    if        (!strncmp(cmd, "show", 4)) {
	desc_load_state(ls, buf);
    } else if (!strncmp(cmd, "help", 4)) {
	load_help(buf);
    } else if (!strncmp(cmd, "default", 7)) {
	set_default_load_state(ls);
    } else if (!strncmp(cmd, "apply", 5)) {
	return(set_load_state(ls));
    } else if (!strncmp(cmd, "quit", 4)) {
	return(-1);

    } else if (!strncmp(cmd, "type=", 5)) {
	ls->load_type = atoi(cmd+5);
    } else if (!strncmp(cmd, "scan_label=", 11)) {
	ls->scan_label = curr_scan_label(cmd+11);
	if (!ls->scan_label) (perror("scan_label:realloc"),1);
    } else if (!strncmp(cmd, "expr_name=", 10)) {
	curr_expr_name(cmd+10);
    } else if (!strncmp(cmd, "stn_name=", 9)) {
	curr_stn_name(cmd+9);
    } else if (!strncmp(cmd, "scan_name=", 10)) {
	curr_scan_name(cmd+10);
    } else if (!strncmp(cmd, "scan_suffix=", 12)) {
	curr_scan_suffix(cmd+12);

    } else if (!strncmp(cmd, "num_files=", 10)) {
	ls->num_files = atoi(cmd+10);
	if (ls->num_files > BMR_MAX_FILES) ls->num_files = BMR_MAX_FILES;
	for (np = 0; np < ls->num_files; np++) {
	    if (!ls->path[np]) ls->path[np] = malloc(8);
	    if (ls->path[np]) snprintf(ls->path[np], 8, ".");
	}
    } else if (!strncmp(cmd, "file_index=", 11)) {
	ls->file_index = atoi(cmd+11);

    } else if (!strncmp(cmd, "path[", 5)) {
	if ((pt = malloc(strlen(cmd))) &&
	    (2 == sscanf(cmd, "path[%d]=%s", &np, pt)))
		ls->path[np] = pt;
    } else if (!strncmp(cmd, "rate=", 5)) {
	ls->load_rate = atol(cmd+5);
    } else if (!strncmp(cmd, "mbps=", 5)) {
	if (ls->load_size == 0) return(2);
	temp = atof(cmd+5) * 1000000.0 / 8.0;		/* Bps */
	ls->load_rate = floor(temp / (ls->load_size));	/* Pps */
    } else if (!strncmp(cmd, "gbps=", 5)) {
	if (ls->load_size == 0) return(2);
	temp = atof(cmd+5) * 1000000000.0 / 8.0;	/* Bps */
	ls->load_rate = floor(temp / (ls->load_size));	/* Pps */

    } else if (!strncmp(cmd, "file_fmt=", 9)) {
	ls->file_fmt = atoi(cmd+9);

    } else if (!strncmp(cmd, "type=", 5)) {
	ls->mem_type = atoi(cmd+5);
    } else if (!strncmp(cmd, "start=", 6)) {
	ls->load_start = timeis(cmd+6);
    } else if (!strncmp(cmd, "stop=", 5)) {
	ls->load_stop = timeis(cmd+5);
    } else if (!strncmp(cmd, "secs=", 5)) {
	ls->load_secs = atoi(cmd+5);
    } else if (!strncmp(cmd, "period=", 7)) {
	ls->load_period = atoi(cmd+7);

    } else if (!strncmp(cmd, "action=init", 11)) {
	ls->action = LOAD_ACT_INIT;
    } else if (!strncmp(cmd, "action=idle", 11)) {
	ls->action = LOAD_ACT_IDLE;
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "Unparsed LOAD command %s\n", cmd);
	return(1);
    }
    return(0);
}

/*
 * eof
 */
