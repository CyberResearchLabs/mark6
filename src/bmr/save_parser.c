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
 * $Id: save_parser.c 1298 2011-08-01 21:20:02Z gbc $
 *
 * SAVE configuration support.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bmr_files.h"
#include "bmr_util.h"
#include "bmr_scan.h"
#include "save_task.h"

/*
 * Set some (harmless) defaults
 * FIXME: tune this up to 8Gbps burst.
 */
void set_default_save_state(SaveState *ss)
{
    if (ss->verb>0) fputs("SAVE default state\n", stderr);
    ss->save_type = BMR_TYPE_RAID0;

    curr_expr_name("EXPR");
    curr_stn_name("STN");
    curr_scan_name("scan");
    curr_scan_suffix("");
    ss->scan_label = curr_scan_label(0);

    ss->save_size = 5008;
    ss->save_rate = 4;

    ss->num_files = 1;
    if (!ss->path[0]) ss->path[0] = malloc(8);
    if (ss->path[0]) snprintf(ss->path[0], 8, ".");
    ss->save_anal = 0;

    ss->overlap = 0;
    ss->ppgroup = 1;
    ss->max_cycles = 1;

    ss->file_index = 0;

    ss->action = SAVE_ACT_INIT;
    ss->file_fmt = BMR_FILE_RAW;
}

/*
 * Dump the current state as a set of commands (for possible reuse)
 */
void save_dump(FILE *fp, SaveState *ss)
{
    int	sf;
    char *slsh;
    fputs("##\n## SAVE commands\n##\n", fp);
    fprintf(fp, "save:save_type=%d\n", ss->save_type);
    fprintf(fp, "save:expr_name=%s\n", curr_expr_name(0));
    fprintf(fp, "save:stn_name=%s\n", curr_stn_name(0));
    fprintf(fp, "save:scan_name=%s\n", curr_scan_name(0));
    fprintf(fp, "save:scan_suffix=%s\n", curr_scan_suffix(0));
    fprintf(fp, "save:scan_label=%s\n", curr_scan_label(0));
    fprintf(fp, "save:num_files=%d\n", ss->num_files);
    for (sf = 0; sf < ss->num_files; sf++) {
	slsh = strrchr(ss->path[sf], '/');
	if (slsh) *slsh = 0;
	fprintf(fp, "save:path[%d]=%s\n", sf, ss->path[sf]);
	if (slsh) *slsh = '/';
    }
    fprintf(fp, "save:anal=%d\n", ss->save_anal);
    fprintf(fp, "save:rate=%ld\n", ss->save_rate);
    fprintf(fp, "save:mbss=%g\n", ss->save_rate*ss->save_size*8.0e-6);
    fprintf(fp, "save:gbss=%g\n", ss->save_rate*ss->save_size*8.0e-9);
    fprintf(fp, "save:ppgroup=%d\n", ss->ppgroup);
    fprintf(fp, "save:file_fmt=%d\n", ss->file_fmt);
}

/*
 * Provide some help
 */
int save_help(char *buf)
{
    return(snprintf(buf, BMR_MAX_MESSAGE,
	"The SAVE configuration commands are:\n"
	"\tsave:save_type=<int>     # (0|1|2) for (Files|Raid0|Raid5)\n"
	"\tsave:scan_label=<string> # name of the scan for files\n"
	"\tsave:expr_name=<string>  # name of the experiment\n"
	"\tsave:stn_name=<string>   # name of the station (files/vdif)\n"
	"\tsave:scan_name=<string>  # name of the scan (for files)\n"
	"\tsave:scan_suffix=<string># optional suffix to scan label\n"
	"\tsave:num_files=<int>     # number of output files to use\n"
	"\tsave:save_anal=<int>     # number of packets to analyze\n"
	"\tsave:path[N]=<string>    # path to the Nth directory\n"
	"\tsave:rate=<long>         # specify packet rate (pps)\n"
	"\tsave:mbps=<float>        # specify packet rate (Mbps)\n"
	"\tsave:gbps=<float>        # specify packet rate (Gbps)\n"
	"\tsave:ppgroup=<int>       # number of packets to write together\n"
	"\tsave:file_fmt=<int>      # 0: raw packet with seq nums\n"
	"\t                         # 1: pkt packet only is saved\n"
	"\t                         # 2: vdf version 0 VDIF\n"
	"\t                         # 3: vdx extended VDIF\n"
	"\n"
	"\tsave:help                # provides this message\n"
	"\tsave:show                # displays SAVE state\n"
	"\tsave:default             # resets to a default state\n"
	"\tsave:apply               # applies all commands\n"
	"\tsave:quit                # provokes a fatal error\n"
    ));
}

/*
 * A rather primitive parser
 */
int save_config(char *cmd, SaveState *ss, char *buf)
{
    double  temp;
    int	    np, nb;

    *buf = 0;
    if (!(cmd = skip_leading_space(cmd))) return(0);

    if        (!strncmp(cmd, "show", 4)) {
	desc_save_state(ss, buf);
    } else if (!strncmp(cmd, "help", 4)) {
	save_help(buf);
    } else if (!strncmp(cmd, "default", 7)) {
	set_default_save_state(ss);
    } else if (!strncmp(cmd, "apply", 5)) {
	return(set_save_state(ss));
    } else if (!strncmp(cmd, "quit", 4)) {
	return(-1);

    } else if (!strncmp(cmd, "type=", 5)) {
	ss->save_type = atoi(cmd+5);
    } else if (!strncmp(cmd, "scan_label=", 11)) {
	ss->scan_label = set_scan_label(cmd+11);
	if (!ss->scan_label) return(perror("scan_label:realloc"),1);
	ss->station = get_station_bits();
    } else if (!strncmp(cmd, "expr_name=", 10)) {
	curr_expr_name(cmd+10);
    } else if (!strncmp(cmd, "stn_name=", 9)) {
	curr_stn_name(cmd+9);
	ss->station = get_station_bits();
    } else if (!strncmp(cmd, "scan_name=", 10)) {
	curr_scan_name(cmd+10);
    } else if (!strncmp(cmd, "scan_suffix=", 12)) {
	curr_scan_suffix(cmd+12);

    } else if (!strncmp(cmd, "num_files=", 10)) {
	ss->num_files = atoi(cmd+10);
	if (ss->num_files > BMR_MAX_FILES) ss->num_files = BMR_MAX_FILES;
	for (np = 0; np < ss->num_files; np++) {
	    if (!ss->path[np]) ss->path[np] = malloc(8);
	    if (ss->path[np]) snprintf(ss->path[np], 8, ".");
	    else return(perror("num_files:malloc"),1);
	}
    } else if (!strncmp(cmd, "save_anal=", 10)) {
	ss->save_anal = atoi(cmd+10);

    } else if (!strncmp(cmd, "path[", 5)) {
	if (1 == sscanf(cmd, "path[%d]=%n", &np, &nb)) {
	    ss->path[np] = realloc(ss->path[np], strlen(cmd));
	    if (ss->path[np]) strcpy(ss->path[np], cmd+nb);
	    else return(perror("path:realloc"),1);
	}

    } else if (!strncmp(cmd, "rate=", 5)) {
	ss->save_rate = atol(cmd+5);
    } else if (!strncmp(cmd, "mbps=", 5)) {
	if (ss->save_size == 0) return(2);
	temp = atof(cmd+5) * 1000000.0 / 8.0;		/* Bps */
	ss->save_rate = floor(temp / (ss->save_size));	/* Pps */
    } else if (!strncmp(cmd, "gbps=", 5)) {
	if (ss->save_size == 0) return(2);
	temp = atof(cmd+5) * 1000000000.0 / 8.0;	/* Bps */
	ss->save_rate = floor(temp / (ss->save_size));	/* Pps */

    } else if (!strncmp(cmd, "ppgroup=", 8)) {
	ss->ppgroup = atoi(cmd+8);
    } else if (!strncmp(cmd, "file_fmt=", 9)) {
	ss->file_fmt = atoi(cmd+9);

    } else if (!strncmp(cmd, "action=init", 11)) {
	ss->action = SAVE_ACT_INIT;
    } else if (!strncmp(cmd, "action=idle", 11)) {
	ss->action = SAVE_ACT_IDLE;
    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "Unparsed SAVE command %s\n", cmd);
	return(1);
    }
    return(0);
}

/*
 * eof
 */
