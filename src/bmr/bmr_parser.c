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
 * $Id: bmr_parser.c 630 2010-09-13 21:18:06Z gbc $
 *
 * Configuration for the server
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bmr_util.h"
#include "bmr_server.h"

/*
 * Set some (harmless) defaults
 */
void set_default_bmr_server_state(BMRServer *bs)
{
    if (bs->verb>0) fputs("SRVR default state\n", stderr);
    bs->std_input = 1;
    bs->tcp_port = BMR_TCP_SERVER_PORT;
    bs->max_clients = 10;
    bs->lfile = 0;
    bs->dfile = 0;
    bs->cmd_sock = -1;
    bs->nfds = -1;
    bs->sel_to.tv_sec = 0;
    bs->sel_to.tv_usec = 500000;
}

/*
 * Dump the current state as a set of commands (for possible reuse)
 */
void bmr_server_dump(FILE *fp, BMRServer *bs)
{
    fputs("##\n## SRVR commands\n##\n", fp);
    fprintf(fp, "server:std_input=%d\n", bs->std_input);
    fprintf(fp, "server:tcp_port=%d\n", bs->tcp_port);
    fprintf(fp, "server:max_clients=%d\n", bs->max_clients);
    fprintf(fp, "server:sel_to=" PRI_TV "\n",
	bs->sel_to.tv_sec, bs->sel_to.tv_usec);
    fprintf(fp, "server:lfile=%s\n", bs->lfile);
    fprintf(fp, "server:dfile=%s\n", bs->dfile);
}

/*
 * Show the current state
 */
void desc_bmr_server_state(BMRServer *bs, char *buf)
{
    int		xx, nb, tb = BMR_MAX_MESSAGE;
    nb = snprintf(buf, tb, "SRVR state:\n"
	"  std_input=%d tcp_port=%d max_clients=%d verb=%d"
	 " sel_to=" PRI_TV "\n"
	"  lfile=%s dfile=%s\n"
	"  cmd_sock:%d clients:%d nfds:%d"
	 " cl_sock:[",
	bs->std_input, bs->tcp_port, bs->max_clients, bs->verb,
	bs->sel_to.tv_sec, bs->sel_to.tv_usec,
	bs->lfile, bs->dfile,
	bs->cmd_sock, bs->clients, bs->nfds
    );
    for (buf+=nb, tb-=nb, xx=0; tb>0 && xx<bs->clients; xx++, buf+=nb, tb-=nb)
	nb = snprintf(buf, tb, " %d", bs->cl_sock[xx]);
    nb = snprintf(buf, tb, " ]\n");
}

/*
 * Provide some commanding help.
 */
int bmr_server_help(char *buf)
{
    return(snprintf(buf, BMR_MAX_MESSAGE,
	"The SRVR configuration commands are:\n"
	"\tserver:std_input=<int>   # nonzero to connect stdio/stdout\n"
	"\tserver:tcp_port=<int>    # TCP port server listens on (%d)\n"
	"\tserver:max_clients=<int> # max number of TCP/UDP fd's\n"
	"\tserver:sel_to=<float>    # max commanding latency (<1.0s)\n"
	"\tserver:lfile=<file>      # file for general status logging\n"
	"\tserver:dfile=<file>      # file for server dispatch debugging\n"
	"\n"
	"\tserver:help              # provides this message\n"
	"\tserver:show              # displays SRVR state\n"
	"\tserver:default           # resets to a default state\n"
	"\tserver:apply             # applies all commands\n"
	"\tserver:quit              # provokes a fatal error\n"
	, BMR_TCP_SERVER_PORT
    ));
}

/*
 * A rather primitive parser.
 */
int bmr_server_config(char *cmd, BMRServer *bs, char *buf)
{
    long tvusec;
    *buf = 0;
    if (!(cmd = skip_leading_space(cmd))) return(0);

    if        (!strncmp(cmd, "show", 4)) {
	desc_bmr_server_state(bs, buf);
    } else if (!strncmp(cmd, "help", 4)) {
	bmr_server_help(buf);
    } else if (!strncmp(cmd, "default", 7)) {
	set_default_bmr_server_state(bs);
    } else if (!strncmp(cmd, "apply", 5)) {
	return(set_bmr_server_state(bs));
    } else if (!strncmp(cmd, "quit", 4)) {
	return(-1);

    } else if (!strncmp(cmd, "std_input", 9)) {
	    bs->std_input = atoi(skip_leading_space(cmd+8));
    } else if (!strncmp(cmd, "tcp_port", 8)) {
	    bs->tcp_port = atoi(skip_leading_space(cmd+8));
    } else if (!strncmp(cmd, "max_clients", 11)) {
	    bs->max_clients = atoi(skip_leading_space(cmd+11));
	    if (bs->max_clients > BMR_MAX_CLIENT_SOCKETS)
		bs->max_clients = BMR_MAX_CLIENT_SOCKETS;
    } else if (!strncmp(cmd, "sel_to", 6)) {
	if (1 == sscanf(skip_leading_space(cmd+6), "%*d.%ld", &tvusec))
	    bs->sel_to.tv_usec = tvusec;
    } else if (!strncmp(cmd, "lfile", 5)) {
	if ((cmd = skip_leading_space(cmd+5))) {
	    bs->lfile = malloc(strlen(cmd));
	    strcpy(bs->lfile, cmd);
	}
    } else if (!strncmp(cmd, "dfile", 5)) {
	if ((cmd = skip_leading_space(cmd+5))) {
	    bs->dfile = malloc(strlen(cmd));
	    strcpy(bs->dfile, cmd);
	}

    /* additional server configuration commands go here */

    } else {
	snprintf(buf, BMR_MAX_MESSAGE, "Unparsed SRVR command %s\n", cmd);
	return(1);
    }
    return(0);
}

/*
 * eof
 */
