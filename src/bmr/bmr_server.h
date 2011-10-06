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
 * $Id: bmr_server.h 593 2010-08-09 15:56:11Z gbc $
 *
 * External interfaces for BMR servers.
 */

#ifndef bmr_server_h
#define bmr_server_h

#include "bmr_common.h"

/* not too large and not too small */
#define BMR_MAX_CLIENT_SOCKETS  20
#define BMR_TCP_SERVER_PORT	2651

/* different types of select() clients */
#define BMR_CLIENT_TYPE_NOT_DEF 0
#define BMR_CLIENT_TYPE_COMMAND 1
#define BMR_CLIENT_TYPE_MONITOR 2
#define BMR_CLIENT_TYPE_LISTEN  3
#define BMR_CLIENT_TYPE_UDP_IN  4
#define BMR_CLIENT_TYPE_UDP_OUT 5
#define BMR_CLIENT_TYPE_TCP_OUT 6
#define BMR_CLIENT_TYPE_PKTSAVE 7
#define BMR_CLIENT_TYPE_PKTLOAD 8

/*
 * General server working variables
 */
typedef struct bmr_server {
    int	    std_input;	    /* allow commands on stdin/0 */
    int	    tcp_port;	    /* network port to listen on */
    int	    max_clients;    /* max num of clients allowed */
    TimeVal sel_to;	    /* timeout limit on select call */
    int	    verb;	    /* verbosity */
    char    *lfile;	    /* file for general logging */
    char    *dfile;	    /* file for dispatch debugging */
    /* working variables */
    int	    cmd_sock;	    /* socket to listen for clients on, */
    int	    clients;	    /* and number clients connected with. */
    char    *motd;	    /* greeting for the clients */
    int	    cl_io;	    /* index of client performing i/o */
    int	    nfds;	    /* max fd seen (read or write) among: */
    int	    cl_sock[BMR_MAX_CLIENT_SOCKETS];
    int	    cl_type[BMR_MAX_CLIENT_SOCKETS];
    Handler **cl_func[BMR_MAX_CLIENT_SOCKETS];
} BMRServer;

/*
 * The following generic and are in bmr_server.c
 */

/* externalized for custom dispatching */
extern BMRServer bss;

/*
 * Functions to get and update the server parameters.  The server
 * actually goes into service with one of the dispatch calls.
 */
extern BMRServer *get_bmr_server_state(void);
extern int set_bmr_server_state(BMRServer *bs);

/*
 * Server setup and dispatch supporting functions
 * We need the ** on the handlers as the internal state machines
 * can asynchronously adjust the handler responsible for the fd.
 */
extern int bmr_add_read_client(int ifd, int type, Handler **ioh, fd_set *rfds);
extern int bmr_add_write_client(int ofd, int type, Handler **ioh, fd_set *wfds);
extern int bmr_server_prep(fd_set *rfds, fd_set *wfds);
extern char *show_bmr_bits(fd_set *msk, char *buf, char hit);

/*
 * Routine to setup tcp server socket and related support.
 * Returns the socket on success, something <0 on failure.
 */
extern int bmr_listener(fd_set *rfds, fd_set *wfds, Handler **, Handler **);
extern ssize_t bmr_accept(void);
extern void bmr_sub_client(int cl);

/*
 * Some generic command caching utilities (bmr_cache.c)
 */
extern int bmr_parse_cache_cmd(char *cmd);
extern int bmr_parse_cache_file(char *file);
extern void bmr_show_cached_cmds(void);
extern char *bmr_cached_cmds_ptr(void);
extern void bmr_cache_free(void);

/*
 * Configuration and description commands (bmr_parser.c).
 */
extern int bmr_server_config(char *cmd, BMRServer *bs, char *buf);
extern void set_default_bmr_server_state(BMRServer *bs);
extern void bmr_server_dump(FILE *fp, BMRServer *bs);

extern void desc_bmr_server_state(BMRServer *bs, char *buf);
extern int bmr_server_help(char *buf);

/*
 * Server-specific code is in *_server.h
 * 
// FAKE + PUSH server
// extern int bmr_faker_dispatch(void);
 *
 * And the command-parser is in *_parser.h:
 */
extern int (* const bmr_response)(char *request, char *response);

#endif /* bmr_server_h */

/*
 * eof
 */
