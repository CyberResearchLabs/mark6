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
 * $Id: record_main.c 539 2010-07-26 21:10:25Z gbc $
 *
 * The bmr_record task which combines GRAB + PUSH into a single server.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "bmr_server.h"
#include "grab_task.h"
#include "save_task.h"

#include "record_server.h"

/*
 * Global options
 */
int	    verb = 0;

/*
 * Private data
 */
static char	motd[BMR_MAX_MESSAGE];

/*
 * Boilerplate
 */
static int cmdhelp = 0;
static int usage(char *name)
{
    printf("Usage: %s [options]\n", name);
    printf( "where the options are:\n"
	    "  -v            verbose, may be repeated for more\n"
	    "  -c cmd        initialization command\n"
	    "  -f file       file of initialization commands\n"
	    "\n"
	    "The -c and -f arguments allow %s to be configured\n"
	    "before any clients attach.  Both are repeatable, and\n"
	    "will provoke a fatal exit if not properly interpreted.\n",
	    name
    );
    return(cmdhelp = 1);
}
static int version(char **argv)
{
    if (!argv[1]) return(0);
    if (!strcmp(argv[1], "--help"))    return(usage(argv[0]));
    if ( strcmp(argv[1], "--version")) return(0);
    printf(__FILE__ "\t[" __DATE__ " " __TIME__ "]\n");
    return(cmdhelp = 1);
}
static int options(int argc, char **argv)
{
    int	    c;
    if (version(argv)) return(1);
    while ((c = getopt(argc, argv, "vc:f:")) != -1) switch(c) {
    case 'v': verb++;						break;
    case 'c': if (bmr_parse_cache_cmd(optarg)) return(1);	break;
    case 'f': if (bmr_parse_cache_file(optarg))	return(2);	break;
    default:							return(3);
    }
    return(0);
}
static int
cmdline(int *argc, char ***argv)
{
    int	    x = options(*argc, *argv);
    *argc -= optind;
    *argv += optind;
    return(x);
}

/*
 * Main Entry.
 */
int main(int argc, char **argv)
{
    int		errs = 0;
    BMRServer	*bsp = get_bmr_server_state();
    GrabState	*gsp = get_grab_state();
    SaveState	*ssp = get_save_state();

    /* set internal defaults to precede cached commands */
    errs += bmr_parse_cache_cmd("server:default");
    errs += bmr_parse_cache_cmd("grab:default");
    errs += bmr_parse_cache_cmd("save:default");
    errs += snprintf(bsp->motd = motd, BMR_MAX_MESSAGE,
	"%s at your service.\n", argv[0]) ? 0 : 1;

    /* basic command line parsing */
    if (cmdline(&argc, &argv)) return(!cmdhelp);

    /* various initializations */
    bsp->verb = gsp->verb = ssp->verb = verb;
    errs += record_exec_cache(bsp, gsp, ssp);
    if (errs) return(fprintf(stderr, "Initialization problem (%d)\n", errs));
    
    /* implicit *:apply in proper order */
    errs += set_bmr_server_state(bsp);
    if (errs) return(fprintf(stderr, "set_bmr_server_state() (%d)\n", errs));
    errs += set_grab_state(gsp);
    if (errs) return(fprintf(stderr, "set_grab_state() failed (%d)\n", errs));
    errs += set_save_state(ssp);
    if (errs) return(fprintf(stderr, "set_save_state() failed (%d)\n", errs));

    /* go into service */
    errs = bmr_record_dispatch();
    if (errs) fprintf(stderr, "bmr_record_dispatch() failed (%d)\n", errs);
    return(errs);
}

/*
 * eof
 */
