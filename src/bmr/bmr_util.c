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
 * $Id: bmr_util.c 983 2011-02-04 21:52:05Z gbc $
 *
 * BMR Server misc utilities
 */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/resource.h>

#include "bmr_util.h"

/*
 * TimeSpec utility: *when -= *delta, returns when
 * It is assumed both when and delta are legal.
 */
TimeSpec *time_decrement(TimeSpec *when, TimeSpec *delta)
{
    when->tv_sec  -= delta->tv_sec;
    when->tv_nsec -= delta->tv_nsec;
    if (when->tv_nsec < 0L) {
	when->tv_nsec += BMR_ONENANOSECOND;
	when->tv_sec  -= 1;
    }
    return(when);
}

/*
 * TimeSpec utility: *when += *delta, returns when
 * It is assumed both when and delta are legal.
 */
TimeSpec *time_increment(TimeSpec *when, TimeSpec *delta)
{
    when->tv_sec  += delta->tv_sec;
    when->tv_nsec += delta->tv_nsec;
    if (when->tv_nsec > BMR_ONENANOSECOND) {
	when->tv_sec  += 1;
	when->tv_nsec -= BMR_ONENANOSECOND;
    }
    return(when);
}

/*
 * Get rid of the newline, &c.
 */
void nuke_trailing_space(char *cmd)
{
    char *end = strrchr(cmd, '\n');
    if (!end) return;
    *end = 0;	/* nuke the newline */
    while (isspace(*end) && end > cmd) end--;
    *++end = 0;
}

/*
 * Advances the command pointer string to something interesting
 * Returns 0 if nothing (of interest) is left.
 */
char *skip_leading_space(char *cmd)
{
    if (!cmd) return(0);

    /* allow #-comments */
    if (*cmd == '#') return(0);

    /* skip space and punctuation */
    while (*cmd && (isspace(*cmd) || ispunct(*cmd))) cmd++;

    /* allow blank lines */
    if (*cmd == '\n') return(0);

    /* allow blank lines */
    if (!*cmd) return(0);

    return(cmd);
}

/*
 * Report resource limits to stderr
 */
void rlimit_env(void)
{
    struct rlimit   ras;
    void	    *one = malloc(1), *two = malloc(1);

    getrlimit(RLIMIT_AS, &ras);
    fprintf(stderr, "RLIMIT_AS is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_DATA, &ras);
    fprintf(stderr, "RLIMIT_DATA is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_FSIZE, &ras);
    fprintf(stderr, "RLIMIT_FSIZE is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_MEMLOCK, &ras);
    fprintf(stderr, "RLIMIT_MEMLOCK is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_NOFILE, &ras);
    fprintf(stderr, "RLIMIT_NOFILE is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_RTPRIO, &ras);
    fprintf(stderr, "RLIMIT_RTPRIO is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);
    getrlimit(RLIMIT_STACK, &ras);
    fprintf(stderr, "RLIMIT_STACK is %lX,%lX\n", ras.rlim_cur, ras.rlim_max);

    fprintf(stderr, "malloc %p/%p, sbrk_cur %p\n", one, two, sbrk(0));
    free(one);
    free(two);
}

/*
 * eof
 */
