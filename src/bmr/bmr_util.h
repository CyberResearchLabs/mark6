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
 * $Id: bmr_util.h 979 2011-02-04 20:14:26Z gbc $
 *
 * BMR Server utilities
 */

#ifndef bmr_util_h
#define bmr_util_h

#include "bmr_common.h"

#define TIMESTYLE_SECS	0   /* UNIX system clock */
#define TIMESTYLE_WEEK	1   /* [+|-]SS[.ss] */
#define TIMESTYLE_DOT	2   /* [YY]YYMMDDHHMMSS[.ss] */
#define TIMESTYLE_VEX	3   /* [YY]YYyDOYdHHhMMmSS[.ss] */
#define TIMESTYLE_VDIF	4   /* XX@SS.ss */
#define TIME_BUFFER_SIZE    40

/* Parses and input time and returns seconds on the UNIX clock */
extern int timevdifepoch;   /* 6-month periods from 2000 < 2032 */
extern int timetruncate;    /* whether to truncate or round */
extern int time_TZ_not_UTC;    /* means TZ may not be UTC */

/* front end parsers */
extern time_t timeis(char *t);	    /* just the seconds */
extern TimeSpec timens(char *t);    /* full nanoseconds */

/* Formats the time (several formats) by style */
extern char *timefmtTT(time_t tt, char *buf, int style);
extern char *timefmtTV(TimeVal tv, char *buf, int style);
extern char *timefmtTS(TimeSpec ts, char *buf, int style);
/* using these: */
extern char *timeTS2secsstr(TimeSpec ts, char *buf);
extern char *timeTS2weekstr(TimeSpec ts, char *buf);
extern char *timeTS2dot_str(TimeSpec ts, char *buf);
extern char *timeTS2vex_str(TimeSpec ts, char *buf);
extern char *timeTS2vdifstr(TimeSpec ts, char *buf);
/* with style determined by */
extern int timestyle(char *t);
extern void time_ns_precision(int n);	/* how many (1..9) digits to print */

/* TimeSpec utility: *when -= *delta (*delta>0, *when>0), returns when */
extern TimeSpec *time_decrement(TimeSpec *when, TimeSpec *delta);
/* TimeSpec utility: *when += *delta (*delta>0, *when>0), returns when */
extern TimeSpec *time_increment(TimeSpec *when, TimeSpec *delta);

/*
 * Advances the command pointer string to something interesting
 * Returns 0 if nothing (of interest) is left.
 */
extern char *skip_leading_space(char *cmd);
extern void nuke_trailing_space(char *cmd);

/*
 * Report resource limits to stderr.
 */
extern void rlimit_env(void);

#endif /* bmr_util_h */

/*
 * eof
 */
