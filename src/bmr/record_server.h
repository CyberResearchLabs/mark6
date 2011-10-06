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
 * $Id: record_server.h 1298 2011-08-01 21:20:02Z gbc $
 *
 * BMR Server customized for record case.
 */

#ifndef record_server_h
#define record_server_h

#include "bmr_server.h"
#include "grab_task.h"
#include "save_task.h"

/* Executes the cached commands */
extern int record_exec_cache(BMRServer *bs, GrabState *gs, SaveState *ss);

/*
 * Entries to the main dispatch loop.  Returns
 *  0 after normal service
 *  nonzero after some error....
 */
/* GRAB + SAVE server */
extern int bmr_record_dispatch(void);

/*
 * Various diagnositics
 */
int record_resp_show(char *resp);
int record_resp_info(char *resp);
int record_resp_counters(char *resp);
int record_resp_basics(char *resp);
int record_resp_rates(char *resp);

#endif /* record_server_h */

/*
 * eof
 */
