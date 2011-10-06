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
 * $Id: buffer_server.h 526 2010-07-23 17:00:55Z gbc $
 *
 * BMR Server customized for buffer case.
 */

#ifndef buffer_server_h
#define buffer_server_h

#include "bmr_server.h"
#include "grab_task.h"
#include "push_task.h"

/* Executes the cached commands */
extern int buffer_exec_cache(BMRServer *bs, GrabState *gs, PushState *ps);

/*
 * Entries to the main dispatch loop.  Returns
 *  0 after normal service
 *  nonzero after some error....
 */
/* GRAB + PUSH server */
extern int bmr_buffer_dispatch(void);

/*
 * Various diagnositics
 */
int buffer_resp_show(char *resp);
int buffer_resp_info(char *resp);
int buffer_resp_counters(char *resp);
int buffer_resp_rates(char *resp);

#endif /* buffer_server_h */

/*
 * eof
 */
