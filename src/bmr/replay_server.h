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
 * $Id: replay_server.h 583 2010-08-03 21:12:57Z gbc $
 *
 * BMR Server customized for replay case.
 */

#ifndef replay_server_h
#define replay_server_h

#include "bmr_server.h"
#include "load_task.h"
#include "push_task.h"

/* Executes the cached commands */
extern int replay_exec_cache(BMRServer *bs, LoadState *ls, PushState *ps);

/*
 * Entries to the main dispatch loop.  Returns
 *  0 after normal service
 *  nonzero after some error....
 */
/* LOAD + PUSH server */
extern int bmr_replay_dispatch(void);

/*
 * Various diagnositics
 */
int replay_resp_show(char *resp);
int replay_resp_info(char *resp);
int replay_resp_counters(char *resp);
int replay_resp_rates(char *resp);

#endif /* replay_server_h */

/*
 * eof
 */
