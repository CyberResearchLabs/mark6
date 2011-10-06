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
 * $Id: replay_info.c 592 2010-08-09 15:54:42Z gbc $
 *
 * REPLAY specific information displays.
 */

#include <stdio.h>
#include <string.h>

#include "bmr_util.h"
#include "bmr_server.h"
#include "load_task.h"
#include "push_task.h"

#include "replay_server.h"

/*
 * Various reporting functions
 */
int replay_resp_show(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    load_config("show", get_load_state(), bbb[1]);
    push_config("show", get_push_state(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
int replay_resp_info(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    desc_load_info(get_load_info(), bbb[1]);
    desc_push_info(get_push_info(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
int replay_resp_counters(char *resp)
{
    static char bbb[7][BMR_MAX_MESSAGE];
    LoadInfo *lip = get_load_info();
    PushInfo *pip = get_push_info();
    desc_load_rate(" LoadRT", lip, bbb[4]);
    desc_push_rate(" SendRT", pip, bbb[5]);
    desc_load_total(" Active", &lip->reading, bbb[0]);
    desc_load_total("Passive", &lip->idling, bbb[1]);
    desc_push_total("Sending", &pip->sending, bbb[2]);
    desc_push_total(" Idling", &pip->idling, bbb[3]);
    desc_push_tosn("TooSoon", &pip->tosn, bbb[6]);
    return(snprintf(resp, BMR_MAX_MESSAGE, PRI_TS " %s" PRI_TS " %s"
	PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s",
	lip->load_time.tv_sec, lip->load_time.tv_nsec, bbb[4],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[5],
	lip->load_time.tv_sec, lip->load_time.tv_nsec, bbb[0],
	lip->load_time.tv_sec, lip->load_time.tv_nsec, bbb[1],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[2],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[3],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[6]
	));
}
int replay_resp_rates(char *resp)
{
    static char bbb[2][BMR_MAX_MESSAGE];
    LoadInfo *lip = get_load_info();
    PushInfo *pip = get_push_info();
    desc_load_rate(" LoadRT", lip, bbb[0]);
    desc_push_rate(" SendRT", pip, bbb[1]);
    return(snprintf(resp, BMR_MAX_MESSAGE,
	PRI_TS " %s" PRI_TS " %s",
	lip->load_time.tv_sec, lip->load_time.tv_nsec, bbb[0],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[1] ));
}

/*
 * eof
 */
