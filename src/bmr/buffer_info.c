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
 * $Id: buffer_info.c 526 2010-07-23 17:00:55Z gbc $
 *
 * BUFFER specific information displays.
 */

#include <stdio.h>
#include <string.h>

#include "bmr_util.h"
#include "bmr_server.h"
#include "grab_task.h"
#include "push_task.h"

#include "buffer_server.h"

/*
 * Various reporting functions
 */
int buffer_resp_show(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    grab_config("show", get_grab_state(), bbb[1]);
    push_config("show", get_push_state(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
int buffer_resp_info(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    desc_grab_info(get_grab_info(), bbb[1]);
    desc_push_info(get_push_info(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
int buffer_resp_counters(char *resp)
{
    static char bbb[7][BMR_MAX_MESSAGE];
    GrabInfo *gip = get_grab_info();
    PushInfo *pip = get_push_info();
    desc_grab_rate(" GrabRT", gip, bbb[4]);
    desc_push_rate(" SendRT", pip, bbb[5]);
    desc_grab_total(" Active", &gip->active, bbb[0]);
    desc_grab_total("Passive", &gip->passive, bbb[1]);
    desc_push_total("Sending", &pip->sending, bbb[2]);
    desc_push_total(" Idling", &pip->idling, bbb[3]);
    desc_push_tosn("TooSoon", &pip->tosn, bbb[6]);
    return(snprintf(resp, BMR_MAX_MESSAGE, PRI_TS " %s" PRI_TS " %s"
	PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s",
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[4],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[5],
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[0],
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[1],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[2],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[3],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[6]
	));
}
int buffer_resp_rates(char *resp)
{
    static char bbb[2][BMR_MAX_MESSAGE];
    GrabInfo *gip = get_grab_info();
    PushInfo *pip = get_push_info();
    desc_grab_rate(" GrabRT", gip, bbb[0]);
    desc_push_rate(" SendRT", pip, bbb[1]);
    return(snprintf(resp, BMR_MAX_MESSAGE,
	PRI_TS " %s" PRI_TS " %s",
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[0],
	pip->push_time.tv_sec, pip->push_time.tv_nsec, bbb[1] ));
}

/*
 * eof
 */
