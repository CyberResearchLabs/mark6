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
 * $Id: record_info.c 1298 2011-08-01 21:20:02Z gbc $
 *
 * RECORD specific information displays.
 */

#include <stdio.h>
#include <string.h>

#include "bmr_util.h"
#include "bmr_server.h"
#include "grab_task.h"
#include "save_task.h"
#include "anal_util.h"

#include "record_server.h"

/*
 * Various reporting functions
 */
int record_resp_show(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    grab_config("show", get_grab_state(), bbb[1]);
    save_config("show", get_save_state(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
int record_resp_info(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    bmr_server_config("show", get_bmr_server_state(), bbb[0]);
    desc_grab_info(get_grab_info(), bbb[1]);
    desc_save_info(get_save_info(), bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, "%s%s%s", bbb[0],bbb[1],bbb[2]));
}
/* a stripped down version of counters */
int record_resp_basics(char *resp)
{
    static char bbb[3][BMR_MAX_MESSAGE];
    GrabInfo *gip = get_grab_info();
    SaveInfo *sip = get_save_info();
    desc_grab_total(" Active", &gip->active, bbb[0]);
    desc_save_total(" Saving", &sip->writing, bbb[1]);
    anal_save_result(bbb[2]);
    return(snprintf(resp, BMR_MAX_MESSAGE, PRI_TS " %s" PRI_TS " %s%s",
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[0],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[1],
	bbb[2]
    ));
}
int record_resp_counters(char *resp)
{
    static char bbb[9][BMR_MAX_MESSAGE];
    GrabInfo *gip = get_grab_info();
    SaveInfo *sip = get_save_info();
    desc_grab_rate(" GrabRT", gip, bbb[4]);
    desc_save_rate("WriteRT", sip, bbb[5]);
    desc_grab_total(" Active", &gip->active, bbb[0]);
    desc_grab_total("Passive", &gip->passive, bbb[1]);
    desc_save_total(" Saving", &sip->writing, bbb[2]);
    desc_save_total(" Idling", &sip->idling, bbb[3]);
    desc_save_notw("NoWrite", &sip->notw, bbb[6]);
    anal_save_result(bbb[8]);
    return(snprintf(resp, BMR_MAX_MESSAGE, PRI_TS " %s" PRI_TS " %s"
	PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s" PRI_TS " %s%s",
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[4],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[5],
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[0],
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[1],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[2],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[3],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[6], bbb[8]
	));
}
int record_resp_rates(char *resp)
{
    static char bbb[2][BMR_MAX_MESSAGE];
    GrabInfo *gip = get_grab_info();
    SaveInfo *sip = get_save_info();
    desc_grab_rate(" GrabRT", gip, bbb[0]);
    desc_save_rate("WriteRT", sip, bbb[1]);
    return(snprintf(resp, BMR_MAX_MESSAGE,
	PRI_TS " %s" PRI_TS " %s",
	gip->grab_time.tv_sec, gip->grab_time.tv_nsec, bbb[0],
	sip->save_time.tv_sec, sip->save_time.tv_nsec, bbb[1] ));
}

/*
 * eof
 */
