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
 * $Id: anal_util.h 1449 2011-09-26 14:36:03Z gbc $
 *
 * Packet analysis utilities
 */

#ifndef anal_util_h
#define anal_util_h

#include "bmr_common.h"
#include "save_task.h"

/* Report analysis results for what which is (eventually) a mask.... */
void anal_save_result(char *buffer);
/* Called at the beginning of the save period with number of packets */
void anal_save_trigger(time_t now, SaveState *ssp, int pkts);
/* Called at the end of the save period */
void anal_save_disable(time_t now);
/* Called at start of grab cycle */
void anal_grab_active(TimeSpec entry);
/* A copy of the first report */
int anal_final_report(char *buf);

/* mk5b emulation mode of mark5c */
#define M5C_MAGIC    0xabaddeed
double mk5b_timestamp(char *buf, uint32_t *pkt);

#endif /* anal_util_h */

/*
 * eof
 */
