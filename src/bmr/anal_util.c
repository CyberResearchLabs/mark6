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
 * $Id: anal_util.c 1449 2011-09-26 14:36:03Z gbc $
 *
 * Packet analysis utilities
 */

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "bmr_util.h"
#include "grab_task.h"
#include "save_task.h"
#include "anal_util.h"

static struct anal_data {
    char	entry_str[TIME_BUFFER_SIZE];
    TimeSpec	entry;	    /* time analysis started */
    TimeSpec	grab;	    /* time grabbing started */
    char	grab_str[TIME_BUFFER_SIZE];
    time_t	when;	    /* analyis start time */
    char	when_str[TIME_BUFFER_SIZE];
    time_t	done;	    /* analyis stop time */
    char	done_str[TIME_BUFFER_SIZE];
    int		pkts;	    /* number of packets */
    SaveState	*ssp;	    /* all we need to know */
    char	firststr[TIME_BUFFER_SIZE];
    double	first_dt;
    char	finalstr[TIME_BUFFER_SIZE];
    double	final_dt;
    uint64_t	scnts[4], samp;
    int		thrid;
} anal_results, *arp = &anal_results;

static char first_anal_result[BMR_MAX_MESSAGE];

/*
 * Fill the buffer with a complete report
 * But do it once only.
 */
void anal_save_result(char *buffer)
{
    double  n0, nn, pp, p3;
    if (!arp->when) {
	*buffer = 0;	/* show result just once */
	return;
    }
    if (arp->samp > 0) {
	n0 = (double)arp->scnts[0] / (double)arp->samp;
	nn = (double)arp->scnts[1] / (double)arp->samp;
	pp = (double)arp->scnts[2] / (double)arp->samp;
	p3 = (double)arp->scnts[3] / (double)arp->samp;
    } else {
	n0 = nn = pp = p3 = 0;
    }
    snprintf(buffer, BMR_MAX_MESSAGE, !arp->ssp ? "%s" :
	"%s  grab  %s ->active, %d pkts\n"
	"%s  first %s (%+.9lf)\n"
	"%s  final %s (%+.9lf)\n"
	"%s  BS[%d] %.3f %.3f %.3f %.3f (%.1f%% %gMs)\n",
	arp->entry_str, arp->grab_str, arp->pkts,
	arp->entry_str, arp->firststr, arp->first_dt,
	arp->entry_str, arp->finalstr, arp->final_dt,
	arp->entry_str, arp->thrid,
	    n0, nn, pp, p3, 100*(nn + pp), (double)arp->samp/1e6
    );
    memcpy(first_anal_result, buffer, BMR_MAX_MESSAGE);
    arp->when = 0;
}

/*
 * Work out the timestamp from the packet.
 */
static double capture_timestamp(char *buf, void *pkt)
{
    uint32_t secs_inre, ref_epoch, df_num_insec, *wd, ns;
    if (arp->ssp->mem_type == BMR_MEM_SEQN) pkt += 8;	/* skip PSN */
    wd = (uint32_t *)pkt;
    if (wd == M5C_MAGIC) return(mk5b_timestamp(buf, wd));
    secs_inre    =  wd[0] & 0x3FFFFFFF;
    df_num_insec =  wd[1] & 0x00FFFFFF;				/* of 31250 */
    ref_epoch    = (wd[1] & 0x3F000000) >> 24;
    ns = df_num_insec * 32000;			      /* 1000000000 / 31250 */
    snprintf(buf, TIME_BUFFER_SIZE, "%02d@%d.%09d", ref_epoch, secs_inre, ns);
    return( secs_inre + 1e-9 * ns );
}

/*
 * grab the time of the first packet
 */
static void analyze_first_pkt(void)
{
    void *first = arp->ssp->mem_read;
    double ts;
    int ns, gsec, gns;
    ts = capture_timestamp(arp->firststr, first);
    ns = sscanf(arp->grab_str, "%*d@%d.%d", &gsec, &gns);
    arp->first_dt = gsec + 1e-9 * gns;
    arp->first_dt -= ts;
    if (2 != ns) arp->first_dt = 0.0;
}
/*
 * grab the time of the final packet
 */
static void analyze_final_pkt(void)
{
    void *final = (void*)(*current_grab_write_ptr);
    double ts;
    int ns, gsec, gns;
    final -= arp->ssp->mem_chunk;
    if (final < arp->ssp->mem_start)
	final += arp->ssp->mem_size;
    ts = capture_timestamp(arp->finalstr, final);
    ns = sscanf(arp->grab_str, "%*d@%d.%d", &gsec, &gns);
    arp->final_dt = gsec + 1e-9 * gns;
    arp->final_dt -= ts;
    if (2 != ns) arp->final_dt = 0.0;
}

/*
 * analyze the group of packets; based on test_code/bit_states.c
 */
static void count_the_bits(uint64_t *op)
{
    int		octets, pairs;
    uint64_t	oct;
    if (arp->ssp->mem_type == BMR_MEM_SEQN) op++;	/* skip PSN */
    if ((op[0] & 0x00000000FFFFFFFFLL) == M5C_MAGIC) return;    /* not vdif */
    octets = (op[1] & 0x0000000000FFFFFFLL) - 4;    /* grab datalen */
    if (arp->samp == 0)
	arp->thrid = (op[1] & 0x03FF000000000000LL) >> 48;
    op += 4;					    /* skip to data */
    while (octets-- > 0) {
	oct = *op++;
	arp->samp += 32;
	for (pairs = 0; pairs < 32; pairs++) {
	    arp->scnts[oct & 0x3LL] ++;
	    oct >>= 2;
	}
    }
}
static void analyze_the_pkts(int pkts)
{
    void *pptr = arp->ssp->mem_read;
    memset(arp->scnts, 0, sizeof(arp->scnts));
    arp->samp = 0;
    while (pkts-- > 0) count_the_bits((uint64_t*)pptr);
}

/*
 * Called to initiate analysis--the packets are immediately analyzed.
 */
void anal_save_trigger(time_t when, SaveState *ssp, int pkts)
{
    if (!arp->grab.tv_sec) return;
    arp->entry = timens("0");
    arp->when = when;
    arp->done = 0;
    arp->pkts = pkts;
    arp->ssp = ssp;
    timefmtTS(arp->entry, arp->entry_str, TIMESTYLE_SECS);
    timefmtTT(when, arp->when_str, TIMESTYLE_VDIF);
    /* ok, now do some actual work */
    timefmtTS(arp->grab, arp->grab_str, TIMESTYLE_VDIF);
    analyze_first_pkt();
    analyze_final_pkt();
    analyze_the_pkts(pkts);
}

/*
 * Called at the end of the save cycle to nuke the analysis results
 * and to prepare for the next round (if any).
 */
void anal_save_disable(time_t done)
{
    memset(arp, 0, sizeof(anal_results));
    arp->done = done;
    timefmtTT(done, arp->done_str, TIMESTYLE_VDIF);
}

/*
 * Called when grab actually goes active
 */
void anal_grab_active(TimeSpec entry)
{
    arp->grab = entry;
}

/*
 * Called to print out the first analysis one final time
 */
int anal_final_report(char *buf)
{
    int nn = strlen(first_anal_result);
    strcpy(buf, first_anal_result);
    return(nn);
}

/*
 * eof
 */
