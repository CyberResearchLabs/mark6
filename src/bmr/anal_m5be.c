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
 * $Id$
 *
 * minimal support for m5b emulation of mark5c
 * cribbed from testcode/scan_range.c
 */

#include <math.h>
#include "bmr_util.h"
#include "anal_util.h"

typedef struct m5b_hdr {
    uint32_t sync_word;
    uint32_t df_num_insec:15;
    uint32_t tvg:1;
    uint32_t usd:12;
    uint32_t yr_from_2000:4;
    uint32_t vlba_secs:20;  /* BCD coded */
    uint32_t vlba_mjd:12;   /* BCD coded */
    uint32_t b_w3;
} Mark5b;

/*
 * Need to work out what time it is.
 * 2000 *is* a leap year.
 * HOPS_JULIAN_EPOCH=UTC hops_time -qU 2451544.5 == 20000101_000000.000000
 * HOPS_JULIAN_EPOCH=UTC hops_time -qU   51544.0 == 20000101_000000.000000
 * This is crude.
 */
static int secs_bcd(int bs)
{
    int secs = 0, ten = 1;
    while (bs) {
        secs += (bs & 0xF) * ten;
        ten *= 10;
        bs >>= 4;
    }
    return(secs);
}
static int xjd_secs(int *semesters, int yrs, int mjd_bcd)
{
    int mje, mjd;
    mjd = secs_bcd(mjd_bcd);
    mje = 51544 + 365*yrs + yrs/4 + (yrs%4>0?1:0);
    mjd += 1000*(mje/1000);     /* to mjd */
    mjd -= mje;                 /* to days of epoch */
    mjd *= 86400;               /* to seconds of epoch */
    *semesters = 2*yrs;
    return(mjd);
}

double mk5b_timestamp(char *buf, uint32_t *pkt)
{
    int		semesters, sre;
    uint64_t	nsc;
    double	secs;
    Mark5b	*m = (Mark5b*)pkt;

    secs = (double)m->df_num_insec / 25600;
    nsc  = rint(1e9 * secs);
    sre  = secs_bcd(m->vlba_secs);
    sre += xjd_secs(&semesters, m->yr_from_2000, m->vlba_mjd);
    secs += sre;
    snprintf(buf, TIME_BUFFER_SIZE, "%02d@%d.%09lu",
	semesters, sre, (unsigned long)nsc);
    return(secs);
}

/*
 * eof
 */
