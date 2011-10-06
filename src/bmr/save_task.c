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
 * $Id: save_task.c 1298 2011-08-01 21:20:02Z gbc $
 *
 * Implementation of the SAVE task.
 */

#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/resource.h>

#include "bmr_files.h"
#include "bmr_scan.h"
#include "bmr_util.h"
#include "grab_task.h"
#include "save_task.h"
#include "anal_util.h"

/*
 * SAVE private data
 */
static SaveState    sss;
static SaveInfo     sis;

static uint64_t	    reference_epoch_is;
static uint64_t	    shifted_station_id;

/*
 * a lookup of fd -> filed[] index for per-file diagnostics
static int	    fsize_max, fd2index[1024];
 */

/*
 * SAVE Public (read-only) Data
 */
int * const	    current_save_fd_ptr = &sss.save_fd;
Writer **           current_save_writer_ptr = &sss.save_writer;

/*
 * Check if writing is possible.
 * Returns nonzero to abort the write.
 ****
 * We have nothing to save if:
 *  the read pointer points to the write pointer
 *   (nothing valid to read)
 *  the read pointer points to an invalid sequence number
 *   (implies source hasn't written here yet)
 *  the read pointer points to a number less than our last
 *   (implies something is wrong--corrective action?)
 *
 * This logic assumes that the dispatch loop handles the
 * eventual nothing-was-written writer() response.
 *
 * There are two versions, based on BMR_MEM_* type.
 */
static int must_not_save_seqn(void)
{
    /* something-to-save checks */
    if (sss.mem_read == *current_grab_write_ptr) return(sis.notw.rw++,1);
    if (*(int64_t*)sss.mem_read == BMR_SEQN_INVALID) return(sis.notw.inv++,2);
    if (*(int64_t*)sss.mem_read <= sss.last_seqn) return(sis.notw.rl++,3);
    return(sis.notw.ok++,0);
}
#ifdef ORIGINAL_save_writer_vde
static int must_not_save_edvs(void)
{
    int64_t edvs_seqn;
    /* something-to-save checks */
    if (sss.mem_read == *current_grab_write_ptr) return(sis.notw.rw++,1);
    if (*(int64_t*)sss.mem_read == BMR_PKT_NOTVALID) return(sis.notw.inv++,2);
    edvs_seqn = BMR_SEQN_UNPACK(((int64_t*)sss.mem_read)[2]);
    if (edvs_seqn <= sss.last_seqn) return(sis.notw.rl++,3);
    return(sis.notw.ok++,0);
}
#endif /* ORIGINAL_save_writer_vde */

/*
 * A modified version of must_not_save*() that inverts the logic and
 * computes how many packets and the last seqn that is to be saved.
 */
static int pkts_to_save(int64_t *seqn)
{
    int pkts = 0;
    void *rptr = sss.mem_read;
    while (pkts < sss.ppgroup) {
	*seqn = BMR_SEQN_UNPACK(((int64_t*)rptr)[2]);
	if (rptr == *current_grab_write_ptr) { sis.notw.rw++; break; }
	if (*(int64_t*)rptr == BMR_PKT_NOTVALID) { sis.notw.inv++; break; }
	if (*seqn <= sss.last_seqn) { sis.notw.rl++; break; }
	pkts ++;		    /* ok to save this one */
	rptr += sss.mem_chunk;	    /* move on to next one */
	if (rptr == sss.mem_start + sss.mem_size) break;
    }
    if (pkts > 0) sis.notw.ok++;
    return(pkts);
}

/*
 * The packet saver for vdif packets, which assumes that the RDBE is
 * generating correct packets, and that the seqn is in the extended
 * data headers.  It is more efficient to write as many packets as
 * are allowed by pkts_to_save().  Note that the number returned is
 * guaranteed not to read past sss.mem_start + sss.mem_size.
 */
static ssize_t save_writer_vde(void)
{
    int	ns, pkts, bytes;
    int64_t seqn = 0;

    sis.writing.called ++;
    if (0 == (pkts = pkts_to_save(&seqn))) return(0);

    sss.last_seqn = seqn;
    bytes = pkts * sss.save_size;
    ns = write(sss.save_fd, sss.mem_read, bytes);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == bytes) {
	sis.writing.bytes += ns;
	sis.writing.pkts += pkts;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next group */
    sss.mem_read += bytes;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

/*
 * This writes the udp packet + seqn to file without any edits
 * and afterwards moves the pointer forward (no matter what).
 * FIXME: vdif header edits, mem_type
 */
static ssize_t save_writer_raw(void)
{
    int	ns;

    sis.writing.called ++;
    if (must_not_save_seqn()) return(0);

    sss.last_seqn = *(int64_t *)sss.mem_read;
    ns = write(sss.save_fd, sss.mem_read, sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

/*
 * Same as save_writer_raw() but without the seqn
 * FIXME: vdif header edits, mem_type
 */
static ssize_t save_writer_psn(void)
{
    int	ns;

    sis.writing.called ++;
    if (must_not_save_seqn()) return(0);

    sss.last_seqn = *(int64_t *)sss.mem_read;
    ns = write(sss.save_fd, sss.mem_read + sizeof(int64_t), sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

/*
 * Same as save_writer_raw() but without the seqn or the psn
 * FIXME: vdif header edits, mem_type
 */
static ssize_t save_writer_pkt(void)
{
    int	ns;

    sis.writing.called ++;
    if (must_not_save_seqn()) return(0);

    sss.last_seqn = *(int64_t *)sss.mem_read;
    ns = write(sss.save_fd, sss.mem_read + 2*sizeof(int64_t), sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

/*
 * Same as save_writer_raw() but without the seqn (no psn).
 * FIXME: vdif header edits, mem_type
 */
static ssize_t save_writer_vdf(void)
{
    int	ns;

    sis.writing.called ++;
    if (must_not_save_seqn()) return(0);

    sss.last_seqn = *(int64_t *)sss.mem_read;
    ns = write(sss.save_fd, sss.mem_read + sizeof(int64_t), sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

#ifdef ORIGINAL_save_writer_vde
/*
 * Similar to save_writer_vdf() but seqn is already in packet header,
 * and (bonus), the RDBE now provides correct header data so there is
 * nothing to repair.
 *
 * FIXME: implement ppgroup
 */
static ssize_t save_writer_vde(void)
{
    int	ns;
    sis.writing.called ++;
    if (must_not_save_edvs()) return(0);
    sss.last_seqn = BMR_SEQN_UNPACK( ((int64_t *)sss.mem_read)[2] );
    ns = write(sss.save_fd, sss.mem_read, sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}
#endif /* ORIGINAL_save_writer_vde */

/*
 * Same as save_writer_vdf() but embed the seqn in the header.
 * FIXME: RDBE has correct bm1 and lb8 values.
 */
static ssize_t save_writer_vdx(void)
{
    int	ns;
    struct ptr { int64_t seqn, hdr12, hdr34, ehdr, ew23; } *ptr;

    sis.writing.called ++;
    if (must_not_save_seqn()) return(0);

    /* transfer the seqn & psn */
    ptr = (struct ptr *)sss.mem_read;
    ptr->ehdr &= BMR_SEQN_EDVMASK;
    ptr->ehdr |= BMR_SEQN_PACK(ptr->seqn);
    if (sss.station) {
	/* FIXME: various header fields */
	ptr->hdr34 &= ~(BMR_PKT_STN_MASK|BMR_PKT_BM1_MASK|BMR_PKT_LB8_MASK);
	ptr->hdr34 |= shifted_station_id;
	/* FIXME: when Alan fixes image */
	ptr->hdr12 &= ~BMR_PKT_RFE_MASK;
	ptr->hdr12 |= reference_epoch_is;
    }

    sss.last_seqn = *(int64_t *)sss.mem_read;
    ns = write(sss.save_fd, sss.mem_read + sizeof(int64_t), sss.save_size);
    if (errno != 0) { sis.writing.syserr ++; errno = 0; }
    /* update counters */
    if (ns == sss.save_size) {
	sis.writing.bytes += ns;
	sis.writing.pkts ++;
    } else {
	sis.writing.sizerr ++;
    }
    /* advance to the next packet */
    sss.mem_read += sss.mem_chunk;
    if (sss.mem_read - sss.mem_start >= sss.mem_size)
	sss.mem_read = sss.mem_start;
    return(ns);
}

/*
 * This is a no-op.
 */
static ssize_t save_idler(void)
{
    sis.idling.called ++;
    return(0);
}

/*
 * For the descriptive tasks, this is more useful than a pointer.
 */
char *save_writer_name(void *writer)
{
    if (writer == save_writer_raw) return("raw");
    if (writer == save_writer_psn) return("psn");
    if (writer == save_writer_pkt) return("pkt");
    if (writer == save_writer_vdf) return("vdf");
    if (writer == save_writer_vde) return("vde");
    if (writer == save_writer_vdx) return("vdx");
    if (writer == save_idler) return("idler");
    return("fault");
}

/*
 * Capture the exact times of entry/exit from writing mode.
 * On entry, capture the time in sis.writing_entry,
 * on exit, increase the total in sis.writing_time.
 */
static void save_write_timing(int entry)
{
    TimeSpec	exit_time;
    if (entry) {
	clock_gettime(CLOCK_REALTIME, &sis.writing_entry);
    } else if (sis.writing_entry.tv_sec > 0) {
	clock_gettime(CLOCK_REALTIME, &exit_time);
	time_increment(&sis.writing_time,
	    time_decrement(&exit_time, &sis.writing_entry));
	sis.writing_entry.tv_sec = 0;	/* accumulate once! */
    }
}

/*
 * Assign the handler based on the action.
 */
static void set_save_action(int action, SaveState *ss)
{
    switch (action) {
    case SAVE_ACT_IDLE:
	if (ss->verb>0 && action != ss->action)
	    fputs("Idle saver\n", stderr);
	ss->save_writer = save_idler;
	break;
    case SAVE_ACT_INIT:
	if (ss->verb>0) fputs("Init saver\n", stderr);
	ss->save_writer = save_idler;
	break;
    case SAVE_ACT_NONE:
	if (ss->verb>0) fputs("None saver\n", stderr);
	ss->save_writer = save_idler;
	save_write_timing(0);
	break;
    case SAVE_ACT_FILE:
	switch (ss->file_fmt) {
	case BMR_FILE_RAW:
	    if (ss->verb>0) fputs("File saver raw\n", stderr);
	    ss->save_writer = save_writer_raw;
	    break;
	case BMR_FILE_PSN:
	    if (ss->verb>0) fputs("File saver psn\n", stderr);
	    ss->save_writer = save_writer_psn;
	    break;
	case BMR_FILE_PKT:
	    if (ss->verb>0) fputs("File saver pkt\n", stderr);
	    ss->save_writer = save_writer_pkt;
	    break;
	case BMR_FILE_VDF:
	    if (ss->verb>0) fputs("File saver vdf\n", stderr);
	    ss->save_writer = save_writer_vdf;
	    break;
	case BMR_FILE_VDE:
	    if (ss->verb>0) fputs("File saver vde\n", stderr);
	    ss->save_writer = save_writer_vde;
	    break;
	case BMR_FILE_VDX:
	    if (ss->verb>0) fputs("File saver vdx\n", stderr);
	    ss->save_writer = save_writer_vdx;
	    break;
	default:
	    if (ss->verb>0) fprintf(stderr,
		"Illegal file_fmt %d\n", ss->file_fmt);
	    action = SAVE_ACT_IDLE;
	}
	save_write_timing(1);
	break;
    default:
	if (ss->verb>0) fprintf(stderr,
	    "Illegal save action %d\n", action);
	action = SAVE_ACT_IDLE;
    }
    ss->action = action;
}

/*
 * Close any files that are open.
 * We also truncate the paths at the dir/file point.
 */
void save_files_yank(void)
{
    static int	again = 0;
    char	*slash;
    int		sf;
    if (sss.verb>0) fputs("Closing SAVE files.\n", stderr);
    if (again++) for (sf = 0; sf < sss.num_files; sf ++) {
	if (sss.filed[sf] >= 0) close(sss.filed[sf]);
	slash = sss.path[sf] ? strrchr(sss.path[sf], '/') : 0;
	if (slash) *slash = 0;
	sss.filed[sf] = -1;
    }
    sss.file_index ++;
    set_save_action(SAVE_ACT_IDLE, &sss);
}

/*
 * Generate a file name from the path, scan and index
 * FIXME -- checks on filesystem capacity?
 */
static char *save_file_path(SaveState *ss, int sf)
{
    ssize_t	plen;
    char	fndx[20], *path = ss->path[sf];
    if (!path) {
	path = malloc(8);
	if (path) snprintf(path, 8, "./");
	else return(perror("malloc:save_file_path"),NULL);
    }
    plen = strlen(path) + strlen(ss->scan_label) + 40;
    path = realloc(path, plen);
    strcat(path, "/");
    strcat(path, ss->scan_label);
    if (ss->num_files > 1)
	snprintf(fndx, sizeof(fndx), "_%d.%s",
	    ss->file_index++, bmr_file_suffix_fmt[ss->file_fmt]);
    else
	snprintf(fndx, sizeof(fndx), ".%s",
	    bmr_file_suffix_fmt[ss->file_fmt]);
    strcat(path, fndx);
    if (ss->verb>0) fprintf(stderr, "File %d is %s\n", sf, path);
    return(path);
}

/*
 * Set up the file infrastructure.
 * Returns nonzero if we cannot proceed.
 * Note that we transfer the file index into the working SaveState
 * so that files are not overwritten and the numbers are continuous.
 *
 * FIXME: reference_epoch_is/shifted_station_id no longer needed.
 */
int save_files_prep(SaveState *ss)
{
    int	    sf;
    if (ss->verb>0) fputs("Opening SAVE files.\n", stderr);
    if (ss->file_index < sss.file_index) ss->file_index = sss.file_index;
    for (sf = 0; sf < ss->num_files; sf++) {
	ss->path[sf] = save_file_path(ss, sf);
	if (!ss->path[sf]) return(fprintf(stderr, "Error on file %d\n", sf));
	ss->filed[sf] = open(ss->path[sf],
	    O_CREAT|O_WRONLY|O_TRUNC, 0666);
	if (ss->filed[sf] < 0) {
	    perror("open");
	    fprintf(stderr, "Error on file %s (%d)\n", ss->path[sf], sf);
	    free(ss->path[sf]);
	    return(sf+1);
	}
    }
    /* station bits shifted to the right place ONCE per setup */
    reference_epoch_is  = ((uint64_t)(20))          << BMR_PKT_RFESHIFT;
    reference_epoch_is &= BMR_PKT_RFE_MASK;
    shifted_station_id  = ((uint64_t)(ss->station)) << BMR_PKT_STNSHIFT;
    shifted_station_id |= ((uint64_t)(1))           << BMR_PKT_BM1SHIFT;
    shifted_station_id |= (ss->save_size >> 3)      << BMR_PKT_LB8SHIFT;
    shifted_station_id &= (BMR_PKT_STN_MASK|BMR_PKT_BM1_MASK|BMR_PKT_LB8_MASK);
    if (ss->verb>0) fprintf(stderr, "Station %02X prepped.\n", ss->station);
    return(0);
}

/*
 * Set up the timing....
 */
static int save_timing_prep(SaveState *ss)
{
    double  gbps = (8e-9 * ss->save_size * ss->save_rate);
    if (ss->save_rate <= 1.0)
	return(fprintf(stderr, "Illegal save pkt rate %ld\n", ss->save_rate));
    if (ss->save_rate < ss->rate_lim)
	return(fprintf(stderr, "Save rate %ld < %ld pkts/s required\n",
	    ss->save_rate, ss->rate_lim));
    if (ss->verb>0) fprintf(stderr, "Target Save rate is %g (Gbps)\n", gbps);
    return(0);
}

/*
 * Check the time and, as required, switch between the
 * writing and not writing states (if not IDLE).  The
 * rep counter decrements after the NONE state, and
 * we shift to the idle state when it hits zero.
 *
 * We care most about performance in the SEND state
 * (which actually has to save packets).
 *
 * INIT -> NONE -> SAVE -> NONE ( -> IDLE finally )
 *
 * The active/passive logic mirrors that of GRAB.  However, if overlap
 * is allowed, we will arrange things so that we never fall passive.
 *
 * Return 0 indicates we want to write for packets,
 * Return 1 indicates the write file descriptors should be ignored.
 *
 * FIXME: overlap not yet coded....
 */
int check_save_state(void)
{
    GrabState	*gsp;
    time_t	now = time(0);
    switch (sss.action) {

    case SAVE_ACT_FILE:
	if (now < sss.save_passive) return(0);
	anal_save_disable(now);
	sss.save_passive += sss.save_period;
	set_save_action(SAVE_ACT_NONE, &sss);
	return(1);

    case SAVE_ACT_NONE:
	if (now < sss.save_active) return(1);
	anal_save_trigger(now, &sss, sss.save_anal);
	sss.save_active += sss.save_period;
	if (sss.save_reps-- > 0) {
	    set_save_action(SAVE_ACT_FILE, &sss);
	    return(0);
	}
	/* fall through to IDLE */
    default:
    case SAVE_ACT_IDLE:
	set_save_action(SAVE_ACT_IDLE, &sss);
	break;

    case SAVE_ACT_INIT:
	gsp = get_grab_state();
	sss.save_period = gsp->grab_period;
	sss.save_active = gsp->grab_start + gsp->grab_secs;
	sss.save_passive = gsp->grab_start + gsp->grab_period;
	sss.save_reps = gsp->grab_cycles;
	set_save_action(SAVE_ACT_NONE, &sss);
	break;

    }
    return(1);
}

/*
 * Work out the number of bytes to be written each input packet
 * RAW includes our sequence number.
 * PSN includes PSN preceding the packet.
 * PKT deletes the PSN preceding the packet.
 * VDF/VDX is just the packet and PSN is internal (if present)
 *
 * FIXME: VDE/VDF/VDX is a mess....
 */
static int save_size_prep(SaveState *ss, GrabState *gsp)
{
    switch (ss->file_fmt) {
    case BMR_FILE_RAW:
	ss->save_size = gsp->udp_size + sizeof(int64_t);
	break;
    case BMR_FILE_PSN:
	ss->save_size = gsp->udp_size;
	break;
    case BMR_FILE_PKT:
	ss->save_size = gsp->udp_size - sizeof(int64_t);
	break;
    case BMR_FILE_VDF:
	ss->save_size = gsp->udp_size;
	if (ss->mem_type == BMR_MEM_EDVS) ss->file_fmt = BMR_FILE_VDE;
	break;
    case BMR_FILE_VDE:
	ss->save_size = gsp->udp_size;
	if (ss->mem_type == BMR_MEM_SEQN) ss->file_fmt = BMR_FILE_VDX;
	break;
    case BMR_FILE_VDX:
	ss->save_size = gsp->udp_size;
	if (ss->mem_type == BMR_MEM_EDVS) ss->file_fmt = BMR_FILE_VDE;
	break;
    default:
	return(fprintf(stderr, "Illegal file_fmt %d\n", ss->file_fmt));
    }
    return(0);
}

/*
 * Work out the buffer parameters based on what grab provides.
 * FIXME: overlap will flush any captured, but not delivered data; ok?
 * FIXME: this rate limit assumes no sending during flush (not true).
 */
static int save_buffer_prep(SaveState *ss)
{
    GrabState	*gsp = get_grab_state();
    ss->mem_type  = gsp->mem_type;
    if (save_size_prep(ss, gsp)) return(1);
    ss->mem_start = gsp->mem_start;
    if (!ss->mem_start) return(fputs("No Save Buffer\n", stderr));
    ss->mem_size  = gsp->mem_size;
    ss->mem_chunk = gsp->mem_chunk;
    ss->last_seqn = gsp->next_seqn - 1;
    if (ss->overlap) {
	ss->mem_read = gsp->mem_write;
	ss->rate_lim = gsp->udp_rate * gsp->grab_secs
		     / ( gsp->grab_period );
    } else {
	ss->mem_read = gsp->mem_write;
	ss->rate_lim = gsp->udp_rate * gsp->grab_secs
		     / ( gsp->grab_idle - gsp->grab_flush );
    }
    return(0);
}

/*
 * Nuking the SaveInfo structure is a good start.
 */
static int save_stats_prep(SaveState *ss)
{
    memset(&sis, 0, sizeof(sis));
    return(0);
}

/*
 * last_seqn is initialized from GRAB by save_buffer_prep_*().
 * FIXME: what do we want to do if it jumps too much?
 */
static void update_save_state(SaveState *ss)
{
    int64_t	old_seqn;
    old_seqn = sss.last_seqn;
    sss = *ss;	    /* make it so, #1 */
    /* FIXME: sss.last_seqn <> old_seqn ? */
    check_save_state();
}

/*
 * Shut down any writing activity in progress and initialize a new activity.
 */
int set_save_state(SaveState *ss)
{
    save_files_yank();
    if (!ss) return(0);	    /* for exit case */

    if (save_stats_prep(ss))
	return(fputs("Failed to initialize save stats.\n", stderr));

    if (save_buffer_prep(ss))
	return(fputs("Failed to initialize save buffer.\n", stderr));

    if (save_timing_prep(ss))
	return(fputs("Failed to initialize save timing.\n", stderr));

    if (save_files_prep(ss)) {
	save_files_yank();
	return(fputs("Failed to initialize save files.\n", stderr));
    }

    update_save_state(ss);
    return(0);
}

/*
 * Methods to return safe copies of our internals.
 */
SaveInfo *get_save_info(void)
{
    static SaveInfo	mine;
    clock_gettime(CLOCK_REALTIME, &sis.save_time);
    sis.save_state = sss;
    mine = sis;
    return(&mine);
}
SaveState *get_save_state(void)
{
    static SaveState	mine;
    mine = sss;
    return(&mine);
}

/*
 * eof
 */
