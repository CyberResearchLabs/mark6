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
 * $Id: grab_task.c 1299 2011-08-02 18:46:14Z gbc $
 *
 * Implementation of the GRAB task.
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#include <netdb.h>
#include <arpa/inet.h>
#include <sys/resource.h>

/* to support the raw interface */
#include <sys/ioctl.h>
#include <net/if.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <net/ethernet.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/udp.h>

#include "bmr_util.h"
#include "grab_task.h"

#if BMR_SERVER==2
#include "anal_util.h"
#endif /* BMR_SERVER==2 */

/* we are assuming the minimal 5-byte IP header size -- fails with VPN */
#define UDP_HLEN (sizeof(struct ether_header)+(5*4)+sizeof(struct udphdr))

/*
 * GRAB private data
 */
static GrabState    gss;
static GrabInfo	    gis;

/*
 * GRAB Public (read-only) Data
 */
void const **		current_grab_write_ptr = (void const **)&gss.mem_write;
int const * const	current_grab_sock_ptr = &gss.grab_sock;
Receiver **		current_grab_recv_ptr = &gss.grab_recv;

/*
 * This receives and discards the packet, keeping separate statistics.
 * 65535 is the theoretical maximum; the practial limit is 65,507
 * (16-bit max minus both the [minimum] 20B IP header + 8B UDP header).
 */
static ssize_t recv_idle(void)
{
    static char private[65536];
    int nb = recv(gss.grab_sock, private, 65535, gss.flags);
    if (errno != 0) { gis.idle.syserr ++; errno = 0; }
    gis.idle.bytes += nb;
    gis.idle.pkts ++;
    if (nb != gss.udp_size) gis.idle.sizerr ++;
    return(0);
}

/*
 * This ignores packets altogether.
 */
static ssize_t recv_punt(void)
{
    gis.idle.called ++;
    return(0);
}

/*
 * This just receives and counts the packet.  It doesn't advance
 * the write point or advance the sequence counter, so this packet
 * isn't going to survive.  Returns number of bytes read.
 */
static ssize_t recv_none(void)
{
    int nb = recv(gss.grab_sock, gss.mem_write + sizeof(int64_t),
	gss.udp_size, gss.flags);
    if (errno != 0) { gis.passive.syserr ++; errno = 0; }
    gis.passive.bytes += nb;
    gis.passive.pkts ++;
    if (nb != gss.udp_size) gis.passive.sizerr ++;
    return(nb);
}

/*
 * This is the normal receive-and-store-into-buffer handler
 * with some minimal tracking for monitoring, using the
 * BMR_MEM_SEQN memory model (seqn + packet).
 * Returns number of bytes read.
 */
static ssize_t recv_buff(void)
{
    int	nb = recv(gss.grab_sock, gss.mem_write + sizeof(int64_t),
	gss.udp_size, gss.flags);
    if (errno != 0) { gis.active.syserr ++; errno = 0; }
    if (nb == gss.udp_size) {
	/* update sequence */
	*(int64_t*)gss.mem_write = gss.next_seqn ++;
	/* advance to the next packet */
	gss.mem_write += gss.mem_chunk;
	if (gss.mem_write - gss.mem_start >= gss.mem_size)
	    gss.mem_write = gss.mem_start;
	*(int64_t*)gss.mem_write = BMR_SEQN_INVALID;
	/* update counters */
	gis.active.bytes += nb;
	gis.active.pkts ++;
    } else {
	gis.active.sizerr ++;
    }
    return(nb);
}

/*
 * This is the receive-and-store-into-buffer handler
 * with some minimal tracking for monitoring, using the
 * BMR_MEM_EDVS memory model (seqn packed into the extended
 * data header of the vdif packet).  We don't preserve the
 * EDV number--it is forced to zero.
 */
static ssize_t recv_bufe(void)
{
    int nb = recv(gss.grab_sock, gss.mem_write, gss.udp_size, gss.flags);
    if (errno != 0) { gis.active.syserr ++; errno = 0; }
    if (nb == gss.udp_size) {
	/* update sequence */
	((int64_t*)gss.mem_write)[2] = BMR_SEQN_PACK(gss.next_seqn++);
	/* advance to the next packet */
	gss.mem_write += gss.mem_chunk;
	if (gss.mem_write - gss.mem_start >= gss.mem_size)
	    gss.mem_write = gss.mem_start;
	*(int64_t*)gss.mem_write = BMR_PKT_NOTVALID;
	/* update counters */
	gis.active.bytes += nb;
	gis.active.pkts ++;
    } else {
	gis.active.sizerr ++;
    }
    return(nb);
}

/*
 * These are the variants for raw device access.  In addition to all
 * that recv_buff() does, we must in addition filter out packets
 * of incorrect protocol or port.
 *
 * Oh, and we must also shift the received bytes to match up with
 * the normal recv_buff() placement.  Since that would be costly,
 * we'll save the end of the previous packet, overwrite it, and
 * then copy it back into place.
 *
 * In raw mode, we'll ignore port numbers greater than 20000
 *
 * FIXME: Note that only BMR_MEM_SEQN is supported here at present.
 */
static ssize_t recv_braw(void)
{
    static char hold[UDP_HLEN];
    void *wp = gss.mem_write + sizeof(int64_t) - gss.udp_hlen;
    int	nb;

    /* save the data soon to be overwritten */
    memcpy(hold, wp, UDP_HLEN);
    
    /* receive the packet + ETH/IP/UDP headers */
    nb = recv(gss.grab_sock, wp, gss.udp_size + UDP_HLEN, /*gss.flags*/0);
    if (errno != 0) { gis.active.syserr ++; errno = 0; }

    /* reduce to UDP packet length and check size, protocol, port */
    nb -= UDP_HLEN;
    if (nb != gss.udp_size)                                 nb = 0;
    if (IPPROTO_UDP != (*(unsigned char*)(wp+UDP_HLEN-19))) nb = 1;
    if (gss.udp_port < 20000 &&
	gss.udp_port != htons(*(uint16_t*)(wp+UDP_HLEN-6))) nb = 2;

    /* restore the data that was overwritten */
    memcpy(wp, hold, UDP_HLEN);

    if (nb == gss.udp_size) {
	/* update sequence */
	*(int64_t*)gss.mem_write = gss.next_seqn ++;
	/* advance to the next packet */
	gss.mem_write += gss.mem_chunk;
	if (gss.mem_write - gss.mem_start >= gss.mem_size)
	    gss.mem_write = gss.mem_start;
	*(int64_t*)gss.mem_write = BMR_SEQN_INVALID;
	/* update counters */
	gis.active.bytes += nb;
	gis.active.pkts ++;
    } else {
	gis.active.sizerr ++;
    }
    return(nb + UDP_HLEN);
}

/*
 * recv_nraw() needs to read a bigger packet--it's ok to read it
 * in the wrong place as we don't care about the data and going
 * off the end is ok due to the guard bytes.
 */
static ssize_t recv_nraw(void)
{
    int nb = recv(gss.grab_sock, gss.mem_write + sizeof(int64_t),
	gss.udp_size + UDP_HLEN, /*gss.flags*/0);
    if (errno != 0) { gis.passive.syserr ++; errno = 0; }
    gis.passive.bytes += nb;
    gis.passive.pkts ++;
    if (nb != gss.udp_size + UDP_HLEN) gis.passive.sizerr ++;
    return(nb);
}

/*
 * For the descriptive tasks, this is more useful than a pointer.
 */
char *grab_recver_name(void *grab)
{
    if (grab == recv_idle) return("idle");
    if (grab == recv_punt) return("punt");
    if (grab == recv_none) return("none");
    if (grab == recv_buff) return("buff");
    if (grab == recv_bufe) return("bufe");
    if (grab == recv_braw) return("braw");
    if (grab == recv_nraw) return("nraw");
    return("fault");
}

/*
 * Capture exact times of entry/exit from active mode.
 * On entry, capture the time in gis.active_entry,
 * on exit, increase the total in gis.active_time.
 */
static void grab_active_timing(int entry)
{
    TimeSpec	exit_time;
    if (entry) {
	clock_gettime(CLOCK_REALTIME, &gis.active_entry);
#if BMR_SERVER==2
	anal_grab_active(gis.active_entry);
#endif /* BMR_SERVER==2 */
    } else if (gis.active_entry.tv_sec > 0) {
	clock_gettime(CLOCK_REALTIME, &exit_time);
	time_increment(&gis.active_time,
	    time_decrement(&exit_time, &gis.active_entry));
	gis.active_entry.tv_sec = 0;	/* accumulate once! */
    }
}

/*
 * To avoid messing with the server select() calls, we'll dup
 * grab_sock back and forth with some socket that should never
 * have any data coming to it, e.g. INADDR_LOOPBACK:65432
 *
 * activate = 1 means to use the real address
 * activate = 0 means to use the fake address
 *
 * either way, we use dup2 to make sure the existing grab_sock
 * number is preserved.  The call to grab_network_prep() uses
 * udp_addr[] and udp_port, and updates grab_sock and udp_hlen.
 */
static void grab_active_network(int activate, GrabState *gs)
{
    char    addr[128];
    int     port, save;

    if (gs->verb>0) fputs("Activating Network\n", stderr);
    
    if (!activate) {				/* save */
	strcpy(addr, gs->udp_addr);
	strcpy(gs->udp_addr, "127.0.0.1");
	port = gs->udp_port;
	gs->udp_port = 65432 + gs->tognet;
    }

    close(save = gs->grab_sock);
    grab_network_prep(gs);

    if (!activate) {				/* restore */
	strcpy(gs->udp_addr, addr);
	gs->udp_port = port;
    }

    if (save != gs->grab_sock) dup2(gs->grab_sock, save);
    else if (gs->verb>1) fprintf(stderr,
	"No need to dup2(%d,%d)\n", gs->grab_sock, save);

    if (gs->verb>0) fprintf(stderr, activate
	? "Activated Data Capture on socket %d\n"
	: "De-activated Data Capture on socket %d\n", gs->grab_sock);
}

/*
 * Move to the desired action.  Originally, this just selected
 * the appropriate recv handler, but since the active state is
 * special, calls to grab_active_timing() [to capture timing]
 * and grab_active_network() [to play games with grab_sock]
 * also appear.
 */
static void set_recv_action(int action, GrabState *gs)
{
    switch (action) {
    case GRAB_ACT_IDLE:
	if (gs->verb>0 && action != gs->action)
	    fputs("Idle reception\n", stderr);
	gs->grab_recv = recv_idle;
	break;
    case GRAB_ACT_INIT:
	if (gs->verb>0) fputs("Init reception\n", stderr);
	gs->grab_recv = recv_idle;
	break;
    case GRAB_ACT_PUNT:
	if (gs->verb>0) fputs("Punting Grab reception\n", stderr);
	gs->grab_recv = recv_punt;
	grab_active_timing(0);
	if (gs->tognet) grab_active_network(0, gs);
	break;
    case GRAB_ACT_NONE:
	if (gs->tognet) grab_active_network(1, gs);
	if (gs->udp_hlen == 0) {
	    if (gs->verb>0) fputs("Passive Grab reception\n", stderr);
	    gs->grab_recv = recv_none;
	} else {
	    if (gs->verb>0) fputs("Passive (Raw) Grab reception\n", stderr);
	    gs->grab_recv = recv_nraw;
	}
	break;
    case GRAB_ACT_BUFF:
	if (gs->udp_hlen == 0) {
	    if (gs->mem_type == BMR_MEM_SEQN) {
		if (gs->verb>0) fputs("Active Grab (seqn)\n", stderr);
		gs->grab_recv = recv_buff;
	    } else {
		if (gs->verb>0) fputs("Active Grab (edvs)\n", stderr);
		gs->grab_recv = recv_bufe;
	    }
	} else {
	    if (gs->verb>0) fputs("Active (Raw) Grab reception\n", stderr);
	    gs->grab_recv = recv_braw;
	}
	grab_active_timing(1);
	break;
    default:
	if (gs->verb>0) fprintf(stderr,
	    "Illegal receiver action %d\n", action);
	action = GRAB_ACT_IDLE;
    }
    gs->action = action;
}

/*
 * Basic checking of UDP sanity.
 * FIXME -- should we check other things here?
 * FIXME -- do we have the correct recv() flags here?
 */
static int chk_grab_net(GrabState *gs)
{
    double  gbps = (8e-9 * gs->udp_size * gs->udp_rate);
    if (gs->udp_size < 64 || gs->udp_size > 9000)
	return(fprintf(stderr, "Illegal grab UDP size %d\n", gs->udp_size));
    gs->flags = MSG_WAITALL;
    if (gs->verb>0) fprintf(stderr, "Target Grab rate is %g (Gbps)\n", gbps);
    if (gs->mem_type && gs->udp_hlen)
	return(fprintf(stderr, "Raw mode/mem_type incompatible\n"));
    return(0);
}

/*
 * Raw socket support based on the internals of libpcap for linux.
 * The socket calls use some rather complicated interface request structure
 * in which we supply the name to learn more about the device.
 */
static int iface_get_arptype(int fd, const char *device)
{
    struct ifreq    ifr;
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, device, sizeof(ifr.ifr_name));
    if (ioctl(fd, SIOCGIFHWADDR, &ifr) == -1) return(-1);
    return(ifr.ifr_hwaddr.sa_family);
}
static int iface_get_id(int fd, const char *device)
{
    struct ifreq    ifr;
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, device, sizeof(ifr.ifr_name));
    if (ioctl(fd, SIOCGIFINDEX, &ifr) == -1) return(-1);
    return(ifr.ifr_ifindex);
}

/*
 * Here we bind at the physical link layer and indicate we
 * want to accept all protocols.  Thus we must self-filter
 * in the recv_* process for UDP and UDP port number.
 */
static int iface_bind(int fd, int ifindex)
{
    struct sockaddr_ll	sll;
    int			err;
    socklen_t		errlen = sizeof(err);

    memset(&sll, 0, sizeof(sll));
    sll.sll_family	= AF_PACKET;
    sll.sll_ifindex	= ifindex;
    sll.sll_protocol	= htons(ETH_P_ALL);

    if (bind(fd, (struct sockaddr *) &sll, sizeof(sll)) == -1)
        return(fputs("bind failed\n", stderr));

    /* this will fail if the interface is down */
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &errlen) == -1)
        return(fputs("device down, perhaps?\n", stderr));
    return(0);
}

/*
 * Deviant version of network prep if we are using raw sockets.
 * Failure leaves gs->grab_sock < 0.  This version sets udp_hlen
 * to an expected value (42) as a flag for the extra processing
 * that this mode requires.
 */
void grab_network_prep_raw(GrabState *gs)
{
    int sock_fd, arptype, ifindex;
    char *device = gs->udp_addr;
    if (gs->verb>0) fprintf(stderr,
	"Prepping Device %s UDP port %d\n", gs->udp_addr, gs->udp_port);

    /* get a RAW socket--this presumes a newer kernel */
    sock_fd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (sock_fd < 0) { perror("socket"); return; }

    /* get the hardware address type */
    arptype = iface_get_arptype(sock_fd, device);
    if (arptype < 0) { perror("iface_get_arptype: %s"); return; }

    /* and the hardware interface number */
    ifindex = iface_get_id(sock_fd, device);
    if (ifindex < 0) { perror("iface_get_id: %s"); return; }

    /* and try to bind to it */
    if (gs->verb>0) fprintf(stderr, "GRAB binding to device %s\n", device);
    if (0 != iface_bind(sock_fd, ifindex)) {
	perror("iface_bind: %s");
	close(sock_fd);
	return;
    }

    /* success: set the socket and expected offset to the packet data */
    gs->grab_sock = sock_fd;
    gs->udp_hlen = UDP_HLEN;
    if (gs->verb>1) fprintf(stderr, "GRAB socket %d\n", gs->grab_sock);
}

/*
 * Set up the network infrastructure.  Failure leaves gs->grab_sock < 0.
 */
void grab_network_prep(GrabState *gs)
{
    struct hostent	*hostenp;
    struct sockaddr_in	dname;
    struct in_addr	addr;
    char		*peer;
    
    if (gs->verb>0) fputs("Entering grab_network_prep\n", stderr);
    gs->grab_sock = -1;
    gs->udp_hlen = 0;
    if (chk_grab_net(gs)) return;

    if (!isdigit(*(gs->udp_addr)) && strncmp(gs->udp_addr,"ANY",3)) {
	grab_network_prep_raw(gs);
	return;
    }
    if (gs->verb>0) fprintf(stderr,
	"Prepping Host %s : %d\n", gs->udp_addr, gs->udp_port);

    /* preparation for the eventual bind */
    dname.sin_family = AF_INET;
    dname.sin_port = htons(gs->udp_port);
    dname.sin_addr.s_addr = (!strcmp(gs->udp_addr, "ANY"))
	? INADDR_ANY
	: (hostenp = gethostbyname(gs->udp_addr))
		   ? *((in_addr_t *) (hostenp->h_addr_list[0]))
		   : inet_addr(gs->udp_addr);
    if (dname.sin_addr.s_addr == INADDR_NONE) {
	fprintf(stderr, "Host:port %s:%d is unintelligible.\n",
	    gs->udp_addr, gs->udp_port);
	return;
    }

    /* get the socket */
    gs->grab_sock = socket(PF_INET, SOCK_DGRAM, 0);
    if (gs->grab_sock < 0) { perror("socket"); return; }

    /* provide a diagnostic */
    addr.s_addr = dname.sin_addr.s_addr;
    peer = inet_ntoa(addr);
    if (gs->verb>0) fprintf(stderr,
	"GRAB binding UDP to address %s:%d (%s:%d)\n",
	gs->udp_addr, gs->udp_port,
	peer ? peer : "any", ntohs(dname.sin_port));

    /* bind the socket to the address */
    if (bind(gs->grab_sock, (struct sockaddr *)&dname, sizeof(dname))) {
	perror("bind");
	gs->grab_sock = -1;
	return;
    }
    if (gs->verb>1) fprintf(stderr, "GRAB socket %d\n", gs->grab_sock);
}

/*
 * This is primitive, but adequate for now.  This is broken out so
 * that later, more comprehensive start-up code could be invoked here.
 * (E.g. the upper bits of seqn may be assigned special meaning.)
 * (E.g. the value could be read from some log file.)
 */
static int64_t init_seqn(int64_t seqn)
{
    static int64_t	init = 0;
    if (init) return(seqn);
    init = 1;
    return(BMR_SEQN_ATBIRTH);
}

/*
 * Back to initial state.  Is there a socket drain/binding time issue?
 */
void grab_network_yank(void)
{
    static int again = 0;
    if (gss.verb>0) fputs("Shut down grab socket.\n", stderr);
    if (again++ && (gss.grab_sock >= 0)) close(gss.grab_sock);
    gss.grab_sock = -1;
    gss.udp_hlen = 0;
    if (gss.mem_start) free(gss.mem_start - BMR_MALLOC_GUARD);
    gss.next_seqn = init_seqn(gss.next_seqn);
    gss.grab_stop = time(0);
    set_recv_action(GRAB_ACT_IDLE, &gss);
}

/*
 * Set up the timing.
 *
 * FIXME: other timing initializations?
 * FIXME: hints to sched_setscheduler()?
 * FIXME: real-time clock capabilities?
 * FIXME: start and time(0) + 3
 */
static int grab_timing_prep(GrabState *gs)
{
    time_t  now = time(0) + 3;

    if (gs->grab_secs < 1)
	return(fputs("min secs is 1\n", stderr));
    if (gs->grab_period < 3)
	return(fputs("min period is 3\n", stderr));
    if (gs->grab_period < gs->grab_secs)
	return(fputs("period < secs\n", stderr));
    if (gs->grab_start <= now)
	return(fputs("Start < NOW + 3s\n", stderr));
    if (gs->grab_stop <= gs->grab_start)
	return(fputs("Stop < Start\n", stderr));

    gs->grab_idle   = gs->grab_period - gs->grab_secs;
    gs->grab_cycles =
	(int)floor((0.5 + gs->grab_stop - gs->grab_start) / gs->grab_period);

    if (gs->grab_flush < 0)
	return(fputs("Flush >= 0 required\n", stderr));
    if (gs->grab_flush >= gs->grab_idle)
	return(fputs("Flush < idle required\n", stderr));

    return(0);
}

/*
 * Check the time and, as required, switch between the
 * punting, active and passive states (if not IDLE).  The rep
 * counter decrements after the passive state, and
 * we shift to the idle state when it hits zero.
 *
 * We care most about performance in the BUFF and NONE states
 * (which actually have to receive packets).
 *
 * INIT -> PUNT -> NONE -> BUFF -> PUNT ( -> IDLE finally )
 *
 * Return 0 indicates we want to watch for packets,
 * Return 1 indicates the packet socket should be ignored.
 */
int check_grab_state(void)
{
    time_t now = time(0);
    switch (gss.action) {

    case GRAB_ACT_BUFF:
	if (now < gss.grab_punting) return(0);
	gss.grab_punting += gss.grab_period;
	set_recv_action(GRAB_ACT_PUNT, &gss);
	return(1);

    case GRAB_ACT_NONE:
	if (now < gss.grab_active) return(0);
	gss.grab_active += gss.grab_period;
	set_recv_action(GRAB_ACT_BUFF, &gss);
	return(0);

    case GRAB_ACT_PUNT:
	if (now < gss.grab_passive) return(1);
	gss.grab_passive += gss.grab_period;
	if (gss.grab_reps-- > 0) {
	    set_recv_action(GRAB_ACT_NONE, &gss);
	    return(0);
	}
	/* fall through to IDLE */
    default:
    case GRAB_ACT_IDLE:
	set_recv_action(GRAB_ACT_IDLE, &gss);
	break;

    case GRAB_ACT_INIT:
	gss.grab_passive = gss.grab_start - gss.grab_flush;
	gss.grab_active = gss.grab_start;
	gss.grab_punting = gss.grab_start + gss.grab_secs;
	gss.grab_reps = gss.grab_cycles;
	set_recv_action(GRAB_ACT_PUNT, &gss);
	break;

    }
    return(1);
}

/*
 * Set up the receive buffer area.
 *
 * FIXME: This can become more elaborate:
 *  check that there is enough heap space and/or sbrk more...
 *  Ideally, we want the receive buffer area to be mlock()-able into RAM.
 *  Alignment issues: mem_chunk needs to be aligned to int64_t.
 *  (GNU libc malloc() always returns 8-byte aligned memory addresses.)
 *  Other initializations include sched_setscheduler()
 *  2 seconds of slop in allocation is a random choice.
 *
 * The write pointer points to the next open slot, and an
 * invalid sequence number is written therein, pending data.
 *
 * The 2*BMR_MALLOC_GUARD bytes are to allow sloppy arithmetic
 * in the raw mode of operation.  gs->mem_start ends up pointed
 * at a region of hte proper size with BMR_MALLOC_GUARD bytes on
 * each size.
 */
static int grab_buffer_prep(GrabState *gs)
{
    if (gs->verb>0) rlimit_env();
    gs->mem_chunk = (gs->mem_type == BMR_MEM_EDVS)
		  ? gs->udp_size
		  : gs->udp_size + sizeof(int64_t);
    gs->mem_size = (2 + gs->grab_secs) * gs->udp_rate * gs->mem_chunk;
    gs->mem_start = malloc(gs->mem_size + 2*BMR_MALLOC_GUARD);
    if (!gs->mem_start)
	return(perror("grab_buffer_prep"),
	       fprintf(stderr, "Attempted %lu bytes\n",
		(unsigned long)gs->mem_size));
    gs->mem_start += BMR_MALLOC_GUARD;
    gs->mem_write = gs->mem_start;
    *(int64_t *)gs->mem_start = (gs->mem_type == BMR_MEM_EDVS)
			      ? BMR_PKT_NOTVALID
			      : BMR_SEQN_INVALID;
    return(0);
}

/*
 * Nuking the GrabInfo structure is a good start.
 */
static int grab_stats_prep(GrabState *gs)
{
    memset(&gis, 0, sizeof(gis));
    return(0);
}

/*
 * At present the only work here is to make sure
 * that the next_seqn counter is monotonic.
 */
static void update_grab_state(GrabState *gs)
{
    int64_t    old_seqn;
    old_seqn = gss.next_seqn;
    gss = *gs;	    /* make it so, #1 */
    gss.next_seqn = old_seqn;
    check_grab_state();
}

/*
 * Shut down active grab activity, and initialize for new activity.
 */
int set_grab_state(GrabState *gs)
{
    grab_network_yank();

    if (grab_stats_prep(gs))
	return(fputs("Failed to initialize grab stats.\n", stderr));

    if (grab_timing_prep(gs))
	return(fputs("Failed to initialize grab timing.\n", stderr));

    if (grab_buffer_prep(gs))
	return(fputs("Failed to initialize grab buffer.\n", stderr));

    grab_network_prep(gs);
    if (gs->grab_sock < 0)
	return(fputs("Failed to initialize grab socket.\n", stderr));

    update_grab_state(gs);
    return(0);
}

/*
 * Methods to return safe copies of our internals.
 */
GrabInfo *get_grab_info(void)
{
    static GrabInfo	mine;
    clock_gettime(CLOCK_REALTIME, &gis.grab_time);
    gis.grab_state = gss;
    mine = gis;
    return(&mine);
}
GrabState *get_grab_state(void)
{
    static GrabState	mine;
    mine = gss;
    return(&mine);
}

/*
 * eof
 */
