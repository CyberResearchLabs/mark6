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
 * $Id: bmr_common.h 984 2011-02-08 17:02:31Z gbc $
 *
 * Some common configuration parameters and other things.
 */

#ifndef bmr_common_h
#define bmr_common_h

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "bmr_files.h"

/*
 * The different types of servers
 */
#define BMR_SERVER_NOGOOD	0
#define BMR_SERVER_BUFFER	1
#define BMR_SERVER_RECORD	2
#define BMR_SERVER_REPLAY	3
#define BMR_SERVER_PHAKER	4

/*
 * A generic buffer size big enough (204 lines) to fit all needs.
 */
#define BMR_MAX_MESSAGE		16384

/*
 * For the truly paranoid
 */
#define BMR_MALLOC_GUARD	16384

/*
 * For time calculations
 */
#define BMR_ONENANOSECOND	1000000000L

/*
 * maximum number of files to use
 */
#define BMR_MAX_FILES		16

/*
 * Flagging overlap if we get to that point.
 */
#define BMR_OVERLAP_FORBIDDEN	0
#define BMR_OVERLAP_ISALLOWED	1

/*
 * A seqn marker for invalid packet data.
 * To allow use within the extended VDIF header,
 * we'll limit the sequence number to 56 bits.
 * The upper 8 bits are for the VDIF version (0).
 */
#define BMR_SEQN_INVALID	0x00FFFFFFFFFFFFFFLL
#define BMR_SEQN_ATBIRTH	0x0000000000000001LL

/*
 * To allow stuffing the sequence number in the
 * extended data header, we must step around the
 * extended data version which is in the high bits
 * of the first 32-bit word.  If seqn < 16777215
 * the low bits are sufficient (537s @ 31250 fr/s).
 * PACK() generates the bits to | with the ehdr octet.
 * UNPACK() retrieves the seqn from the ehdr octet.
 */
#define BMR_SEQN_LO_MASK	0x0000000000FFFFFFLL
#define BMR_SEQN_EDVMASK	0x00000000FF000000LL
#define BMR_SEQN_HI_MASK	0xFFFFFFFF00000000LL
#define BMR_SEQN_PACK(S) \
    ( ((S)&BMR_SEQN_LO_MASK) | (((S)<<8)&BMR_SEQN_HI_MASK) )
#define BMR_SEQN_UNPACK(E) \
    ( ((E)&BMR_SEQN_LO_MASK) | (((E)&BMR_SEQN_HI_MASK)>>8) )

/*
 * The packets all start with a header, and it is not
 * reasonable for all those bits to be 1's.  After that,
 * we have sometimes reason to diddle the header bits.
 */
#define BMR_PKT_NOTVALID	0xFFFFFFFFFFFFFFFFLL

/* first octet */
#define BMR_PKT_RFE_MASK	0x3F00000000000000LL
#define BMR_PKT_RFESHIFT	56;

/* second octet */
#define BMR_PKT_LB8_MASK        0x0000000000FFFFFFLL
#define BMR_PKT_LB8SHIFT        0;
#define BMR_PKT_BM1_MASK	0x7C00000000000000LL
#define BMR_PKT_BM1SHIFT	58;
#define BMR_PKT_STN_MASK	0x0000FFFF00000000LL
#define BMR_PKT_STNSHIFT	32;

/*
 * The recv() system call, but with implied socket, buffer and flags.
 * Returns number of bytes received, or negative on serious error.
 */
#ifndef Receiver_defined
#define Receiver_defined
typedef ssize_t (Receiver)(void);
#endif /* Receiver_defined */

/*
 * The sendto() system call, but with implied socket, dest, buffer and flags.
 * Returns number of bytes sent, or negative on serious error.
 */
#ifndef Sender_defined
#define Sender_defined
typedef ssize_t (Sender)(void);
#endif /* Sender_defined */

/*
 * The write() system call, but with implied file descriptor, &c.
 * Returns number of bytes written, or negative on serious error.
 */
#ifndef Writer_defined
#define Writer_defined
typedef ssize_t (Writer)(void);
#endif /* Writer_defined */

/*
 * The read() system call, but with implied file descriptor, &c.
 * Returns number of bytes read, or negative on serious error.
 */
#ifndef Reader_defined
#define Reader_defined
typedef ssize_t (Reader)(void);
#endif /* Reader_defined */

/*
 * A Receiver, or a Sender, or similar.
 */ 
#ifndef Handler_defined
#define Handler_defined
typedef ssize_t (Handler)(void);
#endif /* Handler_defined */

/*
 * Segmented time things.
 */
#ifndef TimeVal_defined
#define TimeVal_defined
typedef struct timeval TimeVal;
#endif /* TimeVal_defined */
#ifndef TimeSpec_defined
#define TimeSpec_defined
typedef struct timespec TimeSpec;
#endif /* TimeSpec_defined */

/*
 * And another layer of obfuscation hidden.
 */
#ifndef SockAddr_defined
#define SockAddr_defined
typedef struct sockaddr_in SockAddr;
#endif /* SockAddr_defined */

#endif /* bmr_common_h */

/*
 * There must be better way to do this...
 */
#ifndef PRI_BMR_defined
#define PRI_BMR_defined
#if __WORDSIZE == 32
#define PRI_TV "%ld.%06ld"
#define PRI_TS "%ld.%09ld"
#define PRI_TNS "%09ld"
#define PRI_TSS "%ld"
#elif __WORDSIZE == 64
#define PRI_TV "%ld.%06ld"
#define PRI_TS "%ld.%09ld"
#define PRI_TNS "%09ld"
#define PRI_TSS "%ld"
#else
/* #warning "__WORDSIZE not defined" */
#define PRI_TV "%d.%06d"    /* TimeVal */
#define PRI_TS "%d.%09d"    /* TimeSpec */
#define PRI_TNS "%09d"      /* TimeSpec ns */
#define PRI_TSS "%d"
#endif /* __WORDSIZE */
#endif /* PRI_BMR_defined */

/*
 * eof
 */
