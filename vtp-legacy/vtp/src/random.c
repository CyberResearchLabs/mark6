/*
 * Copyright (c) 2003 MIT, Haystack Observatory
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction,including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 */

/*
 *
 *
 * $Source: /usr/local/cvsroot/mit/vtp/src/random.c,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/10 00:32:49 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: random.c,v $
 * Revision 1.1  2003/09/10 00:32:49  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.4  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.3  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.2  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.1  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 *
 *
 */

/* For random number generation */
#include <sys/types.h>   /* u_long */
#include <sys/time.h>    /* gettimeofday() */
#include <unistd.h>      /* get..() */
#include <stdio.h>       /* printf() */
#include <time.h>        /* clock() */
#include <sys/utsname.h> /* uname() */
#ifdef COMMENT
#include "global.h"      /* from RFC 1321 */
#endif COMMENT
#include <openssl/md5.h> /* from RFC 1321 */

#define MD_CTX MD5_CTX
#define MDInit MD5_Init
#define MDUpdate MD5_Update
#define MDFinal MD5_Final

#include "random.h"

/*
 *
 * The following subroutine generates a random 32-bit identifier using
 * the MD5 routines published in RFC 1321 [32].  The system routines may
 * not be present on all operating systems, but they should serve as
 * hints as to what kinds of information may be used.  Other system
 * calls that may be appropriate include
 * o  getdomainname(),
 * o  getwd(), or
 * o  getrusage().
 *
 * "Live" video or audio samples are also a good source of random
 * numbers, but care must be taken to avoid using a turned-off
 * microphone or blinded camera as a source [17].
 *
 * Use of this or a similar routine is recommended to generate the
 * initial seed for the random number generator producing the RTCP
 * period (as shown in Appendix A.7), to generate the initial values for
 * the sequence number and timestamp, and to generate SSRC values.
 * Since this routine is likely to be CPU-intensive, its direct use to
 * generate RTCP periods is inappropriate because predictability is not
 * an issue.  Note that this routine produces the same result on
 * repeated calls until the value of the system clock changes unless
 * different values are supplied for the type argument.
 * @param string A pointer to a string that carries random
 * 			information used to generate the data.
 * @param length Length of the string above.
 * @return A random 32-bit integer.
 */

static u_long md_32(char *string, int length)
{
    MD_CTX context;
    union {
        char   c[16];
        u_long x[4];
    } digest;
    u_long r;
    int i;

    MDInit (&context);
    MDUpdate (&context, string, length);
    MDFinal ((unsigned char *)&digest, &context);
    r = 0;
    for (i = 0; i < 3; i++) {
        r ^= digest.x[i];
    }
    return r;
}                               /* md_32 */

/*
 * Return random unsigned 32-bit quantity.  Use 'type' argument if
 * you need to generate several different values in close succession.
 * @param type A value used to generate several different values in
 *				close succession.
 * @return A random 32 bit number.
 */
u_int32_t random32(int type)
{
    struct {
        int     type;
        struct  timeval tv;
        clock_t cpu;
        pid_t   pid;
        u_long  hid;
        uid_t   uid;
        gid_t   gid;
        struct  utsname name;
    } s;

    gettimeofday(&s.tv, 0);
    uname(&s.name);
    s.type = type;
    s.cpu  = clock();
    s.pid  = getpid();
    s.hid  = gethostid();
    s.uid  = getuid();
    s.gid  = getgid();
    /* also: system uptime */

    return md_32((char *)&s, sizeof(s));
}                               /* random32 */


