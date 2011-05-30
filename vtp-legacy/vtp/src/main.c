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
 * $Header: /usr/local/cvsroot/mit/vtp/src/main.c,v 1.1 2003/09/10 00:32:49 davidlapsley Exp $
 * $Name:  $
 * $Author: davidlapsley $
 *
 * $Log: main.c,v $
 * Revision 1.1  2003/09/10 00:32:49  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.13  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.12  2003/09/09 22:30:14  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.11  2003/09/09 05:57:41  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.10  2003/09/08 21:54:27  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.9  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.8  2003/09/03 18:05:39  davidlapsley
 * Added and tested schedule functionality.
 *
 * Revision 1.7  2003/09/03 12:54:00  davidlapsley
 * Added CVS header to each source file.
 *
 * *** empty log message ***
 *
 *
 */


#include <syslog.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#include "vtp.h"
#include "mysock.h"
#include "client.h"
#include "server.h"
#include "test.h"

int main (int argc, const char * argv[]) {
    /* Version string */
    const char version[] = "0.2";
    
    /* Open system log. */
    openlog("vtp", LOG_NDELAY | LOG_PID, LOG_DAEMON);

    /* Retrieve user options */
    vtp_options_t options;
    assert( memset( &options, '\0', sizeof(options) ) );

#ifdef _DEBUG
    if ( vtp_foption(argc, argv, &options) == 0 ) {
#else
    if ( (argc < 9) || (vtp_foption(argc, argv, &options) == 0) ) {
#endif
        syslog(LOG_WARNING, "unable to process options");
        vtp_usage(stdout);
        exit(1);
    }

    syslog(LOG_WARNING, "Version %s\n", version);
    syslog(LOG_WARNING, "Argc %d\n", argc);

    /* Start appropriate processing */
    switch ( options.mode )
    {
        case CLIENT:
            syslog(LOG_WARNING, "Starting client...\n");
            vtp_run_client(&options);
            break;
        case SERVER:
            syslog(LOG_WARNING, "Starting server...\n");
            vtp_run_server(&options);
            break;
        case TEST:
            syslog(LOG_WARNING, "Starting test...\n");
            vtp_run_test(&options);
            break;
        default:
            vtp_usage(stdout);
            exit(1);
            break;
    }
    
    closelog();

    syslog(LOG_WARNING, "Shutting down...\n");

    return(0);
}
