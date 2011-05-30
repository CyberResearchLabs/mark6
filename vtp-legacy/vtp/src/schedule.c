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
 * $Source: /usr/local/cvsroot/mit/vtp/src/schedule.c,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/10 06:19:29 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: schedule.c,v $
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:50  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.6  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.5  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.4  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.3  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.2  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.1  2003/09/03 19:57:23  davidlapsley
 * Added new rtcp module that implements the RdoubleTCP session functionality.
 * All RTCP state is stored in this module and state management functions
 * are also implemented in this module.
 *
 * RTP module is now mainly for defining RTP/RTCP data types (e.g. packet
 * formats etc.). Most functinality has moved into rtcp module.
 *
 * Currently code compiles but does not link. Need to implement module (interfaces
 * have been done).
 *
 *
 *
 */

#include <assert.h>
#include <limits.h>
#include <syslog.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <rtp.h>

#include "schedule.h"


static int compare_data_func(gconstpointer a,
                             gconstpointer b,
                             gpointer user_data)
{
    if (*(long*)a < *(long*)b) {
        return(-1);
    } else if (*(long*)a > *(long*)b) {
        return(1);
    } else {
        return(0);
    }
}

static void key_destroy_func(gpointer d)
{
    free(d);
}

static void value_destroy_func(gpointer d)
{
    free(d);
}


int schedule_init(schedule_t* s)
{
    assert(s);
    if (s->event_tree == NULL)
        return(0);

    if (!g_thread_supported ()) g_thread_init (NULL);
    
    s->event_tree_mutex=g_mutex_new();
    if (s->event_tree==NULL)
        return(0);

    g_mutex_lock(s->event_tree_mutex);
    s->event_tree = g_tree_new_full(compare_data_func,
                                    NULL,
                                    key_destroy_func,
                                    value_destroy_func);
    g_mutex_unlock(s->event_tree_mutex);

    return(1);
}


void schedule_destroy(schedule_t* s)
{
    g_mutex_lock(s->event_tree_mutex);
    assert(s);
    g_tree_destroy(s->event_tree);
    s->event_tree=NULL;
    g_mutex_unlock(s->event_tree_mutex);
}

void schedule_schedule(schedule_t* s, double *t, rtcp_event_t *e)
{
    assert(s);
    assert(t);
    assert(e);
    g_mutex_lock(s->event_tree_mutex);
    g_tree_insert(s->event_tree, t, e);
    g_mutex_unlock(s->event_tree_mutex);
}

void schedule_reschedule(schedule_t* s, double t, rtcp_event_t *e)
{
    /* XXXX */
    assert(s);
    assert(e);
}

static gboolean schedule_print_traverse_func(gpointer key,
                                             gpointer value,
                                             gpointer data)
{
    assert(key);
    assert(value);
    printf("gtree: key = %ld\n", *(long*)key);
    return(FALSE);
}

extern void schedule_print(schedule_t* s)
{
    assert(s);
    g_mutex_lock(s->event_tree_mutex);
    g_tree_foreach(s->event_tree, schedule_print_traverse_func, NULL);
    g_mutex_unlock(s->event_tree_mutex);
}

static gboolean schedule_earliest_event_traverse_func(gpointer key,
                                                      gpointer value,
                                                      gpointer data)
{
    assert(key);
    assert(value);
    assert(data);
    memcpy(data, key, sizeof(long));
    return(TRUE);
}

extern rtcp_event_t* schedule_earliest_event(schedule_t* s, double* k)
{
    assert(s);
    assert(k);
    
    gpointer key=malloc(sizeof(long));
    memset(key, '\0', sizeof(long));
    gpointer retvalue=NULL;
    g_mutex_lock(s->event_tree_mutex);
    g_tree_foreach(s->event_tree, schedule_earliest_event_traverse_func, key);
    if (key) {
        gpointer value=g_tree_lookup(s->event_tree, key);
        if (value) {
            retvalue=malloc(sizeof(rtcp_event_t));
            memcpy(retvalue, value, sizeof(rtcp_event_t));
            memcpy(k, key, sizeof(long));
            g_tree_remove(s->event_tree, key);
        } else {
            retvalue=NULL;
            k=NULL;
        }
    } else {
        retvalue=NULL;
        k=NULL;
    }
    g_mutex_unlock(s->event_tree_mutex);
    return(retvalue);
}

extern rtcp_event_t* schedule_earliest_event_cp(schedule_t* s, double* k)
{
    assert(s);
    assert(k);

    gpointer key=malloc(sizeof(long));
    memset(key, '\0', sizeof(long));
    gpointer retvalue=NULL;
    g_mutex_lock(s->event_tree_mutex);
    g_tree_foreach(s->event_tree, schedule_earliest_event_traverse_func, key);
    if (key) {
        gpointer value=g_tree_lookup(s->event_tree, key);
        if (value) {
            retvalue=malloc(sizeof(rtcp_event_t));
            memcpy(retvalue, value, sizeof(rtcp_event_t));
            memcpy(k, key, sizeof(long));
        } else {
            retvalue=NULL;
            k=NULL;
        }
    } else {
        syslog(LOG_WARNING, "Unable to find earliest event");
        retvalue=NULL;
        k=NULL;
    }
    g_mutex_unlock(s->event_tree_mutex);
    return(retvalue);
}

/*
 * Schedule test
 */
#ifdef _DEBUG
int schedule_test()
{
    schedule_t s;
    schedule_init(&s);
    int i=0;
    double* t;
    rtcp_event_t* e;
    for (i=0; i<10; i++) {
        t=malloc(sizeof(double));
        *t=i;
        e=malloc(sizeof(rtcp_event_t));
        e->etype = i%2?EVENT_BYE:EVENT_REPORT;
        schedule_schedule(&s, t, e);
    }
    schedule_print(&s);
    gpointer p = NULL;
    gpointer k = malloc(sizeof(long));
    while ( (p=schedule_earliest_event(&s, k))) {
        printf("key: %lf, value: %d\n", *(double*)k, ((rtcp_event_t*)p)->etype);
    }

    schedule_destroy(&s);
    return(1);
}
#endif
