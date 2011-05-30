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
 * $Source: /usr/local/cvsroot/mit/vtp/src/test.c,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/10 06:19:29 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: test.c,v $
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:50  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.9  2003/09/10 00:21:16  davidlapsley
 * Modified build system.
 *
 * Revision 1.8  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 * Revision 1.7  2003/09/09 18:01:37  davidlapsley
 * Initial integration of rtp- and rtcp-related modules with client-related
 * modules.
 *
 * Revision 1.6  2003/09/09 05:57:42  davidlapsley
 * - Removed members and senders from rtcp_member_t (this logic already
 * maintained in OnReceive).
 * - Completed rtcp_send_report(), rtcp_send_bye() functions.
 * - Made buffer sizes in rtcp_session_t an initialization parameter.
 * - client.c: integrating rtcp module
 *
 * Revision 1.5  2003/09/08 21:54:28  davidlapsley
 * - Added random module for generating random 32 bit integers.
 * - Replaced u_intXX style typedefs with standard u_intXX_t types.
 * - Added variables to rtcp_t session member.
 * - Added code file template: template.h which includes the MIT license
 * header.
 *
 * Revision 1.4  2003/09/04 22:07:52  davidlapsley
 * Added new module: members to encapsulate the member tree. The member
 * tree consists of a struct: members_t that encapsulates a glib balances
 * tree. This tree is used to store a list of session members and their
 * state.
 *
 * Revision 1.3  2003/09/03 18:05:39  davidlapsley
 * Added and tested schedule functionality.
 *
 * Revision 1.2  2003/09/03 12:54:01  davidlapsley
 * Added CVS header to each source file.
 *
 *
 */


#include "test.h"

/* For XML */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

/* Event scheduler */
#include <glib.h>

/* Modules to test */
#include <rtp.h>
#include <schedule.h>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <sys/time.h>
#include <assert.h>

#include <xml.h>

/* Sandbox testing */
#ifdef _DEBUG
int vtp_test_hash();
int vtp_test_list();
int vtp_test_xml();
int vtp_test_signal();
int vtp_test_wraparound();
int vtp_test_address();
#endif

int vtp_run_test(vtp_options_t* options)
{
    int ret=1;
#ifdef _DEBUG
    ret=vtp_test_hash()?ret:0;
    ret=vtp_test_list()?ret:0;
    ret=vtp_test_xml()?ret:0;
    ret=vtp_test_signal()?ret:0;
    ret=vtp_test_wraparound()?ret:0;
    ret=vtp_test_address()?ret:0;
    ret=schedule_test()?ret:0;
    ret=xml_test()?ret:0;
#else
    syslog(LOG_WARNING, "Debug support not compiled in.\n");
#endif
    return(ret);
}

#ifdef _DEBUG
/*
 * Hash test
 */
struct point {
    int x;
    int y;
};


void key_destroy_func(gpointer data)
{
    free(data);
}

void value_destroy_func(gpointer data)
{
    free(data);
}

int vtp_test_hash()
{
    printf("vtp_test_list()\n");
    GHashTable *ht = NULL;
    ht = g_hash_table_new_full( g_str_hash,
                                g_str_equal,
                                key_destroy_func,
                                value_destroy_func );
    int i=0;
    for (i=0; i<10; i++) {
        struct point* p;
        p = malloc(sizeof(struct point));
        p->x = i;
        p->y = 2*i;
        char* key = malloc(10);
        sprintf(key, "%d", i);
        void* value = p;
        g_hash_table_insert(ht, key, value);
    }

    for (i=0; i<10; i++) {
        struct point* p;
        char* key = malloc(10);
        sprintf(key, "%d", i);
        p = (struct point*)g_hash_table_lookup(ht, key);
        if (p)
            printf("Key: %s, Value: %d.%d.\n", key, p->x, p->y);
        else
            printf("Key: %s, element not found.\n", key);
        free(key);
    }
    for (i=0; i<10; i++) {
        struct point* p;
        char* key = malloc(10);
        sprintf(key, "%d", i);
        p = (struct point*)g_hash_table_remove(ht, key);
        if (p)
            printf("Key: %s, remove element\n", key);
        else
            printf("Key: %s, element not found.\n", key);
        free(key);
    }

    g_hash_table_destroy(ht);
    return(1);
}

/*
 * List test
 */
int vtp_test_list()
{
    GList* l = NULL;

    printf("vtp_test_list()\n");
    
    gpointer data;

    int i=0;
    for (i=0; i<10; i++) {
        data = malloc(sizeof(struct point));
        ((struct point*)data)->x=i;
        ((struct point*)data)->y=2*i;
        l = g_list_append(l, data);
    }
    GList* curr=NULL;
    for (curr=g_list_first(l); curr; curr=g_list_next(curr) ) {
        struct point* p = curr->data;
        if (p)
            printf("List: Value: %d.%d.\n", p->x, p->y);
        else
            printf("List: element not found.\n");
    }
    for (curr=g_list_first(l); curr; ) {
        curr = g_list_remove(l, curr->data);
    }
     g_list_free(l);
    return(1);
}

/*
 * XML Tests
 */
void vtp_parse_data_receiver(xmlDocPtr doc,
                             xmlNodePtr cur,
                             vtp_options_t* options) {
    xmlChar *key;
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"ip"))) {
            key = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
            printf("vtp_parse_data_receivers: ip = %s\n", key);
            xmlFree(key);
        } else if ((!xmlStrcmp(cur->name, (const xmlChar *)"port"))) {
            key = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
            printf("vtp_parse_data_receivers: port = %s\n", key);
            xmlFree(key);
        }
        cur = cur->next;
    }
    return;
}

void vtp_parse_control_receiver (xmlDocPtr doc,
                                 xmlNodePtr cur,
                                 vtp_options_t* options) {
    xmlChar *key;
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"ip"))) {
            key = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
            printf("vtp_parse_control_receivers: ip = %s\n", key);
            xmlFree(key);
        } else if ((!xmlStrcmp(cur->name, (const xmlChar *)"port"))) {
            key = xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
            printf("vtp_parse_control_receivers: port = %s\n", key);
            xmlFree(key);
        }
        cur = cur->next;
    }
    return;
}

void vtp_parse_data_receivers(xmlDocPtr doc,
                              xmlNodePtr cur,
                              vtp_options_t* options)
{
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"data_receiver"))){
            vtp_parse_data_receiver(doc, cur,  options);
        }
        cur = cur->next;
    }
}

void vtp_parse_control_receivers(xmlDocPtr doc,
                                 xmlNodePtr cur,
                                 vtp_options_t* options)
{
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"control_receiver"))){
            vtp_parse_control_receiver(doc, cur, options);
        }
        cur = cur->next;
    }
}

void vtp_parse_profile(xmlDocPtr doc,
                       xmlNodePtr cur,
                       vtp_options_t* options)
{
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"data_receivers"))){
            vtp_parse_data_receivers(doc, cur, options);
        } else if ((!xmlStrcmp(cur->name, (const xmlChar *)"control_receivers"))){
            vtp_parse_control_receivers(doc, cur, options);
        }
        cur = cur->next;
    }
}


static void vtp_parse_doc(char *docname,
                          vtp_options_t* options) {
    xmlDocPtr doc;
    xmlNodePtr cur;

    doc = xmlParseFile(docname);

    if (doc == NULL ) {
        fprintf(stderr,"Document not parsed successfully. \n");
        return;
    }

    cur = xmlDocGetRootElement(doc);

    if (cur == NULL) {
        fprintf(stderr,"empty document\n");
        xmlFreeDoc(doc);
        return;
    }

    if (xmlStrcmp(cur->name, (const xmlChar *) "egae")) {
        fprintf(stderr,"document of the wrong type, root node != egae");
        xmlFreeDoc(doc);
        return;
    }

    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if ((!xmlStrcmp(cur->name, (const xmlChar *)"profile"))){
            vtp_parse_profile(doc, cur, options);
        }
        cur = cur->next;
    }

    xmlFreeDoc(doc);
    return;
}

int vtp_test_xml()
{
    char *docname = "egae.xml";
    printf("About to parse doc.\n");
    vtp_parse_doc (docname, NULL);
    printf("Parsed doc.\n");
    return(1);
}


static void sig_handler(int signo)
{
    struct timeval t;
    gettimeofday(&t, NULL);
    switch (signo) {
        case SIGUSR1:
            printf("%f: Received SIGUSR1\n", t.tv_sec+t.tv_usec/1e6 );
            break;
        default:
            break;
    }
}

int vtp_test_signal()
{
    struct sigaction act, old_act;
    act.sa_handler=sig_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags=0;
    assert(sigaction(SIGUSR1, &act, &old_act)==0);

    struct timespec rqtp, rmtp;
    rqtp.tv_sec=5;
    rqtp.tv_nsec=0;

    pid_t mypid=getpid();
    printf("My pid: %d\n", mypid);
    /* kill(mypid, SIGUSR1); */

    while (1) {
        if (nanosleep(&rqtp, &rmtp)==-1) {
            printf("Unslept amount: %f\n", rmtp.tv_sec+rmtp.tv_nsec/1e9);
            break;
        } else {
            printf("Slept for full amount\n");
        }
    }
    
    return(1);
}

int vtp_test_wraparound()
{
    unsigned int i, j, k=0;
    for (i=0, j=0; j<3 ; i+=1000000) {
        if (k>i) {
            printf("i:%u k:%u\n", i, k);
            j++;
        }
        k=i;
        printf("%u\n", i);
    }
    return(1);
}

int vtp_test_address()
{
    rtp_hdr_t* hdr=malloc(sizeof(rtp_hdr_t));
    printf("hdr->csrc: %u\n", (unsigned int)hdr->csrc);
    gpointer p=&(hdr->csrc[0]);
    printf("&(hdr->csrc[0]): %u\n", (unsigned int)p);
    return(1);
}

#endif

