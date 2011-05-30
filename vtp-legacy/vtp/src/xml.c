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
 * $Source: /usr/local/cvsroot/mit/vtp/src/xml.c,v $
 * $Revision: 1.2 $
 * $Date: 2003/09/10 06:19:29 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: xml.c,v $
 * Revision 1.2  2003/09/10 06:19:29  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.1  2003/09/10 00:32:50  davidlapsley
 * *** empty log message ***
 *
 * Revision 1.2  2003/09/10 00:21:17  davidlapsley
 * Modified build system.
 *
 * Revision 1.1  2003/09/09 22:30:15  davidlapsley
 * - Added XML parser to the main branch. Converted configuration from
 * command line driven to configuration file driven (too many parameters
 * for the command line).
 * - Integrated server with RTP/RTCP logic.
 *
 *
 *
 */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <mysock.h>

#include "xml.h"

/* Called for open tags <foo bar="baz"> */
static void start_element(GMarkupParseContext *context,
                          const gchar         *element_name,
                          const gchar        **attribute_names,
                          const gchar        **attribute_values,
                          gpointer             user_data,
                          GError             **error)
{
    printf("Start element\n");
    assert(context);
    assert(element_name);
    printf("\tElement name: %s\n", element_name);
    printf("\tAttribute name: %s\n", attribute_names[0]);

    xml_options_t* o=(xml_options_t*)user_data;
    char *elt=malloc(strlen(element_name)+1);
    strcpy(elt, element_name);
    if (element_name) {
        o->tag_stack=g_list_prepend(o->tag_stack, elt);
    } else {
        free(elt);
    }
}

/* Called for close tags </foo> */
static void end_element(GMarkupParseContext *context,
                        const gchar         *element_name,
                        gpointer             user_data,
                        GError             **error)
{
    printf("End element\n");
    assert(context);
    assert(element_name);
    printf("\tElement name: %s\n", element_name);

    xml_options_t* o=(xml_options_t*)user_data;
    GList* first=g_list_first(o->tag_stack);
    if (first && first->data) {
        o->tag_stack=g_list_remove(o->tag_stack, first->data);
    }
}

/* Called for character data */
/* text is not nul-terminated */
static void text(GMarkupParseContext *context,
                 const gchar         *text,
                 gsize                text_len,
                 gpointer             user_data,
                 GError             **error)
{
    printf("text\n");
    xml_options_t* o=(xml_options_t*)user_data;
    GList* first=g_list_first(o->tag_stack);
    char* buf=malloc(text_len+1);
    strncpy(buf, text, text_len+1);
    if (first && first->data) {
        printf("\tElement name: %s\n", (char*)first->data);
        printf("\ttext: %s\n", text);
        if (strcmp(first->data, "mode")==0) {
            if (strcmp(buf, "client")==0) {
                o->options.mode=CLIENT;
            } else if (strcmp(buf, "server")==0) {
                o->options.mode=SERVER;
            } else if (strcmp(buf, "test")==0) {
                o->options.mode=TEST;
            }
        } else if (strcmp(first->data, "ctrl_port")==0) {
            o->options.ctrl_port=atoi(buf);
        } else if (strcmp(first->data, "data_port")==0) {
            o->options.data_port=atoi(buf);
        } else if (strcmp(first->data, "ip")==0) {
            my_inet_aton(buf, &o->options.ip);
        } else if (strcmp(first->data, "file_name")==0) {
            o->options.file_name = malloc(strlen(buf)+1);
            assert( strncpy( o->options.file_name, buf, strlen(buf)+1) );
        } else if (strcmp(first->data, "rtcp_bw")==0) {
            o->options.rtcp_bw = atol(buf);
        } else if (strcmp(first->data, "avg_rtcp_size")==0) {
            o->options.avg_rtcp_size = atol(buf);
        } else if (strcmp(first->data, "ssrc")==0) {
            o->options.ssrc = atol(buf);
        } else if (strcmp(first->data, "sampling_frequency")==0) {
            o->options.sampling_frequency = atol(buf);
        } else if (strcmp(first->data, "samples_per_packet")==0) {
            o->options.samples_per_packet = atol(buf);
        } else if (strcmp(first->data, "bits_per_sample")==0) {
            o->options.bits_per_sample = atol(buf);
        } else if (strcmp(first->data, "sz_ctrl_buf")==0) {
            o->options.sz_ctrl_buf = atol(buf);
        }
    }
    free(buf);
}

/* Called for strings that should be re-saved verbatim in this same
* position, but are not otherwise interpretable.  At the moment
* this includes comments and processing instructions.
*/
/* text is not nul-terminated. */
static void passthrough(GMarkupParseContext *context,
                        const gchar         *passthrough_text,
                        gsize                text_len,
                        gpointer             user_data,
                        GError             **error)
{
    printf("passthrough\n");

}

/* Called on error, including one set by other
* methods in the vtable. The GError should not be freed.
*/
static void error(GMarkupParseContext *context,
                  GError              *error,
                  gpointer             user_data)
{
    printf("error\n");

}

extern int xml_parse(xml_options_t* o, char* file_name)
{
    assert(o);
    assert(file_name);
    
    GMarkupParser* parser=malloc(sizeof(GMarkupParser));
    parser->start_element=start_element;
    parser->end_element=end_element;
    parser->text=text;
    parser->passthrough=passthrough;
    parser->error=error;

    /* Not used */
    GMarkupParseFlags flags;
    /* Tag stack */
    o->tag_stack=NULL;

    /* Not used
        * GDestroyNotify user_data_dnotify;
    */
    GMarkupParseContext* p=g_markup_parse_context_new(parser,
                                                      flags,
                                                      o,
                                                      NULL);

    FILE* in=fopen(file_name, "r");
    if (in==NULL) {
        syslog(LOG_WARNING, "Unable to open file\n");
        return(-1);
    }
    const int sz_buf=128;
    char buf[sz_buf];
    int bytes_read=0;
    GError *err=malloc(sizeof(GError));
    do {
        bytes_read=fread(buf, 1, sz_buf, in);
        if (!g_markup_parse_context_parse(p, buf, bytes_read, &err)) {
            syslog(LOG_WARNING, "Parse error\n");
        }
    } while (!(feof(in) || ferror(in)) && bytes_read>0);
    g_markup_parse_context_end_parse(p, &err);
    /* Also frees the allocated memory */
    g_markup_parse_context_free(p);
    free(parser);
    free(err);
    fclose(in);

    syslog(LOG_WARNING, "Options");
    syslog(LOG_WARNING, "\tctrl_port:          \t%d\n", o->options.ctrl_port);
    syslog(LOG_WARNING, "\tdata_port:          \t%d\n", o->options.data_port);
    syslog(LOG_WARNING, "\tctrl_port:          \t%d\n", o->options.ctrl_port);
    syslog(LOG_WARNING, "\tip:                 \t0%x\n", o->options.ip);
    syslog(LOG_WARNING, "\tmode:               \t%d\n", o->options.mode);
    syslog(LOG_WARNING, "\tfile_name:          \t%s\n", o->options.file_name);
    syslog(LOG_WARNING, "\trtcp_bw:            \t%d\n", o->options.rtcp_bw);
    syslog(LOG_WARNING, "\tavg_rtcp_size:      \t%d\n", o->options.avg_rtcp_size);
    syslog(LOG_WARNING, "\tssrc:               \t%d\n", o->options.ssrc);
    syslog(LOG_WARNING, "\tsampling_frequency: \t%d\n", o->options.sampling_frequency);
    syslog(LOG_WARNING, "\tbits_per_sample:    \t%d\n", o->options.bits_per_sample);
    syslog(LOG_WARNING, "\tsz_ctrl_buf:        \t%d\n", o->options.sz_ctrl_buf);
    return(0);
    
}


#ifdef _DEBUG
extern int xml_test()
{
    GMarkupParser* parser=malloc(sizeof(GMarkupParser));
    parser->start_element=start_element;
    parser->end_element=end_element;
    parser->text=text;
    parser->passthrough=passthrough;
    parser->error=error;

    /* Not used */
    GMarkupParseFlags flags;
    /* Tag stack */
    xml_options_t o;
    o.tag_stack=NULL;

    /* Not used
     * GDestroyNotify user_data_dnotify;
     */
    GMarkupParseContext* p=g_markup_parse_context_new(parser,
                                                      flags,
                                                      &o,
                                                      NULL);

    printf("xml_test()\n");
    FILE* in=fopen("egae.xml", "r");
    const int sz_buf=128;
    char buf[sz_buf];
    int bytes_read=0;
    GError *err=malloc(sizeof(GError));
    do {
        bytes_read=fread(buf, 1, sz_buf, in);
        if (!g_markup_parse_context_parse(p, buf, bytes_read, &err)) {
            printf("Parse error\n");
        }
    } while (!(feof(in) || ferror(in)) && bytes_read>0);
    g_markup_parse_context_end_parse(p, &err);
    g_markup_parse_context_free(p);
    fclose(in);

    printf("Options");
    printf("\tctrl_port:          \t%d\n", o.options.ctrl_port);
    printf("\tdata_port:          \t%d\n", o.options.data_port);
    printf("\tctrl_port:          \t%d\n", o.options.ctrl_port);
    printf("\tip:                 \t0%x\n", o.options.ip);
    printf("\tmode:               \t%d\n", o.options.mode);
    printf("\tfile_name:          \t%s\n", o.options.file_name);
    printf("\trtcp_bw:            \t%d\n", o.options.rtcp_bw);
    printf("\tavg_rtcp_size:      \t%d\n", o.options.avg_rtcp_size);
    printf("\tssrc:               \t%d\n", o.options.ssrc);
    printf("\tsampling_frequency: \t%d\n", o.options.sampling_frequency);
    printf("\tbits_per_sample:    \t%d\n", o.options.bits_per_sample);
    printf("\tsz_ctrl_buf:        \t%d\n", o.options.sz_ctrl_buf);
    return(1);
}
#endif
