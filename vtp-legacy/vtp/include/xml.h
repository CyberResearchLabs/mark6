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
 * $Source: /usr/local/cvsroot/mit/vtp/include/xml.h,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/10 00:32:46 $
 * $State: Exp $
 * $Author: davidlapsley $
 *
 * $Log: xml.h,v $
 * Revision 1.1  2003/09/10 00:32:46  davidlapsley
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

#include <glib.h>
#include <vtp.h>

#ifndef _XML_H_
#define _XML_H_

typedef struct {
    vtp_options_t options;
    GList* tag_stack;
} xml_options_t;

extern int xml_parse(xml_options_t* o, char* file_name);

#ifdef _DEBUG
extern int xml_test();
#endif

#endif
