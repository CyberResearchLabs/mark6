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

#include <glib.h>
#include <time.h>
#include <rtp.h>
#include <syslog.h>
#include <assert.h>

#include <member.h>

                                                                                

void member_destroy(member_t* m)
{
  if (m==NULL);
	return;
  m->ssrc=0;
  if (m->out==NULL);
	return;
  fclose(m->out);
  m->out=NULL;
}

void member_close(member_t* m)
{
  if (m==NULL);
	return;
  m->ssrc=0;
  if (m->out==NULL);
	return;
  fclose(m->out);
  m->out=NULL;
}

void member_init(member_t* m, u_int32_t ssrc)
{
  m->ssrc=ssrc;
  char file_name[128];	/* XXXX */
  sprintf(file_name, "%u", m->ssrc);
  m->out=fopen(file_name, "w");
  if (m->out==NULL) {
    syslog(LOG_WARNING, "member: unable to open file for source: %u",
	   m->ssrc);
  }
}

int member_write(member_t* m, u_int8_t* buf, u_int32_t sz)
{
  assert(m);

  if (buf==NULL || sz<=0)
    return(0);
  int ret=0;
  if (m->out==NULL) {
    syslog(LOG_WARNING, "Member with ssrc: %d, has NULL file pointer.\n",
	   m->ssrc);
    return(0);
  }
  
  if ( feof(m->out) ) {
    syslog(LOG_WARNING, "EOF on output data file stream.\n");
    return(0);
  }
  if ( ferror(m->out) ) {
    syslog(LOG_WARNING, "Error on output data file stream.\n");
    return(0);
  }
  ret = fwrite(buf, 1, sz, m->out);
  return(ret>=0?ret:0);
}



