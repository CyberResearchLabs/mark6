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
 * $Id: bmr_cache.c 497 2010-07-16 17:40:16Z gbc $
 *
 * Provides a (simple) mechanism for ganging up a set of commands.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bmr_server.h"

extern int verb;

/*
 * The cache is a newline separated list.
 */
static char *cached_cmds = 0;
static int   cached_size = 0;

/*
 * Visibility for consumers.
 */
char *bmr_cached_cmds_ptr(void)
{
    return(cached_cmds);
}

/*
 * Cleanup.
 */
void bmr_cache_free(void)
{
    free(cached_cmds);
    cached_cmds = 0;
    cached_size = 0;
}

/*
 * For debugging
 */
void bmr_show_cached_cmds(void)
{
    fputs("Cache has:\n", stderr);
    fputs(cached_cmds, stderr);
    fputs("=========.\n", stderr);
}

/*
 * Caches a command
 */
int bmr_parse_cache_cmd(char *cmd)
{
    int	    len = strlen(cmd);
    char    *end;
    cached_size += len + 3;
    cached_cmds = realloc(cached_cmds, cached_size);
    if (!cached_cmds) return(perror("realloc"),1);
    if (verb>1) fprintf(stderr, "Saving cmd %s\n", cmd);
    end = cached_cmds + strlen(cached_cmds);
    strcpy(end, cmd);
    strcat(end, "\n");
    return(0);
}

/*
 * Caches a file of commands
 */
int bmr_parse_cache_file(char *file)
{
    FILE    *fp = fopen(file, "r");
    char    cmd[256];
    int	    x = 0;
    if (verb>0) fprintf(stderr, "Opening file %s\n", file);
    if (!fp) return(perror("fopen"),1);
    while (!x && fgets(cmd, sizeof(cmd), fp))
	x += bmr_parse_cache_cmd(cmd);
    if (!feof(fp)) x ++;
    fclose(fp);
    return(x);
}

/*
 * eof
 */
