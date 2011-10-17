//      denemo_client.c
//      
//      Copyright 2011 Richard Shann 
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.

#include <stdio.h>
#include <signal.h>
#include <glib.h>
#include "config.h"
int main(int argc, char **argv)
{
	int ret = 0;
	printf("Denemo argc %d arg %s\n", argc, argv[argc-1]);
	if(argc==4) { //line column and filename, though we don't need that
	FILE *fp = fopen("/home/rshann/.denemo-0.9.3/pid", "r");
	if(fp) {
				__pid_t pid;
				fscanf(fp, "%d", &pid);			
				fclose(fp);
				fp = fopen(g_build_filename(g_get_home_dir(),".denemo-"VERSION, "lylocation.txt", NULL), "w");
				if(fp) {
					fprintf(fp, "%s %s", argv[1], argv[2]);
					fclose(fp);

				if(pid)
					ret = kill( pid, SIGUSR1);
				else
					ret = -1;
				}
			}
	} else
		ret = -1;	
	return ret;
}

