/*
 * convert from SoundFont file to vkeybd preset list file
 *
 * Copyright (c) 1999-2000 by Takashi Iwai
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdlib.h>
#include "util.h"
#include "sffile.h"

/* parse soundfont file and give the preset list */
int main(int argc, char **argv)
{
	FILE *fp;
	SFInfo sf;
	int i;

	if (argc < 2) {
		fprintf(stderr, "usage: sftovkb soundfont\n");
		exit(1);
	}

	/*fprintf(stderr, "opening %s..\n", argv[1]);*/
	if ((fp = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr, "can't open soundfont file\n");
		exit(1);
	}
	if (load_soundfont(&sf, fp, TRUE)) {
		fprintf(stderr, "fail to read soundfont file\n");
		exit(1);
	}
	fclose(fp);

	for (i = 0; i < sf.npresets-1; i++) {
		char *p;
		/* convert illegal characters */
		for (p = sf.preset[i].hdr.name; *p; p++) {
			if (!isprint(*p) || *p == '{' || *p == '}')
				*p = ' ';
			else if (*p == '[')
				*p = '(';
			else if (*p == ']')
				*p = ')';
		}
		printf("%d %d %s\n", 
		       sf.preset[i].bank,
		       sf.preset[i].preset,
		       sf.preset[i].hdr.name);
	}
	free_soundfont(&sf);
	return 0;
}

