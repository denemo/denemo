#ifndef EXPORTCSOUND_H
#define EXPORTCSOUND_H

#include <denemo/denemo.h>

gchar pitch2char(int pitch);
float duration2time(int duration, int dots, int tempo);
float pitchtopch(gchar pitch, int enshift, int octave);
int exportcsound(gchar *thefilename, DenemoScore *si, gint start,
		 gint end);
int write_stave(FILE *fp, DenemoStaff *curstaffstruct, gint i, gint start, gint end,
		DenemoScore *si);


#endif /*EXPORTCSOUND_H*/
