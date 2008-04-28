/* frogio.h
 *
 * Function prototypes to the frog i/o language
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 1999, 2000, 2001 Adam Tee
 */ 

#ifndef FROGIO_H
#define FROGIO_H
#include <denemo/denemo.h> 
#include <stdio.h>
#include <locale.h>
struct durations
{
  gfloat beat;
  gfloat prevduration;
};

int filesaveselection (gchar * file, DenemoScore *si);
void writesinglestave (FILE *fp, DenemoStaff *curstaffstruct,gint start, gint end);
void writepolyphony (FILE *fp, DenemoStaff *curstaffstruct, DenemoStaff *extravoice);
struct durations writeobjects (FILE * fp, DenemoStaff * curstaffstruct,
			       DenemoObject * mudelaitem, gint bar, gint k,
			       struct durations *beat);

void unimplemented ();
int filesave (gchar * file, DenemoScore *si, gint start, gint end,
	      gint polyphony);
gfloat durationtofloat (gint duration, gint dots);
char *pitchtonotename (gint pitch, gint enharmonic, char *note);
int pitchtooctave (gint pitch);
gint Keytoint (char *keyname);
enum clefs cleftoenum (char *clefname);
gint fetchenharmonic (char *notename);
gint fetchnotename (char *notename, int octave);


gint floattoduration (float duration, gboolean is_tup);
void savekey (FILE * fp, gint key, gboolean min, gint bar, gfloat beat);
void saveclef (FILE * fp, gint clef);

#endif
