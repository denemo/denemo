/* midi.h
 * header files for Brian Delaney's direct output to /dev/sequencer
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>

void midi_cleanup ();

void seqbuf_dump ();

gint midi_init ();

void playnotes (gboolean doit, chord chord_to_play,int prognum);

void midi_lock ();

void midi_unlock ();

gint playsong (DenemoScore *si);
