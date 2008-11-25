/* midi.h
 * header file for Brian Delaney's direct output to /dev/sequencer
 * and input from /dev/midi
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>

void midi_cleanup ();

gint midi_init ();

void playnotes (gboolean doit, chord chord_to_play,int prognum);

void play_midikey(gint key, double duration, double volume, gint channel);
void process_midi_event(gchar *buf);
