/* exportmidi.h
 * Header file for Standard MIDI output
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *
 * (c) 2001 Per Andersson 
 * 2009 RTS */

#include <gtk/gtk.h>

gdouble exportmidi (gchar * filename, DenemoMovement * si, gint start, gint end);

gchar *substitute_midi_values (gchar * str, gint channel, gint volume);

void free_midi_data (DenemoMovement * si);

int dia_to_midinote (int offs);