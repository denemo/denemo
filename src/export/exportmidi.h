/* exportmidi.h
 * Header file for Standard MIDI output
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *
 * (c) 2001 Per Andersson
 * 2009- 2013 RTS */
#ifndef EXPORTMIDI_H
#define EXPORTMIDI_H
#include <gtk/gtk.h>
#include "smf.h"
gdouble exportmidi (gchar * filename, DenemoMovement * si);

gdouble load_lilypond_midi (gchar * outfile, gboolean keep);

gchar *substitute_midi_values (gchar * str, gint channel, gint volume);

void free_midi_data (DenemoMovement * si);

int dia_to_midinote (int offs);

#endif
