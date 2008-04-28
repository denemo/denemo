/* exportmidi.h
 * Header file for Standard MIDI output
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *
 * (c) 2001 Per Andersson */

#include <gtk/gtk.h>

gdouble exportmidi( gchar * filename, DenemoScore *si, gint start, gint end);
