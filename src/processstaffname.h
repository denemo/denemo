/* processstaffname.h
 * for Denemo, the GNU graphical frontend to GNU Lilypond
 *
 * (c) 2000-2005 Matthew Hiller */

#include <glib.h>

void set_lily_name (GString * denemo_name, GString * lily_name);

void set_denemo_name (GString * lily_name, GString * denemo_name);

gint canonicalize_denemo_name (gchar * proposal, GString * denemo_name);
