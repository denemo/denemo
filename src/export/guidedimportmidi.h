/* guidedimportmidi.h

 Richard Shann

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA

*/

#ifndef GUIDEDIMPORTMIDI_H
#define GUIDEDIMPORTMIDI_H

#include "smf.h"
gint
guidedImportMidi (gchar * filename);
smf_tempo_t *get_recorded_midi_tempo (gint index);
gdouble get_recorded_midi_duration (void);
smf_tempo_t *get_recorded_midi_tempo (gint index);
gint get_imported_midi_track (gint track);
gint get_imported_midi_tracks (void);
gint get_current_midi_track (void);
gboolean delete_imported_midi (void);
gboolean compute_midi_note_durations (void);
gboolean midi_is_from_file (void);
#endif
