//      sourceaudio.h
//
//      Copyright 2012 Richard Shann <rshann@debian2>
//
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.





#ifndef SOURCEAUDIO_H
#define SOURCEAUDIO_H

#include <denemo/denemo.h>

//Opens the audio file filename for mixing with the current score. Closes any previously opened audio for this score.
gboolean open_source_audio (gchar * filename);
gboolean close_source_audio (void);

void rewind_audio (void);

gboolean get_audio_sample (float *sample);

gboolean audio_is_playing ();
void start_audio_playing (gboolean annotate);
void stop_audio_playing (void);

//get the timing of the next measure, removing it from the list
gdouble get_audio_timing (void);
gboolean set_lead_in (gdouble secs);
gboolean open_source_audio_file (void);
#endif
