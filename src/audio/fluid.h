/*
 * fluid.h
 * JACK audio and MIDI backends.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef FLUID_H
#define FLUID_H

#include <denemo/denemo_types.h>


int fluidsynth_init (DenemoPrefs * config, unsigned int samplerate);
void fluidsynth_shutdown ();


/**
 * Feeds a MIDI event to the synth engine.
 */
void fluidsynth_feed_midi (unsigned char *event_data, size_t event_length);

/**
 * Sends an all-notes-off event to the synth engine.
 */
void fluidsynth_all_notes_off ();

/**
 * Renders the given number of audio frames into a buffer.
 */
void fluidsynth_render_audio (unsigned int nframes, float *left_channel, float *right_channel);

/**
 * Select the soundfont to use for playback
 */
void choose_sound_font (GtkWidget * widget, GtkWidget * fluidsynth_soundfont);
void reset_synth_channels (void);
void fluid_set_gain (gdouble gain);
#endif // FLUID_H
