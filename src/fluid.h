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


int fluidsynth_init(DenemoPrefs *config, unsigned int samplerate);
void fluidsynth_shutdown();


/**
 * Feeds a MIDI event to the synth engine.
 */
void feed_fluidsynth_midi(unsigned char *event_data, size_t event_length);

/**
 * Renders the given number of audio frames into a buffer.
 */
void render_fluidsynth_audio(unsigned int nframes, float *left_channel, float *right_channel);





//void fluidsynth_start_restart();
//gchar * fluidsynth_get_default_audio_driver();
//void fluid_playpitch(int key, int duration, int channel, int vol);
//void fluid_output_midi_event(unsigned char *buffer);
//void choose_sound_font (GtkWidget * widget, GtkWidget *fluidsynth_soundfont);
//void fluid_midi_play(gchar *scheme_callback);
//void fluid_midi_stop(void);
//void fluid_midi_panic(void);
//gint fluid_kill_timer(void);
//void fluid_rhythm_feedback(gint duration, gboolean rest, gboolean dot);
//void advance_time(gdouble seconds);


#endif // FLUID_H
