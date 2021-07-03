/*
 * midi.h
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef MIDI_H
#define MIDI_H

#include <denemo/denemo.h>
#include "smf.h"


#define MIDI_NOTE_OFF         0x80
#define MIDI_NOTE_ON          0x90
#define MIDI_KEY_PRESSURE     0xA0
#define MIDI_CONTROL_CHANGE   0xB0
#define MIDI_PROGRAM_CHANGE   0xC0
#define MIDI_CHANNEL_PRESSURE 0xD0
#define MIDI_PITCH_BEND       0xE0
#define SYS_EXCLUSIVE_MESSAGE1  0xF0


void generate_midi (void);
gdouble get_time (void);
gdouble get_playuntil (void);
void adjust_midi_velocity (gchar * buf, gint percent);
void add_after_touch (gchar * buf);
void change_tuning (gdouble * cents);
gdouble get_midi_on_time (GList * events);
gdouble get_midi_off_time (GList * events);
DenemoObject *get_obj_for_start_time (smf_t * smf, gdouble time);
DenemoObject *get_obj_for_end_time (smf_t * smf, gdouble time);


void update_position (smf_event_t * event);

void start_playing (gchar * callback);
void pause_playing ();

void stop_playing ();
gboolean is_playing ();
gboolean is_paused ();
double get_start_time ();
double get_end_time ();
void update_start_time (double adjust);
void initialize_until_time (void);
smf_event_t *get_smf_event (double until_time);

void play_recorded_midi (void);

void handle_midi_event (gchar * buf);


gboolean intercept_midi_event (gint * midi);

gint get_midi_channel (DenemoStaff * staff);
gint get_midi_prognum (DenemoStaff * staff);
gint get_midi_port (DenemoStaff * staff);

void change_tuning (gdouble * cents);
int noteon_key (smf_event_t * event);
void toggle_paused ();
void play_adjusted_midi_event (gchar * buf);
gboolean set_midi_capture (gboolean set);
void process_midi_event (gchar * buf);

gdouble get_time_at_cursor (void);
#endif // MIDI_H
