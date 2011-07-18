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



void generate_midi();
gdouble get_time();
gdouble get_midi_on_time(GList *events);
gdouble get_midi_off_time(GList *events);
DenemoObject *get_obj_for_start_time(smf_t *smf, gdouble time);
DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time);


void update_position(smf_event_t *event);

void start_playing();
void stop_playing();
gboolean is_playing();
double get_start_time();
double get_end_time();

smf_event_t *get_smf_event(double until_time);


void handle_midi_event(gchar *buf);


//void midi_cleanup ();
//
//gint midi_init ();
//gint get_midi_channel();
//gint get_midi_prognum();
//gint get_midi_port();
//void playnotes (gboolean doit, chord *chord_to_play,int prognum);

//void play_midikey(gint key, double duration, double volume, gint channel);
//void process_midi_event(gchar *buf);
//void playpitch(double pitch, double duration, double volume, int channel);
//void start_midi_input(void);


#endif // MIDI_H
