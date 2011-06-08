/*
 * audiobackend.h
 * Interface definition for audio and MIDI backends.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "audiobackend.h"
#include "dummybackend.h"

#include "midi.h"
#include "audio.h"


#define NOTE_OFF            0x80
#define NOTE_ON             0x90
#define KEY_PRESSURE        0xA0
#define CONTROL_CHANGE      0xB0
#define PROGRAM_CHANGE      0xC0
#define CHANNEL_PRESSURE    0xD0
#define PITCH_BEND          0xE0
#define MIDI_SYSTEM_RESET   0xFF


static backend_t *backends[NUM_BACKENDS] = { NULL };


backend_t *
get_backend(backend_type_t backend)
{
  if (backend == DEFAULT_BACKEND || backends[backend] == NULL) {
    return &dummy_backend;
  }
  return backends[backend];
}


void
midi_play(gchar *callback)
{
  // TODO
}

void
midi_stop()
{
  // TODO
}


int
play_midi_event(backend_type_t backend, int port, unsigned char *buffer)
{
  return get_backend(backend)->play_midi_event(port, buffer);
}

static gboolean
play_note_noteoff_callback(gpointer data)
{
  backend_type_t backend = (((int)data) >> 24);
  int port = (((int)data) >> 16) & 0xff;
  int channel = (((int)data) >> 8) & 0xff;
  int key = ((int)data) & 0xff;

  unsigned char buffer[] = {
    NOTE_OFF | channel,
    key,
    0
  };

  play_midi_event(backend, port, buffer);

  return FALSE;
}

int
play_note(backend_type_t backend, int port, int channel, int key, int duration, int volume)
{
  unsigned char buffer[] = {
    NOTE_ON | channel,
    key,
    (volume ? volume : 127) * Denemo.gui->si->master_volume
  };

  int r = play_midi_event(backend, port, buffer);

  // FIXME: this limits the number of ports to 256...
  gpointer data = (gpointer) (backend << 24 | port << 16 | channel << 8 | key);
  g_timeout_add(duration, play_note_noteoff_callback, data);

  return r;
}

int
play_notes(backend_type_t backend, int port, int channel, chord *chord_to_play)
{
  // TODO
  return 0;
}

int
rhythm_feedback(backend_type_t backend, int duration, gboolean rest, gboolean dot)
{
  // TODO
  return 0;
}


int
panic(backend_type_t backend)
{
  return get_backend(backend)->panic();
}

int
panic_all()
{
  backend_type_t n;
  for (n = 0; n < NUM_BACKENDS; ++n) {
    panic(n);
  }

  return 0;
}


void
queue_redraw_all()
{
  displayhelper(Denemo.gui);
}

void
queue_redraw_cursor()
{
  region_playhead();
}



// FIXME: not quite sure what to do with these yet

// from fluid.c
void advance_time(gdouble seconds) { }


// from midi.c
gint midi_init () { return 0; }

gint init_midi_input() { return 0; }
void start_midi_input() { }
gint stop_midi_input() { return 0; }

gint get_midi_channel() { return 0; }
gint get_midi_prognum() { return 0; }

gdouble get_midi_on_time(GList *events) { return 0.0; }
gdouble get_midi_off_time(GList *events) { return 0.0; }
gdouble get_time() { return 0.0; }
gboolean set_midi_capture(gboolean set) { return FALSE; }

void process_midi_event(gchar *buf) { }

gboolean intercept_midi_event(gint *midi) { return FALSE; }

DenemoObject *get_obj_for_start_time(smf_t *smf, gdouble time) { return NULL; }
DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time) { return NULL; }

void change_tuning(gdouble *cents) { }


// from audiocapture.c
int pa_main(AubioCallback *fn) { return 0; }
int init_audio_out() { return 0; }

int collect_data_for_tuning(int ok) { return 0; }

double determine_frequency() { return 0.0; }

void set_frequency_smoothing(double fraction) { }

void setTuningTarget(double pitch) { }

