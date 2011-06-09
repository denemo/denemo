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

#ifdef _HAVE_JACK_
  #include "jackbackend.h"
#endif

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


static backend_t * get_backend(backend_type_t backend)
{
  if (backend == DEFAULT_BACKEND) {
    // FIXME: this should be configurable
    return backends[MIDI_BACKEND];
  }
  return backends[backend];
}


int audiobackend_initialize(DenemoPrefs *config)
{
  char const *driver;

  // FIXME: add new setting to DenemoPrefs
  g_print("audio driver is '%s'\n", config->fluidsynth_audio_driver->str);

  driver = config->fluidsynth_audio_driver->str;

  if (strcmp(driver, "jack") == 0) {
#ifdef _HAVE_JACK_
    backends[AUDIO_BACKEND] = &jack_audio_backend;
#else
    g_warning("JACK backend is not enabled\n");
#endif
  } else if (strcmp(driver, "dummy") == 0) {
    // do nothing
  } else {
    g_warning("unknown audio backend '%s'\n", driver);
  }

  if (backends[AUDIO_BACKEND] == NULL) {
    backends[AUDIO_BACKEND] = &dummy_backend;
  }


  // FIXME: add new setting to DenemoPrefs
  g_print("MIDI driver is '%s'\n", config->fluidsynth_midi_driver->str);

  driver = config->fluidsynth_midi_driver->str;

  if (strcmp(driver, "jack") == 0) {
#ifdef _HAVE_JACK_
    backends[MIDI_BACKEND] = &jack_midi_backend;
#else
    g_warning("JACK backend is not enabled\n");
#endif
  } else if (strcmp(driver, "dummy") == 0) {
    // do nothing
  } else {
    g_warning("unknown MIDI backend '%s'\n", driver);
  }

  if (backends[MIDI_BACKEND] == NULL) {
    backends[MIDI_BACKEND] = &dummy_backend;
  }


  // FIXME: check for errors
  get_backend(AUDIO_BACKEND)->initialize(config);
  get_backend(MIDI_BACKEND)->initialize(config);

  return 0;
}


int audiobackend_destroy()
{
  get_backend(AUDIO_BACKEND)->destroy();
  get_backend(MIDI_BACKEND)->destroy();

  backends[AUDIO_BACKEND] = NULL;
  backends[MIDI_BACKEND] = NULL;

  return 0;
}



void midi_play(gchar *callback)
{
  generate_midi();

  // TODO
}

void midi_stop()
{
  // TODO
}


int play_midi_event(backend_type_t backend, int port, unsigned char *buffer)
{
  return get_backend(backend)->play_midi_event(port, buffer);
}

static gboolean play_note_noteoff_callback(gpointer data)
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

int play_note(backend_type_t backend, int port, int channel, int key, int duration, int volume)
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

int play_notes(backend_type_t backend, int port, int channel, chord *chord_to_play)
{
  // TODO
  return 0;
}

int rhythm_feedback(backend_type_t backend, int duration, gboolean rest, gboolean dot)
{
  // TODO
  return 0;
}


int panic(backend_type_t backend)
{
  return get_backend(backend)->panic();
}

int panic_all()
{
  backend_type_t n;
  for (n = 0; n < NUM_BACKENDS; ++n) {
    panic(n);
  }

  return 0;
}


void input_midi_event(backend_type_t backend, int port, unsigned char *buffer)
{
    // TODO
}

void feed_midi(unsigned char *buffer)
{
    // TODO: add fluidsynth code here
}

void render_audio(unsigned int nframes, float buffer[])
{
    // TODO: add fluidsynth code here
}

void queue_redraw_all()
{
  displayhelper(Denemo.gui);
}

void queue_redraw_playhead()
{
  region_playhead();
}



// FIXME: not quite sure what to do with these yet

// from fluid.c
void advance_time(gdouble seconds) { }


// from audiocapture.c
int pa_main(AubioCallback *fn) { return 0; }
int init_audio_out() { return 0; }

int collect_data_for_tuning(int ok) { return 0; }

double determine_frequency() { return 0.0; }

void set_frequency_smoothing(double fraction) { }

void setTuningTarget(double pitch) { }

