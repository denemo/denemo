/*
 * jackbackend.c
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

#include "jackbackend.h"


static int jack_audio_initialize(DenemoPrefs *config)
{
  return 0;
}

static int jack_audio_destroy()
{
  return 0;
}

static int jack_audio_reconfigure(DenemoPrefs *config)
{
  return 0;
}

static int jack_audio_play_midi_event(int port, unsigned char *buffer)
{
  return 0;
}

static int jack_audio_panic() {
  return 0;
}


static int jack_midi_initialize(DenemoPrefs *config)
{
  return 0;
}

static int jack_midi_destroy()
{
  return 0;
}

static int jack_midi_reconfigure(DenemoPrefs *config)
{
  return 0;
}

static int jack_midi_play_midi_event(int port, unsigned char *buffer)
{
  return 0;
}

static int jack_midi_panic() {
  return 0;
}


backend_t jack_audio_backend = {
  jack_audio_initialize,
  jack_audio_destroy,
  jack_audio_reconfigure,
  jack_audio_play_midi_event,
  jack_audio_panic,
};

backend_t jack_midi_backend = {
  jack_midi_initialize,
  jack_midi_destroy,
  jack_midi_reconfigure,
  jack_midi_play_midi_event,
  jack_midi_panic,
};
