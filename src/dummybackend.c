/*
 * dummybackend.c
 * Dummy audio and MIDI backend.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "dummybackend.h"


static int dummy_initialize(DenemoPrefs *config)
{
  return 0;
}

static int dummy_destroy()
{
  return 0;
}

static int dummy_reconfigure(DenemoPrefs *config)
{
  return 0;
}

static int dummy_play_midi_event(int port, unsigned char *buffer)
{
  return 0;
}

static int dummy_panic() {
  return 0;
}


backend_t dummy_backend = {
  dummy_initialize,
  dummy_destroy,
  dummy_reconfigure,
  dummy_play_midi_event,
  dummy_panic,
};
