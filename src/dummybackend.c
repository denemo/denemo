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
  g_print("initializing dummy backend\n");
  return 0;
}

static int dummy_destroy()
{
  g_print("destroying dummy backend\n");
  return 0;
}

static int dummy_reconfigure(DenemoPrefs *config)
{
  g_print("reconfiguring dummy backend\n");
  return 0;
}

static int dummy_play_midi_event(int port, unsigned char *buffer)
{
  int channel = buffer[0] & 0x0f;
  int type = (buffer[0] & 0xf0) >> 4;
  g_print("playing midi event: port=%n, channel=%n, type=%x\n");
  return 0;
}

static int dummy_panic() {
  g_print("panicking\n");
  return 0;
}


backend_t dummy_backend = {
  dummy_initialize,
  dummy_destroy,
  dummy_reconfigure,
  dummy_play_midi_event,
  dummy_panic,
};
