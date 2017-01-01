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

#include "audio/dummybackend.h"
#include "audio/midi.h"

#include <glib.h>


// only affects GUI update, 10 Hz should be enough
static int const PLAYBACK_INTERVAL = 100000;


static GThread *process_thread = NULL;
static GCond process_cond;
static gboolean quit_thread = FALSE;

static gboolean dummy_audio = FALSE;
static gboolean dummy_midi = FALSE;

static double playback_start_time;


static gpointer
process_thread_func (gpointer data)
{
  static GMutex mutex;
  gint64 end_time;
  g_mutex_lock (&mutex);
  for (;;)
    {
      end_time = g_get_monotonic_time () +  (PLAYBACK_INTERVAL * G_TIME_SPAN_SECOND)/1000000;
      g_cond_wait_until (&process_cond, &mutex, end_time);

      if (g_atomic_int_get (&quit_thread))
        {
          break;
        }

      GTimeVal tv;
      g_get_current_time (&tv);
      double now = (double) tv.tv_sec + tv.tv_usec / 1000000.0;
      double playback_time = now - playback_start_time;

      unsigned char event_data[3];
      size_t event_length;
      double event_time;

      double until_time = playback_time + PLAYBACK_INTERVAL / 1000000.0;

      if (g_atomic_int_get (&dummy_audio))
        {
          // clear the audio event queue
          while (read_event_from_queue (AUDIO_BACKEND, event_data, &event_length, &event_time, until_time))
            {
              // do nothing. this is the dummy backend after all
            }
          if (is_playing ())
            {
                update_playback_time (TIMEBASE_PRIO_DUMMY, playback_time);
            }
        }

      if (g_atomic_int_get (&dummy_midi))
        {
          // clear the MIDI event queue
          while (read_event_from_queue (MIDI_BACKEND, event_data, &event_length, &event_time, until_time))
            {
              // do nothing. this is the dummy backend after all
            }
        }
    }
  g_mutex_unlock (&mutex);
  return NULL;
}


static void
start_process_thread ()
{
  if (!process_thread)
    {
      process_thread = g_thread_try_new ("Dummy process", process_thread_func, NULL, NULL);
    }
}


static void
stop_process_thread ()
{
  if (dummy_audio || dummy_midi)
    {
      return;
    }

  if (process_thread)
    {
      g_atomic_int_set (&quit_thread, TRUE);
      g_cond_signal (&process_cond);
      g_thread_join (process_thread);

      process_thread = NULL;
    }
}


static int
dummy_audio_initialize (DenemoPrefs * config)
{
  g_message ("Initializing dummy audio backend");

  start_process_thread ();

  g_atomic_int_set (&dummy_audio, TRUE);

  return 0;
}

static int
dummy_midi_initialize (DenemoPrefs * config)
{
  g_message ("Initializing dummy MIDI backend");

  start_process_thread ();

  g_atomic_int_set (&dummy_midi, TRUE);

  return 0;
}


static int
dummy_audio_destroy ()
{
  g_message ("Destroying dummy audio backend");

  g_atomic_int_set (&dummy_audio, FALSE);

  stop_process_thread ();

  return 0;
}

static int
dummy_midi_destroy ()
{
  g_message ("Destroying dummy MIDI backend");

  g_atomic_int_set (&dummy_midi, FALSE);

  stop_process_thread ();

  return 0;
}


static int
dummy_audio_reconfigure (DenemoPrefs * config)
{
  dummy_audio_destroy ();
  return dummy_audio_initialize (config);
}

static int
dummy_midi_reconfigure (DenemoPrefs * config)
{
  dummy_midi_destroy ();
  return dummy_midi_initialize (config);
}


static int
dummy_start_playing ()
{
  GTimeVal tv;
  g_get_current_time (&tv);
  playback_start_time = (double) tv.tv_sec + tv.tv_usec / 1000000.0;
  playback_start_time -= get_playback_time ();
  return 0;
}


static int
dummy_stop_playing ()
{
  return 0;
}


static int
dummy_panic ()
{
  return 0;
}


backend_t dummy_audio_backend = {
  dummy_audio_initialize,
  dummy_audio_destroy,
  dummy_audio_reconfigure,
  dummy_start_playing,
  dummy_stop_playing,
  dummy_panic,
};

backend_t dummy_midi_backend = {
  dummy_midi_initialize,
  dummy_midi_destroy,
  dummy_midi_reconfigure,
  dummy_start_playing,
  dummy_stop_playing,
  dummy_panic,
};
