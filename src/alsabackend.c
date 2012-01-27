/*
 * alsabackend.c
 * ALSA sequencer MIDI backend.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "alsabackend.h"
#include "midi.h"

#include <alsa/asoundlib.h>
#include <glib.h>


static char const *ALSA_SEQ_CLIENT_NAME = "denemo";

static int const PLAYBACK_INTERVAL = 5000;


static snd_seq_t *seq;

static int in_port_id;
static int out_port_id;

static snd_midi_event_t *parser;

static GThread *process_thread;
static GCond *process_cond;
static gboolean quit_thread = FALSE;

static gboolean reset = FALSE;

static double playback_start_time;


static gpointer process_thread_func(gpointer data) {
  GMutex *mutex = g_mutex_new();

  for (;;) {
    // FIXME: GTimeVals are not monotonic
    GTimeVal timeval;
    g_get_current_time(&timeval);
    g_time_val_add(&timeval, PLAYBACK_INTERVAL);

    g_cond_timed_wait(process_cond, mutex, &timeval);

    if (g_atomic_int_get(&quit_thread)) {
      break;
    }

    snd_seq_event_t *pev;
    while (snd_seq_event_input(seq, &pev) >= 0) {
      unsigned char buffer[3];

      snd_midi_event_reset_decode(parser);
      if (snd_midi_event_decode(parser, buffer, sizeof(buffer), pev) > 0) {
        input_midi_event(MIDI_BACKEND, 0, buffer);
      }
    }


    GTimeVal tv;
    g_get_current_time(&tv);
    double now = (double)tv.tv_sec + tv.tv_usec / 1000000.0;
    double playback_time = now - playback_start_time;

    unsigned char event_data[3];
    size_t event_length;
    double event_time;

    double until_time = playback_time + PLAYBACK_INTERVAL / 1000000.0;

    if (reset) {
      int n;
      for (n = 0; n < 16; ++n) {
        snd_seq_event_t ev;
        snd_seq_ev_set_controller(&ev, n, 123, 0);
        snd_seq_ev_set_subs(&ev);
        snd_seq_ev_set_direct(&ev);
        snd_seq_ev_set_source(&ev, out_port_id);
        snd_seq_event_output_direct(seq, &ev);
      }
      reset = FALSE;
    }

    while (read_event_from_queue(MIDI_BACKEND, event_data, &event_length, &event_time, until_time)) {
      snd_seq_event_t ev;

      snd_midi_event_reset_encode(parser);
      snd_midi_event_encode(parser, event_data, event_length, &ev);

      snd_seq_ev_set_subs(&ev);
      snd_seq_ev_set_direct(&ev);
      snd_seq_ev_set_source(&ev, out_port_id);

      snd_seq_event_output_direct(seq, &ev);
    }

    if (is_playing()) {
      update_playback_time(TIMEBASE_PRIO_MIDI, playback_time);
    }
  }

  g_mutex_free(mutex);
  return NULL;
}



static int alsa_seq_initialize(DenemoPrefs *config) {
  g_print("initializing ALSA sequencer MIDI backend\n");

  // create sequencer client
  if (snd_seq_open(&seq, "hw", SND_SEQ_OPEN_DUPLEX, SND_SEQ_NONBLOCK) < 0) {
      g_warning("error opening alsa sequencer");
      return -1;
  }

  snd_seq_set_client_name(seq, ALSA_SEQ_CLIENT_NAME);

  // create input port
  in_port_id = snd_seq_create_simple_port(seq, "midi_in",
                                          SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE,
                                          SND_SEQ_PORT_TYPE_APPLICATION);
  if (in_port_id < 0) {
    g_warning("error creating sequencer output port");
    return -1;
  }

  // create output port
  out_port_id = snd_seq_create_simple_port(seq, "midi_out",
                                          SND_SEQ_PORT_CAP_READ | SND_SEQ_PORT_CAP_SUBS_READ,
                                          SND_SEQ_PORT_TYPE_APPLICATION);

  if (in_port_id < 0) {
    g_warning("error creating sequencer output port");
    return -1;
  }

  // initialize MIDI event parser
  if (snd_midi_event_new(12, &parser)) {
      g_warning("error initializing MIDI event parser");
      return -1;
  }
  snd_midi_event_init(parser);
  snd_midi_event_no_status(parser, 1);


  process_cond = g_cond_new();

  process_thread = g_thread_create_full(process_thread_func, NULL, 262144, TRUE, FALSE, G_THREAD_PRIORITY_NORMAL, NULL);


  return 0;
}


static int alsa_seq_destroy() {
  g_print("destroying ALSA sequencer MIDI backend\n");

  g_atomic_int_set(&quit_thread, TRUE);
  g_cond_signal(process_cond);
  g_thread_join(process_thread);

  g_cond_free(process_cond);


  snd_midi_event_free(parser);

  snd_seq_delete_port(seq, in_port_id);
  snd_seq_delete_port(seq, out_port_id);

  snd_seq_close(seq);

  return 0;
}


static int alsa_seq_reconfigure(DenemoPrefs *config) {
  alsa_seq_destroy();
  return alsa_seq_initialize(config);
}


static int alsa_seq_start_playing() {
  GTimeVal tv;
  g_get_current_time(&tv);
  playback_start_time = (double)tv.tv_sec + tv.tv_usec / 1000000.0;
  playback_start_time -= get_playback_time();
  return 0;
}


static int alsa_seq_stop_playing() {
  reset = TRUE;
  return 0;
}


static int alsa_seq_panic() {
  reset = TRUE;
  return 0;
}


backend_t alsa_seq_midi_backend = {
  alsa_seq_initialize,
  alsa_seq_destroy,
  alsa_seq_reconfigure,
  alsa_seq_start_playing,
  alsa_seq_stop_playing,
  alsa_seq_panic,
};
