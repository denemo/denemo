/*
 * audiointerface.h
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

#include "audiointerface.h"
#include "eventqueue.h"
#include "dummybackend.h"

#ifdef _HAVE_JACK_
  #include "jackbackend.h"
#endif
#ifdef _HAVE_PORTAUDIO_
  #include "portaudiobackend.h"
#endif
#ifdef _HAVE_PORTMIDI_
  #include "portmidibackend.h"
#endif
#ifdef _HAVE_ALSA_
  #include "alsabackend.h"
#endif

#include "midi.h"
#include "audio.h"
#include "commandfuncs.h"
#include "draw.h"

#include <glib.h>
#include <string.h>
#include <stdint.h>


static backend_t *backends[NUM_BACKENDS] = { NULL };

#define PLAYBACK_QUEUE_SIZE 1024
#define IMMEDIATE_QUEUE_SIZE 32
#define INPUT_QUEUE_SIZE 256

// the time in µs after which the queue thread wakes up, whether it has been
// signalled or not
#define QUEUE_TIMEOUT 100000

static event_queue_t *event_queues[NUM_BACKENDS] = { NULL };


static GThread *queue_thread;
static GCond *queue_cond;
static GMutex *queue_mutex;

static double playback_start_time;
// FIXME: synchronize access from multiple threads
static volatile double playback_time;

static gboolean quit_thread;
static gboolean signalled = FALSE;
static gboolean must_redraw_all = FALSE;
static gboolean must_redraw_playhead = FALSE;

static smf_event_t *redraw_event;

static gpointer queue_thread_func(gpointer data);
static void signal_queue();


static backend_t * get_backend(backend_type_t backend) {
  if (backend == DEFAULT_BACKEND) {
    // FIXME: this should be configurable
    //return backends[MIDI_BACKEND];
    return backends[AUDIO_BACKEND];
  } else {
    return backends[backend];
  }
}

static event_queue_t *get_event_queue(backend_type_t backend) {
  if (backend == DEFAULT_BACKEND) {
    // FIXME
   // return event_queues[MIDI_BACKEND];
    return event_queues[AUDIO_BACKEND];
  } else {
    return event_queues[backend];
  }
}


static int initialize_audio(DenemoPrefs *config) {
  char const *driver = config->audio_driver->str;

  g_print("audio driver is '%s' %d\n", driver, strcmp(driver, "portaudio"));

  if (strcmp(driver, "jack") == 0) {
#ifdef _HAVE_JACK_
    backends[AUDIO_BACKEND] = &jack_audio_backend;
#else
    g_warning("JACK backend is not enabled\n");
#endif
  } else if (strcmp(driver, "portaudio") == 0) {
#ifdef _HAVE_PORTAUDIO_
    backends[AUDIO_BACKEND] = &portaudio_backend;
#else
    g_warning("PortAudio backend is not enabled\n");
#endif
  } else if (strcmp(driver, "dummy") == 0) {
    // do nothing
  } else {
    g_warning("unknown audio backend '%s'\n", driver);
  }

  if (backends[AUDIO_BACKEND] == NULL) {
    backends[AUDIO_BACKEND] = &dummy_audio_backend;
  }

  event_queues[AUDIO_BACKEND] = event_queue_new(PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, 0);

  int ret = get_backend(AUDIO_BACKEND)->initialize(config);

  if (ret) {
    g_warning("initializing audio backend '%s' failed, falling back to dummy", driver);
    backends[AUDIO_BACKEND] = &dummy_audio_backend;
    ret = get_backend(AUDIO_BACKEND)->initialize(config);
  }

  return ret;
}


static int initialize_midi(DenemoPrefs *config) {
  char const *driver = config->midi_driver->str;

  g_print("MIDI driver is '%s'\n", driver);

  if (strcmp(driver, "jack") == 0) {
#ifdef _HAVE_JACK_
    backends[MIDI_BACKEND] = &jack_midi_backend;
#else
    g_warning("JACK backend is not enabled\n");
#endif
  } else if (strcmp(driver, "portmidi") == 0) {
#ifdef _HAVE_PORTMIDI_
    backends[MIDI_BACKEND] = &portmidi_backend;
#else
    g_warning("PortMidi backend is not enabled\n");
#endif
  } else if (strcmp(driver, "alsa") == 0) {
#ifdef _HAVE_ALSA_
    backends[MIDI_BACKEND] = &alsa_seq_midi_backend;
#else
    g_warning("ALSA backend is not enabled\n");
#endif
  } else if (strcmp(driver, "dummy") == 0) {
    // do nothing
  } else {
    g_warning("unknown MIDI backend '%s'\n", driver);
  }

  if (backends[MIDI_BACKEND] == NULL) {
    backends[MIDI_BACKEND] = &dummy_midi_backend;
  }

  event_queues[MIDI_BACKEND] = event_queue_new(PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, INPUT_QUEUE_SIZE);

  int ret = get_backend(MIDI_BACKEND)->initialize(config);

  if (ret) {
    g_warning("initializing MIDI backend '%s' failed, falling back to dummy", driver);
    backends[MIDI_BACKEND] = &dummy_midi_backend;
    ret = get_backend(MIDI_BACKEND)->initialize(config);
  }

  return ret;
}


int audio_initialize(DenemoPrefs *config) {
  queue_thread = NULL;
  quit_thread = FALSE;
  redraw_event = NULL;

  queue_cond = g_cond_new();
  queue_mutex = g_mutex_new();

  if (initialize_audio(config)) {
    goto err;
  }
  if (initialize_midi(config)) {
    goto err;
  }

  queue_thread = g_thread_create_full(queue_thread_func, NULL, 262144, TRUE, FALSE, G_THREAD_PRIORITY_NORMAL, NULL);

  if (queue_thread == NULL) {
    goto err;
  }

  return 0;

err:
  audio_shutdown();
  return -1;
}


static int destroy(backend_type_t backend) {
  get_backend(backend)->destroy();
  backends[backend] = NULL;

  event_queue_free(event_queues[backend]);

  return 0;
}


int audio_shutdown() {
  g_atomic_int_set(&quit_thread, TRUE);

  if (queue_thread) {
    signal_queue();

    g_thread_join(queue_thread);
  }

  if (get_backend(AUDIO_BACKEND)) {
    destroy(AUDIO_BACKEND);
  }

  if (get_backend(MIDI_BACKEND)) {
    destroy(MIDI_BACKEND);
  }

  g_cond_free(queue_cond);
  g_mutex_free(queue_mutex);

  return 0;
}


static gboolean redraw_all_callback(gpointer data) {
  gdk_threads_enter();
  //displayhelper(Denemo.gui);
  gtk_widget_queue_draw(Denemo.scorearea);
  gdk_threads_leave();
  return FALSE;
}

static gboolean redraw_playhead_callback(gpointer data) {
  gdk_threads_enter();

  DenemoScore *si = Denemo.gui->si;

  smf_event_t *event = (smf_event_t*) data;

  si->playingnow = event->user_pointer;
  si->playhead = event->time_seconds;

  region_playhead();

  gdk_threads_leave();
  return FALSE;
}


static gboolean handle_midi_event_callback(gpointer data) {
  gdk_threads_enter();

  midi_event_t * ev = (midi_event_t *) data;

  // TODO: handle backend type and port
  handle_midi_event((gchar *)ev->data);

  gdk_threads_leave();

  g_free(ev);

  return FALSE;
}


static void reset_playback_queue(backend_type_t backend) {
  if (get_event_queue(backend)) {
    event_queue_reset_playback(get_event_queue(backend));
  }
}


static gboolean write_event_to_queue(backend_type_t backend, smf_event_t *event) {
  return event_queue_write_playback(get_event_queue(backend), event);
}


gboolean read_event_from_queue(backend_type_t backend, unsigned char *event_buffer, size_t *event_length,
                               double *event_time, double until_time) {
  double playback_time = get_playback_time();

  if (playback_time > get_end_time()) {
    if (is_playing() && playback_time > 0.0) {
      midi_stop();
    }
  }

  return event_queue_read_output(get_event_queue(backend), event_buffer, event_length, event_time, until_time);
}


GStaticMutex smfmutex = G_STATIC_MUTEX_INIT;
static gpointer queue_thread_func(gpointer data) {
  g_mutex_lock(queue_mutex);

  for (;;) {
    if (!g_atomic_int_get(&signalled)) {
      GTimeVal timeval;
      g_get_current_time(&timeval);
      g_time_val_add(&timeval, QUEUE_TIMEOUT);

      g_cond_timed_wait(queue_cond, queue_mutex, &timeval);
      signalled = FALSE;
    }

    if (g_atomic_int_get(&quit_thread)) {
      g_print("that's it, i quit!\n");
      break;
    }


    // TODO: audio capture

    midi_event_t *ev;

    while ((ev = event_queue_read_input(get_event_queue(MIDI_BACKEND))) != NULL) {
      g_idle_add_full(G_PRIORITY_HIGH_IDLE, handle_midi_event_callback, (gpointer)ev, NULL);
    }


    if (is_playing()) {
      smf_event_t * event;
      double until_time = playback_time + 5.0;

//      printf("playback_time=%f, until_time=%f\n", playback_time, until_time);
    g_static_mutex_lock (&smfmutex);
      while ((event = get_smf_event(until_time))) {
        write_event_to_queue(AUDIO_BACKEND, event);
        write_event_to_queue(MIDI_BACKEND, event);
      }
    g_static_mutex_unlock (&smfmutex);
    }


    if (g_atomic_int_get(&must_redraw_all)) {
      g_atomic_int_set(&must_redraw_all, FALSE);
      g_atomic_int_set(&must_redraw_playhead, FALSE);

      g_idle_add_full(G_PRIORITY_HIGH_IDLE, redraw_all_callback, NULL, NULL);
    }

    if (g_atomic_int_get(&must_redraw_playhead)) {
      g_atomic_int_set(&must_redraw_playhead, FALSE);

      g_idle_add_full(G_PRIORITY_HIGH_IDLE, redraw_playhead_callback, (gpointer)redraw_event, NULL);
    }
  }

  g_mutex_unlock(queue_mutex);

  return NULL;
}


static void signal_queue() {
  g_mutex_lock(queue_mutex);
  g_atomic_int_set(&signalled, TRUE);
  g_cond_signal(queue_cond);
  g_mutex_unlock(queue_mutex);
}


static gboolean try_signal_queue() {
  if (g_mutex_trylock(queue_mutex)) {
    g_atomic_int_set(&signalled, TRUE);
    g_cond_signal(queue_cond);
    g_mutex_unlock(queue_mutex);
    return TRUE;
  } else {
    return FALSE;
  }
}


void update_playback_time(backend_timebase_prio_t prio, double new_time) {
  if (!((prio == TIMEBASE_PRIO_AUDIO) ||
        (prio == TIMEBASE_PRIO_MIDI && get_backend(AUDIO_BACKEND) == &dummy_audio_backend) ||
        (get_backend(AUDIO_BACKEND) == &dummy_audio_backend && get_backend(MIDI_BACKEND) == &dummy_midi_backend))) {
    // ignore new playback time if another backend has higher priority
    return;
  }

  if (new_time != playback_time) {
    playback_time = new_time;

    // if the lock fails, the playback time update will be delayed until the
    // queue thread wakes up on its own
    if (!try_signal_queue()) {
      g_debug("couldn't signal playback time update to queue");
    }
  }
}

double get_playback_time() {
  return playback_time;
}


void midi_play(gchar *callback) {
  generate_midi();

  reset_playback_queue(AUDIO_BACKEND);
  reset_playback_queue(MIDI_BACKEND);

  g_print("starting playback\n");

  playback_start_time = get_start_time();
  playback_time = playback_start_time;

  start_playing(callback);

  get_backend(AUDIO_BACKEND)->start_playing();
  get_backend(MIDI_BACKEND)->start_playing();
}


void midi_stop() {
  g_print("stopping playback\n");

  get_backend(AUDIO_BACKEND)->stop_playing();
  get_backend(MIDI_BACKEND)->stop_playing();

  stop_playing();

  reset_playback_queue(AUDIO_BACKEND);
  reset_playback_queue(MIDI_BACKEND);
}


#define MIDI_EOX (0xF7)

int play_midi_event(backend_type_t backend, int port, unsigned char *buffer) {
 guchar ev[1+255];/* 1 length byte plus up to 255 data bytes */
 gint i = 3;
 if(buffer[0] == SYS_EXCLUSIVE_MESSAGE1) {
    for(i=0;i<255;i++)
      if(buffer[i]==MIDI_EOX)
        break;
    if(i==255) return FALSE;
 }
 *ev = i;
 memcpy(ev+1, buffer, i);
 return event_queue_write_immediate(get_event_queue(backend), ev, i+1); 
}


static gboolean play_note_noteoff_callback(gpointer data) {
  backend_type_t backend = (((intptr_t)data) >> 24);
  int port = (((intptr_t)data) >> 16) & 0xff;
  int channel = (((intptr_t)data) >> 8) & 0xff;
  int key = ((intptr_t)data) & 0xff;

  unsigned char buffer[] = {
    MIDI_NOTE_OFF | channel,
    key,
    0
  };

  play_midi_event(backend, port, buffer);

  return FALSE;
}

int play_note(backend_type_t backend, int port, int channel, int key, int duration, int volume) {
  unsigned char buffer[] = {
    MIDI_NOTE_ON | channel,
    key,
    (volume ? volume : 127) * Denemo.gui->si->master_volume
  };

  int r = play_midi_event(backend, port, buffer);

  // XXX this limits the number of ports to 256...
  gpointer data = (gpointer)(intptr_t) (backend << 24 | port << 16 | channel << 8 | key);
  g_timeout_add(duration, play_note_noteoff_callback, data);

  return r;
}

int play_notes(backend_type_t backend, int port, int channel, chord *chord_to_play) {
  if (chord_to_play->notes) {
    GList *g;
    for (g = chord_to_play->notes; g; g = g->next) {
      note * n = g->data;

      /* Because mid_c_offset is a measure of notes and we need a measure of
       * half-steps, this array will help */
      const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

      gint offset = n->mid_c_offset;

      /* 60 is middle-C in MIDI keys */
      gchar key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
      key += n->enshift;

      // FIXME
      play_note(backend, port, channel, key, 200, 127);
    }
  }

  return 0;
}

/* give audible feedback for entering a rhythmic element */
static gint rhythm_sounds[] = {41,48,64,62,60,70, 81, 69, 79};
int rhythm_feedback(backend_type_t backend, gint duration, gboolean rest, gboolean dot) {
  if(dot)
    play_note(backend, 0, 9, 67, 100, 60*Denemo.gui->si->master_volume);
  else
    play_note(backend, 0, 9, rhythm_sounds[duration], rest?100:200, 127*Denemo.gui->si->master_volume);
  //add extra sound effect for rests
  if(rest)
    play_note(backend, 0, 9, 46, 300, 127*Denemo.gui->si->master_volume);
 
  //g_print("playing %d %d\n", rhythm_sounds[duration], (60/(4*Denemo.gui->si->tempo*(1<<duration)))*1000);

  return 0;
}


int panic(backend_type_t backend) {
  g_print("panicking\n");
  return get_backend(backend)->panic();
}

int panic_all() {
  backend_type_t n;
  for (n = 0; n < NUM_BACKENDS; ++n) {
    panic(n);
  }

  return 0;
}


void input_midi_event(backend_type_t backend, int port, unsigned char *buffer) {
  midi_event_t ev;
  ev.backend = backend;
  ev.port = port;
  // FIXME: size might be less than 3
  memcpy(&ev.data, buffer, 3);

  // normalize events: replace note-on with zero velocity by note-off
  if ((ev.data[0] & 0xf0) == MIDI_NOTE_ON && ev.data[2] == 0) {
    ev.data[0] = (ev.data[0] & 0x0f) | MIDI_NOTE_OFF;
  }

  event_queue_write_input(get_event_queue(backend), &ev);

  // if the lock fails, processing of the event will be delayed until the
  // queue thread wakes up on its own
  if (!try_signal_queue()) {
    g_debug("couldn't signal MIDI event input to queue");
  }
}


void queue_redraw_all() {
  g_atomic_int_set(&must_redraw_all, TRUE);

  if (!try_signal_queue()) {
    g_debug("couldn't signal redraw request to queue");
  }
}

void queue_redraw_playhead(smf_event_t *event) {
  g_atomic_int_set(&must_redraw_playhead, TRUE);
  redraw_event = event;

  if (!try_signal_queue()) {
    g_debug("couldn't signal redraw request to queue");
  }
}



// FIXME: not quite sure what to do with these yet

// from fluid.c
void advance_time(gdouble seconds) { }

#if 0
// from audiocapture.c
int pa_main(AubioCallback *fn) { return 0; }
int init_audio_out() { return 0; }

int collect_data_for_tuning(int ok) { return 0; }

double determine_frequency() { return 0.0; }

void set_frequency_smoothing(double fraction) { }

void setTuningTarget(double pitch) { }
#endif
