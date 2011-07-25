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
#ifdef _HAVE_ALSA_
  #include "alsabackend.h"
#endif

#include "midi.h"
#include "audio.h"
#include "commandfuncs.h"
#include "draw.h"

#include "ringbuffer.h"

#include <glib.h>
#include <string.h>


typedef struct capture_event_t {
  backend_type_t backend;
  int port;
  unsigned char data[3];
} capture_event_t;


static backend_t *backends[NUM_BACKENDS] = { NULL };

#define PLAYBACK_QUEUE_SIZE 1024
#define CAPTURE_QUEUE_SIZE 256

// the time in µs after which the queue thread wakes up, whether it has been
// signalled or not
#define QUEUE_TIMEOUT 100000

static jack_ringbuffer_t *playback_queues[NUM_BACKENDS] = { NULL };
static jack_ringbuffer_t *capture_queues[NUM_BACKENDS] = { NULL };


static GThread *queue_thread;
static GCond *queue_cond;
static GMutex *queue_mutex;

static double playback_start_time;
// FIXME: synchronize access from multiple threads
static volatile double playback_time;

static gboolean quit_thread = FALSE;
static gboolean must_redraw_all = FALSE;
static gboolean must_redraw_playhead = FALSE;

static smf_event_t *redraw_event = NULL;

static gpointer queue_thread_func(gpointer data);


static backend_t * get_backend(backend_type_t backend) {
  if (backend == DEFAULT_BACKEND) {
    // FIXME: this should be configurable
    return backends[MIDI_BACKEND];
  } else {
    return backends[backend];
  }
}

static jack_ringbuffer_t * get_playback_queue(backend_type_t backend) {
  if (backend == DEFAULT_BACKEND) {
    // FIXME
    return playback_queues[MIDI_BACKEND];
  } else {
    return playback_queues[backend];
  }
}


static int initialize_audio(DenemoPrefs *config) {
  char const *driver = config->audio_driver->str;

  g_print("audio driver is '%s'\n", driver);

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
    backends[AUDIO_BACKEND] = &dummy_audio_backend;
  }

  playback_queues[AUDIO_BACKEND] = jack_ringbuffer_create(PLAYBACK_QUEUE_SIZE * sizeof(smf_event_t *));

  return get_backend(AUDIO_BACKEND)->initialize(config);
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

  playback_queues[MIDI_BACKEND] = jack_ringbuffer_create(PLAYBACK_QUEUE_SIZE * sizeof(smf_event_t *));
  capture_queues[MIDI_BACKEND] = jack_ringbuffer_create(CAPTURE_QUEUE_SIZE * sizeof(capture_event_t));

  return get_backend(MIDI_BACKEND)->initialize(config);
}


int audiobackend_initialize(DenemoPrefs *config) {
  queue_cond = g_cond_new();
  queue_mutex = g_mutex_new();

  // FIXME: check for errors
  initialize_audio(config);
  initialize_midi(config);

  queue_thread = g_thread_create_full(queue_thread_func, NULL, 262144, TRUE, FALSE, G_THREAD_PRIORITY_NORMAL, NULL);

  return 0;
}


static int destroy(backend_type_t backend) {
  get_backend(backend)->destroy();
  backends[backend] = NULL;

  if (get_playback_queue(backend)) {
    jack_ringbuffer_free(get_playback_queue(backend));
    playback_queues[backend] = NULL;
  }

  if (capture_queues[backend]) {
    jack_ringbuffer_free(capture_queues[backend]);
    capture_queues[backend] = NULL;
  }

  return 0;
}


int audiobackend_destroy() {
  g_atomic_int_set(&quit_thread, TRUE);

  g_mutex_lock(queue_mutex);
  g_cond_signal(queue_cond);
  g_mutex_unlock(queue_mutex);

  g_thread_join(queue_thread);

  destroy(AUDIO_BACKEND);
  destroy(MIDI_BACKEND);

  g_cond_free(queue_cond);
  g_mutex_free(queue_mutex);

  return 0;
}


static gboolean redraw_all_callback(gpointer data) {
  gdk_threads_enter();
  displayhelper(Denemo.gui);
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

  capture_event_t * ev = (capture_event_t *) data;

  // TODO: handle backend type and port
  handle_midi_event((gchar *)ev->data);

  gdk_threads_leave();

  g_free(ev);

  return FALSE;
}


static void reset_playback_queue(backend_type_t backend) {
  if (get_playback_queue(backend)) {
    jack_ringbuffer_reset(get_playback_queue(backend));
  }
}


static gboolean write_event_to_queue(backend_type_t backend, smf_event_t *event) {
  jack_ringbuffer_t *queue = get_playback_queue(backend);

  if (!queue) {
    return FALSE;
  }

  int ret = jack_ringbuffer_write(queue, (char const *)&event, sizeof(smf_event_t*));

  return ret == sizeof(smf_event_t*);
}


gboolean read_event_from_queue(backend_type_t backend, unsigned char *event_buffer, size_t *event_length,
                               double *event_time, double until_time) {
  jack_ringbuffer_t *queue = get_playback_queue(backend);

  if (!queue) {
    return FALSE;
  }

  for (;;) {
    smf_event_t *event;

//    printf("is_playing=%d, playback_time=%f, end_time=%f\n", is_playing(), playback_time, get_end_time());

    if (playback_time > get_end_time()) {
      if (is_playing() && playback_time > 0.0) {
        midi_stop();
      }

//      printf("no more events to play\n");

      return FALSE;
    }
    else if (!jack_ringbuffer_read_space(queue)) {
//      printf("no event to play right now\n");

      return FALSE;
    }

    jack_ringbuffer_peek(queue, (char *)&event, sizeof(smf_event_t*));

    if (event->time_seconds >= until_time) {
//      printf("no event to play right now\n");

      return FALSE;
    }

    if (smf_event_is_metadata(event)) {
      // consume metadata event and continue with the next one
      jack_ringbuffer_read_advance(queue, sizeof(smf_event_t*));
      continue;
    }

    // consume the event
    jack_ringbuffer_read_advance(queue, sizeof(smf_event_t*));

    g_assert(event->midi_buffer_length <= 3);

    update_position(event);

    memcpy(event_buffer, event->midi_buffer, event->midi_buffer_length);
    *event_length = event->midi_buffer_length;
    *event_time = event->time_seconds;

//    printf("event_time=%f\n", *event_time);

    return TRUE;
  }
}



static gpointer queue_thread_func(gpointer data) {
  g_mutex_lock(queue_mutex);

  for (;;) {
    GTimeVal timeval;
    g_get_current_time(&timeval);
    g_time_val_add(&timeval, QUEUE_TIMEOUT);

    g_cond_timed_wait(queue_cond, queue_mutex, &timeval);

    if (g_atomic_int_get(&quit_thread)) {
      g_print("that's it, i quit!\n");
      break;
    }


    // TODO: audio capture
    while (jack_ringbuffer_read_space(capture_queues[MIDI_BACKEND])) {
      capture_event_t * ev = g_malloc(sizeof(capture_event_t));
      jack_ringbuffer_read(capture_queues[MIDI_BACKEND], (char *)ev, sizeof(capture_event_t));

      g_idle_add_full(G_PRIORITY_HIGH_IDLE, handle_midi_event_callback, (gpointer)ev, NULL);
    }


    if (is_playing()) {
      smf_event_t * event;
      double until_time = playback_time + 5.0;

//      printf("playback_time=%f, until_time=%f\n", playback_time, until_time);

      while ((event = get_smf_event(until_time))) {
        write_event_to_queue(AUDIO_BACKEND, event);
        write_event_to_queue(MIDI_BACKEND, event);
      }
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
    if (g_mutex_trylock(queue_mutex)) {
      g_cond_signal(queue_cond);
      g_mutex_unlock(queue_mutex);
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

  start_playing();

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




int play_midi_event(backend_type_t backend, int port, unsigned char *buffer) {
  return get_backend(backend)->play_midi_event(port, buffer);
}


static gboolean play_note_noteoff_callback(gpointer data) {
  backend_type_t backend = (((int)data) >> 24);
  int port = (((int)data) >> 16) & 0xff;
  int channel = (((int)data) >> 8) & 0xff;
  int key = ((int)data) & 0xff;

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

  // FIXME: this limits the number of ports to 256...
  gpointer data = (gpointer) (backend << 24 | port << 16 | channel << 8 | key);
  g_timeout_add(duration, play_note_noteoff_callback, data);

  return r;
}

int play_notes(backend_type_t backend, int port, int channel, chord *chord_to_play) {
  // TODO
  return 0;
}

int rhythm_feedback(backend_type_t backend, int duration, gboolean rest, gboolean dot) {
  // TODO
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
  capture_event_t ev;
  ev.backend = backend;
  ev.port = port;
  // FIXME: size might be less than 3
  memcpy(&ev.data, buffer, 3);

  jack_ringbuffer_write(capture_queues[backend], (char *)&ev, sizeof(capture_event_t));

  // if the lock fails, processing of the event will be delayed until the
  // queue thread wakes up on its own
  if (g_mutex_trylock(queue_mutex)) {
    g_cond_signal(queue_cond);
    g_mutex_unlock(queue_mutex);
  }
}


void queue_redraw_all() {
  g_atomic_int_set(&must_redraw_all, TRUE);

  if (g_mutex_trylock(queue_mutex)) {
    g_cond_signal(queue_cond);
    g_mutex_unlock(queue_mutex);
  }
}

void queue_redraw_playhead(smf_event_t *event) {
  g_atomic_int_set(&must_redraw_playhead, TRUE);
  redraw_event = event;

  if (g_mutex_trylock(queue_mutex)) {
    g_cond_signal(queue_cond);
    g_mutex_unlock(queue_mutex);
  }
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

