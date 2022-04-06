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

#include "audio/audiointerface.h"
#include "audio/eventqueue.h"
#include "audio/dummybackend.h"
#include "source/sourceaudio.h"
#ifdef _HAVE_JACK_
#include "audio/jackbackend.h"
#endif
#ifdef _HAVE_PORTAUDIO_
#include "audio/portaudiobackend.h"
#endif
#ifdef _HAVE_PORTMIDI_
#include "audio/portmidibackend.h"
#endif
#ifdef _HAVE_ALSA_
#include "audio/alsabackend.h"
#endif

#include "audio/midi.h"
#include "audio/audio.h"
#include "command/commandfuncs.h"
#include "display/draw.h"

#include <glib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

static backend_t *backends[NUM_BACKENDS] = { NULL };

#define PLAYBACK_QUEUE_SIZE 1024
#define IMMEDIATE_QUEUE_SIZE 32
#define INPUT_QUEUE_SIZE 256
#define MIXER_QUEUE_SIZE 50000
#define RUBBERBAND_QUEUE_SIZE 50000


// the time in µs after which the queue thread wakes up, whether it has been
// signalled or not
#define QUEUE_TIMEOUT 100000

static event_queue_t *event_queues[NUM_BACKENDS] = { NULL };


static GThread *queue_thread;
static GCond queue_cond;
static GMutex queue_mutex;

static double playback_start_time;
// FIXME: synchronize access from multiple threads
static volatile double playback_time;

static gboolean quit_thread;
static gboolean signalled = FALSE;
static gboolean must_redraw_all = FALSE;
static gboolean must_redraw_playhead = FALSE;

static smf_event_t *redraw_event;

#ifndef  _HAVE_PORTAUDIO_
gdouble get_playback_speed (void)
{
    return 1.0; //Rubberband can do slowdown, backend should define its own version of this
}
void set_playback_speed (double speed) {}
#endif

static gpointer queue_thread_func (gpointer data);
static void signal_queue ();



static backend_t *
get_backend (backend_type_t backend)
{
  if (backend == DEFAULT_BACKEND)
    {
      // FIXME: this should be configurable
      //return backends[MIDI_BACKEND];
      return backends[AUDIO_BACKEND];
    }
  else
    {
      return backends[backend];
    }
}

static event_queue_t *
get_event_queue (backend_type_t backend)
{
  if (backend == DEFAULT_BACKEND)
    {
      // FIXME
      // return event_queues[MIDI_BACKEND];
      return event_queues[AUDIO_BACKEND];
    }
  else
    {
      return event_queues[backend];
    }
}

static int
initialize_audio (DenemoPrefs * config)
{
  char const *driver = config->audio_driver->str;

  g_message ("Audio driver is '%s'", driver);

  if (strcmp (driver, "jack") == 0)
    {
#ifdef _HAVE_JACK_
      backends[AUDIO_BACKEND] = &jack_audio_backend;
#else
      g_warning ("JACK backend is not enabled");
#endif
    }
  else if (strcmp (driver, "portaudio") == 0)
    {
#ifdef _HAVE_PORTAUDIO_
      backends[AUDIO_BACKEND] = &portaudio_backend;
#else
      g_warning ("PortAudio backend is not enabled");
#endif
    }
  else if (strcmp (driver, "dummy") == 0)
    {
      // do nothing
    }
  else
    {
      g_warning ("Unknown audio backend '%s'", driver);
    }

  if (backends[AUDIO_BACKEND] == NULL)
    {
      backends[AUDIO_BACKEND] = &dummy_audio_backend;
    }

  //event_queues[AUDIO_BACKEND] = event_queue_new(PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, 0);

  int ret = get_backend (AUDIO_BACKEND)->initialize (config);

  if (ret)
    {
      g_warning ("Initializing audio backend '%s' failed, falling back to dummy", driver);
      backends[AUDIO_BACKEND] = &dummy_audio_backend;
      ret = get_backend (AUDIO_BACKEND)->initialize (config);
    }

  return ret;
}


static int
initialize_midi (DenemoPrefs * config)
{
  char const *driver = config->midi_driver->str;

  g_message ("MIDI driver is '%s'", driver);

  if (strcmp (driver, "jack") == 0)
    {
#ifdef _HAVE_JACK_
      backends[MIDI_BACKEND] = &jack_midi_backend;
#else
      g_warning ("JACK backend is not enabled");
#endif
    }
  else if (strcmp (driver, "portmidi") == 0)
    {
#ifdef _HAVE_PORTMIDI_
      backends[MIDI_BACKEND] = &portmidi_backend;
#else
      g_warning ("PortMidi backend is not enabled");
#endif
    }
  else if (strcmp (driver, "alsa") == 0)
    {
#ifdef _HAVE_ALSA_
      backends[MIDI_BACKEND] = &alsa_seq_midi_backend;
#else
      g_warning ("ALSA backend is not enabled");
#endif
    }
  else if (strcmp (driver, "dummy") == 0)
    {
      // do nothing
    }
  else
    {
      g_warning ("Unknown MIDI backend '%s'", driver);
    }

  if (backends[MIDI_BACKEND] == NULL)
    {
      backends[MIDI_BACKEND] = &dummy_midi_backend;
    }

  //event_queues[MIDI_BACKEND] = event_queue_new(PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, INPUT_QUEUE_SIZE);

  int ret = get_backend (MIDI_BACKEND)->initialize (config);

  if (ret)
    {
      g_warning ("Initializing MIDI backend '%s' failed, falling back to dummy", driver);
      backends[MIDI_BACKEND] = &dummy_midi_backend;
      ret = get_backend (MIDI_BACKEND)->initialize (config);
    }

  return ret;
}

gboolean
have_midi (void)
{
  return strcmp (Denemo.prefs.portmidi_input_device->str, "default") && (backends[MIDI_BACKEND] != &dummy_midi_backend);
}

int
audio_initialize (DenemoPrefs * config)
{
  queue_thread = NULL;
  quit_thread = FALSE;
  redraw_event = NULL;

  //&queue_cond = g_cond_new (); since GLib 2.32 no longer needed, static declaration is enough
  //&queue_mutex = g_mutex_new (); since GLib 2.32 no longer needed, static declaration is enough
  event_queues[AUDIO_BACKEND] = event_queue_new (PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, 0, MIXER_QUEUE_SIZE
#ifdef _HAVE_RUBBERBAND_
, RUBBERBAND_QUEUE_SIZE
#endif
);
  event_queues[MIDI_BACKEND] = event_queue_new (PLAYBACK_QUEUE_SIZE, IMMEDIATE_QUEUE_SIZE, INPUT_QUEUE_SIZE, 0
#ifdef _HAVE_RUBBERBAND_
, 0
#endif
);
  if (initialize_audio (config) || initialize_midi(config))
    {
      audio_shutdown ();
      return -1;
    }

  queue_thread = g_thread_try_new ("Queue Thread", queue_thread_func, NULL, NULL);

  if (queue_thread == NULL)
    {
      audio_shutdown();
      return -1;
    }

  return 0;
}

static int
destroy (backend_type_t backend)
{
  get_backend (backend)->destroy ();
  backends[backend] = NULL;

  event_queue_free (event_queues[backend]);

  return 0;
}


int
audio_shutdown ()
{
  g_atomic_int_set (&quit_thread, TRUE);

  if (queue_thread)
    {
      signal_queue ();

      g_thread_join (queue_thread);
    }

  if (get_backend (AUDIO_BACKEND))
    {
      destroy (AUDIO_BACKEND);
    }

  if (get_backend (MIDI_BACKEND))
    {
      destroy (MIDI_BACKEND);
    }

 //g_cond_free (&queue_cond); since GLib 2.32 no longer needed, static declaration is enough
 //g_mutex_free (&queue_mutex); since GLib 2.32 no longer needed, static declaration is enough

  return 0;
}

static gboolean do_queue_draw (void) {
    draw_score_area();
    return FALSE;
}
static gboolean
redraw_all_callback (gpointer data)
{

  g_main_context_invoke (NULL, (GSourceFunc)do_queue_draw, NULL);

  return FALSE;
}

static gboolean
redraw_playhead_callback (gpointer data)
{
  DenemoMovement *si = Denemo.project->movement;

  smf_event_t *event = (smf_event_t *) data;

  if (gtk_widget_has_focus (Denemo.scorearea) && gtk_widget_is_focus (Denemo.scorearea))
	si->playingnow = event->user_pointer;
  else
	si->playingnow = NULL;
  si->playhead = event->time_seconds;

  g_main_context_invoke (NULL, (GSourceFunc)do_queue_draw, NULL);

  return FALSE;
}

static gboolean do_handle_midi_event (gchar *data) {
  handle_midi_event (data);
  g_free(data);
  return FALSE;
}
static gboolean
handle_midi_event_callback (gpointer data)
{

  midi_event_t *ev = (midi_event_t *) data;

  // TODO: handle backend type and port
  gchar *evdata = g_malloc (sizeof (ev->data));
  memcpy (evdata, ev->data, sizeof (ev->data));
  g_main_context_invoke (NULL, (GSourceFunc)do_handle_midi_event, evdata);

  g_free (ev);

  return FALSE;
}


static void
reset_playback_queue (backend_type_t backend)
{
  if (get_event_queue (backend))
    {
      event_queue_reset_playback (get_event_queue (backend));
    }
}

static void
reset_mixer_queue (backend_type_t backend)
{
  if (get_event_queue (backend))
    {
      event_queue_reset_mixer (get_event_queue (backend));
    }
}
#ifdef _HAVE_RUBBERBAND_
static void
reset_rubberband_queue (backend_type_t backend)
{
  if (get_event_queue (backend))
    {
      event_queue_reset_rubberband (get_event_queue (backend));
    }
}
#endif

static gboolean
write_event_to_queue (backend_type_t backend, smf_event_t * event)
{
  return event_queue_write_playback (get_event_queue (backend), event);
}

static gboolean
write_sample_to_mixer_queue (backend_type_t backend, float *sample)
{
  return event_queue_write_mixer (get_event_queue (backend), sample);
}
#ifdef _HAVE_RUBBERBAND_
gboolean
write_samples_to_rubberband_queue (backend_type_t backend, float *sample, gint num)
{gint i;
  for(i=0;i<num;i++) {
    if(!event_queue_write_rubberband (get_event_queue (backend), sample+i))
        return FALSE;
    }
    return TRUE;
}
#endif
gboolean
read_event_from_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length, double *event_time, double until_time)
{
  double thetime = get_playback_time (); // this is the value of the global playback_time which is volatile

  if (thetime > get_end_time ())
    {
      if (is_playing () && thetime > 0.0) // this is the same value rather than the global, but what the check is for I do not know
        {
          midi_stop ();
        }
    }

  return event_queue_read_output (get_event_queue (backend), event_buffer, event_length, event_time, until_time);
}

gboolean
read_event_from_mixer_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length)
{
  return mixer_queue_read_output (get_event_queue (backend), event_buffer, event_length);
}
#ifdef _HAVE_RUBBERBAND_

gboolean
read_event_from_rubberband_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length)
{
  return rubberband_queue_read_output (get_event_queue (backend), event_buffer, event_length);
}
#endif
GMutex smfmutex;// = G_STATIC_MUTEX_INIT;
static gpointer
queue_thread_func (gpointer data)
{
  g_mutex_lock (&queue_mutex);

  for (;;)
    {
      if (!g_atomic_int_get (&signalled))
        {
          gint64 end_time = g_get_monotonic_time () +  (QUEUE_TIMEOUT * G_TIME_SPAN_SECOND)/1000000;
          g_cond_wait_until (&queue_cond, &queue_mutex, end_time);
          signalled = FALSE;
        }

      if (g_atomic_int_get (&quit_thread))
        {
          g_message ("That's it, I quit!");
          break;
        }


      // TODO: audio capture

      midi_event_t *ev;

      while ((ev = event_queue_read_input (get_event_queue (MIDI_BACKEND))) != NULL)
        {
          g_idle_add_full (G_PRIORITY_HIGH_IDLE, handle_midi_event_callback, (gpointer) ev, NULL);
        }


      if (is_playing ())
        {
          smf_event_t *event;
          double until_time = playback_time + 5.0;


      //printf("playback_time=%f, until_time=%f\n", playback_time, until_time);
          g_mutex_lock (&smfmutex);
          while ((event = get_smf_event (until_time)))
            {
              write_event_to_queue (AUDIO_BACKEND, event);//g_print ("queue gets 0x%hhX 0x%hhX 0x%hhX\n", *(event->midi_buffer+0), *(event->midi_buffer+1), *(event->midi_buffer+2));

              write_event_to_queue (MIDI_BACKEND, event);
            }
          g_mutex_unlock (&smfmutex);
        }

      if (audio_is_playing ())
        {
          float sample[2];      //two channels assumed FIXME
          //FIXME I think this will drop samples if they can't be put in the queue, should find if there is space for a sample before getting it.
#ifdef DISABLE_AUBIO
#else
          while (get_audio_sample (sample) && write_sample_to_mixer_queue (AUDIO_BACKEND, sample))
            ;
#endif
        }


      if (g_atomic_int_get (&must_redraw_all))
        {
          g_atomic_int_set (&must_redraw_all, FALSE);
          g_atomic_int_set (&must_redraw_playhead, FALSE);

          g_idle_add_full (G_PRIORITY_HIGH_IDLE, redraw_all_callback, NULL, NULL);
        }

      if (g_atomic_int_get (&must_redraw_playhead))
        {
          g_atomic_int_set (&must_redraw_playhead, FALSE);

          g_idle_add_full (G_PRIORITY_HIGH_IDLE, redraw_playhead_callback, (gpointer) redraw_event, NULL);
        }
    }

  g_mutex_unlock (&queue_mutex);

  return NULL;
}


static void
signal_queue ()
{
  g_mutex_lock (&queue_mutex);
  g_atomic_int_set (&signalled, TRUE);
  g_cond_signal (&queue_cond);
  g_mutex_unlock (&queue_mutex);
}


static gboolean
try_signal_queue ()
{
  if (g_mutex_trylock (&queue_mutex))
    {
      g_atomic_int_set (&signalled, TRUE);
      g_cond_signal (&queue_cond);
      g_mutex_unlock (&queue_mutex);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}
static gboolean time_reset = FALSE;

void
update_playback_time (backend_timebase_prio_t prio, double new_time)
{
  if (!((prio == TIMEBASE_PRIO_AUDIO) || (prio == TIMEBASE_PRIO_MIDI && get_backend (AUDIO_BACKEND) == &dummy_audio_backend) || (get_backend (AUDIO_BACKEND) == &dummy_audio_backend && get_backend (MIDI_BACKEND) == &dummy_midi_backend)))
    {
      // ignore new playback time if another backend has higher priority
      return;
    }
  if(time_reset)
    {
        time_reset = FALSE;
        return;
    }
  if (new_time != playback_time)
    {
      playback_time = new_time;
      // midi_play tries to set playback_time, which then gets overriden by the call in the portaudio callback.
      // if the lock fails, the playback time update will be delayed until the
      // queue thread wakes up on its own
      if (!try_signal_queue ())
        {
          ;//this is continuously emitted by windows which has debug on. g_debug ("Couldn't signal playback time update to queue");
        }
    }
}

double
get_playback_time (void)
{
  return playback_time;
}

#ifdef _HAVE_JACK_
void midi_play(gchar *callback) {
  generate_midi();

  reset_playback_queue(AUDIO_BACKEND);
  reset_playback_queue(MIDI_BACKEND);

  g_print("JACK starting playback\n");

  playback_start_time = get_start_time();
  playback_time = playback_start_time;

  start_playing(callback);

  get_backend(AUDIO_BACKEND)->start_playing();
  get_backend(MIDI_BACKEND)->start_playing();
}

#else
void
midi_play (gchar * callback)
{
  generate_midi ();

  reset_playback_queue (AUDIO_BACKEND);
  reset_playback_queue (MIDI_BACKEND);

  g_message ("Starting playback");
  start_playing (callback);
  do {//FIXME, this is a crude attempt to get the playback_time set without the callback from portaudio re-writing it.
    playback_time = playback_start_time;
    time_reset = TRUE;
    playback_time = playback_start_time;
    get_backend (AUDIO_BACKEND)->start_playing ();// this must pick up the playback_start_time, which won't happen if an interrrupt has occurred meanwhile.
    } while(fabs(playback_time - playback_start_time) > 0.0001);
  g_message ("Starting playback at %f - should be %f", playback_start_time, playback_time);
  get_backend (MIDI_BACKEND)->start_playing ();
}
#endif


void
audio_play (void)
{
  reset_mixer_queue (AUDIO_BACKEND);
#ifdef _HAVE_RUBBERBAND_
  reset_rubberband_queue (AUDIO_BACKEND);
#endif
  playback_start_time = get_start_time ();
  g_print ("starting audio playback at %f\n", playback_start_time);
  playback_time = playback_start_time;

}

void
midi_stop (void)
{
  //g_message ("Stopping playback");

  get_backend (AUDIO_BACKEND)->stop_playing ();
  get_backend (MIDI_BACKEND)->stop_playing ();

  stop_playing ();
#ifdef DISABLE_AUBIO
#else
  stop_audio_playing ();
#endif
  reset_playback_queue (AUDIO_BACKEND);
  reset_playback_queue (MIDI_BACKEND);
  reset_mixer_queue (AUDIO_BACKEND);
#ifdef _HAVE_RUBBERBAND_
  reset_rubberband_queue (AUDIO_BACKEND);
#endif
   gtk_widget_queue_draw (Denemo.playbackview);
}


#define MIDI_EOX (0xF7)

int
play_midi_event (backend_type_t backend, int port, unsigned char *buffer)
{
  if(Denemo.non_interactive)
    return TRUE;
  guchar ev[1 + 255];           /* 1 length byte plus up to 255 data bytes */
  gint i = 3;
#ifndef _HAVE_JACK_
  if (buffer[0] == SYS_EXCLUSIVE_MESSAGE1)
    {
      for (i = 0; i < 255; i++)
        if (buffer[i] == MIDI_EOX)
          break;
      if (i == 255)
        return FALSE;
    }
#endif
  *ev = i;
  memcpy (ev + 1, buffer, i);//g_print (" midibytes 0x%hhX 0x%hhX 0x%hhX\n", *(buffer+0), *(buffer+1), *(buffer+2));

  return event_queue_write_immediate (get_event_queue (backend), ev, i + 1);
}


static gboolean
play_note_noteoff_callback (gpointer data)
{
  backend_type_t backend = (((intptr_t) data) >> 24);
  int port = (((intptr_t) data) >> 16) & 0xff;
  int channel = (((intptr_t) data) >> 8) & 0xff;
  int key = ((intptr_t) data) & 0xff;

  unsigned char buffer[] = {
    MIDI_NOTE_OFF | channel,
    key,
    0
  };

  play_midi_event (backend, port, buffer);

  return FALSE;
}

int
play_note (backend_type_t backend, int port, int channel, int key, int duration, int volume)
{
  if(Denemo.non_interactive)
    return -1;

  unsigned char buffer[] = {
    MIDI_NOTE_ON | channel,
    key,
    (volume ? volume : 127) * Denemo.project->movement->master_volume
  };

  int r = play_midi_event (backend, port, buffer);

  // XXX this limits the number of ports to 256...
  gpointer data = (gpointer) (intptr_t) (backend << 24 | port << 16 | channel << 8 | key);
  g_timeout_add (duration, play_note_noteoff_callback, data);

  return r;
}

int
play_notes (backend_type_t backend, int port, int channel, chord * chord_to_play)
{
  if (chord_to_play->notes)
    {
	  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
      GList *g;
      for (g = chord_to_play->notes; g; g = g->next)
        {
          note *n = g->data;

          /* Because mid_c_offset is a measure of notes and we need a measure of
           * half-steps, this array will help */
          const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

          gint offset = n->mid_c_offset;

          /* 60 is middle-C in MIDI keys */
          gchar key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
          key += n->enshift;
		  key += curstaff->transposition;
          // FIXME
          play_note (backend, port, channel, key, 200, 127);
        }
    }

  return 0;
}

/* give audible feedback for entering a rhythmic element */
static gint rhythm_sounds[] = { 41, 48, 64, 62, 60, 70, 81, 69, 79 };

int
rhythm_feedback (backend_type_t backend, gint duration, gboolean rest, gboolean dot)
{
  int key;
  if (dot)
    play_note (backend, 0, 9, 67, 100, 60);
  else
    play_note (backend, 0, 9, rhythm_sounds[duration], rest ? 100 : 200, 127);
  //add extra sound effect for rests
  if (rest)
    play_note (backend, 0, 9, 46, 300, 127 * Denemo.project->movement->master_volume);
  while ((key = GPOINTER_TO_INT( g_queue_pop_head (Denemo.project->pending_midi)) ))
    play_note (backend, 0, 9, key, 300, 127);
  //g_debug("playing %d %d\n", rhythm_sounds[duration], (60/(4*Denemo.project->movement->tempo*(1<<duration)))*1000);

  return 0;
}


int
panic (backend_type_t backend)
{
 // g_critical ("Panicking");
  return get_backend (backend)->panic ();
}

int
panic_all (void)
{
  backend_type_t n;
  for (n = 0; n < NUM_BACKENDS; ++n)
    {
      panic (n);
    }

  return 0;
}


void
input_midi_event (backend_type_t backend, int port, unsigned char *buffer)
{
  midi_event_t ev;
  ev.backend = backend;
  ev.port = port;
  // FIXME: size might be less than 3
  memcpy (&ev.data, buffer, 3);

  // normalize events: replace note-on with zero velocity by note-off
  if ((ev.data[0] & 0xf0) == MIDI_NOTE_ON && ev.data[2] == 0)
    {
      ev.data[0] = (ev.data[0] & 0x0f) | MIDI_NOTE_OFF;
    }

  event_queue_write_input (get_event_queue (backend), &ev);

  // if the lock fails, processing of the event will be delayed until the
  // queue thread wakes up on its own
  if (!try_signal_queue ())
    {
      g_debug ("Couldn't signal MIDI event input to queue");
    }
}


void
queue_redraw_all ()
{
  g_atomic_int_set (&must_redraw_all, TRUE);

  if (!try_signal_queue ())
    {
      g_debug ("Couldn't signal redraw request to queue");
    }
}

void
queue_redraw_playhead (smf_event_t * event)
{
  g_atomic_int_set (&must_redraw_playhead, TRUE);
  redraw_event = event;

  if (!try_signal_queue ())
    {
      g_debug ("Couldn't signal redraw request to queue");
    }
}



// FIXME: not quite sure what to do with these yet

// from fluid.c
void
advance_time (gdouble seconds)
{
}

#if 0
// from audiocapture.c
int
pa_main (AubioCallback * fn)
{
  return 0;
}

int
init_audio_out ()
{
  return 0;
}

int
collect_data_for_tuning (int ok)
{
  return 0;
}

double
determine_frequency ()
{
  return 0.0;
}

void
set_frequency_smoothing (double fraction)
{
}

void
setTuningTarget (double pitch)
{
}
#endif

