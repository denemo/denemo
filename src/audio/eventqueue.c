/*
 * eventqueue.c
 * event queue for audio/MIDI backends.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "audio/eventqueue.h"
#include "audio/midi.h"
#include "ui/moveviewport.h"

#include <glib.h>
#include <string.h>


event_queue_t *
event_queue_new (size_t playback_queue_size, size_t immediate_queue_size, size_t input_queue_size, size_t mixer_queue_size
#ifdef _HAVE_RUBBERBAND_
, size_t rubberband_queue_size
#endif
)
{
  event_queue_t *queue = g_malloc0 (sizeof (event_queue_t));

  if (playback_queue_size)
    {
      queue->playback = jack_ringbuffer_create (playback_queue_size * sizeof (smf_event_t *));
      jack_ringbuffer_reset (queue->playback);
    }

  if (immediate_queue_size)
    {
      queue->immediate = jack_ringbuffer_create (immediate_queue_size * sizeof (midi_event_t));
      jack_ringbuffer_reset (queue->immediate);
    }

  if (input_queue_size)
    {
      queue->input = jack_ringbuffer_create (input_queue_size * sizeof (midi_event_t));
      jack_ringbuffer_reset (queue->input);
    }
  if (mixer_queue_size)
    {
      queue->mixer = jack_ringbuffer_create (mixer_queue_size * sizeof (float));
      jack_ringbuffer_reset (queue->mixer);
    }
#ifdef _HAVE_RUBBERBAND_
  if (rubberband_queue_size)
    {
      queue->rubberband = jack_ringbuffer_create (rubberband_queue_size * sizeof (float));
      jack_ringbuffer_reset (queue->rubberband);
    }
#endif

  return queue;
}


void
event_queue_free (event_queue_t * queue)
{
  if (queue->playback)
    {
      jack_ringbuffer_free (queue->playback);
    }

  if (queue->immediate)
    {
      jack_ringbuffer_free (queue->immediate);
    }

  if (queue->input)
    {
      jack_ringbuffer_free (queue->input);
    }

  g_free (queue);
}


void
event_queue_reset_playback (event_queue_t * queue)
{
  if (queue->playback)
    {
      jack_ringbuffer_reset (queue->playback);
    }
}

void
event_queue_reset_mixer (event_queue_t * queue)
{
  if (queue->mixer)
    {
      jack_ringbuffer_reset (queue->mixer);
    }
}
#ifdef _HAVE_RUBBERBAND_
void
event_queue_reset_rubberband (event_queue_t * queue)
{
  if (queue->rubberband)
    {
      jack_ringbuffer_reset (queue->rubberband);
    }
}
#endif

gboolean
event_queue_write_playback (event_queue_t * queue, smf_event_t * event)
{
  if (!queue->playback || jack_ringbuffer_write_space (queue->playback) < sizeof (smf_event_t *))
    {
      return FALSE;
    }

  size_t n = jack_ringbuffer_write (queue->playback, (char const *) &event, sizeof (smf_event_t *));

  return n == sizeof (smf_event_t *);
}


gboolean
event_queue_write_immediate (event_queue_t * queue, guchar * data, guint length)
{
  if (!queue->immediate || jack_ringbuffer_write_space (queue->immediate) < length)
    {
      return FALSE;
    }
  size_t n = jack_ringbuffer_write (queue->immediate, (char const *) data, length);

  return n == length;

}

gboolean
event_queue_write_mixer (event_queue_t * queue, float *data)
{
  if (!queue->mixer || jack_ringbuffer_write_space (queue->mixer) < sizeof (float))
    {
      return FALSE;
    }
  size_t n = jack_ringbuffer_write (queue->mixer, (char const *) data, sizeof (float));

  return n == sizeof (float);

}

#ifdef _HAVE_RUBBERBAND_
gboolean
event_queue_write_rubberband (event_queue_t * queue, float *data)
{
  if (!queue->rubberband || jack_ringbuffer_write_space (queue->rubberband) < sizeof (float))
    {
      return FALSE;
    }
  size_t n = jack_ringbuffer_write (queue->rubberband, (char const *) data, sizeof (float));

  return n == sizeof (float);

}
#endif

static gboolean do_page_viewport(void)
{
  if (gtk_widget_has_focus (Denemo.scorearea) && gtk_widget_is_focus (Denemo.scorearea))
    page_viewport ();
  return FALSE;
}

static gboolean
page_viewport_callback (gpointer data)
{

  g_main_context_invoke (NULL, (GSourceFunc)do_page_viewport, NULL);

  return FALSE;
}

static void
page_for_time (gdouble time_seconds)
{
  DenemoMovement *si = Denemo.project->movement;
  if ((si->rightmost_time > 0.0) && (time_seconds > si->rightmost_time))
    {
      si->rightmost_time = -1;
      g_idle_add_full (G_PRIORITY_HIGH_IDLE, page_viewport_callback, NULL, NULL);
    }
}


gboolean
mixer_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length)
{
  if (jack_ringbuffer_read_space (queue->mixer) >=  (*event_length) * sizeof (float))
    {
      *event_length = jack_ringbuffer_read (queue->mixer, (char *) event_buffer, (*event_length) * sizeof (float)) / sizeof (float);
      return TRUE;
    }
  *event_length = 0;
  return FALSE;
}
#ifdef _HAVE_RUBBERBAND_
gboolean
rubberband_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length)
{
  if (jack_ringbuffer_read_space (queue->rubberband) >=  (*event_length) * sizeof (float))
    {
      *event_length = jack_ringbuffer_read (queue->rubberband, (char *) event_buffer, (*event_length) * sizeof (float)) / sizeof (float);
      return TRUE;
    }
  *event_length = 0;
  return FALSE;
}
#endif
gboolean
event_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length, double *event_time, double until_time)
{

#if 0
//old fixed length code
  if (jack_ringbuffer_read_space (queue->immediate))
    {
      midi_event_t event;

      jack_ringbuffer_read (queue->immediate, (char *) &event, sizeof (midi_event_t));

      memcpy (event_buffer, &event.data, 3);
      // FIXME
      *event_length = 3;
      *event_time = 0.0;

      return TRUE;
    }
#else
  if (jack_ringbuffer_read_space (queue->immediate))
    {
      jack_ringbuffer_data_t vec[2];
      jack_ringbuffer_get_read_vector (queue->immediate, vec);
      if (vec[0].len)
        {
          guchar length;
          jack_ringbuffer_read (queue->immediate, (char*) &length, 1);
          //g_assert (length < 255);
          jack_ringbuffer_read (queue->immediate, (char*) event_buffer, length);
          *event_length = length;
          *event_time = 0.0;
          return TRUE;
        }
    }
#endif

  if (!queue->playback)
    {
      return FALSE;
    }

  for (;;)
    {
      smf_event_t *event;

//    printf("is_playing=%d, playback_time=%f, end_time=%f\n", is_playing(), get_playback_time(), get_end_time());

      if (!jack_ringbuffer_read_space (queue->playback))
        {
//      printf("no more events in playback queue\n");

          return FALSE;
        }

      jack_ringbuffer_peek (queue->playback, (char *) &event, sizeof (smf_event_t *));

      if (event->time_seconds >= until_time)
        {
//      printf("no event to play right now\n");

          return FALSE;
        }

      if (smf_event_is_metadata (event))
        {
          // consume metadata event and continue with the next one
          jack_ringbuffer_read_advance (queue->playback, sizeof (smf_event_t *));
          continue;
        }

      // consume the event
      jack_ringbuffer_read_advance (queue->playback, sizeof (smf_event_t *));

      //g_assert(event->midi_buffer_length <= 3);

      update_position (event);
      adjust_midi_velocity ((gchar*)event->midi_buffer, 100 - Denemo.prefs.dynamic_compression);
      memcpy (event_buffer, event->midi_buffer, event->midi_buffer_length);
      *event_length = event->midi_buffer_length;
      *event_time = event->time_seconds;
      page_for_time (*event_time);

//    printf("event_time=%f\n", *event_time);

      return TRUE;
    }
}


gboolean
event_queue_write_input (event_queue_t * queue, midi_event_t const *event)
{
  if (!queue->input || jack_ringbuffer_write_space (queue->input) < sizeof (midi_event_t))
    {
      return FALSE;
    }

  size_t n = jack_ringbuffer_write (queue->input, (char *) event, sizeof (midi_event_t));

  return n == sizeof (midi_event_t);
}


midi_event_t *
event_queue_read_input (event_queue_t * queue)
{
  if (!queue->input)
    {
      return NULL;
    }

  if (jack_ringbuffer_read_space (queue->input))
    {
      midi_event_t *ev = g_malloc (sizeof (midi_event_t));
      jack_ringbuffer_read (queue->input, (char *) ev, sizeof (midi_event_t));
      return ev;
    }
  else
    {
      return NULL;
    }
}
