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

#include "eventqueue.h"
#include "midi.h"

#include <glib.h>
#include <string.h>


event_queue_t *event_queue_new(size_t playback_queue_size, size_t immediate_queue_size, size_t input_queue_size) {
  event_queue_t *queue = g_malloc0(sizeof(event_queue_t *));

  if (playback_queue_size) {
    queue->playback = jack_ringbuffer_create(playback_queue_size * sizeof(smf_event_t *));
  }

  if (immediate_queue_size) {
    queue->immediate = jack_ringbuffer_create(immediate_queue_size * sizeof(input_event_t));
  }

  if (input_queue_size) {
    queue->input = jack_ringbuffer_create(input_queue_size * sizeof(input_event_t));
  }

  return queue;
}


void event_queue_free(event_queue_t *queue) {
  if (queue->playback) {
    jack_ringbuffer_free(queue->playback);
  }

  if (queue->immediate) {
    jack_ringbuffer_free(queue->immediate);
  }

  if (queue->input) {
    jack_ringbuffer_free(queue->input);
  }

  g_free(queue);
}


void event_queue_reset_playback(event_queue_t *queue) {
  if (queue->playback) {
    jack_ringbuffer_reset(queue->playback);
  }
}


gboolean event_queue_write_playback_event(event_queue_t *queue, smf_event_t *event) {
  if (!queue->playback || jack_ringbuffer_write_space(queue->playback) < sizeof(smf_event_t*)) {
    return FALSE;
  }

  size_t n = jack_ringbuffer_write(queue->playback, (char const *)&event, sizeof(smf_event_t*));

  return n == sizeof(smf_event_t*);
}


gboolean event_queue_write_immediate_event(event_queue_t *queue, input_event_t *event) {
  if (!queue->immediate || jack_ringbuffer_write_space(queue->immediate) < sizeof(input_event_t)) {
    return FALSE;
  }

  size_t n = jack_ringbuffer_write(queue->immediate, (char const *)event, sizeof(input_event_t));

  return n == sizeof(input_event_t);
}


gboolean event_queue_read_event(event_queue_t *queue, unsigned char *event_buffer, size_t *event_length,
                                double *event_time, double until_time) {
  if (jack_ringbuffer_read_space(queue->immediate)) {
    input_event_t event;
    jack_ringbuffer_read(queue->immediate, (char *)&event, sizeof(input_event_t));

    memcpy(event_buffer, &event.data, 3);
    // FIXME
    *event_length = 3;
    *event_time = 0.0;

    return TRUE;
  }

  if (!queue->playback) {
    return FALSE;
  }

  for (;;) {
    smf_event_t *event;

//    printf("is_playing=%d, playback_time=%f, end_time=%f\n", is_playing(), get_playback_time(), get_end_time());

    double playback_time = get_playback_time();

    // FIXME: do this in audiointerface.c
    if (playback_time > get_end_time()) {
      if (is_playing() && playback_time > 0.0) {
        midi_stop();
      }

//      printf("no more events to play\n");

      return FALSE;
    }
    else if (!jack_ringbuffer_read_space(queue->playback)) {
//      printf("no event in playback queue\n");

      return FALSE;
    }

    jack_ringbuffer_peek(queue->playback, (char *)&event, sizeof(smf_event_t*));

    if (event->time_seconds >= until_time) {
//      printf("no event to play right now\n");

      return FALSE;
    }

    if (smf_event_is_metadata(event)) {
      // consume metadata event and continue with the next one
      jack_ringbuffer_read_advance(queue->playback, sizeof(smf_event_t*));
      continue;
    }

    // consume the event
    jack_ringbuffer_read_advance(queue->playback, sizeof(smf_event_t*));

    g_assert(event->midi_buffer_length <= 3);

    update_position(event);

    memcpy(event_buffer, event->midi_buffer, event->midi_buffer_length);
    *event_length = event->midi_buffer_length;
    *event_time = event->time_seconds;

//    printf("event_time=%f\n", *event_time);

    return TRUE;
  }
}


gboolean event_queue_input_event(event_queue_t *queue, input_event_t const *event) {
  if (!queue->input || jack_ringbuffer_write_space(queue->input) < sizeof(input_event_t)) {
    return FALSE;
  }

  size_t n = jack_ringbuffer_write(queue->input, (char *)event, sizeof(input_event_t));

  return n == sizeof(input_event_t);
}


input_event_t * event_queue_read_input_event(event_queue_t *queue) {
  if (!queue->input) {
    return NULL;
  }

  if (jack_ringbuffer_read_space(queue->input)) {
    input_event_t *ev = g_malloc(sizeof(input_event_t));
    jack_ringbuffer_read(queue->input, (char *)ev, sizeof(input_event_t));
    return ev;
  } else {
    return NULL;
  }
}
