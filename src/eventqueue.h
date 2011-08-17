/*
 * eventqueue.h
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

#ifndef EVENTQUEUE_H
#define EVENTQUEUE_H

#include "audiointerface.h"
#include "ringbuffer.h"

#include "smf.h"


typedef struct event_queue_t {
  jack_ringbuffer_t *playback;
  jack_ringbuffer_t *immediate;
  jack_ringbuffer_t *capture;
} event_queue_t;


event_queue_t *event_queue_new(size_t playback_queue_size, size_t immediate_queue_size, size_t capture_queue_size);

void event_queue_free(event_queue_t *queue);

void event_queue_reset_playback(event_queue_t *queue);

gboolean event_queue_write_playback_event(event_queue_t *queue, smf_event_t *event);

gboolean event_queue_read_event(event_queue_t *queue, unsigned char *event_buffer, size_t *event_length,
                                double *event_time, double until_time);

gboolean event_queue_input_capture_event(event_queue_t *queue, capture_event_t const *ev);

capture_event_t * event_queue_read_capture_event(event_queue_t *queue);


#endif // EVENTQUEUE_H
