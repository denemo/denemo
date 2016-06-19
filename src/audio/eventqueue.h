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

#include "audio/audiointerface.h"
#ifdef _HAVE_JACK_
#include <jack/ringbuffer.h>
#else
#include "audio/ringbuffer.h"
#endif

#include "smf.h"


/**
 * Event queue structure for input/output of MIDI events to/from backends.
 */
typedef struct event_queue_t
{
  /**
   * The playback queue. Events from the SMF structure are written to this
   * queue a few seconds in advance to ensure precise timing with no dropouts.
   */
  jack_ringbuffer_t *playback;
  /**
   * The queue for immediate event output. Events written to this queue will
   * be played back as soon as possible.
   */
  jack_ringbuffer_t *immediate;
  /**
   * The input queue.
   */
  jack_ringbuffer_t *input;

  /* mixer queue - audio for mixing with playback output */
  jack_ringbuffer_t *mixer;

 #ifdef _HAVE_RUBBERBAND_
 /* rubberband queue - for audio stretching */
 jack_ringbuffer_t *rubberband;
 #endif

} event_queue_t;


/**
 * Creates a new event queue.
 *
 * @param playback_queue_size   the maximum number of events in the playback
 *                              queue
 * @param immediate_queue_size  the maximum number of events in the immediate
 *                              playback queue
 * @param input_queue_size      the maximum number of events in the input
 *                              queue
 *
 * @return                      the new event queue
 */
event_queue_t *event_queue_new (size_t playback_queue_size, size_t immediate_queue_size, size_t input_queue_size, size_t mixer_queue_size
#ifdef _HAVE_RUBBERBAND_
, size_t rubberband_queue_size
#endif
);

/**
 * Frees the given queue.
 */
void event_queue_free (event_queue_t * queue);

/**
 * Clears the playback queue.
 */
void event_queue_reset_playback (event_queue_t * queue);
/**
 * Clears the mixer queue.
 */
void event_queue_reset_mixer (event_queue_t * queue);

#ifdef _HAVE_RUBBERBAND_
/**
 * Clears the rubberband queue.
 */
void event_queue_reset_rubberband (event_queue_t * queue);
#endif

/**
 * Writes an SMF event to the playback queue.
 *
 * @param event   the event to be written to the queue. Only a pointer to the
 *                event will be stored in the queue, not the event data itself.
 *
 * @return        TRUE if the event was successfully written to the queue
 */
gboolean event_queue_write_playback (event_queue_t * queue, smf_event_t * event);

/**
 * Writes an event to the immmediate playback queue.
 *
 * @param data   the event or extended_event to be written to the queue. The event data will be
 *                copied.
 * @param length  length of the event or extended_event to be written to the queue.
 *
 * @return        TRUE if the event was successfully written to the queue
 */
gboolean event_queue_write_immediate (event_queue_t * queue, guchar * data, guint length);


/**
 * Writes an audio sample to the mixer queue.
 *
 * @param event   the sample to be written to the queue.
 *
 * @return        TRUE if the sample was successfully written to the queue
 */
gboolean event_queue_write_mixer (event_queue_t * queue, float *sample);

#ifdef _HAVE_RUBBERBAND_
/**
 * Writes single sample to the rubberband queue.
 *
 * @param event   the sample to be written to the queue.
 *
 * @return        TRUE if the sample was successfully written to the queue
 */
gboolean event_queue_write_rubberband (event_queue_t * queue, float *sample);

#endif

/**
 * Reads an event from one of the output queues.
 *
 * @param[out] event_buffer   the event data
 * @param[out] event_length   the length of the event in bytes
 * @param[out] event_time     the time of the event (in seconds from the start
 *                            of the score)
 * @param until_time          the playback time up to which events should be
 *                            returned
 *
 * @return                    TRUE if an event was written to the output
 *                            parameters
 */
gboolean event_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length, double *event_time, double until_time);


gboolean mixer_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length);
#ifdef _HAVE_RUBBERBAND_
gboolean rubberband_queue_read_output (event_queue_t * queue, unsigned char *event_buffer, size_t * event_length);
#endif
/**
 * Writes an event to the input queue.
 *
 * @param event   the event to be written to the queue. The event data will be
 *                copied.
 *
 * @return        TRUE if the event was successfully written to the queue
 */
gboolean event_queue_write_input (event_queue_t * queue, midi_event_t const *event);

/**
 * Reads an event from the input queue.
 *
 * @return  a pointer to a newly allocated structure containing the event data.
 *          The caller is responsible for calling g_free() on this pointer.
 */
midi_event_t *event_queue_read_input (event_queue_t * queue);


#endif // EVENTQUEUE_H
