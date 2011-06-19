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

#ifndef AUDIOBACKEND_H
#define AUDIOBACKEND_H

#include <denemo/denemo_types.h>


/**
 * The common interface for all audio and MIDI backends.
 */
typedef struct backend_t {
  /**
   * Initializes the backend with the given configuration.
   *
   * FIXME: the DenemoPrefs argument is both too general (backend-specific
   * settings would be enough) and redundant (there's a global instance
   * anyway).
   */
  int (*initialize)(DenemoPrefs *config);

  /**
   * Destroys the backend and cleans up all its resources.
   */
  int (*destroy)();

  /**
   * Changes the backend's configuration, without destroying and recreating
   * it.
   */
  int (*reconfigure)(DenemoPrefs *config);

  /**
   * Called when playback is started.
   */
  int (*start_playing)();

  /**
   * Called when playback is stopped.
   */
  int (*stop_playing)();

  /**
   * Plays a single MIDI event.
   *
   * FIXME: this may not be feasible for all backends (at least JACK would
   * require some kind of buffering between this function and the process
   * callback)
   */
  int (*play_midi_event)(int port, unsigned char *buffer);

  /**
   * Sends a MIDI panic CC and/or resets the synth engine.
   */
  int (*panic)();

} backend_t;


typedef enum backend_type_t {
  DEFAULT_BACKEND = -1,
  AUDIO_BACKEND = 0,
  MIDI_BACKEND = 1
} backend_type_t;

// we only have two backends at the same time: audio and MIDI
#define NUM_BACKENDS 2


/**
 * Initializes the audio/MIDI subsystem.
 */
int audiobackend_initialize(DenemoPrefs *config);

/**
 * Destroys and cleans up the audio/MIDI subsystem.
 */
int audiobackend_destroy();



/**
 * Used to be:
 * void fluid_midi_play(gchar *scheme_callback)
 * void jack_midi_play(gchar *scheme_callback)
 */
void midi_play(gchar *callback);

/**
 * Used to be:
 * void fluid_midi_stop()
 * void jack_midi_playback_stop()
 * int jack_kill_timer()
 */
void midi_stop();

/**
 * Plays a single MIDI event.
 *
 * Used to be:
 * void fluid_output_midi_event(unsigned char *buffer)
 * void jack_output_midi_event(unsigned char *buffer, gint client_number, gint port_number)
 */
int play_midi_event(backend_type_t backend, int port, unsigned char *buffer);

/**
 * Plays a single note.
 *
 * Used to be:
 * void fluid_playpitch(int key, int duration, int channel, int vol)
 * void jack_playpitch(gint key, gint duration)
 */
int play_note(backend_type_t backend, int port, int channel, int key, int duration, int volume);


/**
 * FIXME: the seems to be something missing here...
 *
 * Used to be:
 * void playnotes(gboolean doit, chord *chord_to_play, int channel)
 */
int play_notes(backend_type_t backend, int port, int channel, chord *chord_to_play);

/**
 * FIXME: what exactly does this do?
 *
 * Used to be:
 * void fluid_rhythm_feedback(gint duration, gboolean rest, gboolean dot)
 */
int rhythm_feedback(backend_type_t backend, int duration, gboolean rest, gboolean dot);


/**
 * Sends a MIDI panic CC and/or resets the synth engine.
 *
 * Used to be:
 * void fluid_midi_panic()
 * void jack_midi_panic()
 */
int panic(backend_type_t backend);

int panic_all();


gboolean read_event_from_queue(backend_type_t backend, unsigned char *event_buffer, size_t *event_length,
                               double *event_time, double until_time);


void update_playback_time(backend_type_t backend, double new_time);

/**
 * Called when a MIDI event was received.
 */
void input_midi_event(backend_type_t backend, int port, unsigned char *buffer);


/**
 * Feeds a MIDI event to the synth engine.
 */
void feed_midi(unsigned char *buffer);

/**
 * Renders the given number of audio frames into a buffer.
 */
void render_audio(unsigned int nframes, float buffer[]);






/**
 * Queues a full redraw of the GUI.
 */
void queue_redraw_all();

/**
 * Queues a redraw of the playhead.
 */
void queue_redraw_playhead();


#endif // AUDIOBACKEND_H
