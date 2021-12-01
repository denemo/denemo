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

#ifndef AUDIOINTERFACE_H
#define AUDIOINTERFACE_H

#include <denemo/denemo_types.h>


/**
 * The common interface for all audio and MIDI backends.
 */
typedef struct backend_t
{
  /**
   * Initializes the backend with the given configuration.
   *
   * FIXME: the DenemoPrefs argument is both too general (backend-specific
   * settings would be enough) and redundant (there's a global instance
   * anyway).
   *
   * @param config  the configuration object containing the backend's settings
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*initialize) (DenemoPrefs * config);

  /**
   * Destroys the backend and cleans up all its resources.
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*destroy) ();

  /**
   * Changes the backend's configuration, if possible without destroying and
   * recreating the backend.
   *
   * This is not used by any backends at the moment, but will be needed, for
   * example, to configure the audio input settings.
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*reconfigure) (DenemoPrefs * config);

  /**
   * Called when playback is started.
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*start_playing) ();

  /**
   * Called when playback is stopped.
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*stop_playing) ();

  /**
   * Sends a MIDI panic CC and/or resets the synth engine.
   *
   * @return        zero on success, a negative error code on failure
   */
  int (*panic) ();

} backend_t;


/**
 * An enum that identifies a backend's type.
 */
typedef enum backend_type_t
{
  /**
   * The default backend (either audio or MIDI)
   */
  DEFAULT_BACKEND = -1,
  /**
   * The audio backend
   */
  AUDIO_BACKEND = 0,
  /**
   * The MIDI backend
   */
  MIDI_BACKEND = 1
} backend_type_t;


// we only have two backends at the same time: audio and MIDI
#define NUM_BACKENDS 2


/**
 * An enum that specifies a backend's priority regarding the playback time.
 */
typedef enum backend_timebase_prio_t
{
  /**
   * The backend synchronizes to an external sample clock
   */
  TIMEBASE_PRIO_AUDIO = 3,
  /**
   * The backend is not bound to any external clock
   */
  TIMEBASE_PRIO_MIDI = 2,
  /**
   * The backend requires no timing information
   */
  TIMEBASE_PRIO_DUMMY = 1
} backend_timebase_prio_t;


typedef struct midi_event_t
{
  backend_type_t backend;
  int port;
  int length;
  unsigned char data[3];
} midi_event_t;


/**
 * Initializes the audio/MIDI subsystem.
 *
 * @return        zero on success, a negative error code on failure
 */
int audio_initialize (DenemoPrefs * config);

/**
 * Destroys and cleans up the audio/MIDI subsystem.
 *
 * @return        zero on success, a negative error code on failure
 */
int audio_shutdown ();

/**
 * Starts playing the current movement.
 */
void midi_play (gchar * callback);

/**
 * Starts playing the current movements source audio.
 */
void audio_play (void);

/**
 * Stops playing the current movement.
 */
void midi_stop ();

/**
 * Plays a single MIDI event.
 *
 * @param backend   the type of backend to be used
 * @param port      the number of the backend's output port to be used. ignored
 *                  by most backends.
 * @param buffer    the MIDI data to be sent
 */
int play_midi_event (backend_type_t backend, int port, unsigned char *buffer);

/**
 * Plays a single note.
 *
 * @param backend   the type of backend to be used
 * @param port      the number of the backend's output port to be used. ignored
 *                  by most backends.
 * @param channel   the note's MIDI channel
 * @param key       the MIDI note number
 * @param duration  the note's duration in milliseconds
 * @param volume    the note's volume (1-127)
 */
int play_note (backend_type_t backend, int port, int channel, int key, int duration, int volume);

/**
 * Plays a chord.
 *
 * @param backend   the type of backend to be used
 * @param port      the number of the backend's output port to be used. ignored
 *                  by most backends.
 * @param channel   the note's MIDI channel
 * @param chord     the chord structure containing the individual notes to be played
 */
int play_notes (backend_type_t backend, int port, int channel, chord * chord_to_play);

/**
 * Not yet implemented.
 */
int rhythm_feedback (backend_type_t backend, int duration, gboolean rest, gboolean dot);

/**
 * Sends a MIDI panic CC and/or resets the synth engine.
 *
 * @param backend   the backend to be reset
 */
int panic (backend_type_t backend);

/**
 * Sends a MIDI panic CC and/or resets the synth engine for all active
 * backends.
 */
int panic_all (void);



/**
 * Called by a backend to read midi events queued for playback.
 *
 * @param backend             the type of backend
 * @param[out] event_buffer   the event data
 * @param[out] event_length   the length of the event in bytes
 * @param[out] event_time     the time of the event (in seconds from the start
 *                            of the score)
 * @param until_time          the playback time up to which events should be
 *                            returned
 *
 * @return                    TRUE if an event was written to the output
 *                            parameters, FALSE if there is no event to be
 *                            played
 */
gboolean read_event_from_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length, double *event_time, double until_time);
gboolean read_event_from_mixer_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length);
#ifdef _HAVE_RUBBERBAND_
gboolean read_event_from_rubberband_queue (backend_type_t backend, unsigned char *event_buffer, size_t * event_length);
gboolean write_samples_to_rubberband_queue (backend_type_t backend, float *sample, gint len);
#endif
/**
 * Called by a backend to notify the audio subsystem that the current playback
 * time changed. Usually this is called once per period during playback.
 *
 * @param prio        the backend's timebase priority
 * @param new_time    the new playback time in milliseconds
 */
void update_playback_time (backend_timebase_prio_t prio, double new_time);

/**
 * Returns the current playback time in milliseconds.
 */
double get_playback_time ();
/**
 * sets the current playback slowdown factor.
 */
void set_playback_speed (double speed);
/**
 * Called by a backend when an incoming MIDI event was received.
 *
 * @param backend   the backend that received the event
 * @param port      the port that received the event (zero if there is only
 *                  one port)
 * @param buffer    the MIDI event data
 */
void input_midi_event (backend_type_t backend, int port, unsigned char *buffer);



/**
 * Queues a full redraw of the GUI.
 */
void queue_redraw_all ();

/**
 * Queues a redraw of the playhead.
 */
void queue_redraw_playhead ();

extern GMutex smfmutex;

gboolean have_midi (void);

void advance_time (gdouble seconds);

/*
 * Returns a slowdown factor; the backend is only emitting audio at this rate
 * */
gdouble get_playback_speed (void);

#endif // AUDIOINTERFACE_H
