/*
 * portaudiobackend.c
 * PortAudio backend.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "portaudiobackend.h"
#include "midi.h"
#include "fluid.h"

#include <portaudio.h>
#include <glib.h>
#include <string.h>


// FIXME: these should be configurable
#define SAMPLE_RATE 44100
#define PERIOD_SIZE 256


static PaStream *stream;

static unsigned long playback_frame = 0;

static gboolean reset_audio = FALSE;


static double nframes_to_seconds(unsigned long nframes) {
  return nframes / (double)SAMPLE_RATE;
}

static unsigned long seconds_to_nframes(double seconds) {
  return (unsigned long)(SAMPLE_RATE * seconds);
}


static int stream_callback(const void * input_buffer,
                           void * output_buffer,
                           unsigned long frames_per_buffer,
                           const PaStreamCallbackTimeInfo * time_info,
                           PaStreamCallbackFlags status_flags,
                           void * user_data) {
  float **buffers = (float **) output_buffer;

  size_t i;
  for (i = 0; i < 2; ++i) {
    memset(buffers[i], 0, frames_per_buffer * sizeof(float));
  }

#ifdef _HAVE_FLUIDSYNTH_
  if (reset_audio) {
    fluidsynth_all_notes_off();
    reset_audio = FALSE;
    return paContinue;
  }

  unsigned char event_data[3];
  size_t event_length;
  double event_time;

  double until_time = nframes_to_seconds(playback_frame + frames_per_buffer);

  while (read_event_from_queue(AUDIO_BACKEND, event_data, &event_length, &event_time, until_time)) {
    fluidsynth_feed_midi(event_data, event_length);
  }

  fluidsynth_render_audio(frames_per_buffer, buffers[0], buffers[1]);
#endif


  if (is_playing()) {
    playback_frame += frames_per_buffer;

    update_playback_time(TIMEBASE_PRIO_AUDIO, nframes_to_seconds(playback_frame));
  }

  return paContinue;
}


static int portaudio_initialize(DenemoPrefs *config) {
  g_print("initializing PortAudio backend\n");

  PaStreamParameters output_parameters;
  PaError err;

  err = Pa_Initialize();
  if (err != paNoError) {
    g_warning("initializing PortAudio failed\n");
    return -1;
  }

  // FIXME: allow device selection in preferences
  output_parameters.device = Pa_GetDefaultOutputDevice();

  if (output_parameters.device == paNoDevice) {
    g_warning("no default output device\n");
    return -1;
  }

  output_parameters.channelCount = 2;
  output_parameters.sampleFormat = paFloat32 | paNonInterleaved;
  output_parameters.suggestedLatency = Pa_GetDeviceInfo(output_parameters.device)->defaultLowOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(&stream, NULL, &output_parameters,
                      SAMPLE_RATE, PERIOD_SIZE, paClipOff,
                      stream_callback, NULL);
  if (err != paNoError) {
    g_warning("couldn't open output stream\n");
    return -1;
  }

#ifdef _HAVE_FLUIDSYNTH_
  if (fluidsynth_init(config, SAMPLE_RATE)) {
    return -1;
  }
#endif

  err = Pa_StartStream(stream);
  if (err != paNoError) {
    g_warning("couldn't start output stream\n");
    return -1;
  }

  return 0;
}


static int portaudio_destroy() {
  g_print("destroying PortAudio backend\n");

  PaError err;

  err = Pa_CloseStream(stream);
  if (err != paNoError) {
    g_warning("closing stream failed: %d, %s\n", err, Pa_GetErrorText(err));
    return -1;
  }

  Pa_Terminate();

#ifdef _HAVE_FLUIDSYNTH_
  fluidsynth_shutdown();
#endif

  return 0;
}


static int portaudio_reconfigure(DenemoPrefs *config) {
  portaudio_destroy();
  return portaudio_initialize(config);
}


static int portaudio_start_playing() {
  playback_frame = seconds_to_nframes(get_playback_time());
  return 0;
}


static int portaudio_stop_playing() {
  reset_audio = TRUE;
  return 0;
}


static int portaudio_play_midi_event(int port, unsigned char *buffer) {
  // TODO
  return 0;
}


static int portaudio_panic() {
  reset_audio = TRUE;
  return 0;
}


backend_t portaudio_backend = {
  portaudio_initialize,
  portaudio_destroy,
  portaudio_reconfigure,
  portaudio_start_playing,
  portaudio_stop_playing,
  portaudio_play_midi_event,
  portaudio_panic,
};

