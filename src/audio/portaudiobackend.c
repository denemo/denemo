#ifdef _HAVE_PORTAUDIO_
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
#include <glib/gstdio.h>
#include "audio/portaudiobackend.h"
#include "audio/portaudioutil.h"
#ifdef _HAVE_RUBBERBAND_
    #include <rubberband/rubberband-c.h>
#endif
#include "audio/midi.h"
#include "audio/fluid.h"
#include "audio/audiointerface.h"

#include <portaudio.h>
#include <glib.h>
#include <string.h>
#include "export/audiofile.h"
#include "core/utils.h"

static PaStream *stream;
static unsigned long sample_rate;

static unsigned long playback_frame = 0;

static gboolean reset_audio = FALSE;

static gint ready = FALSE;

static double slowdown = 1.0; //2.0 = twice as long ie half speed.

#ifdef _HAVE_RUBBERBAND_
static gboolean rubberband_active = FALSE;
static RubberBandState rubberband;
static gint rubberband_init(DenemoPrefs *config) {
    rubberband = rubberband_new(sample_rate, 2 /* channels */, RubberBandOptionProcessRealTime | RubberBandOptionStretchPrecise,
    slowdown, 1.0);
    //rubberband_set_debug_level(rubberband, 3);
    return 0;
}
void set_playback_speed (double speed) {
    if(rubberband==NULL)
        rubberband_init(&Denemo.prefs);
    Denemo.project->movement->end_time /= slowdown;
    Denemo.project->movement->start_time /= slowdown;
    if(speed>1.01) {
        slowdown = speed;
        rubberband_active = TRUE;
    }
    else
    {
        slowdown = 1.0;
        rubberband_active = FALSE;
    }
    rubberband_set_time_ratio(rubberband, slowdown);
    Denemo.project->movement->end_time *= slowdown;
    Denemo.project->movement->start_time *= slowdown;
}

gdouble get_playback_speed (void)
{
    return slowdown;
}
#else
gdouble get_playback_speed (void)
{
    return 1.0; //Rubberband can do slowdown, backend should define its own version of this
}
void set_playback_speed (double speed) {}
#endif


static double
nframes_to_seconds (unsigned long nframes)
{
  return nframes / (double) sample_rate;
}

static unsigned long
seconds_to_nframes (double seconds)
{
  return (unsigned long) (sample_rate * seconds);
}

#define MAX_MESSAGE_LENGTH (255)        //Allow single sysex blocks, ie 0xF0, length, data...0xF7  where length is one byte.

static void record_audio(float ** buffers, unsigned long frames_per_buffer){
  // Recording audio out - only one channel is saved at the moment, so source audio (which is dumped in the second channel) is not recorded.
  if (Denemo.prefs.maxrecordingtime <= 0)
    return;
  static FILE *fp = NULL;
  if (Denemo.project && Denemo.project->audio_recording)
    {
      static guint recorded_frames;
      if (fp == NULL)
        {
          const gchar *filename = recorded_audio_filename ();
          fp = fopen (filename, "wb");
          recorded_frames = 0;
          if (fp == NULL)
            g_warning ("Could not open denemo-output");
          else
            g_info ("Opened output file %s", filename);
        }
      if (fp)
        {
          if (recorded_frames / 44100 < Denemo.prefs.maxrecordingtime)
            {
              fwrite (buffers[0], sizeof (float), frames_per_buffer, fp);
              recorded_frames += frames_per_buffer;
            }
          else
            {               //only warn once, don't spew out warnings...
              if (recorded_frames < G_MAXINT)
                {
                  recorded_frames = G_MAXINT;
                  g_warning ("Recording length exceeded preference (%d seconds); use the Change Preferences dialog to alter this", Denemo.prefs.maxrecordingtime);
                }
            }
        }
    }
  else
    {
      if (fp)
        {
          fclose (fp);
          fp = NULL;
          g_message ("File closed samples are raw data, Little Endian (? or architecture dependent), mono");
        }
    }
}

static int
stream_callback (const void *input_buffer, void *output_buffer, unsigned long frames_per_buffer, const PaStreamCallbackTimeInfo * time_info, PaStreamCallbackFlags status_flags, void *user_data)
{
  float **buffers = (float **) output_buffer;
#ifdef _HAVE_RUBBERBAND_
  static gboolean initialized = FALSE;
  if (!initialized) {
      rubberband_set_max_process_size(rubberband, frames_per_buffer);
      initialized = TRUE;
  }
#endif

  size_t i;
  for (i = 0; i < 2; ++i)
    {
      memset (buffers[i], 0, frames_per_buffer * sizeof (float));
    }

  if (!ready)
    return paContinue;

#ifdef _HAVE_FLUIDSYNTH_
  if (reset_audio)
    {
      fluidsynth_all_notes_off ();
      reset_synth_channels ();
      reset_audio = FALSE;
      return paContinue;
    }

  unsigned char event_data[MAX_MESSAGE_LENGTH]; //needs to be long enough for variable length messages...
  size_t event_length = MAX_MESSAGE_LENGTH;
  double event_time;

  double until_time = nframes_to_seconds (playback_frame + frames_per_buffer);
#ifdef _HAVE_RUBBERBAND_
  gint available = rubberband_available(rubberband);
if((!rubberband_active) || (available < (gint)frames_per_buffer)) {
#endif

  while (read_event_from_queue (AUDIO_BACKEND, event_data, &event_length, &event_time, until_time/slowdown))
    {//g_print("%x %x %x\n", event_data[0], event_data[1], event_data[2] );
      fluidsynth_feed_midi (event_data, event_length);  //in fluid.c note fluidsynth api ues fluid_synth_xxx these naming conventions are a bit too similar
    }

  fluidsynth_render_audio (frames_per_buffer, buffers[0], buffers[1]);  //in fluid.c calls fluid_synth_write_float()

// Now get any audio to mix - dump it in the left hand channel for now
  event_length = frames_per_buffer;
  read_event_from_mixer_queue (AUDIO_BACKEND, (void *) buffers[1], &event_length);

#ifdef _HAVE_RUBBERBAND_
  }
  //if there is stuff available use it and give buffers[] to rubber band to process
  if(rubberband_active)
      {
      if(available < (gint)frames_per_buffer)
          rubberband_process(rubberband, (const float * const*)buffers, frames_per_buffer, 0);
      available = rubberband_available(rubberband);
      if(available >= (gint)frames_per_buffer)
          {
              rubberband_retrieve(rubberband, buffers, frames_per_buffer);//re-use buffers[] as they are available...
              write_samples_to_rubberband_queue (AUDIO_BACKEND, buffers[0], frames_per_buffer);
              write_samples_to_rubberband_queue (AUDIO_BACKEND,  buffers[1], frames_per_buffer);
              available -= frames_per_buffer;
          }
      event_length = frames_per_buffer;
      read_event_from_rubberband_queue (AUDIO_BACKEND, (unsigned char *) buffers[0], &event_length);
      event_length = frames_per_buffer;
      read_event_from_rubberband_queue (AUDIO_BACKEND, (unsigned char *) buffers[1],  &event_length);
      }
#endif //_HAVE_RUBBERBAND_

  if (until_time < get_playuntil ())
    {
#endif //_HAVE_FLUIDSYNTH_
      playback_frame += frames_per_buffer;
      update_playback_time (TIMEBASE_PRIO_AUDIO, nframes_to_seconds (playback_frame));
#ifdef _HAVE_FLUIDSYNTH_
    }
#endif //_HAVE_FLUIDSYNTH_

  // This is probably a bad idea to do heavy work in an audio callback
  record_audio(buffers, frames_per_buffer);
  return paContinue;
}

static int
actual_portaudio_initialize (DenemoPrefs * config)
{
  sample_rate = config->portaudio_sample_rate;

#ifdef _HAVE_FLUIDSYNTH_
  g_message ("Initializing Fluidsynth");
  if (fluidsynth_init (config, sample_rate))
    {
      g_warning ("Initializing Fluidsynth FAILED!");
      return -1;
    }
#endif
#ifdef _HAVE_RUBBERBAND_
  g_message ("Initializing Rubberband");
 if (rubberband_init (config))
    {
      g_warning ("Initializing Rubberband FAILED!");
      return -1;
    }
#endif
  g_unlink (recorded_audio_filename ());

  g_message ("Initializing PortAudio backend");
  g_info("PortAudio version: %s", Pa_GetVersionText());

  PaStreamParameters output_parameters;
  PaError err;

  err = Pa_Initialize ();
  if (err != paNoError)
    {
      g_warning ("Initializing PortAudio failed");
      return -1;
    }

  output_parameters.device = get_portaudio_device_index (config->portaudio_device->str);

  if (output_parameters.device == paNoDevice)
    {
      output_parameters.device =   get_portaudio_device_index ("default");
       if (output_parameters.device == paNoDevice)
        {
            g_warning("No PortAudio device %s and no default either.", config->portaudio_device->str);
            return -1;
        }
    }

  PaDeviceInfo const *info = Pa_GetDeviceInfo (output_parameters.device);

  if (!info)
    {
      g_warning ("Invalid device '%s'", config->portaudio_device->str);
      return -1;
    }

  char const *api_name = Pa_GetHostApiInfo (info->hostApi)->name;
  g_message ("Opening output device '%s: %s'", api_name, info->name);

  output_parameters.channelCount = 2;
  output_parameters.sampleFormat = paFloat32 | paNonInterleaved;
  output_parameters.suggestedLatency = Pa_GetDeviceInfo (output_parameters.device)->defaultLowOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;
  err = Pa_OpenStream (&stream, NULL, &output_parameters, config->portaudio_sample_rate, config->portaudio_period_size, paNoFlag /* make this a pref??? paClipOff */ , stream_callback, NULL);
  if (err != paNoError)
    {
      g_warning ("Couldn't open output stream");
      return -1;
    }
  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      g_warning ("Couldn't start output stream");
      return -1;
    }

  return 0;
}

static int
ready_now ()
{
  ready = TRUE;
  return FALSE;
}

static int
portaudio_initialize (DenemoPrefs * config)
{
  g_idle_add ((GSourceFunc) ready_now, NULL);
  return actual_portaudio_initialize (config);
}

static int
portaudio_destroy ()
{
  g_message ("Destroying PortAudio backend");
  ready = FALSE;
  PaError err;

  err = Pa_CloseStream (stream);
  if (err != paNoError)
    {
      g_warning ("Closing stream failed: %d, %s", err, Pa_GetErrorText (err));
      return -1;
    }

  Pa_Terminate ();

#ifdef _HAVE_FLUIDSYNTH_
  fluidsynth_shutdown ();
#endif

  return 0;
}


static int
portaudio_reconfigure (DenemoPrefs * config)
{
  portaudio_destroy ();
  return portaudio_initialize (config);
}


static int
portaudio_start_playing ()
{
  playback_frame = seconds_to_nframes (get_playback_time ());
  return 0;
}


static int
portaudio_stop_playing ()
{
  reset_audio = TRUE;
  return 0;
}


static int
portaudio_panic ()
{
  reset_audio = TRUE;
  return 0;
}


backend_t portaudio_backend = {
  portaudio_initialize,
  portaudio_destroy,
  portaudio_reconfigure,
  portaudio_start_playing,
  portaudio_stop_playing,
  portaudio_panic,
};
#endif //_HAVE_PORTAUDIO_
