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
#include "portaudiobackend.h"
#include "portaudioutil.h"
#ifdef _HAVE_RUBBERBAND_
	#include <rubberband/rubberband-c.h>
#endif
#include "midi.h"
#include "fluid.h"
#include "audiointerface.h"

#include <portaudio.h>
#include <glib.h>
#include <string.h>
#include "audiofile.h"


static PaStream *stream;
static unsigned long sample_rate;

static unsigned long playback_frame = 0;

static gboolean reset_audio = FALSE;

static gint ready = FALSE;

static double slowdown = 1.0; //2.0 = twice as long ie half speed.
static gboolean rubberband_active = FALSE;

#ifdef _HAVE_RUBBERBAND_
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
	Denemo.gui->si->end_time /= slowdown;
	Denemo.gui->si->start_time /= slowdown;
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
	Denemo.gui->si->end_time *= slowdown;
	Denemo.gui->si->start_time *= slowdown;
}

gdouble get_playback_speed (void)
{
	return slowdown;	
}
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

static int
stream_callback (const void *input_buffer, void *output_buffer, unsigned long frames_per_buffer, const PaStreamCallbackTimeInfo * time_info, PaStreamCallbackFlags status_flags, void *user_data)
{
  float **buffers = (float **) output_buffer;
 // static float *spare[2];
 // if(spare[0]==NULL)
//	{
	//	spare[0] = g_malloc0(512*sizeof(float));		
	//	spare[1] = g_malloc0(512*sizeof(float));
//	}
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
    {//g_print("");!!!! madness, the call to initialize audio fails with channels assert in alsa code without this with -O0, with -O2 fails anyway!!!
      fluidsynth_feed_midi (event_data, event_length);  //in fluid.c note fluidsynth api ues fluid_synth_xxx these naming conventions are a bit too similar
    }

  fluidsynth_render_audio (frames_per_buffer, buffers[0], buffers[1]);  //in fluid.c calls fluid_synth_write_float()

// Now get any audio to mix - dump it in the left hand channel for now
  event_length = frames_per_buffer;
  read_event_from_mixer_queue (AUDIO_BACKEND, (void *) buffers[1], &event_length);

#ifdef _HAVE_RUBBERBAND_  
  }
#endif

#ifdef _HAVE_RUBBERBAND_
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
// Recording audio out - only one channel is saved at the moment, so source audio (which is dumped in the second channel) is not recorded.
  if (Denemo.prefs.maxrecordingtime)
    {
      static FILE *fp = NULL;
      if (Denemo.gui && Denemo.gui->audio_recording)
        {
          static guint recorded_frames;
          if (fp == NULL)
            {
              extern const gchar *get_user_data_dir (void);
              const gchar *filename = recorded_audio_filename ();
              fp = fopen (filename, "wb");
              recorded_frames = 0;
              if (fp == NULL)
                g_warning ("Could not open denemo-output");
              else
                g_print ("Opened output file");
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
              g_print ("File closed samples are raw data, Little Endian (? or architecture dependent), mono");
            }
        }
    }

  return paContinue;
}

static int
actual_portaudio_initialize (DenemoPrefs * config)
{

  sample_rate = config->portaudio_sample_rate;

#ifdef _HAVE_FLUIDSYNTH_
  g_print ("Initializing Fluidsynth\n");
  if (fluidsynth_init (config, sample_rate))
    {
      g_warning ("Initializing Fluidsynth FAILED!\n");
      return -1;
    }
#endif
#ifdef _HAVE_RUBBERBAND_
 if (rubberband_init (config))
    {
      g_warning ("Initializing Rubberband FAILED!\n");
      return -1;
    }
#endif
  g_unlink (recorded_audio_filename ());

  g_print ("Initializing PortAudio backend\n");

  PaStreamParameters output_parameters;
  PaError err;

  err = Pa_Initialize ();
  if (err != paNoError)
    {
      g_warning ("initializing PortAudio failed\n");
      return -1;
    }

  output_parameters.device = get_portaudio_device_index (config->portaudio_device->str);

  if (output_parameters.device == paNoDevice)
    {
      return -1;
    }

  PaDeviceInfo const *info = Pa_GetDeviceInfo (output_parameters.device);

  if (!info)
    {
      g_warning ("invalid device '%s\n'", config->portaudio_device->str);
      return -1;
    }

  char const *api_name = Pa_GetHostApiInfo (info->hostApi)->name;
  g_print ("opening output device '%s: %s'\n", api_name, info->name);

  output_parameters.channelCount = 2;
  output_parameters.sampleFormat = paFloat32 | paNonInterleaved;
  output_parameters.suggestedLatency = Pa_GetDeviceInfo (output_parameters.device)->defaultLowOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;
  err = Pa_OpenStream (&stream, NULL, &output_parameters, config->portaudio_sample_rate, config->portaudio_period_size, paNoFlag /* make this a pref??? paClipOff */ , stream_callback, NULL);
  if (err != paNoError)
    {
      g_warning ("couldn't open output stream\n");
      return -1;
    }
  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      g_warning ("couldn't start output stream\n");
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
  g_print ("destroying PortAudio backend\n");
  ready = FALSE;
  PaError err;

  err = Pa_CloseStream (stream);
  if (err != paNoError)
    {
      g_warning ("closing stream failed: %d, %s\n", err, Pa_GetErrorText (err));
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
