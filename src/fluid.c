/*
 * fluid.c
 * JACK audio and MIDI backends.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "fluid.h"
#include "midi.h"

#include <fluidsynth.h>
#include <glib.h>
//#include <sys/time.h>
//#include "moveviewport.h"
//#include "playback.h"

#define MAX_NUMBER_OF_TRACKS    128
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define KEY_PRESSURE            0xA0
#define CONTROL_CHANGE          0xB0
#define PROGRAM_CHANGE          0xC0
#define CHANNEL_PRESSURE        0xD0
#define PITCH_BEND              0xE0
#define MIDI_SYSTEM_RESET       0xFF

#define SYS_EXCLUSIVE_MESSAGE1  0xF0

static fluid_settings_t* settings = NULL;
static fluid_synth_t* synth = NULL;

static int sfont_id = -1;
//static fluid_audio_driver_t* adriver;




//static gint start_fluid_settings()
//{
//  settings = new_fluid_settings();
//  if (!settings){
//    g_warning("\nFailed to create the settings\n");
//    fluidsynth_shutdown();
//    return 1;
//  }
//  return 0;
//}


int fluidsynth_init(DenemoPrefs *config, unsigned int samplerate)
{
  g_debug("\nStarting FLUIDSYNTH\n"); 

  settings = new_fluid_settings();
  if (!settings) {
    g_warning("\nFailed to create the settings\n");
//    fluidsynth_shutdown();
    return -1;
  }

  /* Change the settings if necessary*/
//  fluid_settings_setstr(settings, "audio.jack.id", "Denemo");
//  fluid_settings_setint(settings, "audio.jack.autoconnect", 1);

//  fluid_settings_setstr(settings, "audio.driver", Denemo.prefs.fluidsynth_audio_driver->str);
  fluid_settings_setint(settings, "synth.reverb.active" , config->fluidsynth_reverb ? 1 : 0);
  fluid_settings_setint(settings, "synth.chorus.active" , config->fluidsynth_chorus ? 1 : 0);
//  if(Denemo.prefs.fluidsynth_sample_rate>(22050-1)) {
//    gint success = 
//      fluid_settings_setnum(settings, "synth.sample-rate" ,(double) Denemo.prefs.fluidsynth_sample_rate);
//    g_print("Setting sample rate %f %s\n",(double) Denemo.prefs.fluidsynth_sample_rate, success?"- success":"- FAILURE");
//  }
//
//  if(Denemo.prefs.fluidsynth_period_size>(64-1)) {
//   fluid_settings_setint(settings, "audio.period-size" , Denemo.prefs.fluidsynth_period_size);
//    g_print("Setting audio.period-size to %d\n", Denemo.prefs.fluidsynth_period_size);
//  }
//
//#ifdef G_OS_WIN32
//  if(Denemo.prefs.fluidsynth_sample_rate<22050) {
//    fluid_settings_setnum(settings, "synth.sample-rate" , 44100.0);
//    g_print("Setting sample rate to %f Hz\n", 44100.0);
//  }
//  if(Denemo.prefs.fluidsynth_period_size<64) {
//   fluid_settings_setint(settings, "audio.period-size" , 2048);
//    g_print("Setting audio.period-size to %d\n", 2048);
//  }
//#endif


  /* Create the synthesizer. */
  synth = new_fluid_synth(settings);
  if (!synth) {
    g_warning("\nFailed to create the settings\n");
    fluidsynth_shutdown();
//    delete_fluid_synth(synth);
    return -1;
  }

//  /* Create the audio driver. */
//  adriver = new_fluid_audio_driver(settings, synth);
//  if (!adriver) {
//    g_warning("\nFailed to create the audio driver\n");
//    fluidsynth_shutdown();
//    return -1;
//  }

  /* Load a SoundFont*/
  sfont_id = fluid_synth_sfload(synth, config->fluidsynth_soundfont->str, FALSE);
  if (sfont_id == -1) {
    g_warning("\nFailed to load the soundfont\n");
    fluidsynth_shutdown();
    return -1;
  }

  /* Select bank 0 and preset 0 in the SoundFont we just loaded on
  channel 0 */
  fluid_synth_program_select(synth, 0, sfont_id, 0, 0);

  gint i;
  for(i=0;i<16;i++)
    fluid_synth_program_change(synth, i, 0);
   return 0;
}

void fluidsynth_shutdown()
{
  /* Clean up */
  g_debug("\nStopping FLUIDSYNTH\n");

  if (sfont_id != -1) {
    fluid_synth_sfunload(synth, sfont_id, FALSE);
  }

//  if (adriver)
//    delete_fluid_audio_driver(adriver);
//  adriver = NULL;

  if (synth) {
    delete_fluid_synth(synth);
  }
  synth = NULL;

  if (settings) {
    delete_fluid_settings(settings); 
  }
  settings = NULL;
}

//void
//fluidsynth_start_restart (void){
//  if (synth){
//    fluidsynth_shutdown();
//    fluidsynth_init();
//  }
//  else {
//    fluidsynth_init();
//  }
//}

//static gboolean noteoff_callback(gint notenum){
//  gint key = notenum&0xFF;
//  gint chan = notenum>>8;
//  //g_print("turning off %d on channel %d\n", key, chan);
//  if (synth)
//    fluid_synth_noteoff(synth, chan, key);
//  return FALSE;
//}
//
//
//void fluid_all_notes_off_channel(gint chan) {
//  fluid_synth_cc(synth, chan, 123, 0);
//}
//
//void fluid_all_notes_off(void) {
//  gint chan;
//  for(chan=0;chan<16;chan++)
//  fluid_all_notes_off_channel(chan);
//}




/**
 * Select the soundfont to use for playback
 */
//void
//choose_sound_font (GtkWidget * widget, GtkWidget *fluidsynth_soundfont)
//{
//  GtkWidget *sf;
//  GtkFileFilter *filter;
//
//  sf = gtk_file_chooser_dialog_new (_("Choose SoundFont File"),
//				     GTK_WINDOW (Denemo.window),
//				    GTK_FILE_CHOOSER_ACTION_OPEN,
//				    GTK_STOCK_CANCEL,
//				    GTK_RESPONSE_REJECT,
//				    GTK_STOCK_OPEN,
//				    GTK_RESPONSE_ACCEPT, NULL);
//
//  //TODO Should we filter????
//  //filter = gtk_file_filter_new ();
//  //gtk_file_filter_set_name (filter, "Soundfont file");
//  //gtk_file_filter_add_pattern (filter, "*.sf");
//  //gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (fs), filter);
//
//  gtk_widget_show_all (sf);
//  if (gtk_dialog_run (GTK_DIALOG (sf)) == GTK_RESPONSE_ACCEPT)
//    {
//      g_string_assign (Denemo.prefs.fluidsynth_soundfont,
//		       gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (sf)));
//  	/* this will only work for 1 sound font */
//        gtk_entry_set_text (GTK_ENTRY (fluidsynth_soundfont), Denemo.prefs.fluidsynth_soundfont->str);
//
//    }
//  gtk_widget_destroy (sf);
//}




//void fluid_midi_panic()
//{
//  fluid_synth_system_reset(synth);
//  Denemo.gui->si->end_time = pause_time = -1.0;
//  Denemo.gui->si->start_time =  0.0;
//  displayhelper(Denemo.gui);
//}
//


void feed_fluidsynth_midi(unsigned char *event_data, size_t event_length) {
  int channel = (event_data[0] & 0x0f);
  int type = (event_data[0] & 0xf0);

  switch (type) {
    case NOTE_ON: {
      int velocity = ((int)(Denemo.gui->si->master_volume * event_data[2]));
      if (velocity > 0x7F) velocity = 0x7F;
      fluid_synth_noteon(synth, channel, event_data[1], velocity);
      }
      break;
    case NOTE_OFF:
      fluid_synth_noteoff(synth, channel, event_data[1]);
      break;
    case CONTROL_CHANGE:
       fluid_synth_cc(synth, channel, event_data[1], event_data[2]);
       break;
     case PROGRAM_CHANGE:
       fluid_synth_program_change(synth, channel, event_data[1]);
       break;
     case PITCH_BEND:
       fluid_synth_pitch_bend(synth, channel, event_data[1] + (event_data[2] << 8));
       break;
//     case MIDI_SYSTEM_RESET:
//       fluid_synth_system_reset(synth);
//       break;

//    case SYS_EXCLUSIVE_MESSAGE1:
//      if(FLUIDSYNTH_VERSION_MAJOR>=1 && FLUIDSYNTH_VERSION_MINOR>=1)
//        fluid_synth_sysex(synth, buffer+1, 19, NULL, 0, NULL, FALSE);
//      else
//        g_warning("Not supported by this fluidsynth version use >=1.1");
//      //char *response, int *response_len, int *handled, int dryrun)
//      break;
//
//      //FIXME - duplicate code below 
    default:
      g_warning("MIDI message Not handled");
  }
}


void render_fluidsynth_audio(unsigned int nframes, float *left_channel, float *right_channel) {
  fluid_synth_write_float(synth, nframes, left_channel, 0, 1, right_channel, 0, 1);
}

