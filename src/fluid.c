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
#include "pitchentry.h"

#include <fluidsynth.h>
#include <glib.h>


static fluid_settings_t* settings = NULL;
static fluid_synth_t* synth = NULL;
static int sfont_id = -1;


int fluidsynth_init(DenemoPrefs *config, unsigned int samplerate)
{
  g_debug("\nStarting FLUIDSYNTH\n"); 

  settings = new_fluid_settings();
  if (!settings) {
    g_warning("\nFailed to create the settings\n");
    return -1;
  }

  fluid_settings_setnum(settings, "synth.sample-rate" , (double) samplerate);

  fluid_settings_setint(settings, "synth.reverb.active" , config->fluidsynth_reverb ? 1 : 0);
  fluid_settings_setint(settings, "synth.chorus.active" , config->fluidsynth_chorus ? 1 : 0);

  // create the synthesizer
  synth = new_fluid_synth(settings);
  if (!synth) {
    g_warning("\nFailed to create the settings\n");
    fluidsynth_shutdown();
    return -1;
  }

  // load a sound font
  sfont_id = fluid_synth_sfload(synth, config->fluidsynth_soundfont->str, FALSE);
  if (sfont_id == -1) {
    g_warning("\nFailed to load the soundfont\n");
    fluidsynth_shutdown();
    return -1;
  }

  // select bank 0 and preset 0 in the soundfont we just loaded on channel 0
  fluid_synth_program_select(synth, 0, sfont_id, 0, 0);

  gint i;
  for(i=0;i<16;i++)
    fluid_synth_program_change(synth, i, 0);

  if(Denemo.prefs.pitchspellingchannel)
    fluid_synth_program_change(synth, Denemo.prefs.pitchspellingchannel, Denemo.prefs.pitchspellingprogram);
  set_tuning();

  return 0;
}


void fluidsynth_shutdown()
{
  g_debug("\nStopping FLUIDSYNTH\n");

  if (sfont_id != -1) {
    fluid_synth_sfunload(synth, sfont_id, FALSE);
  }

  if (synth) {
    delete_fluid_synth(synth);
  }
  synth = NULL;

  if (settings) {
    delete_fluid_settings(settings); 
  }
  settings = NULL;
}


void fluidsynth_feed_midi(unsigned char *event_data, size_t event_length) {
  int channel = (event_data[0] & 0x0f);
  int type = (event_data[0] & 0xf0);

  switch (type) {
    case MIDI_NOTE_ON: {
      int velocity = ((int)(Denemo.gui->si->master_volume * event_data[2]));
      if (velocity > 0x7F) velocity = 0x7F;
      fluid_synth_noteon(synth, channel, event_data[1], velocity);
      }
      break;
    case MIDI_NOTE_OFF:
      fluid_synth_noteoff(synth, channel, event_data[1]);
      break;
    case MIDI_CONTROL_CHANGE:
       fluid_synth_cc(synth, channel, event_data[1], event_data[2]);
       break;
    case MIDI_PROGRAM_CHANGE:
      fluid_synth_program_change(synth, channel, event_data[1]);
      break;
    case MIDI_PITCH_BEND:
      fluid_synth_pitch_bend(synth, channel, event_data[1] + (event_data[2] << 7));
      break;
    case SYS_EXCLUSIVE_MESSAGE1:
   // g_print("length %d\n", event_length);
      fluid_synth_sysex(synth, event_data+1, event_length-1, NULL, 0, NULL, FALSE);
      break;
    default:
      g_warning("MIDI message type %x not handled", type);
  }
}


//void fluid_all_notes_off_channel(gint chan) {
//  fluid_synth_cc(synth, chan, 123, 0);
//}
//
//void fluid_all_notes_off(void) {
//  gint chan;
//  for(chan=0;chan<16;chan++)
//  fluid_all_notes_off_channel(chan);
//}


void fluidsynth_all_notes_off() {
  // FIXME: this call has the potential to cause an xrun and/or disconnect us from JACK
  fluid_synth_system_reset(synth);
}

void fluidsynth_render_audio(unsigned int nframes, float *left_channel, float *right_channel) {
  printf("\nsynth == %d, nframes == %d, left_channel == %f right_channel == %f\n",synth, nframes, left_channel, right_channel);
  fluid_synth_write_float(synth, nframes, left_channel, 0, 1, right_channel, 0, 1);
}

/**
 * Select the soundfont to use for playback
 */
void
choose_sound_font (GtkWidget * widget, GtkWidget *fluidsynth_soundfont)
{
  GtkWidget *sf;
  GtkFileFilter *filter;

  sf = gtk_file_chooser_dialog_new (_("Choose SoundFont File"),
                                     GTK_WINDOW (Denemo.window),
                                    GTK_FILE_CHOOSER_ACTION_OPEN,
                                    GTK_STOCK_CANCEL,
                                    GTK_RESPONSE_REJECT,
                                    GTK_STOCK_OPEN,
                                    GTK_RESPONSE_ACCEPT, NULL);

  //TODO Should we filter????
  //filter = gtk_file_filter_new ();
  //gtk_file_filter_set_name (filter, "Soundfont file");
  //gtk_file_filter_add_pattern (filter, "*.sf");
  //gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (fs), filter);

  gtk_widget_show_all (sf);
  if (gtk_dialog_run (GTK_DIALOG (sf)) == GTK_RESPONSE_ACCEPT)
    {
      g_string_assign (Denemo.prefs.fluidsynth_soundfont,
                       gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (sf)));
        /* this will only work for 1 sound font */
        gtk_entry_set_text (GTK_ENTRY (fluidsynth_soundfont), Denemo.prefs.fluidsynth_soundfont->str);

    }
  gtk_widget_destroy (sf);
}


