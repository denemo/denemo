/* playback.c
 * Midi playback for a specific portion of
 * a score
 *
 * (c) 2000-2005 Adam Tee
 */


#include <denemo/denemo.h>
#include "audio/playback.h"
#include "export/exportlilypond.h"
#include "export/exportmidi.h"
#include "command/staff.h"
#include "command/score.h"
#include "ui/dialogs.h"
#include "core/prefops.h"
#include "core/utils.h"
#include "core/external.h"
#include "audio/audiointerface.h"
#include "audio/midi.h"
#include "core/view.h"
#include "source/sourceaudio.h"
#include "audio/fluid.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
#include <wait.h>
#endif
#include <errno.h>

#ifndef _HAVE_FLUIDSYNTH_

int fluidsynth_init (DenemoPrefs * config, unsigned int samplerate){};
void fluidsynth_shutdown (){};


/**
 * Feeds a MIDI event to the synth engine.
 */
void fluidsynth_feed_midi (unsigned char *event_data, size_t event_length){};

/**
 * Sends an all-notes-off event to the synth engine.
 */
void fluidsynth_all_notes_off (){};

/**
 * Renders the given number of audio frames into a buffer.
 */
void fluidsynth_render_audio (unsigned int nframes, float *left_channel, float *right_channel){};

/**
 * Select the soundfont to use for playback
 */
void choose_sound_font (GtkWidget * widget, GtkWidget * fluidsynth_soundfont){};
void reset_synth_channels (void){};
void fluid_set_gain (gdouble gain){};



#endif


/* start playing the current movement as MIDI
 * the name ext_... is anachronistic, Fluidsynth or Jack are normally used.
 */
void
ext_midi_playback (DenemoAction * action, DenemoScriptParam * param)
{
  if(Denemo.non_interactive)
    return;

  GET_1PARAM (action, param, callback);

  set_playbutton ();
  if (is_playing ())
    {
      toggle_paused ();
      fluidsynth_all_notes_off ();
      return;
    }
#ifdef DISABLE_AUBIO
#else
  //rewind_audio(); done in start_audio_playing()
  start_audio_playing (FALSE);
#endif
  midi_play (callback);
}


// a function that stops any play in progress, and starts play again.
void
restart_play (void)
{
  stop_midi_playback (NULL, NULL);
  ext_midi_playback (NULL, NULL);
}

void
stop_midi_playback (DenemoAction * action, DenemoScriptParam* param)
{
  midi_stop ();
  if (Denemo.playbackview)
    gtk_widget_queue_draw (Denemo.playbackview);
  set_playbutton ();    
  draw_score_area();     //update playhead on screen
}

void
playback_panic (void)
{
  panic_all ();
}

/**
 * Dialog function used to select measure range
 * This is similar to printrangedialog in print.c
 */

void
PlaybackRangeDialog ()
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  GtkWidget *from_time;
  GtkWidget *to_time;

  dialog = gtk_dialog_new_with_buttons (_("Play range in seconds:"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  //TODO calculate hightest number in seconds
  gdouble max_end_time = 7200.0;
  //g_list_length (((DenemoStaff *) (gui->movement->thescore->data))->measures);

  label = gtk_label_new (_("Play from time"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  from_time = gtk_spin_button_new_with_range (0.0, max_end_time, 0.1);
  gtk_box_pack_start (GTK_BOX (hbox), from_time, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_time), (gdouble) gui->movement->start_time);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_time = gtk_spin_button_new_with_range (0.0, max_end_time, 0.1);
  gtk_box_pack_start (GTK_BOX (hbox), to_time, TRUE, TRUE, 0);

  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_time), (gdouble) gui->movement->end_time);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->movement->start_time = gtk_spin_button_get_value (GTK_SPIN_BUTTON (from_time));
      gui->movement->end_time = gtk_spin_button_get_value (GTK_SPIN_BUTTON (to_time));
      //gtk_widget_destroy (dialog);
    }

  gtk_widget_destroy (dialog);
}
