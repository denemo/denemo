/* playback.c
 * Midi playback for a specific portion of
 * a score
 *
 * (c) 2000-2005 Adam Tee
 */


#include <denemo/denemo.h>
#include "exportlilypond.h"
#include "exportmidi.h"
#include "staffops.h"
#include "scoreops.h"
#include "dialogs.h"
#include "prefops.h"
#include "utils.h"
#include "external.h"
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
#include "jackmidi.h"
#include "fluid.h"

static gint timeout_id = 0, kill_id=0;
static gdouble duration = 0.0;

const gchar *
get_midi_audio_pointer(gchar *audio_device)
{
  if (!strcmp(audio_device, Fluidsynth))
    return Fluidsynth;
  else if (!strcmp(audio_device, Jack))
    return Jack;
  else if (!strcmp(audio_device, Portaudio))
    return Portaudio;
  else if (!strcmp(audio_device, None))
    return None;

#ifdef _HAVE_FLUIDSYNTH_
  return Fluidsynth;
#else 
  return None;
#endif
}

void set_tempo (void) {
  gdouble tempo = Denemo.gui->si->master_tempo;
  if(tempo<0.001 || (tempo>0.999 && tempo<1.001))
    return;
  Denemo.gui->si->tempo *= tempo;
  Denemo.gui->si->start_time /= tempo;
  Denemo.gui->si->end_time /= tempo;

  Denemo.gui->si->master_tempo = 1.0;
  score_status (Denemo.gui, TRUE);
  exportmidi(NULL, Denemo.gui->si, 0, 0);
}


/* start playing the current movement as MIDI
 * the name ext_... is anachronistic, Fluidsynth or Jack are normally used.
 */
void
ext_midi_playback (GtkAction * action, DenemoScriptParam *param) {
  GET_1PARAM(action, param, callback);
  set_tempo();
  if (Denemo.prefs.midi_audio_output == Jack)
    jack_midi_play(callback);
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    fluid_midi_play(callback);
  else infodialog("Nothing chosen to play back on:\nLook in Edit->Change Preferences->MIDI/Audio->MIDI/Audio Output\nRestart Denemo after setting this to Internal Synth.");

}

void stop_midi_playback (GtkAction * action, gpointer param) {
 if (Denemo.prefs.midi_audio_output == Jack){
   jack_midi_playback_stop();
   jack_kill_timer();
 }
 else if (Denemo.prefs.midi_audio_output == Fluidsynth){
   fluid_midi_stop();
 }

 gtk_widget_queue_draw (Denemo.scorearea);//update playhead on screen
}

void
playback_panic()
{
  if (Denemo.prefs.midi_audio_output == Jack)
    jack_midi_panic();
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    fluid_midi_panic();   
}

/** 
 * Dialog function used to select measure range 
 * This is similar to printrangedialog in print.c
 */

void
PlaybackRangeDialog(){
  DenemoGUI *gui = Denemo.gui;	
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  GtkWidget *from_time;
  GtkWidget *to_time;
  
  dialog = gtk_dialog_new_with_buttons (_("Play range in seconds:"),
	 GTK_WINDOW (Denemo.window),
	 (GtkDialogFlags) (GTK_DIALOG_MODAL |
	      GTK_DIALOG_DESTROY_WITH_PARENT),
	 GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
	 GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);
  
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  //TODO calculate hightest number in seconds
  gdouble max_end_time = 7200.0;
  //g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  label = gtk_label_new (_("Play from time"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
 
  from_time =
  gtk_spin_button_new_with_range (0.0, max_end_time, 0.1);
  gtk_box_pack_start (GTK_BOX (hbox), from_time, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_time),
	     (gdouble) gui->si->start_time);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_time =
  gtk_spin_button_new_with_range (0.0, max_end_time, 0.1);
  gtk_box_pack_start (GTK_BOX (hbox), to_time, TRUE, TRUE, 0);

  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_time),
	      (gdouble) gui->si->end_time);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->start_time =
	gtk_spin_button_get_value (GTK_SPIN_BUTTON (from_time));
      gui->si->end_time =
	gtk_spin_button_get_value (GTK_SPIN_BUTTON (to_time));
      //gtk_widget_destroy (dialog);
    }
  
  gtk_widget_destroy (dialog);
}
