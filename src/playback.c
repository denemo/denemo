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

gchar *
get_midi_audio_pointer(gchar *audio_device)
{
  if (!strcmp(audio_device, Fluidsynth))
    return Fluidsynth;
  else if (!strcmp(audio_device, Jack))
    return Jack;
  else if (!strcmp(audio_device, Portaudio))
    return Portaudio;

#ifdef _HAVE_FLUIDSYNTH_
  return Fluidsynth;
#else 
  return Portaudio;
#endif
}

static gint move_on(DenemoGUI *gui){
  if(timeout_id==0)
    return FALSE;
  //g_print("Current measure %d\n", gui->si->currentmeasurenum);
  set_currentmeasurenum (gui, gui->si->currentmeasurenum+1);

  return TRUE;
}

static gint kill_timer(void){
  if(timeout_id>0)
    g_source_remove(timeout_id);
  timeout_id= 0;
  if(kill_id)
    g_source_remove(kill_id);
  kill_id = 0;
  return TRUE;
}




/* start or restart an external midi player
 * trying avoid multiple instances of it
 * if start==FALSE stop the playback rather than start it
 */
static void
ext_midi_playback_control (gboolean start)
{
  DenemoGUI *gui = Denemo.gui;
  FILE *fp;
  int got, ok;
  GError *err = NULL;
  GPid playerpid = GPID_UNREF;
  gchar *mididata = NULL;
  gchar *pidpath = get_temp_filename (ext_pidfiles[EXT_MIDI]);

  /* need to by synchronised (one player at a time) */

  /* need: singleton access lock (because multiple views)
   * the old *NIX and *BSD on descriptors : <fcntl.h>
   * the SYSV and POSIX on buffered FILE* : <stdio.h>
   */

  /* check that the midi player exists */
  gchar *playerpath = g_find_program_in_path (Denemo.prefs.midiplayer->str);
  if (playerpath == NULL)
    {
      /* show a warning dialog */
      GtkWidget *dialog =
        gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
                                GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_WARNING,
                                GTK_BUTTONS_OK,
                                _("Could not find %s program"),
                                Denemo.prefs.midiplayer->str);
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                _("Please edit the chosen "
                                                  "midi player in the "
                                                  "preferences."));
      gtk_dialog_run (GTK_DIALOG (dialog));

      /* free the memory and return */
      gtk_widget_destroy (dialog);
      return;
    }

  fp = fopen (pidpath, "rb+");
  if(!fp)
    return;
#ifdef HAVE_FILE_LOCKS
  if (ftrylockfile (fp))	/* might not exist on old BSD's */
    {
      g_debug ("ext_midi: another instance is working\n");
      /* better chance next time */
      fclose (fp);
      g_free (pidpath);
      return;
    }
#endif
  /* ok, it's our turn: */
  got = fread (&playerpid, sizeof(GPid), 1, fp);
  rewind (fp);
  g_debug ("ext_midi: got player %x\n", playerpid);
  /* ensure we got something */
  if (got && (playerpid != GPID_UNREF))
    {
      g_debug ("ext_midi: killing player %x\n", playerpid);
      kill_process (playerpid);
    }
  if(!start) {
    fclose (fp);
    g_free (pidpath);
    return;
  }
  mididata = get_temp_filename ("denemoplayback.mid");
  if(gui->si->markstaffnum)
    duration = exportmidi (mididata, gui->si, gui->si->firstmeasuremarked, gui->si->lastmeasuremarked);
  else 
    if(gui->si->end)
      duration = exportmidi (mididata, gui->si, gui->si->start, gui->si->end);
    else
      if(gui->si->currentmeasurenum>1)
	duration = exportmidi (mididata, gui->si, gui->si->currentmeasurenum, 0/* means to end */);
      else
	duration = exportmidi (mididata, gui->si, 0, 0/* means whole file */);
  // g_print("Values are %d %d %d\n", gui->si->end,gui->si->start, gui->si->currentmeasurenum);
  gchar *argv[] = {
    Denemo.prefs.midiplayer->str,
    mididata,
    NULL};
  ok = g_spawn_async (NULL,             /* dir */
                      argv, NULL,       /* env */
                      G_SPAWN_SEARCH_PATH | G_SPAWN_DO_NOT_REAP_CHILD,
                      NULL,             /* child setup func */
                      NULL,             /* user data passed to setup */
                      &playerpid,       /* child pid */
                      &err);
  if (!ok)
    {
      g_warning ("ext_midi: error spawning pid %x: %s", playerpid,
                 err->message);
      g_error_free (err);
    }
  else
    {
      g_debug ("ext_midi: spawned %x\n", playerpid);
      if (playerpid != GPID_UNREF)
        fwrite (&playerpid, sizeof(GPid), 1, fp);
    }
#ifdef HAVE_FILE_LOCKS
  funlockfile (fp);		/* might not exist on old BSD's */
#endif
  fclose (fp);
  g_free (pidpath);
  g_free (mididata);
  // first measure to play at start
  if(gui->si->markstaffnum)
    set_currentmeasurenum (gui,gui->si->firstmeasuremarked);
  else    
    set_currentmeasurenum (gui, gui->si->currentmeasurenum);
  if(gui->si->end==0) {//0 means not set, we move the cursor on unless the specific range was specified
    DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
  //FIXME add a delay before starting the timer.
  timeout_id = g_timeout_add ( 4*((double)staff->timesig.time1/(double)staff->timesig.time2)/(gui->si->tempo/(60.0*1000.0)), 
			       (GSourceFunc)move_on, gui);
  kill_id = g_timeout_add ((guint)(duration*1000), (GSourceFunc)kill_timer, NULL);
  }
  return;
}

/* start or restart an external midi player
 * trying avoid multiple instances of it
 */
void
ext_midi_playback (GtkAction * action, DenemoScriptParam *param)
{
  GET_1PARAM(action, param, callback);
  if (Denemo.prefs.midi_audio_output == Jack)
    jack_midi_play(callback);
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    fluid_midi_play(callback);
  else  
    ext_midi_playback_control (TRUE);
}

void stop_midi_playback (GtkAction * action, gpointer param) {
 if (Denemo.prefs.midi_audio_output == Jack){
   jack_midi_playback_stop();
   jack_kill_timer();
 }
 else if (Denemo.prefs.midi_audio_output == Fluidsynth){
   fluid_midi_stop();
 }
 else {
   ext_midi_playback_control (FALSE);
   kill_timer();
 }
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
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, TRUE, TRUE, 0);

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
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), to_time, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_time),
	      (gdouble) gui->si->end_time);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->start_time =
	gtk_spin_button_get_value_as_float (GTK_SPIN_BUTTON (from_time));
      gui->si->end_time =
	gtk_spin_button_get_value_as_float (GTK_SPIN_BUTTON (to_time));
      //gtk_widget_destroy (dialog);
    }
  
  gtk_widget_destroy (dialog);
}

