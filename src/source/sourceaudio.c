//      sourceaudio.c
//
//      Copyright 2012 Richard Shann <rshann@debian2>
//
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.

#include <denemo/denemo.h>

#ifdef DISABLE_AUBIO
    gboolean
    audio_is_playing (void)
    {
      return FALSE;
    }
#else
#include <stdio.h>
#include <sndfile.h>
#include <fcntl.h>
#include <aubio/aubio.h>
#include "core/utils.h"
#include "core/view.h"
#include "audio/midi.h"
#include "export/exportmidi.h"
#include "source/sourceaudio.h"
#include "command/keyresponses.h"
#include "audio/audiointerface.h"
#if GTK_MAJOR_VERSION==3
#include <gdk/gdkkeysyms-compat.h>      //FIXME Look for something more gtk3 like
#endif

static gint remaining_leadin = 0;         //number of frames of silence before playing audio
static gboolean playing = FALSE;



#include <aubio/aubio.h>
#include "core/utils.h"
#include "core/view.h"
#include "audio/midi.h"
#include "export/exportmidi.h"
#include "source/sourceaudio.h"
#include "command/keyresponses.h"
#include "audio/audiointerface.h"

//Creates a list of times which the aubio onset detector thinks are note onset times for the audio Denemo->si->recording
//Result is placed in Denemo->si->note_onsets
void
generate_note_onsets (void)
{
  DenemoRecording *audio = Denemo.project->movement->recording;
  gint channels = audio->channels;

 smpl_t threshold = 0.3;
 smpl_t silence = -90.;
 uint_t buffer_size = 1024;
 uint_t overlap_size = 512;

 uint_t samplerate = 44100;

 aubio_onset_t *o = new_aubio_onset("default",
     buffer_size, overlap_size, samplerate);
 fvec_t *ibuf = new_fvec (overlap_size);
 fvec_t *onset = new_fvec (2);

  unsigned int pos = 0;         /*frames%dspblocksize */
  unsigned int i;               /*channels */
  unsigned int j;               /*frames */

  busy_cursor (Denemo.notebook);
  gtk_window_set_modal (progressbar (_("Analysing Audio"), NULL), TRUE);

  rewind_audio ();
  if (audio->notes)
    {
      g_list_free_full (audio->notes, g_free);
      audio->notes = NULL;
    }
  for (j = 0; j < (unsigned) audio->nframes; j++)
    {
      sf_read_float (audio->sndfile, ibuf->data + pos, 2);   //g_debug("\t%f", ibuf->data[0][pos]);
      if (pos == overlap_size - 1)
        {
          /* block loop */
          gtk_main_iteration_do (FALSE);
          aubio_onset_do (o, ibuf, onset);
          while (gtk_events_pending ())
            gtk_main_iteration ();
          if(onset->data[0] != 0) {
              DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
              note->timing = aubio_onset_get_last(o);/* aubio_onset_get_delay_s(o) for seconds */
            audio->notes = g_list_append (audio->notes, note);// g_print ("Onset found timing %d\n", (gint)note->timing);
          }
          pos = -1;             /* so it will be zero next j loop */
        }                       /* end of if pos==overlap_size-1 */
      pos++;
    }
if (audio->notes==NULL) g_warning ("No onsets found\n");
  del_aubio_onset (o);
  del_fvec (ibuf);
  del_fvec (onset);
  aubio_cleanup ();
  progressbar_stop ();
  normal_cursor (Denemo.notebook);
}
gboolean
get_audio_sample (float *sample)
{
  if (!playing)
    return FALSE;
  gboolean ret = FALSE;
  if (remaining_leadin)
    {
      *sample = *(sample + 1) = 0.0;
      remaining_leadin--;
      ret = TRUE;
    }
  else
    {
      if (Denemo.project->movement && Denemo.project->movement->recording && Denemo.project->movement->recording->sndfile)
        {
          ret = (2 == sf_read_float (Denemo.project->movement->recording->sndfile, sample, 2));
          if (ret)
            {
				*sample *= Denemo.project->movement->recording->volume;
				*(sample + 1) *= Denemo.project->movement->recording->volume;
			}
        }
    }
  return ret;
}

gboolean
open_source_audio (gchar * filename)
{
  SF_INFO sfinfo;
  DenemoRecording *temp;
  sfinfo.format = 0;

  delete_recording();

  if (filename)
    {
      gpointer sndfile = sf_open (filename, SFM_READ, &sfinfo);
      if (sndfile)
        {
          temp = (DenemoRecording *) g_malloc (sizeof (DenemoRecording));
          temp->type = DENEMO_RECORDING_AUDIO;
          temp->sndfile = sndfile;
          temp->filename = g_strdup (filename);
          temp->samplerate = sfinfo.samplerate;
          temp->channels = sfinfo.channels;
          temp->nframes = (int) sf_seek (temp->sndfile, -1, SEEK_END);
          temp->leadin = 0;/*  */
          g_print ("sndfile: %s sample rate is %d channels %d containing %d \n", sf_strerror (temp->sndfile), sfinfo.samplerate, sfinfo.channels, temp->nframes);

		  if (temp->nframes>0)
				{
				
				  temp->volume = 1.0;
				  g_mutex_lock (&smfmutex);
				  Denemo.project->movement->recording = temp;
				  g_mutex_unlock (&smfmutex);
				  update_leadin_widget (0.0);
				  if (sfinfo.channels != 2)
					warningdialog (_("Audio is not stereo - expect bad things!"));
				  if (sfinfo.samplerate != 44100)
					warningdialog (_("Audio does not have 44100 sample rate: this could be bad"));
				  //FIXME here generate a click track if the score is empty
				  if (Denemo.project->movement->smfsync != Denemo.project->movement->changecount)
					{
					  exportmidi (NULL, Denemo.project->movement);  //generate a timebase
					}
				  
				  generate_note_onsets ();
				  draw_score_area();
				}
        }
    }
  return (Denemo.project->movement->recording != NULL);
}

gboolean
close_source_audio (void)
{
  gboolean ret = (Denemo.project->movement->recording != NULL);
  (void) open_source_audio (NULL);
  return ret;
}

void
rewind_audio (void)
{
  if (Denemo.project->movement->recording && (Denemo.project->movement->recording->type==DENEMO_RECORDING_AUDIO))
    {
      if (Denemo.project->movement->recording->sndfile == NULL)
        {
          gint leadin = Denemo.project->movement->recording->leadin;  /* not part of the audio file itself */
          open_source_audio (Denemo.project->movement->recording->filename);
          if (Denemo.project->movement->recording==NULL)
          {
            g_warning("Unable to open audio file");
            return;
          }
          if (Denemo.project->movement->recording->samplerate)
            {
              Denemo.project->movement->recording->leadin = leadin;//this is tautological FIXME
              update_leadin_widget (((double) leadin) / Denemo.project->movement->recording->samplerate);
            }
        }
        
      gdouble start = get_start_time ();
      if (start < 0.0)
        start = 0.0;
      gint startframe = start * Denemo.project->movement->recording->samplerate;
      startframe += Denemo.project->movement->recording->leadin;
      if (startframe < 0)
        {
          remaining_leadin = -startframe;//we will need to issue this many frames of silence before the audio
          startframe = 0;
        }
      else
        remaining_leadin = 0;//we will sf_seek to start the audio straight away from there     
		sf_seek (Denemo.project->movement->recording->sndfile, 0, SEEK_SET);
    }
}

gboolean
set_lead_in (gdouble secs)
{
  if (Denemo.project->movement->recording)
    {
      if ((Denemo.project->movement->recording->type==DENEMO_RECORDING_AUDIO) && Denemo.project->movement->recording->sndfile == NULL)
        {
            open_source_audio (Denemo.project->movement->recording->filename);
            if (Denemo.project->movement->recording==NULL) {
                g_warning("Unable to open source audio");
                return FALSE;
            }
        }
      Denemo.project->movement->recording->leadin = secs * Denemo.project->movement->recording->samplerate;
      return TRUE;
    }
  return FALSE;
}



static gboolean annotating = FALSE;
static GQueue *timings = NULL;  //list of measure start times in microseconds from start, as indicated by the user.
static gint
record_timing (GtkWidget * widget, GdkEventKey * event)
{
  if (event->keyval == GDK_Escape)
    {
      call_out_to_guile ("(if (defined? 'DenemoAudioAnnotateFinished) (DenemoAudioAnnotateFinished))");
      stop_audio_playing ();
    }
  else
    {
      g_queue_push_tail (timings, GINT_TO_POINTER ((gint) (1000000 * get_playback_time ())));
      call_out_to_guile ("(DenemoAudioAnnotate)");
    }
  return TRUE;
}

gdouble
get_audio_timing (void)
{
  gdouble ret = -1.0;
  if (!g_queue_is_empty (timings))
    {
      ret = (GPOINTER_TO_INT (g_queue_pop_head (timings))) / 1000000.0;
    }
  return ret;
}

void
start_audio_playing (gboolean annotate)
{
  if (annotate)
    {
      annotating = TRUE;
      if (timings)
        g_queue_clear (timings);
      else
        timings = g_queue_new ();
      g_signal_connect (G_OBJECT (Denemo.scorearea), "key_press_event", G_CALLBACK (record_timing), NULL);
      g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_keypress_event), NULL);
      g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_press), NULL);
      g_signal_handlers_block_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_release), NULL);


    }
  rewind_audio ();

  initialize_until_time ();
  audio_play ();
  playing = TRUE;
}

void
stop_audio_playing (void)
{
  playing = FALSE;
  if (annotating)
    {
      annotating = FALSE;
      g_signal_handlers_disconnect_by_func (G_OBJECT (Denemo.scorearea), record_timing, NULL);
      g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_keypress_event), NULL);
      g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_press), NULL);
      g_signal_handlers_unblock_by_func (G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_release), NULL);
    }
}

gboolean
open_source_audio_file (void)
{
  gboolean ret = FALSE;
  GtkWidget *dialog = gtk_file_chooser_dialog_new (_("Open Audio Source File"),
                                                   NULL,
                                                   GTK_FILE_CHOOSER_ACTION_OPEN,
                                                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                                                   _("_Open"), GTK_RESPONSE_ACCEPT,
                                                   NULL);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *filename;
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      gtk_widget_destroy (dialog);



      ret = open_source_audio (filename);
      g_free (filename);

      if (!ret)
        warningdialog (_("Could not load the audio file. Note only stereo with sample rate 44100 are supported at present. Use Audacity or similar to convert."));
    }
  else
    gtk_widget_destroy (dialog);

  return ret;
}
gboolean
audio_is_playing (void)
{
  return playing;
}
#endif //DISABLE_AUBIO



