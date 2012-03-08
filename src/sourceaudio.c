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


#include <stdio.h>
#include <sndfile.h>
#include <fcntl.h>
#if GTK_MAJOR_VERSION==3
  #include <gdk/gdkkeysyms-compat.h> //FIXME Look for something more gtk3 like
#endif
#include "view.h"
#include "midi.h"
#include "sourceaudio.h"
#include "keyresponses.h"
#include "audiointerface.h"

static gint leadin = 0;//number of frames of silence before playing audio
static gboolean playing = FALSE;

gboolean get_audio_sample(float *sample) {
  if(!playing) return FALSE;
  gboolean ret = FALSE;
  if(leadin) {
    *sample = *(sample+1) = 0.0;
    leadin--;
    ret = TRUE;
  } else {
  if(Denemo.gui->si && Denemo.gui->si->audio && Denemo.gui->si->audio->sndfile) {
    ret = (2 == sf_read_float(Denemo.gui->si->audio->sndfile, sample, 2));
    if(ret)
      *sample *= Denemo.gui->si->audio->volume;
      *(sample+1) *= Denemo.gui->si->audio->volume;
    }
  }
  return ret;
}

gboolean
open_source_audio(gchar *filename) {
  SF_INFO sfinfo;
  DenemoAudio *temp;
  sfinfo.format = 0;
  //FIXME a better name for the mutex which originally was just for midi data, but will work for audio data too.
  if(Denemo.gui->si->audio && Denemo.gui->si->audio->sndfile) {
    temp = Denemo.gui->si->audio;
    g_static_mutex_lock (&smfmutex);
    Denemo.gui->si->audio = NULL;
    g_static_mutex_unlock (&smfmutex);
    sf_close(temp->sndfile);
    g_free(temp->filename);
    g_free(temp);
    
  }
  if(filename) {
    gpointer sndfile = sf_open(filename, SFM_READ, &sfinfo);
    if(sndfile) {
      temp = (DenemoAudio*)g_malloc(sizeof(DenemoAudio));
      temp->sndfile = sndfile;
      temp->filename = g_strdup(filename);
      temp->samplerate = sfinfo.samplerate;
      temp->channels = sfinfo.channels;
      g_print("sndfile: %s sample rate is %d channels %d containing %d \n", sf_strerror(temp->sndfile), sfinfo.samplerate, sfinfo.channels, sf_seek(temp->sndfile, -1, SEEK_END));
      //FIXME warn if samplerate != Denemo.prefs.samplerate
      temp->volume = 1.0;
      g_static_mutex_lock (&smfmutex);
      Denemo.gui->si->audio = temp;
      g_static_mutex_unlock (&smfmutex);
      update_leadin_widget(-1.0);
    }
  }
  Denemo.gui->si->audio?
    gtk_widget_show(Denemo.audio_vol_control):gtk_widget_hide(Denemo.audio_vol_control);
  return (Denemo.gui->si->audio != NULL);
}

gboolean close_source_audio(void) {
  gboolean ret = (Denemo.gui->si->audio != NULL);
  (void)open_source_audio(NULL);
  return ret;
}

void
rewind_audio(void) {
    if(Denemo.gui->si->audio) {
      if(Denemo.gui->si->audio->sndfile == NULL) {
        gint leadin = Denemo.gui->si->audio->leadin;/* not part of the audio file itself */
        open_source_audio (Denemo.gui->si->audio->filename);
        if(Denemo.gui->si->audio->samplerate) {
          Denemo.gui->si->audio->leadin = leadin;
          update_leadin_widget(((double)leadin)/Denemo.gui->si->audio->samplerate);
        }
      }
      gdouble start = get_start_time();
      if(start<0.0) start = 0.0;
      gint startframe =  start*Denemo.gui->si->audio->samplerate;
      startframe += Denemo.gui->si->audio->leadin;
       if(startframe < 0) {
         leadin = -startframe;
         startframe = 0;
        } else
        leadin = 0;     
      sf_seek(Denemo.gui->si->audio->sndfile, startframe, SEEK_SET);
    } else
    gtk_widget_hide(Denemo.audio_vol_control);
}

gboolean set_lead_in(gdouble secs) {
   if(Denemo.gui->si->audio) {
      if(Denemo.gui->si->audio->sndfile == NULL)
        open_source_audio (Denemo.gui->si->audio->filename);
      if(Denemo.gui->si->audio->sndfile) {
       Denemo.gui->si->audio->leadin = secs*Denemo.gui->si->audio->samplerate;
       return TRUE;
      }
   }
  return FALSE;
}




gboolean audio_is_playing(void) {
  return playing;
}



static gboolean annotating = FALSE;
static GQueue *timings = NULL;//list of measure start times in microseconds from start, as indicated by the user.
static gint record_timing(GtkWidget * widget, GdkEventKey * event) {
  if(event->keyval== GDK_Escape) {
    call_out_to_guile("(if (defined? 'DenemoAudioAnnotateFinished) (DenemoAudioAnnotateFinished))");
    stop_audio_playing();
  }
  else {
    g_queue_push_tail(timings, (gpointer)((gint)(1000000 * get_playback_time())));
    call_out_to_guile("(DenemoAudioAnnotate)");
  }
  return TRUE;
}

gdouble get_audio_timing(void) {
  gdouble ret = -1.0;
  if(!g_queue_is_empty(timings)) {
   ret = ((gint)(g_queue_pop_head(timings)))/1000000.0;
  }
 return ret;
}

void start_audio_playing(gboolean annotate) {
  if(annotate) {
    annotating = TRUE;
    if(timings)
      g_queue_clear(timings);
    else
      timings = g_queue_new();
    g_signal_connect (G_OBJECT (Denemo.scorearea), "key_press_event",
		    G_CALLBACK (record_timing), NULL);
    g_signal_handlers_block_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_keypress_event), NULL);
    g_signal_handlers_block_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_press), NULL);
    g_signal_handlers_block_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_release), NULL);

    
  }
  rewind_audio();
  
  initialize_until_time();
  audio_play();
  playing = TRUE;
}

void stop_audio_playing(void) {
  playing = FALSE;
  if(annotating) {
    annotating = FALSE;
    g_signal_handlers_disconnect_by_func(G_OBJECT (Denemo.scorearea), record_timing, NULL);
    g_signal_handlers_unblock_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_keypress_event), NULL);
    g_signal_handlers_unblock_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_press), NULL);
    g_signal_handlers_unblock_by_func(G_OBJECT (Denemo.scorearea), G_CALLBACK (scorearea_button_release), NULL);
  }
}
gboolean
open_source_audio_file(void) {
  gboolean ret = FALSE;
  GtkWidget *dialog = gtk_file_chooser_dialog_new ("Open Audio Source File",
				      NULL,
				      GTK_FILE_CHOOSER_ACTION_OPEN,
				      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
				      NULL);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
    char *filename;
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    ret = open_source_audio (filename);
    g_free (filename);
    }
  gtk_widget_destroy (dialog);
  return ret;
}
