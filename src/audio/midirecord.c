/*midirecord.c
 * record midi input
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2021 Richard Shann
 *
 * * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include <denemo/denemo.h>
#include "audio/midirecord.h"
#include "audio/pitchentry.h"
#include "audio/audiointerface.h"
#include "core/view.h"
#include "core/utils.h"
#include <sndfile.h>
#include "export/exportmidi.h"
#include "export/guidedimportmidi.h"


static gboolean playing_recorded_midi = FALSE;
static gint recording_time;
static GtkWidget *MidiRecordButton;//the button in the MIDI input panel that starts a MIDI recording.
static void free_one_recorded_note (DenemoRecordedNote *n)
	{
		g_free (n->midi_event);
		g_free (n);
	}

void delete_recording (void)
{
  //FIXME a better name for the mutex which originally was just for midi data, but will work for audio data too.
  if (Denemo.project->movement && Denemo.project->movement->recording)
    {
      DenemoRecording *temp = Denemo.project->movement->recording;
      Denemo.project->movement->recording = NULL;
      if (temp->sndfile)
        sf_close (temp->sndfile);
      g_free (temp->filename);
      g_list_free_full (temp->notes, (GDestroyNotify)free_one_recorded_note);
      g_free (temp);
      Denemo.project->movement->recording = NULL;
      Denemo.project->movement->marked_onset = NULL;
      Denemo.project->movement->smfsync = G_MAXINT;
     
     if (Denemo.project->midi_recording && (Denemo.project->midi_destination & MIDIRECORD))
		toggle_midi_record ();
    }
}

void new_midi_recording (void) {
  DenemoMovement *si = Denemo.project->movement;
  DenemoRecording *recording;

  DenemoStaff* topstaff = (DenemoStaff*) si->thescore->data;
  topstaff->space_above = MAX (topstaff->space_above, 50);//leave room for MIDI track above
  if(Denemo.project->movement->recording && (Denemo.project->movement->recording->type==DENEMO_RECORDING_MIDI))
		delete_recording ();
  recording = (DenemoRecording *) g_malloc (sizeof (DenemoRecording));
  Denemo.project->movement->marked_onset = NULL;
  recording->type = DENEMO_RECORDING_MIDI;
  recording->samplerate = 44100;
  recording_time = -1;//unset
  recording->offset = 0;
  Denemo.project->midi_recording = TRUE;
  Denemo.project->movement->recording = recording;

}
void resume_midi_recording (void) {
	DenemoMovement *si = Denemo.project->movement;
	si->smfsync = G_MAXINT;
	recording_time = -1;
    Denemo.project->midi_recording = TRUE;
    Denemo.project->midi_destination |= MIDIRECORD;
	//g_print ("resuming\n");
}

void delete_last_recorded_note (void)
{
	pause_recording_midi ();
	DenemoMovement *si = Denemo.project->movement;
	GList *g = g_list_last (si->recording->notes);
	DenemoRecordedNote *note = g->data;
	gint start_of_deleted_note;

	gboolean was_recording = Denemo.project->midi_recording;
	Denemo.project->midi_recording = FALSE;
    Denemo.project->midi_destination ^= MIDIRECORD;//turn off recording
      
	if ((note->midi_event[0]&0xF0)==MIDI_NOTE_ON)
		{
			start_of_deleted_note = note->timing;
			si->recording->notes = g_list_remove_link (si->recording->notes, g);
			free_one_recorded_note (note);
			g_free (g);	
		} else
		{
			gint midi = note->midi_event[1];
			GList* h = g->prev;
			si->recording->notes = g_list_remove_link (si->recording->notes, g);
			free_one_recorded_note (note);
			g_list_free (g);
	
			for (g=h; g ;g = g->prev)
				{
					DenemoRecordedNote *note = g->data;
					gint next = note->midi_event[1];
					if (g==h) 
						start_of_deleted_note = note->timing;
					if (midi == next)//the noteon for the deleted note
						{
							si->recording->notes = g_list_remove_link (si->recording->notes, g);
							free_one_recorded_note (note);
							g_list_free (g);	
							break;
						}
				}
		}
	if (si->recording->notes == NULL)
		{
			si->marked_onset = NULL;
			g_free (si->recording);
			si->recording = NULL;
		}
	else
		{
			Denemo.project->movement->marked_onset = g_list_last (si->recording->notes);
			//this is the NOTEOFF event
			gint end_of_recording = ((DenemoRecordedNote*)si->marked_onset->data)->timing;
			gdouble gap = (start_of_deleted_note - end_of_recording)/(double)si->recording->samplerate;
			//g_print ("Gap between last notes was %f\n", gap);
			if (gap > 0.4)
				gap = 0.2;
				//g_print ("Note off was at %f\t", ((DenemoRecordedNote*)si->marked_onset->data)->timing/(double)si->recording->samplerate);
			((DenemoRecordedNote*)si->marked_onset->data)->timing += (gint)(gap*si->recording->samplerate);
			//g_print ("Note off now at %f\t", ((DenemoRecordedNote*)si->marked_onset->data)->timing/(double)si->recording->samplerate);

			Denemo.project->movement->marked_onset = Denemo.project->movement->marked_onset->prev;//move to the NOTEON
		}
	if (was_recording)
		resume_midi_recording ();
	
}   
	
//Add the passed midi to a recording in Denemo.project->movement
void record_midi (gchar * buf)
{
	static gdouble current_time;
	DenemoMovement *si = Denemo.project->movement;
	gboolean initial = FALSE;
	static gdouble old_time = 0;
	gdouble new_time = get_time ();
	if ((recording_time != -1) && ((new_time - old_time) > Denemo.prefs.recording_timeout/1000.0))
		recording_time = -1;//force resume if we have been too long away
	old_time = new_time;
	if(Denemo.project->midi_recording && Denemo.project->movement->recording && (((buf[0]&0xF0)==MIDI_NOTE_ON) || (buf[0]&0xF0)==MIDI_NOTE_OFF))
			{
				DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
				note->midi_event = g_malloc0(3);
				memcpy (note->midi_event, buf, 3);
				if (recording_time == -1)
					{
						gdouble start;
						if (si->recording->notes) //resume
							{
							 start = 0.2 + ((DenemoRecordedNote*)g_list_last (si->recording->notes)->data)->timing/(double)si->recording->samplerate;//should be the last noteoff time
							 //g_print ("resuming at %f seconds\n", start);
							}
						else {
								initial = TRUE;
								start = get_recording_start_time ();
								si->recording->offset = start;
							 }
						current_time = new_time - start;
						si->smfsync = G_MAXINT;
						//si->marked_onset = g_list_last (Denemo.project->movement->recording->notes);
					}
				recording_time = (new_time - current_time) * si->recording->samplerate;
				note->timing = recording_time;
				//g_print ("Storing NOTE%s at %f\n", ((buf[0]&0xF0)==MIDI_NOTE_ON)?"ON":"OFF", recording_time/(double)si->recording->samplerate);
				notenum2enharmonic (buf[1], &(note->mid_c_offset), &(note->enshift), &(note->octave));
				si->recording->notes = g_list_append (si->recording->notes, note);
				if (initial) si->marked_onset = si->recording->notes;
			}
}

gdouble get_recording_start_time (void)
	{
		DenemoMovement *si = Denemo.project->movement;
		if (si->smfsync != si->changecount)
			exportmidi (NULL, si);
		return get_time_at_cursor ();
	}	
	
static gboolean play_buffer (gchar *buffer)
{
	if (playing_recorded_midi)
		{
			DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
			play_midi_event (DEFAULT_BACKEND, curstaffstruct->midi_port, buffer);
		}
return FALSE;
}
static void end_play (void)
{
	playing_recorded_midi = FALSE;// so if someone tries to toggle back on after it has finished playing it will re-start
}
static gboolean play_recorded_notes (GList *notenode)
{
	DenemoMovement *si = Denemo.project->movement;
	pause_recording_midi ();
	if (!si->recording)
		return FALSE;//no more
	if (notenode==NULL)
		return FALSE;
	if (!playing_recorded_midi)
		return FALSE;
	DenemoRecordedNote *note = (DenemoRecordedNote *)notenode->data;
	gdouble rate = si->recording->samplerate;
	gdouble start = note->timing/rate;
	gint last_off;
	while (notenode)
		{
			note = (DenemoRecordedNote *)notenode->data;
			DenemoRecordedNote *nextnote = notenode->next?(DenemoRecordedNote *)notenode->next->data:NULL;
			DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;
			gchar *buffer = note->midi_event;
			buffer[0] = (buffer[0]&0xF0) | curstaffstruct->midi_channel;
			buffer[2] = 127;//Full volume
			last_off = (note->timing/rate - start)* 1000;
			g_timeout_add (last_off, (GSourceFunc)play_buffer, buffer);
			//g_print ("schedule %x note %d at %.2f\n", (char)buffer[0], buffer[1], note->timing/rate - start);
			notenode = notenode->next;
		}
	g_timeout_add (last_off, (GSourceFunc)end_play, NULL);	
	return FALSE;
}

static void midi_recording_help (void)
{
	
}  
static void mark_recorded_note (gint position)
{
	pause_recording_midi ();
	Denemo.project->movement->marked_onset_position = position;
    gtk_widget_queue_draw(Denemo.scorearea);//sets marked_onset to match marked_onset_position during re-draw
}   
  
    
void popup_recording_menu (gint position)
{
	pause_recording_midi ();
	//Chord Recognition interval (number of ms to count as chord not note)
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Help"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (midi_recording_help), NULL);
  
  if (Denemo.project->midi_recording && (Denemo.project->midi_destination & MIDIRECORD))
	{
		item = gtk_menu_item_new_with_label (_("Stop/Pause Recording Notes"));
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
		g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_midi_record), NULL);	
	}
	
  item = gtk_menu_item_new_with_label (_("Resume Recording Notes"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (resume_midi_recording), NULL);
  
  item = gtk_menu_item_new_with_label (_("Mark this Recorded Note"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (mark_recorded_note), GINT_TO_POINTER (position));

  item = gtk_menu_item_new_with_label (_("Sync Marked Recorded Note to Denemo Cursor Note"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (synchronize_recording), NULL);

  //if (!(Denemo.project->midi_recording && (Denemo.project->midi_destination & MIDIRECORD)))
	{
	  item = gtk_menu_item_new_with_label (_("Delete Last Recorded Note"));
	  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
	  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (delete_last_recorded_note), NULL);
	}
  item = gtk_menu_item_new_with_label (_("Delete MIDI Recording"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (pb_midi_delete), NULL);
    
  
  
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
	
}

static gboolean start_recorded_midi_play (void)
{
	play_recorded_midi ();
	return FALSE;	
}
void play_recorded_midi (void)
{
	pause_recording_midi ();
	DenemoMovement *si = Denemo.project->movement;
	if (si->marked_onset_position)
		{
		gtk_widget_queue_draw (Denemo.scorearea);
		g_timeout_add (100, (GSourceFunc) start_recorded_midi_play, NULL);
		return;
		}
	if (playing_recorded_midi)
		playing_recorded_midi = FALSE;
	else
		{
		playing_recorded_midi = TRUE;
		if (si->recording)
			play_recorded_notes (si->marked_onset? si->marked_onset : si->recording->notes);
		}
}
void pause_recording_midi (void)
{
	recording_time = -1;	
}

void declare_record_button (GtkWidget *midirecordbutton)
	{
		MidiRecordButton = midirecordbutton;
	}
	
//callback for record button, starts/stops MIDI recording
gboolean toggle_midi_record (void)
{
	if (Denemo.project->midi_recording)
	{
		Denemo.project->midi_recording = FALSE;
	}
	if ((Denemo.project->midi_destination & MIDIRECORD))
	{
	  Denemo.project->midi_destination ^= MIDIRECORD;
	  set_midi_in_status ();
	  return TRUE;
	}

	if (Denemo.project->movement->recording && (Denemo.project->movement->recording->type == DENEMO_RECORDING_AUDIO))
	{
	  warningdialog (_("Cannot mix audio and MIDI recordings"));
	  return FALSE;
	}

	if (Denemo.project->movement->recorded_midi_track && midi_is_from_file ())
	{
	  warningdialog (_("Cannot mix MIDI recordings with imported MIDI - delete imported MIDI first"));
	  return FALSE;
	}

	if (!Denemo.project->movement->recording)
		new_midi_recording ();
	else
		resume_midi_recording ();
	Denemo.project->midi_destination |= MIDIRECORD;

	set_midi_in_status ();
	switch_back_to_main_window ();
	return TRUE;
}
gboolean marked_midi_note_is_noteoff (void)
{
	DenemoMovement *si = Denemo.project->movement;
	if (si->marked_onset)
		{
			DenemoRecordedNote *note = si->marked_onset->data;
			return ((note->midi_event[0]&0xF0)) == MIDI_NOTE_OFF;
		}
	return FALSE;
}

void advance_marked_midi (gint steps)
{
	DenemoMovement *si = Denemo.project->movement;
	if (steps==0) return;
	if (steps>0)
			while (steps-- && si->marked_onset)
				{ 
					si->marked_onset = si->marked_onset->next;
					while (si->marked_onset && marked_midi_note_is_noteoff ())
						si->marked_onset = si->marked_onset->next;
				}
	else
		while (steps++ && si->marked_onset)
			{
				si->marked_onset = si->marked_onset->prev;
				while (si->marked_onset && marked_midi_note_is_noteoff ())
						si->marked_onset = si->marked_onset->prev;
			}
}

void synchronize_recording (void)
{
	Denemo.project->movement->smfsync = G_MAXINT;
	generate_midi ();
	gdouble newoffset = get_time_at_cursor ();
	
	if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes)
		{
			GList *g = Denemo.project->movement->recording->notes;
			gdouble offset = ((DenemoRecordedNote *)g->data)->timing/(double)Denemo.project->movement->recording->samplerate;
			if (Denemo.project->movement->marked_onset)
				offset = ((DenemoRecordedNote *)Denemo.project->movement->marked_onset->data)->timing/(double)Denemo.project->movement->recording->samplerate;
			g_print ("current offset %f new time of start %f\n", offset, newoffset);
			for (;g;g=g->next)
				{
					DenemoRecordedNote *note = g->data;
					//g_print ("before %f\t", note->timing/(double)Denemo.project->movement->recording->samplerate);
					note->timing += (newoffset - offset)*Denemo.project->movement->recording->samplerate;
					//g_print ("after %f\n", note->timing/(double)Denemo.project->movement->recording->samplerate);
				}
			Denemo.project->movement->recording->offset = newoffset;
		}
	Denemo.project->movement->smfsync = G_MAXINT;
	generate_midi ();		
}

void scale_recording (gdouble scale)// keeps Denemo.project->movement->marked_onset->timing constant while scaling values before and after
	{
	double rate = (double)Denemo.project->movement->recording->samplerate;
	if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes)
		{
			GList *g = Denemo.project->movement->recording->notes;
			gdouble fixed = rate * ((DenemoRecordedNote *)g->data)->timing;
			if (Denemo.project->movement->marked_onset)
				fixed = rate * ((DenemoRecordedNote *)Denemo.project->movement->marked_onset->data)->timing;
			for (;g;g=g->next)
						{
							DenemoRecordedNote *note = g->data;
							//g_print ("value %d becomes\t", note->timing);
							note->timing = (fixed + (rate * note->timing - fixed)*scale)/rate;
							//g_print ("value%d\n", note->timing);
							
						}					
			}
		}
