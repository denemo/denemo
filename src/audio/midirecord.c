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
#include "command/lilydirectives.h"


static gboolean playing_recorded_midi = FALSE;
static gint recording_time;
static GtkWidget *MidiRecordButton;//the button in the MIDI input panel that starts a MIDI recording.
static void free_one_recorded_note (DenemoRecordedNote *n)
	{
		g_free (n->midi_event);
		g_free (n);
		if (Denemo.project->movement->recording && Denemo.project->movement->recording->sync && (Denemo.project->movement->recording->sync->data == n))
			Denemo.project->movement->recording->sync = NULL;
	}
static gdouble get_recording_start_time (void)
	{
		DenemoMovement *si = Denemo.project->movement;
		if (si->smfsync != si->changecount)
			si->end_time = -1.0, exportmidi (NULL, si);
		return get_time_at_cursor ();
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
      Denemo.project->movement->smfsync = G_MAXINT;
     
     if (Denemo.project->midi_recording && (Denemo.project->midi_destination & MIDIRECORD))
		toggle_midi_record ();
    }
}

void new_midi_recording (void) {
  DenemoMovement *si = Denemo.project->movement;
  DenemoRecording *recording;
  if(Denemo.project->movement->recording && (Denemo.project->movement->recording->type==DENEMO_RECORDING_MIDI))
		delete_recording ();
  recording = (DenemoRecording *) g_malloc0 (sizeof (DenemoRecording));
  recording->type = DENEMO_RECORDING_MIDI;
  recording->samplerate = 44100;
  recording_time = -1;//unset
  Denemo.project->midi_recording = TRUE;
  Denemo.project->movement->recording = recording;
  //~ if (si->smfsync != si->changecount)
			//~ exportmidi (NULL, si); //si->end_time is now the time in seconds of the current score, if the recording extends past this time create more clicks
  //si->end_time = -1.0, exportmidi (NULL, si);		
}


void delete_last_recorded_note (void)
{
	DenemoMovement *si = Denemo.project->movement;
	GList *g = g_list_last (si->recording->notes);
	DenemoRecordedNote *note = g->data;
	gint start_of_deleted_note;

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
							if (si->recording->marked_onset == g) 
								si->recording->marked_onset = NULL;	
							si->recording->notes = g_list_remove_link (si->recording->notes, g);
							free_one_recorded_note (note);
							g_list_free (g);
							
							break;
						}
				}
		}
	if (si->recording->notes == NULL)
		{
			si->recording->marked_onset = NULL;
			g_free (si->recording);
			si->recording = NULL;
			Denemo.project->midi_recording = FALSE;
			Denemo.project->midi_destination ^= MIDIRECORD;
			toggle_midi_record ();//turn recording back on 
		}
	else
		{
			g = g_list_last (si->recording->notes);
			//this is the NOTEOFF event
			gint end_of_recording = ((DenemoRecordedNote*)g->data)->timing;
			gdouble gap = (start_of_deleted_note - end_of_recording)/(double)si->recording->samplerate;
			//g_print ("Gap between last notes was %f\n", gap);
			if (gap > 0.4)
				gap = 0.2;
				//g_print ("Note off was at %f\t", ((DenemoRecordedNote*)si->marked_onset->data)->timing/(double)si->recording->samplerate);
			((DenemoRecordedNote*)g->data)->timing += (gint)(gap*si->recording->samplerate);
		}
}   
gboolean midi_track_present (void)
{
	DenemoStaff *topstaff = (DenemoStaff*) Denemo.project->movement->thescore->data;
	return find_directive (topstaff->clef.directives, DENEMO_CLICK_TRACK_NAME) != NULL;
} 
gdouble measure_duration (void)
{
	DenemoStaff *topstaff = (DenemoStaff*) Denemo.project->movement->thescore->data;
	DenemoMeasure *themeasure = (DenemoMeasure*)topstaff->themeasures->data,
					*nextmeasure = topstaff->themeasures->next? (DenemoMeasure*)(topstaff->themeasures->next)->data : NULL;
	if (themeasure && nextmeasure)
		return nextmeasure->earliest_time - themeasure->earliest_time ;
	else return -1.0;
}
//Add the passed midi event to a recording in Denemo.project->movement
void record_midi (gchar * buf)
{
	static gdouble current_time;
	DenemoMovement *si = Denemo.project->movement;
	DenemoRecordedNote *note = NULL;
	gboolean initial = FALSE;
	static gdouble old_time = 0;
	static gchar last_0;//last midi event type
	static gchar last_1;//last midi key
	gboolean resumed = (si->recording && si->recording->notes && (si->recording->marked_onset==NULL));
	gdouble new_time = get_time ();
	if ((last_0 == buf[0]) && (last_1 == buf[1]) && (new_time - old_time < 0.01))
		return;//key bounce - ignore. This happens with mouse driven virtual keyboard
	last_0 = buf[0];
	last_1 = buf[1];
	if (resumed && si->currentobject && (!si->cursor_appending) && !si->currentobject->next) //when resuming move right to appending if possible
		movecursorright (NULL, NULL);

	if ((recording_time != -1) && ((new_time - old_time) > Denemo.prefs.recording_timeout/1000.0))
		recording_time = -1;//force resume if we have been too long away
	old_time = new_time;
	if(Denemo.project->midi_recording && Denemo.project->movement->recording && (((buf[0]&0xF0)==MIDI_NOTE_ON) || (buf[0]&0xF0)==MIDI_NOTE_OFF))
			{
				note = g_malloc0(sizeof(DenemoRecordedNote));
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
								start = get_recording_start_time ();// time at cursor, after sync-ing the smf
							 }
						current_time = new_time - start;
						si->smfsync = G_MAXINT;
						//si->marked_onset = g_list_last (Denemo.project->movement->recording->notes);
					}
				si->recording->click_track_created = TRUE;//this should really only be set once, after the click track is created, but that is done in scheme which doesn't have access to set it. Would need to create a callback to do that.					
				if ((new_time - current_time) > si->rightmost_time)
						page_viewport();

				if ((new_time - current_time) > si->end_time)
						{
						si->smfsync = G_MAXINT;
						si->end_time = -1;
						exportmidi (NULL, si);
						if ((new_time - current_time) > si->end_time)
							{
								call_out_to_guile ("(d-AppendMeasureAllStaffs)(d-MoveToEnd)");
								si->end_time = -1;
								exportmidi (NULL, si);//g_print ("appended - going to end %0.2f %0.2f\n", si->end_time, (new_time - current_time));
							}
						}
				recording_time = (new_time - current_time) * si->recording->samplerate;
				note->timing = recording_time;
				//g_print ("Storing NOTE%s at %f leadin %f\n", ((buf[0]&0xF0)==MIDI_NOTE_ON)?"ON":"OFF", recording_time/(double)si->recording->samplerate, si->recording->leadin/(double)si->recording->samplerate);
				notenum2enharmonic (buf[1], &(note->mid_c_offset), &(note->enshift), &(note->octave));
				si->recording->notes = g_list_append (si->recording->notes, note);
				if (initial) si->recording->marked_onset = si->recording->notes;
			}
	if (resumed) 
		{
			si->recording->marked_onset = g_list_last (si->recording->notes);
			//call_out_to_guile ("(d-ExtendClickTrack)");
			//synchronize_recording ();
		}
}


static gboolean play_from (GList *notenode)
{
	DenemoMovement *si = Denemo.project->movement;
	pause_recording_midi ();
	if (!si->recording)
		return FALSE;//no more
	if (notenode==NULL)
		return FALSE;
	DenemoRecordedNote* note = (DenemoRecordedNote*)notenode->data;
	DenemoRecordedNote *nextnote = notenode->next?(DenemoRecordedNote *)notenode->next->data:NULL;
	DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;
	gdouble rate = (double)si->recording->samplerate;
	gdouble this_time = ((DenemoRecordedNote*)notenode->data)->timing/rate;
	gchar *buffer = note->midi_event;
	if (playing_recorded_midi)
		{	
			buffer[0] = (buffer[0]&0xF0) | curstaffstruct->midi_channel;
			buffer[2] = 127;//Full volume
			play_midi_event (DEFAULT_BACKEND, curstaffstruct->midi_port, buffer);
			if (nextnote)
				{
					gdouble next_play_time = (nextnote->timing/rate - this_time)* 1000;
					g_timeout_add (next_play_time, (GSourceFunc)play_from, notenode->next);
				}
			else 
				playing_recorded_midi = FALSE;
		}
	else 
		{
			if ((buffer[0]&0xF0)==MIDI_NOTE_OFF)
				{
					buffer[0] = (buffer[0]&0xF0) | curstaffstruct->midi_channel;
					play_midi_event (DEFAULT_BACKEND, curstaffstruct->midi_port, buffer);
					if (nextnote)
						{
							gdouble next_play_time = (nextnote->timing/rate - this_time)* 1000;
							if ((nextnote->midi_event[0]&0xF0)==MIDI_NOTE_OFF)
								g_timeout_add (next_play_time, (GSourceFunc)play_from, notenode->next);//it would be safer to do all notes off...
						}
				}
		}
	return FALSE; //do not call again
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
	play_from (notenode);
	return FALSE;
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
	if (si->recording->marked_onset_position)
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
			play_recorded_notes (si->recording->marked_onset? si->recording->marked_onset : si->recording->notes);
		}
}
void pause_recording_midi (void)
{
	recording_time = -1;	
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
	set_midi_capture (FALSE); //cannot combine recording with MIDI capture
	if (!Denemo.project->movement->recording)
		new_midi_recording ();

	Denemo.project->midi_destination |= MIDIRECORD;
	Denemo.project->midi_recording = TRUE;
	set_midi_in_status ();
	switch_back_to_main_window ();
	return TRUE;
}
gboolean marked_midi_note_is_noteoff (void)
{
	DenemoMovement *si = Denemo.project->movement;
	if (!si->recording) return FALSE;
	if (si->recording->marked_onset)
		{
			DenemoRecordedNote *note = si->recording->marked_onset->data;
			return ((note->midi_event[0]&0xF0)) == MIDI_NOTE_OFF;
		}
	return FALSE;
}

void advance_marked_midi (gint steps)
{
	DenemoMovement *si = Denemo.project->movement;
	if (steps==0) return;
	if (!si->recording) return;
	if (steps>0)
			while (steps-- && si->recording->marked_onset)
				{ 
					si->recording->marked_onset = si->recording->marked_onset->next;
					while (si->recording->marked_onset && marked_midi_note_is_noteoff ())
						si->recording->marked_onset = si->recording->marked_onset->next;
				}
	else
		while (steps++ && si->recording->marked_onset)
			{
				si->recording->marked_onset = si->recording->marked_onset->prev;
				while (si->recording->marked_onset && marked_midi_note_is_noteoff ())
						si->recording->marked_onset = si->recording->marked_onset->prev;
			}
}


static void sync_leadin (void)
{

	gdouble newoffset = get_recording_start_time ();//draw.c compares the ->timing value with the time at the note it is drawing, 
	//we set leadin which draw.c uses when MIDI recording present
	
	if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes)
		{
			GList *g = Denemo.project->movement->recording->notes;
			gdouble offset = ((DenemoRecordedNote *)g->data)->timing/(double)Denemo.project->movement->recording->samplerate;
			if (Denemo.project->movement->recording->marked_onset)
				offset = ((DenemoRecordedNote *)Denemo.project->movement->recording->marked_onset->data)->timing/(double)Denemo.project->movement->recording->samplerate;
			//g_print ("current offset %f new time of start %f\n", offset, newoffset);
			Denemo.project->movement->recording->leadin = -(newoffset - offset)*Denemo.project->movement->recording->samplerate;
		}	
}

void synchronize_recording (void)
{
	gdouble measure_duration;
	GList *curmeas = Denemo.project->movement->currentmeasure;
	Denemo.project->movement->end_time = -1;
	exportmidi (NULL, Denemo.project->movement);
	if (curmeas->prev)
		measure_duration = ((DenemoMeasure*)curmeas->data)->earliest_time
												- ((DenemoMeasure*)curmeas->prev->data)->earliest_time;
	else if (curmeas->next)
		measure_duration = ((DenemoMeasure*)curmeas->next->data)->earliest_time - ((DenemoMeasure*)curmeas->data)->earliest_time;
	else
		return;

	if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes && Denemo.project->movement->recording->marked_onset)
		{
			GList *g = Denemo.project->movement->recording->marked_onset;
			if ((Denemo.project->movement->recording->sync == NULL))
					{
						Denemo.project->movement->recording->sync = g;
						sync_leadin ();
					}
			else
				{
					//g_print ("Measure duration is %0.2f\n", measure_duration);
					gdouble this = ((DenemoRecordedNote *)Denemo.project->movement->recording->marked_onset->data)->timing/(double)Denemo.project->movement->recording->samplerate;
					gdouble that = ((DenemoRecordedNote *)Denemo.project->movement->recording->sync->data)->timing/(double)Denemo.project->movement->recording->samplerate;
					gdouble factor = (this-that)/measure_duration; //recorded midi notes from here on need their timings relative to the time of sync note scaled by factor
					//g_print ("Factor %0.2f\tthis %0.2f that %0.2f\n", factor, this, that);
					g = Denemo.project->movement->recording->sync;
					gint leadin = ((DenemoRecordedNote *)g->data)->timing;
					Denemo.project->movement->recording->sync = Denemo.project->movement->recording->marked_onset;
					if (factor>0.1 && factor < 10)
						for (g=g->next;g;g=g->next)
							{
								DenemoRecordedNote *note = (DenemoRecordedNote *)g->data;
								//g_print ("Note %0.2f\t", note->timing/44100.0);
								note->timing = leadin + (gint)((gdouble)(note->timing-leadin)/factor);
								//g_print (" to Note %0.2f\n", note->timing/44100.0);
								
								if ((g->next == NULL) && (note->timing)/(double)Denemo.project->movement->recording->samplerate > Denemo.project->movement->end_time)
									{
										call_out_to_guile ("(d-AppendMeasureAllStaffs)");
										Denemo.project->movement->end_time = -1;
										exportmidi (NULL, Denemo.project->movement);
									}
							}
					else
						sync_leadin (), g_warning ("Change of tempo is out of range");
				}
		}

//g_print ("Timing of the marked onset (new sync) is %0.2f\n", ((DenemoRecordedNote *)Denemo.project->movement->marked_onset->data)->timing/44100.0);

}


void scale_recording (gdouble scale)// keeps Denemo.project->movement->marked_onset->timing constant while scaling values before and after
	{
	double rate = (double)Denemo.project->movement->recording->samplerate;
	if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes)
		{
			GList *g = Denemo.project->movement->recording->notes;
			gdouble fixed = rate * ((DenemoRecordedNote *)g->data)->timing;
			if (Denemo.project->movement->recording->marked_onset)
				fixed = rate * ((DenemoRecordedNote *)Denemo.project->movement->recording->marked_onset->data)->timing;
			for (;g;g=g->next)
						{
							DenemoRecordedNote *note = g->data;
							//g_print ("value %d becomes\t", note->timing);
							note->timing = (fixed + (rate * note->timing - fixed)*scale)/rate;
							//g_print ("value%d\n", note->timing);
							
						}					
			}
		}
