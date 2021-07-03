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
#include "core/view.h"
#include "core/utils.h"
#include <sndfile.h>



static gint recording_time;

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
    }
}

void new_midi_recording (void) {
  DenemoRecording *recording;

			
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
	g_print ("resuming\n");
}

void delete_last_recorded_note (void)
{
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
			g_print ("Gap between last notes was %f\n", gap);
			if (gap > 0.4)
				gap = 0.2;
				g_print ("Note off was at %f\t", ((DenemoRecordedNote*)si->marked_onset->data)->timing/(double)si->recording->samplerate);
			((DenemoRecordedNote*)si->marked_onset->data)->timing += (gint)(gap*si->recording->samplerate);
			g_print ("Note off now at %f\t", ((DenemoRecordedNote*)si->marked_onset->data)->timing/(double)si->recording->samplerate);

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
							 start = ((DenemoRecordedNote*)g_list_last (si->recording->notes)->data)->timing/(double)si->recording->samplerate;//should be the last noteoff time
							 g_print ("resuming at %f seconds\n", start);
							}
						else {
								initial = TRUE;
								start = get_recording_start_time ();
								si->recording->offset = start;
							 }
						current_time = get_time () - start;
						si->smfsync = G_MAXINT;
						si->marked_onset = g_list_last (Denemo.project->movement->recording->notes);
					}
				recording_time = (get_time () - current_time) * si->recording->samplerate;
				note->timing = recording_time;
				g_print ("Storing NOTE%s at %f\n", ((buf[0]&0xF0)==MIDI_NOTE_ON)?"ON":"OFF", recording_time/(double)si->recording->samplerate);
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
