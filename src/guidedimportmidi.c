/*importmidi.c
 * midi file import functions
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005 AJAnderson
 *
 * 	TODO
 *
 *  multi voice support
 *  lyrics 
 *  triplet support
 *
 */

#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <denemo/denemo.h>
#include "staffops.h"
#include "smf.h"
#include "view.h"
#include "utils.h"
#include "file.h"
#include "commandfuncs.h"
#include "processstaffname.h"


static smf_t *smf = NULL;
static current_track = 0;

#define TEXT			0x01
#define COPYRIGHT		0X02
#define META_TRACK_NAME         0x03
#define META_INSTR_NAME		0x04

#define META_TEMPO              0x51
#define META_TIMESIG            0x58
#define META_KEYSIG             0x59
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define AFTERTOUCH		0xA0
#define CTRL_CHANGE             0xB0
#define PGM_CHANGE              0xC0
#define CHNL_PRESSURE		0xD0
#define PCH_WHEEL		0xE0
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define META_EVENT              0xFF





/**
 * Insert time signature into current staff 
 *
 */
static void
dotimesig (gint numerator, gint denominator)
{
  DenemoGUI *gui = Denemo.gui;
  /*only does initial TS */
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;

  curstaffstruct->timesig.time1 = numerator;
  curstaffstruct->timesig.time2 = denominator;
}

/**
 * Insert key signature into the current staff
 *
 */
static void
dokeysig (gint isminor, gint key)
{
  DenemoGUI *gui = Denemo.gui;
  if (key > 7)
    key = key - 256;            /*get flat key num, see keysigdialog.cpp */
  g_debug ("\nkey = %d\n", key);
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  curstaffstruct->keysig.number = key;
  curstaffstruct->keysig.isminor = isminor;
  dnm_setinitialkeysig (curstaffstruct, key, isminor);
}

static void
dotempo (gint tempo)
{
  DenemoGUI *gui = Denemo.gui;
  gui->si->tempo = (gint) (6.0e7 / (double) tempo); //FIXME insert as change of tempo instead
  g_warning("Changed si->tempo to %d\n", gui->si->tempo);
}

static void
dotrackname (gchar * name)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  if (name)
    g_string_assign (curstaffstruct->denemo_name, name);
}

static void
doinstrname (gchar * name)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  if (name)
    g_string_assign (curstaffstruct->midi_instrument, name);
}




static smf_t *
cmd_load (gchar * file_name)
{
  smf_t *smf;

  smf = smf_load (file_name);
  if (smf == NULL)
    {
      g_critical ("Couldn't load '%s'.", file_name);
      return NULL;
    }
  g_message ("File '%s' loaded.", file_name);
  g_message ("%s.", smf_decode (smf));

  return smf;
}

#define BUFFER_SIZE 1024

static void
decode_metadata (const smf_event_t * event)
{
  switch (event->midi_buffer[1])
    {
    case META_TRACK_NAME:
      //return smf_event_decode_textual(event, "Sequence/Track Name");
      dotrackname (smf_event_extract_text (event));

    case META_INSTR_NAME:
      //printf("\nInstrument text = %s\n", smf_string_from_event(event));
      doinstrname (smf_event_extract_text (event));

    default:
      break;
    }
}



/**
 * Process note off command
 */
static void
donoteoff (const smf_event_t * event)
{

}

/**
 * Process note on command 
 */
static void
donoteon (const smf_event_t * event)
{
	DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
	note->timing = event->time_seconds * Denemo.gui->si->recording->samplerate;
	notenum2enharmonic (noteon_key(event), &(note->mid_c_offset), &(note->enshift), &(note->octave));
	note->event = event;
	Denemo.gui->si->recording->notes = g_list_append (Denemo.gui->si->recording->notes, note);
}



static void
decode_midi_event (const smf_event_t * event)
{
  gint channel;
  gchar note[5];

  /* + 1, because user-visible channels used to be in range <1-16>. */
  channel = (event->midi_buffer[0] & 0x0F) + 1;

  switch (event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)
    {
    case NOTE_OFF:
     
      donoteoff (event);
      break;

    case NOTE_ON:
      if (event->midi_buffer[2])
        donoteon (event);
      else
        donoteoff (event);
      break;
      
    default:
      break;
    }
}



static void
event_for_staff (smf_event_t * event)
{
  if (smf_event_is_metadata (event))
    decode_metadata (event);
  else
    decode_midi_event (event);

}

static gint
create_staff (gint track)
{
  smf_event_t *event;
  smf_track_t *selected_track;
  smf_rewind (smf);
  selected_track = smf_get_track_by_number (smf, track);
  while ((event = smf_track_get_next_event (selected_track)) && event)
    event_for_staff (event);
  return 1;
}

/* create the smf for the recorded track if it does not already belong to it
 * this happens when the user records MIDI using a MIDI controller 
 * otherwise add it to smf, or if already present in smf (user_pointer points to smf) re-attach so that smf can be used. */
static void ensure_smf (void) {
	
	if (Denemo.gui->si->recorded_midi_track) 
	{	smf_track_t *track = Denemo.gui->si->recorded_midi_track;
		if(smf==NULL)
		{
			smf = smf_new ();
			smf_add_track (smf, track);
			
		}
		else
		{
			if(track->user_pointer != smf)
			  {
				track->smf = NULL;
				smf_add_track (smf, track);
				track->user_pointer = smf;	
			  }
			else
			  track->smf = smf;
		}
	}
}
static void guess_note_length (gdouble quarternotes, gint *dur, gint *dot)
{
	gint ticks = (gint)(384*quarternotes+0.5);
	if(ticks<7) {*dur = 8;*dot = 0; return;}
	if(ticks<10) {*dur = 8;*dot = 1; return;}
	if(ticks<13) {*dur = 7;*dot = 0; return;}
	if(ticks<19) {*dur = 7;*dot = 1; return;}
	if(ticks<25) {*dur = 6;*dot = 0; return;}
	if(ticks<37) {*dur = 6;*dot = 1; return;}
	if(ticks<49) {*dur = 5;*dot = 0; return;}
	if(ticks<73) {*dur = 5;*dot = 1; return;}
	if(ticks<97) {*dur = 4;*dot = 0; return;}
	if(ticks<145) {*dur = 4;*dot = 1; return;}
	if(ticks<193) {*dur = 3;*dot = 0; return;}
	if(ticks<289) {*dur = 3;*dot = 1; return;}
	if(ticks<385) {*dur = 2;*dot = 0; return;}
	if(ticks<577) {*dur = 2;*dot = 1; return;}
	if(ticks<769) {*dur = 1;*dot = 0; return;}
	if(ticks<1153) {*dur = 1;*dot = 1; return;}
	if(ticks<1537) {*dur = 0;*dot = 0; return;}
	if(ticks<2205) {*dur = 0;*dot = 1; return;}
    *dur = *dot = 0;
}
static void compute_midi_note_durations (void)
{
DenemoRecording *recording = Denemo.gui->si->recording;
	if (recording)
	{
		
		GList *g;
		ensure_smf ();
		smf_rewind (smf);
		for(g = recording->notes;g;g=g->next)
		{
			DenemoRecordedNote *note = g->data;
			smf_event_t *event = note->event;
			if( event && (0 == smf_seek_to_event (smf, event)))
				{
					smf_event_t *next;
					while ((next = smf_get_next_event (smf)))
					  {
						if (((next->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_OFF) && (next->midi_buffer[1] == event->midi_buffer[1]))
						{
							smf_tempo_t *tempo = smf_get_tempo_by_seconds (smf, event->time_seconds);
							double spqn = (tempo? tempo->microseconds_per_quarter_note/1000000.0: 60.0/Denemo.gui->si->tempo);
							guess_note_length((next->time_seconds - event->time_seconds)/spqn, &note->duration, &note->dots);
							//g_print("spqn %f dur %f %d %d\n", spqn, (next->time_seconds - event->time_seconds), note->duration, note->dots);
							break;
						}
					  }
					 smf_rewind (smf);	
					
				}
		}
	}
}

static gint
readtrack (gint track)
{
  gint ret = 0;
  smf_track_t *selected_track;
  
  //double track_duration = smf_get_length_seconds (smf);
  
  smf_rewind (smf);
  if (track > 0 && track <= smf->number_of_tracks)
    {
      selected_track = smf_get_track_by_number (smf, track);
      new_midi_recording ();
      create_staff (track);
      //re-attach the current Denemo.gui->si->recorded_midi_track to smf or delete it if it is not in smf
      if(Denemo.gui->si->recorded_midi_track)
      {
		 if(((smf_track_t *)Denemo.gui->si->recorded_midi_track)->user_pointer != smf)
			smf_track_delete(Denemo.gui->si->recorded_midi_track);		  
		 else
			((smf_track_t *)Denemo.gui->si->recorded_midi_track)->smf = smf;
	  }
	  selected_track->user_pointer = smf;
      Denemo.gui->si->recorded_midi_track = selected_track;
      compute_midi_note_durations (); //fills Denemo.gui->si->recording->notes with the note durations
      ((smf_track_t *)Denemo.gui->si->recorded_midi_track)->smf = NULL; // we detach this track from smf, so it can be attached to the playback smf; we cannot use smf while this is done, as it thinks it still owns the track.	  
	current_track = track;
    }
   else
	ret = -1;
  return ret;
}




gint get_imported_midi_track (gint track) {
	ensure_smf ();
	if(smf)
		return readtrack(track);
	else
		return -1;
}
gint get_imported_midi_tracks (void) {
	ensure_smf ();
	if(smf)
		return smf->number_of_tracks;
	else
		return -1;
}
gint get_current_midi_track (void) {
		return current_track;
}


smf_tempo_t *get_recorded_midi_tempo (gint index)
{
	ensure_smf ();
	if(smf && index>=0)
		return smf_get_tempo_by_number(smf, index);
	else return NULL;
}




double
my_smf_get_length_seconds(const smf_t *smf)
{
	int i;
	double seconds = 0.0;

	for (i = 1; i <= smf->number_of_tracks; i++) {
		smf_track_t *track;
		smf_event_t *event;

	       	track = smf_get_track_by_number(smf, i);
		assert(track);

		event = smf_track_get_last_event(track);
		/* Empty track? */
		if (event == NULL)
			continue;
//g_print("my seconds %f\n", event->time_seconds );
		if (event->time_seconds > seconds)
			seconds = event->time_seconds;
	}

	return (seconds);
}

gdouble get_recorded_midi_duration (void)
{
	ensure_smf ();
	if(smf) {
#if 0
		double val1, val2;
		val1 = smf_get_length_seconds (smf);
		val2 = my_smf_get_length_seconds (smf);
		if((int)val1 != (int)val2)
			g_critical ("Call to smf_get_length_seconds has yielded bad value: %f should be %f\n", val1, val2);
		return val2;
#else
		//return smf_get_length_seconds (smf);
		g_print("my value %f\n", my_smf_get_length_seconds (smf));
		double val = smf_get_length_seconds (smf);
		//g_print("smf val %f\n", val);
		return val;
#endif
	}
	else 
	return 0.0;
}


gint
guidedImportMidi (gchar * filename)
{
  if (smf) 
	{ 
	gint track=1;
	for (track =1; track <= smf->number_of_tracks; track++)
			smf_get_track_by_number (smf, track)->smf = smf;
	smf_delete (smf);
	smf = NULL;
	}
  Denemo.gui->si->recorded_midi_track = NULL;
 
  /* load the file */
  smf = cmd_load (filename);
  if (!smf)
    return -1;

  return 0;
}
