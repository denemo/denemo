/*importmidi.c
 * midi file import functions
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005 AJAnderson
 *
 *  TODO
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
#include "command/staff.h"
#include "smf.h"
#include "core/view.h"
#include "core/utils.h"
#include "export/file.h"
#include "command/commandfuncs.h"
#include "command/processstaffname.h"
#include "export/exportmidi.h"
#include "audio/pitchentry.h"
#include "audio/midirecord.h"


static smf_t *smf = NULL;
static gint current_track = 0;
static gboolean smf_from_file = FALSE;

#define TEXT            0x01
#define COPYRIGHT       0X02
#define META_TRACK_NAME         0x03
#define META_INSTR_NAME     0x04

#define META_TEMPO              0x51
#define META_TIMESIG            0x58
#define META_KEYSIG             0x59
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define AFTERTOUCH      0xA0
#define CTRL_CHANGE             0xB0
#define PGM_CHANGE              0xC0
#define CHNL_PRESSURE       0xD0
#define PCH_WHEEL       0xE0
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define META_EVENT              0xFF





/**
 * Insert time signature into current staff
 *
 */
static void
dotimesig (gint numerator, gint denominator)
{
  DenemoProject *gui = Denemo.project;
  /*only does initial TS */
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;

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
  DenemoProject *gui = Denemo.project;
  if (key > 7)
    key = key - 256;            /*get flat key num, see keysigdialog.cpp */
  g_debug ("\nkey = %d\n", key);
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  curstaffstruct->keysig.number = key;
  curstaffstruct->keysig.isminor = isminor;
  dnm_setinitialkeysig (curstaffstruct, key, isminor);
}

static void
dotempo (gint tempo)
{
  DenemoProject *gui = Denemo.project;
  gui->movement->tempo = (gint) (6.0e7 / (double) tempo); //FIXME insert as change of tempo instead
  g_warning("Changed si->tempo to %d", gui->movement->tempo);
}

static void
dotrackname (gchar * name)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  if (name)
    g_string_assign (curstaffstruct->denemo_name, name);
}

static void
doinstrname (gchar * name)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
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
decode_metadata (const smf_event_t * event, gint number)
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
 * Process note off command, at present we have no need to store note off events...
 */
static void
donoteoff (const smf_event_t * event, gint number, gint track)
{

}

/**
 * Process note on command
 */
static void
donoteon (smf_event_t * event, gint number, gint track)
{
    DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
    note->timing = event->time_seconds * Denemo.project->movement->recording->samplerate;
    notenum2enharmonic (noteon_key(event), &(note->mid_c_offset), &(note->enshift), &(note->octave));
    note->event_number = number;
    note->track_number = track;
    Denemo.project->movement->recording->notes = g_list_append (Denemo.project->movement->recording->notes, note);
}



static void
decode_midi_event (smf_event_t * event, gint number, gint track)
{
  gint channel;
  gchar note[5];

  /* + 1, because user-visible channels used to be in range <1-16>. */
  channel = (event->midi_buffer[0] & 0x0F) + 1;

  switch (event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)
    {
    case NOTE_OFF:

      donoteoff (event, number, track);
      break;

    case NOTE_ON:
      if (event->midi_buffer[2])
        donoteon (event, number, track);
      else
        donoteoff (event, number, track);
      break;

    default:
      break;
    }
}



static void
event_for_staff (smf_event_t * event, gint number, gint track)
{
  if (smf_event_is_metadata (event))
    decode_metadata (event, number);
  else
    decode_midi_event (event, number, track);

}

static gint
create_staff (gint track)
{
	gint number = 1;
  smf_event_t *event;
  smf_track_t *selected_track;
  smf_rewind (smf);
  selected_track = smf_get_track_by_number (smf, track);
  while ((event = smf_track_get_next_event (selected_track)) && event)
    event_for_staff (event, number++, track);
  return 1;
}

/* create the global smf for the recorded track if it does not already belong to it
 * this happens when the user records MIDI using a MIDI controller, add the track and set user_pointer to -1 to indicate that the smf is for recorded MIDI
 * otherwise add it to smf, or if already present in smf (user_pointer points to track number) re-attach so that smf can be used. */
static void ensure_smf (void) {
    if (Denemo.project->movement->recorded_midi_track)
    {   smf_track_t *track = Denemo.project->movement->recorded_midi_track;
        if(smf==NULL)
        {
            smf = smf_new ();
            smf_add_track (smf, track);
            track->user_pointer = GINT_TO_POINTER(-1);
        }
        else
        {
          track->smf = smf;
          if(smf_from_file)
          {
              assert(track->user_pointer>0);
              track->track_number = GPOINTER_TO_INT(track->user_pointer);
          }
         else
            track->track_number = 1;
        }
    }
}
static void guess_note_length (gdouble quarternotes, gint *dur, gint *dot)
{
    int vals[] = {
    7
    ,10
    ,13
    ,19
    ,25
    ,37
    ,49
    ,73
    ,97
    ,145
    ,193
    ,289
    ,385
    ,577
    ,769
    ,1153
    ,1537
    ,2205
};
//#define formula(n) ((vals[n+1]+vals[n])/2) // ad hoc formula, nothing really works for guessing durations.

#define formula(n) (vals[n])

    gint ticks = (gint)(384*quarternotes+0.5);
    if(ticks < formula(0)) {*dur = 8;*dot = 0; return;}
    if(ticks < formula(1)) {*dur = 8;*dot = 1; return;}
    if(ticks < formula(2)) {*dur = 7;*dot = 0; return;}
    if(ticks < formula(3)) {*dur = 7;*dot = 1; return;}
    if(ticks < formula(4)) {*dur = 6;*dot = 0; return;}
    if(ticks < formula(5)) {*dur = 6;*dot = 1; return;}
    if(ticks < formula(6)) {*dur = 5;*dot = 0; return;}
    if(ticks < formula(7)) {*dur = 5;*dot = 1; return;}
    if(ticks < formula(8)) {*dur = 4;*dot = 0; return;}
    if(ticks < formula(9)) {*dur = 4;*dot = 1; return;}
    if(ticks < formula(10)) {*dur = 3;*dot = 0; return;}
    if(ticks < formula(11)) {*dur = 3;*dot = 1; return;}
    if(ticks < formula(12)) {*dur = 2;*dot = 0; return;}
    if(ticks < formula(13)) {*dur = 2;*dot = 1; return;}
    if(ticks < formula(14)) {*dur = 1;*dot = 0; return;}
    if(ticks < formula(15)) {*dur = 1;*dot = 1; return;}
    if(ticks < formula(16)) {*dur = 0;*dot = 0; return;}
    if(ticks<2705) {*dur = 0;*dot = 1; return;}
    *dur = *dot = 0;
}
gboolean compute_midi_note_durations (void)
{
    gboolean ret = FALSE;
    DenemoRecording *recording = Denemo.project->movement->recording;
    if (recording)
    {
        GList *g;
        ensure_smf ();
        smf_rewind (smf);
        for(g = recording->notes;g;g=g->next)
        {
            DenemoRecordedNote *note = g->data;
            int event_number = note->event_number;
            int track_number = note->track_number;
            smf_event_t *event = smf_track_get_event_by_number(smf_get_track_by_number(smf, track_number), event_number);
            
            //smf is a static in this file ugh.
            
            if(event && (0 == smf_seek_to_event (smf, event)))
                {
                    smf_event_t *next;
                    while ((next = smf_get_next_event (smf)))
                      {
                        if (((next->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_OFF) && (next->midi_buffer[1] == event->midi_buffer[1]))
                        {
                            smf_tempo_t *tempo = smf_get_tempo_by_seconds (smf, event->time_seconds);
                            double spqn = (tempo? tempo->microseconds_per_quarter_note/1000000.0: 60.0/Denemo.project->movement->tempo);
                            guess_note_length((next->time_seconds - event->time_seconds)/spqn, &note->duration, &note->dots);
                            //g_debug("spqn %f dur %f %d %d\n", spqn, (next->time_seconds - event->time_seconds), note->duration, note->dots);
                            ret = TRUE;
                            break;
                        }
                      }
                     smf_rewind (smf);
                }
        }
    }
    return ret;
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
      //re-attach the current Denemo.project->movement->recorded_midi_track to smf or delete it if it is not in smf
      if(Denemo.project->movement->recorded_midi_track)
      {
         if(((smf_track_t *)Denemo.project->movement->recorded_midi_track)->user_pointer == NULL)
            smf_track_delete(Denemo.project->movement->recorded_midi_track);
         else
            ((smf_track_t *)Denemo.project->movement->recorded_midi_track)->smf = smf;
      }
      selected_track->user_pointer = GINT_TO_POINTER(track);
      Denemo.project->movement->recorded_midi_track = selected_track;
      compute_midi_note_durations (); //fills Denemo.project->movement->recording->notes with the note durations
      ((smf_track_t *)Denemo.project->movement->recorded_midi_track)->smf = NULL; // we detach this track from smf, so it can be attached to the playback smf; we cannot use smf while this is done, as it thinks it still owns the track.
      current_track = track;
    }
   else
    ret = -1;
  exportmidi (NULL, Denemo.project->movement);
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
    if(smf)
        return smf->number_of_tracks;
    else
        if(Denemo.project->movement->recorded_midi_track)
            return 1;
return 0;
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
//g_debug("my seconds %f\n", event->time_seconds );
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
        g_debug("my value %f\n", my_smf_get_length_seconds (smf));
        double val = smf_get_length_seconds (smf);
        //g_debug("smf val %f\n", val);
        return val;
#endif
    }
    else
    return 0.0;
}

gboolean delete_imported_midi (void) {
  if(is_playing ())
    {
        stop_playing();
        return FALSE;
    }
  if (smf)
    {
  //  gint track;
  //  for (track=1; track <= smf->number_of_tracks; track++)
    //        smf_get_track_by_number (smf, track)->smf = smf;
    // FIXME, this crashes with the assertion smf.c:752: smf_get_track_by_number: Assertion `track_number >= 1' failed. in some circumstances.
    smf = NULL;
    current_track = 0;
    }
  if(Denemo.project && Denemo.project->movement)
    Denemo.project->movement->recorded_midi_track = NULL;
  delete_recording ();
  smf_from_file = FALSE;
  return TRUE;
}

gint
guidedImportMidi (gchar * filename)
{
  delete_imported_midi ();
  /* load the file */
  smf = cmd_load (filename);
  if (!smf)
    return -1;
  smf_from_file = TRUE;
  return 0;
}

gboolean midi_is_from_file (void)
{
    return smf_from_file;
}
