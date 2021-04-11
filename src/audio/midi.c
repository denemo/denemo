/*
 * midi.c
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2000-2005 Brian Delaney
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include <denemo/denemo.h>
#include "audio/midi.h"
#include "audio/audiointerface.h"
#include "smf.h"
#include "export/exportmidi.h"
#include "display/draw.h"
#include "core/view.h"
#include "audio/pitchentry.h"
#include "audio/instrumentname.h"
#include "printview/svgview.h"

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define SHAVING (0.01)          //seconds to shave off a note start time to ensure stopping before noteon is sent, and starting with noteon first note may depend of speed of machine??? FIXME



static volatile gboolean playing = FALSE;

static double last_draw_time;

// huh?
static gboolean midi_capture_on = FALSE;        //any midi events not caught by midi_divert will be dropped if this is true

static gdouble play_until = G_MAXDOUBLE;

/* MIDI in handling diversion to scheme scripts of MIDI in data */
static gint *divert_midi_event;
static gint divert_midi_id = 0; //id of the DenemoProject which wants to intercept midi events

static GQueue midi_queue = G_QUEUE_INIT;
static gint
put_get_midiqueue (gint midi)
{
  if (g_queue_is_empty (&midi_queue))
    return midi;
  g_queue_push_tail (&midi_queue, GINT_TO_POINTER (midi));
  return GPOINTER_TO_INT( g_queue_pop_head (&midi_queue) );
}

static void
put_midiqueue (gint midi)
{
  g_queue_push_tail (&midi_queue, GINT_TO_POINTER (midi));
}

static gint
get_midiqueue (void)
{
  return GPOINTER_TO_INT( g_queue_pop_head (&midi_queue) );
}

/*End of MIDI in handling diversion to scheme scripts of MIDI in data */

void
update_position (smf_event_t * event)
{
  DenemoMovement *si = Denemo.project->movement;

  if (event)
    {
      if (((event->midi_buffer[0] & 0xf0) == MIDI_NOTE_ON) && ((event->time_seconds - last_draw_time) > Denemo.prefs.display_refresh))
        {
          last_draw_time = event->time_seconds;
          queue_redraw_playhead (event);
        }
    }
  else
    {
      if (si)
        {
          si->playingnow = NULL;
          si->playhead = 0;
          queue_redraw_all ();
        }
    }
}

static void
safely_add_track (smf_t * smf, smf_track_t * track)
{
    track->smf = NULL;
    smf_add_track (smf, track);
}

static void
safely_track_remove_from_smf (smf_track_t * track)
{
  if (track->smf != NULL && (track->track_number>=1))
    smf_track_remove_from_smf (track);
  track->smf = NULL;
}

static GString *callback_script = NULL;
void
start_playing (gchar * callback)
{
  smf_t *smf = Denemo.project->movement->smf;
  if (callback && *callback)
    callback_script = g_string_new (callback);
  if (Denemo.project->movement->recorded_midi_track)
    safely_add_track (Denemo.project->movement->smf, Denemo.project->movement->recorded_midi_track);

  fix_start_end_ordering ();
  smf_rewind (smf);
  gdouble start = (Denemo.project->movement->start_time/get_playback_speed()) - SHAVING;
  if(smf_seek_to_seconds (smf, (start>0.0)?start:0.0))
    g_warning("smf_seek_to_seconds %f failed", start);

  initialize_until_time ();

  initialize_playhead ();

  playing = TRUE;
  last_draw_time = -1.0;//needed to trigger drawing first note
}

static gboolean
stop_play_callback (gchar * thescript)
{
  call_out_to_guile (thescript);
  g_free (thescript);
  return FALSE;
}

static gboolean do_set_playbutton (gboolean paused)
{
    set_playbutton (paused);
    return FALSE;
}
static gboolean
update_playbutton_callback (gboolean paused)
{

  g_main_context_invoke (NULL, (GSourceFunc)do_set_playbutton, GINT_TO_POINTER(paused));

  return FALSE;
}

static void
finish_recording (void)
{
  if ((Denemo.project->midi_destination & MIDIRECORD))
    {
      Denemo.project->midi_destination ^= MIDIRECORD;
      g_idle_add_full (G_PRIORITY_HIGH_IDLE, (GSourceFunc) show_midi_record_control, NULL, NULL);
    }
}
void
stop_playing ()
{
  update_position (NULL);
  g_idle_add_full (G_PRIORITY_HIGH_IDLE, (GSourceFunc) update_playbutton_callback, GINT_TO_POINTER (is_paused ()), NULL);
  playing = FALSE;
  play_until = -G_MAXDOUBLE;
  if (Denemo.project->movement && Denemo.project->movement->recorded_midi_track)
    {
      safely_track_remove_from_smf (Denemo.project->movement->recorded_midi_track);
      finish_recording ();
    }
  if (callback_script)
    {
      g_idle_add_full (G_PRIORITY_HIGH_IDLE, (GSourceFunc) stop_play_callback, g_string_free (callback_script, FALSE), NULL);
      callback_script = NULL;
    }
}

void
toggle_paused ()
{
  if (play_until < 0.0)
    play_until = G_MAXDOUBLE;
  else
    play_until = -G_MAXDOUBLE;
}

gboolean
is_playing ()
{
  return playing;
}

gboolean
is_paused ()
{
  return play_until < 0.0;
}

gdouble
get_playuntil (void)
{
  return play_until;
}

void
update_playback_start_time (double adjust)
{
  if (Denemo.project && Denemo.project->movement)
    {
      Denemo.project->movement->start_time += adjust;
    }
}

double
get_start_time ()
{
  if (Denemo.project && Denemo.project->movement && (Denemo.project->movement->start_time > 0.0))
    {
      return Denemo.project->movement->start_time;
    }
  else
    {
      return 0.0;
    }
}


double
get_end_time ()
{
  if (Denemo.project && Denemo.project->movement && Denemo.project->movement->smf)
    {
      if (Denemo.project->movement->end_time < 0.0)
        Denemo.project->movement->end_time = smf_get_length_seconds (Denemo.project->movement->smf);
      return Denemo.project->movement->end_time;
    }
  else
    {
      return 0.0;
    }
}


smf_event_t *
get_smf_event (double until_time)
{
  if (Denemo.project == NULL || Denemo.project->movement == NULL || Denemo.project->movement->smf == NULL)
    return NULL;
  smf_t *smf = Denemo.project->movement->smf;

  if (until_time > Denemo.project->movement->end_time)
    {
      until_time = Denemo.project->movement->end_time;
    }

  for (;;)
    {
      smf_event_t *event = smf_peek_next_event (smf);

      if (event == NULL || event->time_seconds >= until_time)
        {
          return NULL;
        }

      if (smf_event_is_metadata (event))
        {
          // consume metadata event and continue with the next one
          event = smf_get_next_event (smf);
          continue;
        }

      // consume the event
      event = smf_get_next_event (smf);
      return event;
    }
}




gdouble
get_time ()
{
  GTimeVal tv;
  double seconds;

  g_get_current_time (&tv);

  seconds = tv.tv_sec + tv.tv_usec / 1000000.0;
  return seconds;
}


void
generate_midi (void)
{
  if ((Denemo.project->movement->smf == NULL) || (Denemo.project->movement->smfsync != Denemo.project->movement->changecount))
    {
      exportmidi (NULL, Denemo.project->movement);
    }

  if (Denemo.project->movement->smf == NULL)
    {
      g_critical ("Loading SMF failed.");
    }
}


/* return the time of the last event on the list events */
gdouble
get_midi_off_time (GList * events)
{
  smf_event_t *event = g_list_last (events)->data;
  return event->time_seconds;
}

/* return the time of the first event on the list events */
gdouble
get_midi_on_time (GList * events)
{
  smf_event_t *event = events->data;
  return event->time_seconds;
}


//finds the first note which comes ON after the passed time
DenemoObject *
get_obj_for_start_time (smf_t * smf, gdouble time)
{
  if (time < 0.0)
    time = 0.0;
  static smf_event_t *event;
  smf_event_t *initial = smf_peek_next_event (smf);
  gdouble total = smf_get_length_seconds (smf);
  time = (time > total ? total : time);
  if(smf_seek_to_seconds (smf, time))
    g_debug("smf_seek_to_seconds failed");
  do
    {
      event = smf_get_next_event (smf);
    }
  while (event && (((event->midi_buffer[0] & 0xF0) == MIDI_NOTE_OFF) || !event->user_pointer));
  if (initial && smf_seek_to_event (smf, initial))
      g_debug("smf_seek_to_event failed");  //if (event) g_debug("sought for endObj %f found %f\n", time, event->time_seconds);
  if (event)
    return (DenemoObject *) (event->user_pointer);
  return get_object_for_time (time, TRUE);
}
//finds the first note which comes OFF after the passed time
DenemoObject *
get_obj_for_end_time (smf_t * smf, gdouble time)
{
  if (time < 0.0)
    time = 0.0;
  static smf_event_t *event = NULL;
  smf_event_t *initial = smf_peek_next_event (smf);
  gdouble total = smf_get_length_seconds (smf);
  time = (time > total ? total : time);
  if(smf_seek_to_seconds (smf, time))
    g_debug("smf_seek_to_seconds failed");
  do
    {
      event = smf_get_next_event (smf);
    }
  while (event && (((event->midi_buffer[0] & 0xF0) == MIDI_NOTE_ON) || !event->user_pointer));
  if (initial && smf_seek_to_event (smf, initial))
      g_debug("smf_seek_to_event failed");//if (event) g_debug("sought for startObj %f found %f\n", time, event->time_seconds);
  if (event)
    return (DenemoObject *) (event->user_pointer);
  //midi is generated by LilyPond, no user_pointer, get timings from events.txt
  return get_object_for_time (time, FALSE);
}



/**
 * action_note_into_score
  enters ( or (if mode==INPUTEDIT and appending) edits the note at the cursor)
 * the parameters specify which note
 * @mid_c_offset
 * @enshift enharmonic adjustment -1 is one flat etc..
 * @octave
 */
static void
action_note_into_score (gint mid_c_offset, gint enshift, gint octave)
{//g_print ("action note into score\n");
  DenemoProject *gui = Denemo.project;
  gui->last_source = INPUTMIDI;
  gui->movement->cursor_y = gui->movement->staffletter_y = mid_c_offset;
  gui->movement->cursor_y += 7 * octave;
  Denemo.project->movement->pending_enshift = enshift;
  edit_or_append_pitch (mid_c_offset, TRUE);
  Denemo.project->movement->pending_enshift = 0;
  displayhelper (gui);
}

static void
add_or_delete_note_to_chord (gint mid_c_offset, gint enshift, gint octave)
{//g_print ("add or delete note to chord\n");
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj;
  gui->last_source = INPUTMIDI;
  gui->movement->cursor_y = gui->movement->staffletter_y = mid_c_offset;
  gui->movement->cursor_y += 7 * octave;
  if(insert_or_delete_chordnote (enshift))
    setenshift (gui->movement, enshift);
  displayhelper (gui);
}

typedef struct enharmonic
{
  gint mid_c_offset;
  gint enshift;
  gint octave;
} enharmonic;


void new_midi_recording (void) {
  DenemoRecording *recording;
  if(Denemo.project->movement->recording && (Denemo.project->movement->recording->type==DENEMO_RECORDING_MIDI))
    {
      //FIXME a better name for the mutex which originally was just for midi data, but will work for audio data too.
      recording = Denemo.project->movement->recording;
      g_mutex_lock (&smfmutex);
      Denemo.project->movement->recording = NULL;
      g_mutex_unlock (&smfmutex);
      g_free (recording->filename);
      g_free (recording);
      g_list_free_full (recording->notes, g_free);
     }
  recording = (DenemoRecording *) g_malloc (sizeof (DenemoRecording));
  recording->type = DENEMO_RECORDING_MIDI;
  recording->samplerate = 44100;
  Denemo.project->movement->recording = recording;
}

//Add the passed midi to a recording in Denemo.project->movement
static void
record_midi (gchar * buf, gdouble time)
{
  buf[0] |= 0xF; //here force the channel to 15
  smf_event_t *event = smf_event_new_from_pointer (buf, 3);
  if (event && smf_event_is_valid (event))
    {
      if (Denemo.project->movement->recorded_midi_track && ((smf_track_t *) Denemo.project->movement->recorded_midi_track)->smf)
        {

          smf_track_add_event_seconds (Denemo.project->movement->recorded_midi_track, event, time);
          if(Denemo.project->movement->recording && noteon_key(event))
            {
                DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
                note->timing = event->time_seconds * Denemo.project->movement->recording->samplerate;
                notenum2enharmonic (noteon_key(event), &(note->mid_c_offset), &(note->enshift), &(note->octave));
                note->event = event;
                Denemo.project->movement->recording->notes = g_list_append (Denemo.project->movement->recording->notes, note);
            }
        }
      else
        {
          smf_event_delete (event);
          gdk_beep ();
        }
    }
}

static void
do_one_note (gint mid_c_offset, gint enshift, gint notenum)
{//g_print("do one note Adding mask %x, Chord mask %x\n", (Denemo.keyboard_state & ADDING_MASK) , (Denemo.keyboard_state & CHORD_MASK));
  if ((Denemo.keyboard_state & ADDING_MASK) && (Denemo.keyboard_state & CHORD_MASK))
    {

      add_or_delete_note_to_chord (mid_c_offset, enshift, notenum);
    }
  else
    {
      DenemoObject *curobj = NULL;
      //check for non-printing notes - back up to the first non-printing note.
      gboolean non_printing_note = FALSE;
      PushPosition (NULL, NULL);
      while (cursor_to_prev_note ())
        {
          curobj = Denemo.project->movement->currentobject->data;
          if (!curobj->isinvisible)
            break;
          else
            non_printing_note = TRUE;
        }
      if (Denemo.project->movement->currentobject)
        {
          curobj = Denemo.project->movement->currentobject->data;
          if (non_printing_note)
            {
              if (!curobj->isinvisible)
                cursor_to_next_note ();
              (void)pop_position ();//Discard the pushed position
            }
          else
            PopPosition (NULL, NULL);// go to where we started, as there are no non-printing notes
        }
       else
            PopPosition (NULL, NULL);// go to where we started, as there are no non-printing notes
      action_note_into_score (mid_c_offset, enshift, notenum);

      if (Denemo.keyboard_state & ADDING_MASK)
        Denemo.keyboard_state |= CHORD_MASK;
      set_midi_in_status ();
    }
}


static gboolean
get_current (enharmonic * enote)
{
  DenemoObject *curObj = NULL;
  if (Denemo.project->movement->currentobject)
    {
      curObj = Denemo.project->movement->currentobject->data;
      if (curObj && curObj->type == CHORD)
        {
          chord *thechord = (chord *) curObj->object;
          if (thechord->notes)
            {
              note *thenote = (note *) thechord->notes->data;
              enote->mid_c_offset = offsettonumber (thenote->mid_c_offset);
              enote->enshift = thenote->enshift;
              return TRUE;
            }
        }
    }
  return FALSE;
}

static gboolean
get_previous (enharmonic * enote)
{
  DenemoObject *curObj = NULL;
  if (Denemo.project->movement->currentobject)
    {
      if (Denemo.project->movement->currentobject->prev)
        curObj = Denemo.project->movement->currentobject->prev->data;
      else
        {
          if (Denemo.project->movement->currentmeasure->prev && Denemo.project->movement->currentmeasure->prev->data)
            {
             DenemoMeasure *m = (DenemoMeasure*)Denemo.project->movement->currentmeasure->prev->data;
             curObj = m->objects?g_list_last (m->objects)->data:NULL;
            }
        }
    }
  if (curObj && curObj->type == CHORD)
    {
      chord *thechord = (chord *) curObj->object;
      if (thechord->notes)
        {
          note *thenote = (note *) thechord->notes->data;
          enote->mid_c_offset = offsettonumber (thenote->mid_c_offset);
          enote->enshift = thenote->enshift;
          return TRUE;
        }
    }
  return FALSE;
}


static gboolean at_nonprinting (void)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  DenemoObject *curObj = Denemo.project->movement->currentobject->data;
  return (curObj->type == CHORD && curObj->isinvisible);
}

/*  take an action for the passed note. Enter/edit/check the score following the mode and keyboard state. */
static gint
midiaction (gint notenum)
{
  gboolean new_measure = Denemo.project->movement->cursoroffend;
  DenemoProject *gui = Denemo.project;
  if (gui == NULL)
    return TRUE;
  if (gui->movement == NULL)
    return TRUE;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  enharmonic enote, prevenote;
  gboolean have_previous;
  //g_print("midiaction Adding mask %x, Chord mask %x\n", (Denemo.keyboard_state & ADDING_MASK) , (Denemo.keyboard_state & CHORD_MASK));
  notenum2enharmonic (notenum, &enote.mid_c_offset, &enote.enshift, &enote.octave);
  if (Denemo.project->movement->cursor_appending)
    have_previous = get_current (&prevenote);
  else
    have_previous = get_previous (&prevenote);

  if (!(Denemo.keyboard_state & CHECKING_MASK))
    stage_undo (gui->movement, ACTION_STAGE_END);     //undo is a queue so this is the end :)

  if ((gui->mode & INPUTEDIT) || (Denemo.keyboard_state & CHECKING_MASK))
    {
      static gboolean beep = FALSE;
      gboolean is_tied = FALSE;
      gint measure = gui->movement->currentmeasurenum;
      if (Denemo.project->movement->currentobject)
        {
          DenemoObject *curObj = Denemo.project->movement->currentobject->data;
          if (curObj->type == CHORD)
            {
              do
                {
                  curObj = Denemo.project->movement->currentobject->data;
                  chord *thechord = (chord *) curObj->object;
                  is_tied = (!Denemo.prefs.ignore_ties) && thechord->is_tied;

//#define check_midi_note(a,b,c,d) ((a->mid_c_offset==b)&&(a->enshift==c))?playnote(a,curstaffstruct->midi_channel):gdk_beep();
                  if ((Denemo.keyboard_state & CHECKING_MASK) && thechord->notes)
                    {
                      //later - find note nearest cursor and
                      note *thenote = (note *) thechord->notes->data;
//            check_midi_note(thenote, enote.mid_c_offset + 7 *(enote.octave), enote.enshift, enote.octave);
                      if ((!curObj->isinvisible) && (thenote->mid_c_offset == (enote.mid_c_offset + 7 * (enote.octave))) && (thenote->enshift == enote.enshift))
                        {
                          gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
                          play_note (DEFAULT_BACKEND, 0 /*port */ , curstaffstruct->midi_channel, midi + curstaffstruct->transposition, 300 /*duration */ , 0);
                        }
                      else
                        {
                          gdk_beep ();
                          break;        //do not move on to next note
                        }
                    }
                  else
                    {

                      do_one_note (enote.mid_c_offset, enote.enshift, enote.octave);

                    }
                  if (Denemo.project->movement->cursor_appending)
                    break;
                    curObj = Denemo.project->movement->currentobject->data;
                    thechord = (chord *) curObj->object;
                    is_tied = (!Denemo.prefs.ignore_ties) && thechord->is_tied;
                }
              while ((!(Denemo.keyboard_state & ADDING_MASK)) && next_editable_note () && is_tied);
            }
          else //there is a current object that is not a chord
            {
              if (gui->movement->cursor_appending)
                {
                    do_one_note (enote.mid_c_offset, enote.enshift, enote.octave);
                    next_insert_or_editable_note();
                    //in some circumstance this fails to advance to the next editable note, the following checks for that.
                    if (Denemo.project->movement->currentobject)
                        {
                            curObj = Denemo.project->movement->currentobject->data;
                            if(!curObj->isinvisible)
                               next_editable_note ();
                        }
                }
              else
                gdk_beep ();
            }
          if (gui->mode & INPUTRHYTHM)
            {
              //g_print("measure was %d now %d with appending %d\n", measure, gui->movement->currentmeasurenum, gui->movement->cursor_appending);
              if (!beep && (measure != gui->movement->currentmeasurenum) && !gui->movement->cursor_appending)
                beep = TRUE;
              else if (beep)
                signal_measure_end (), beep = FALSE;
            }
        }
      else
        {                       // no current object
          do_one_note (enote.mid_c_offset, enote.enshift, enote.octave);
          next_insert_or_editable_note();//next_editable_note ();//if we have gone back from an empty measure we need this.
        }
    }
  else
    {                           // not INPUTEDIT
      action_note_into_score (enote.mid_c_offset, enote.enshift, enote.octave);
    }
  if (!(Denemo.keyboard_state & CHECKING_MASK))
    {
      stage_undo (gui->movement, ACTION_STAGE_START);
    }
  draw_score_area();     //just for advancing the cursor.
  if (!(Denemo.keyboard_state & CHECKING_MASK))
    {
      if (Denemo.prefs.immediateplayback)
        {
          gint channel = curstaffstruct->midi_channel;

          if (have_previous && check_interval (enote.mid_c_offset, enote.enshift, prevenote.mid_c_offset, prevenote.enshift))
            channel = Denemo.prefs.pitchspellingchannel;

          play_note (DEFAULT_BACKEND, 0 /*port */ , channel, notenum, 300 /*duration */ , 0);
          if(new_measure)
            signal_measure_end();
        }
    }

  return TRUE;
}


gboolean
set_midi_capture (gboolean set)
{
  gboolean ret = midi_capture_on;
  midi_capture_on = set;
  if (!set)
    divert_midi_id = 0;
  return ret;
}


#define command ((*buf)&0xF0)
#define notenumber ((*(buf+1))&0x7F)
#define velocity ((*(buf+2))&0x7F)
void
adjust_midi_velocity (gchar * buf, gint percent)
{
  if ((command == MIDI_NOTE_ON) && buf[2])
    buf[2] = 127 - (gint) ((127 - buf[2]) * percent / 100.0);
}

void add_after_touch (gchar * buf)
{
   if (Denemo.prefs.damping)
    {
      static gdouble times[0x7F]; //takes no account of channel, really only good for one channel.
      //HACK IN kill pitch bend and "modulation" wheel here
      if (command == MIDI_PITCH_BEND)
        {g_print ("Dropping pitch bend\n"); *buf=0; return;} 
      if (command == 0xB0)
        {g_print ("Dropping controller change message\n"); *buf=0;  return;} 
        
        
      if (command == MIDI_NOTE_ON)
        {
          times[notenumber] = get_time ();
        }
      if (command == MIDI_NOTE_OFF)
        {
          //g_debug("after %f seconds\n", get_time()-times[notenumber]);

          buf[0] = MIDI_NOTE_ON;        //or the channel here
          buf[2] = 60 / exp ((get_time () - times[notenumber]) * 1);    //scale according to the time
          return;
        }
    }
}

void
process_midi_event (gchar * buf)
{
  if (command == MIDI_CONTROL_CHANGE && (notenumber == 0x40))
    {
      if (velocity == 0x7F)
        {//PEDAL DOWN
        if (Denemo.project->movement->cursor_appending || at_nonprinting ())
            Denemo.keyboard_state |= ADDING_MASK;
        else
            Denemo.keyboard_state |= CHORD_MASK | ADDING_MASK;
        }
      else
        {
          Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
          next_insert_or_editable_note();//next_insert_or_editable_note ();
        }
      set_midi_in_status ();
      displayhelper (Denemo.project);
    }
  if ((0xFFFFFF & *(gint *) buf) == 0)
    {
      set_midi_capture (FALSE);
      g_queue_clear (&midi_queue);
      if (divert_midi_event)
        {
          *divert_midi_event = 0;
          divert_midi_event = NULL;
          gtk_main_quit ();
        }
      //g_debug("queue emptied %d\n", g_queue_get_length(&midi_queue));
    }
  else
    {
      if (command == MIDI_NOTE_ON)
        midiaction (notenumber);
      else if (command == MIDI_CONTROL_CHANGE)
        {
          gchar *command_name = get_midi_control_command (notenumber, velocity);
          if (command_name)
            {
              execute_callback_from_name (command_name);
              g_free (command_name);
            }
          else
            {
              if (notenumber == 0x40)
                {               //Foot Pedal
                  if (velocity == 0x7F)
                    {
                    if ((Denemo.project->movement->cursor_appending) || at_nonprinting ())
                        Denemo.keyboard_state |= ADDING_MASK;
                    else
                        Denemo.keyboard_state |= CHORD_MASK | ADDING_MASK;
                    }
                  else
                    {
                      Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
                      //next_editable_note (); this causes a double advance...
                    }
                  set_midi_in_status ();
                  displayhelper (Denemo.project);
                }
            }
        }
      else if (command == MIDI_PITCH_BEND)
        {
          gchar *command_name = get_midi_pitch_bend_command ((notenumber << 8) + velocity);
          if (command_name)
            {
              execute_callback_from_name (command_name);
              g_free (command_name);
            }
        }
    }
}


void
initialize_until_time (void)
{
  if ((Denemo.project->midi_destination & MIDIPLAYALONG) && Denemo.project->movement->currentobject)
    {
      DenemoObject *obj = Denemo.project->movement->currentobject->data;
      if (obj->type == CHORD)
        {
          chord *thechord = obj->object;
          if (thechord->notes)
            {
              play_until = obj->earliest_time - SHAVING;        //g_debug("initial until %f\n", play_until);
            }
        }
    }
  else
    play_until = G_MAXDOUBLE;
}

//test if the midi event in buf is a note-on for the current note
//if so set play_until
//advance cursor to next note
static void
advance_until_time (gchar * buf)
{
  if (Denemo.project->movement->currentobject)
    {
      DenemoObject *obj = Denemo.project->movement->currentobject->data;
      if (obj->type != CHORD)
        if (cursor_to_next_chord ())
          obj = Denemo.project->movement->currentobject->data;

      if (Denemo.project->movement->currentobject && obj->type == CHORD)
        {
          chord *thechord = obj->object;
          if (thechord->notes)
            {
              note *thenote = thechord->notes->data;
              if (((buf[0] & 0xf0) == MIDI_NOTE_ON) && buf[2] && buf[1] == (dia_to_midinote (thenote->mid_c_offset) + thenote->enshift))
                {
                  gdouble thetime = get_time ();
                  Denemo.project->movement->start_player = thetime - obj->earliest_time;

                  if ((!Denemo.prefs.ignore_ties) && thechord->is_tied && cursor_to_next_note ())
                    {
                      obj = Denemo.project->movement->currentobject->data;
                    }
                  //IF THE NEXT OBJ IS A REST ADVANCE OVER IT/THEM
                  do
                    {
                      if (!cursor_to_next_note ())      //if(!cursor_to_next_chord())
                        {
                          play_until = G_MAXDOUBLE;
                          break;
                        }
                      else
                        {
                          obj = Denemo.project->movement->currentobject->data;
                          thechord = obj->object;
                          play_until = obj->earliest_time - SHAVING;
                          //g_debug("play until %f\n", play_until);
                        }
                    }
                  while (!thechord->notes);
                }
            }
        }
      else
        g_warning ("Not on a chord");
    }
  else
    g_warning ("Not on an object");
}

static void
adjust_to_staff (gchar * buf)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  gint channel = curstaffstruct->midi_channel;
  if ((buf[0] & SYS_EXCLUSIVE_MESSAGE1) == MIDI_NOTE_ON)
    {
      buf[0] = MIDI_NOTE_ON | channel;
      buf[1] += curstaffstruct->transposition;
    }
  else if ((buf[0] & SYS_EXCLUSIVE_MESSAGE1) == MIDI_NOTE_OFF)
    {
      buf[0] = MIDI_NOTE_OFF | channel;
      buf[1] += curstaffstruct->transposition;
    }
}

//Event generated by MIDI controller or Scheme script
//adjusts the note-on volume by preferred dynamic compression and plays the passed event on default backend
void
play_adjusted_midi_event (gchar * buf)
{
  adjust_midi_velocity (buf, 100 - Denemo.prefs.dynamic_compression);
  add_after_touch (buf);
  adjust_to_staff (buf);
  //g_print ("play adj midibytes 0x%hhX 0x%hhX 0x%hhX\n", *(buf+0), *(buf+1), *(buf+2));
  play_midi_event (DEFAULT_BACKEND, 0, (guchar*) buf);
}

#define EDITING_MASK (GDK_SHIFT_MASK)
//these are event generated by a MIDI controller or Scheme script
void
handle_midi_event (gchar * buf)
{
  //g_print ("play adj midibytes 0x%hhX 0x%hhX 0x%hhX\n", *(buf+0), *(buf+1), *(buf+2));
  if (Denemo.prefs.notesonlymidiin && (((*buf)&0xF0) != MIDI_NOTE_ON) && (((*buf)&0xF0) != MIDI_NOTE_OFF))
        {//g_print ("Dropping messages other than note on/off\n"); 
		*buf=0;  
		return;}
  //g_print("%x : ready %d %x queue %d\n", midi_capture_on, divert_midi_event!=NULL, (0xFFFFFF & *(gint*)buf), g_queue_get_length(&midi_queue));
  if (midi_capture_on && divert_midi_id == Denemo.project->id)
    {
      // this is only good for one endianness - FIXME ??
      if (divert_midi_event)
        {
          *divert_midi_event = (0xFFFFFF & put_get_midiqueue (*(gint *) buf));
          divert_midi_event = NULL;
          gtk_main_quit ();
        }
      else
        {
          put_midiqueue (*(gint *) buf);
        }
      return;                   //this *is* reached
    }
  if ((Denemo.project->midi_destination & MIDIRECORD) || (Denemo.project->midi_destination & (MIDIPLAYALONG | MIDICONDUCT)))
    {
      if (Denemo.project->midi_destination & MIDIRECORD)
        record_midi (buf, get_playback_time ());
      if (Denemo.project->midi_destination & (MIDIPLAYALONG))
        advance_until_time (buf);//FIXME is this thread-safe????
      else
        play_adjusted_midi_event (buf);//play_midi_event (DEFAULT_BACKEND, 0, (guchar *) buf);
    }
  else
    {
      if ((Denemo.keyboard_state == (GDK_SHIFT_MASK | GDK_LOCK_MASK)) ||
           Denemo.keyboard_state == (GDK_CONTROL_MASK) ||
           Denemo.keyboard_state == (ADDING_MASK) ||
           Denemo.keyboard_state == ((ADDING_MASK) | (CHORD_MASK)) ||
           Denemo.keyboard_state == (GDK_CONTROL_MASK | GDK_LOCK_MASK) ||
           Denemo.keyboard_state == 0)
        process_midi_event (buf);
      else if (Denemo.keyboard_state == (GDK_SHIFT_MASK) || Denemo.keyboard_state == (GDK_LOCK_MASK))
        {
          play_adjusted_midi_event (buf);
        }
    }
}


gboolean
intercept_midi_event (gint * midi)
{
  if (divert_midi_event)
    {
      //infodialog (_("Not exiting the previous MIDI capture loop"));
      g_warning ("intercept_midi_event called while divert_midi_event is TRUE - Cannot return to script");
      // divert_midi_event = NULL;
      // return FALSE;
      set_midi_capture (FALSE);
      g_queue_clear (&midi_queue);
      return FALSE;
    }
  if (g_queue_is_empty (&midi_queue))
    {
      divert_midi_event = midi;
      divert_midi_id = Denemo.project->id;
      set_midi_capture (TRUE);
      gtk_main ();
      divert_midi_event = NULL;
      return TRUE;
    }
  else
    {
      *midi = (0xFFFFFF & get_midiqueue ());
      //g_debug("getting from queue %x\n", *midi);
    }
  return TRUE;
}


gint
get_midi_channel (DenemoStaff * staff)
{
  if (!strcmp (staff->midi_instrument->str, "drums"))
    {
      return 9;
    }
  else
    {
      gint tracknumber = Denemo.project->movement->currentstaffnum - 1;
      tracknumber = (tracknumber >= 9) ? tracknumber + 1 : tracknumber;
      return tracknumber & 0xF;
    }
}


gint
get_midi_prognum (DenemoStaff * staff)
{
  if (staff->midi_channel == 9)
    {
      return 0;
    }
  else
    {
      return select_program (staff->midi_instrument->str);
    }
}


gint
get_midi_port (DenemoStaff * staff)
{
  return staff->midi_port;
}



/* change the MIDI output tuning */
void
change_tuning (gdouble * cents)
{
  guchar buffer[] = {
    0xF0, 0x7F,                 //               Universal Real-Time SysEx header

    0x7F,                       //<device ID>         ID of target device (7F = all devices)

    0x08,                       //             sub-ID#1 = "MIDI Tuning Standard"

    0x08,                       //             sub-ID#2 = "scale/octave tuning 1-byte form (Real-Time)"

    0x03,                       /*            channel/options byte 1
                                   bits 0 to 1 = channel 15 to 16
                                   bit 2 to 6 = reserved for future expansion */

    0x7F,                       //            channel byte 2 - bits 0 to 6 = channel 8 to 14

    0x7F,                       //            channel byte 3 - bits 0 to 6 = channel 1 to 7
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /*  [ss]            12 byte tuning offset of 12 semitones from C to B
       00H means -64 cents
       40H means 0 cents (equal temperament)
       7FH means +63 cents */

    0xF7                        //      EOX
  };
  gint i;
  for (i = 0; i < 12; i++)
    buffer[i + 8] = 64 + (cents[i] + 0.5);
  play_midi_event (DEFAULT_BACKEND, 0, buffer);
}

//return the midi key of the passed event if note on, else 0

int
noteon_key (smf_event_t * event)
{
  if ((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == MIDI_NOTE_ON)
    return event->midi_buffer[1];
  return 0;
}
