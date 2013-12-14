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
#include "midi.h"
#include "audiointerface.h"
#include "smf.h"
#include "exportmidi.h"
#include "draw.h"
#include "view.h"
#include "pitchentry.h"
#include "instrumentname.h"

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>


#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define NOTE_ON                 0x90
#define NOTE_OFF                0x80

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
  DenemoMovement *si = Denemo.project->si;

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
  if (track->smf == NULL)
    smf_add_track (smf, track);
}

static void
safely_track_remove_from_smf (smf_track_t * track)
{
  if (track->smf != NULL)
    smf_track_remove_from_smf (track);
}

static GString *callback_script = NULL;
void
start_playing (gchar * callback)
{
  smf_t *smf = Denemo.project->si->smf;
  if (callback && *callback)
    callback_script = g_string_new (callback);
  if (Denemo.project->si->recorded_midi_track)
    safely_add_track (Denemo.project->si->smf, Denemo.project->si->recorded_midi_track);

  set_start_and_end_objects_for_draw ();
  smf_rewind (smf);
  gdouble start = (Denemo.project->si->start_time/get_playback_speed()) - SHAVING;
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

static gboolean
update_playbutton_callback (gboolean paused)
{
  gdk_threads_enter ();
  set_playbutton (paused);
  gdk_threads_leave ();
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
  if (Denemo.project->si && Denemo.project->si->recorded_midi_track)
    {
      safely_track_remove_from_smf (Denemo.project->si->recorded_midi_track);
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
  if (Denemo.project && Denemo.project->si)
    {
      Denemo.project->si->start_time += adjust;
    }
}

double
get_start_time ()
{
  if (Denemo.project && Denemo.project->si && (Denemo.project->si->start_time > 0.0))
    {
      return Denemo.project->si->start_time;
    }
  else
    {
      return 0.0;
    }
}


double
get_end_time ()
{
  if (Denemo.project && Denemo.project->si && Denemo.project->si->smf)
    {
      if (Denemo.project->si->end_time < 0.0)
        Denemo.project->si->end_time = smf_get_length_seconds (Denemo.project->si->smf);
      return Denemo.project->si->end_time;
    }
  else
    {
      return 0.0;
    }
}


smf_event_t *
get_smf_event (double until_time)
{
  if (Denemo.project == NULL || Denemo.project->si == NULL || Denemo.project->si->smf == NULL)
    return NULL;
  smf_t *smf = Denemo.project->si->smf;

  if (until_time > Denemo.project->si->end_time)
    {
      until_time = Denemo.project->si->end_time;
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
      if (event->midi_buffer_length > 3)
        {
          g_warning ("Not Dropping event %d\n", event->midi_buffer_length);
          //continue;
        }

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
generate_midi ()
{
  if ((Denemo.project->si->smf == NULL) || (Denemo.project->si->smfsync != Denemo.project->si->changecount))
    {
      exportmidi (NULL, Denemo.project->si, 0, 0);
    }

  if (Denemo.project->si->smf == NULL)
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
      g_debug("smf_seek_to_event failed");  //if (event) g_print("sought for endObj %f found %f\n", time, event->time_seconds);
  if (event)
    return (DenemoObject *) (event->user_pointer);
  return NULL;
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
      g_debug("smf_seek_to_event failed");//if (event) g_print("sought for startObj %f found %f\n", time, event->time_seconds);
  if (event)
    return (DenemoObject *) (event->user_pointer);
  return NULL;
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
{
  DenemoProject *gui = Denemo.project;
  gui->last_source = INPUTMIDI;
  gui->si->cursor_y = gui->si->staffletter_y = mid_c_offset;
  gui->si->cursor_y += 7 * octave;
  shiftcursor (gui, mid_c_offset);
  setenshift (gui->si, enshift);
  displayhelper (gui);
}

static void
add_note_to_chord (gint mid_c_offset, gint enshift, gint octave)
{
  DenemoProject *gui = Denemo.project;
  gui->last_source = INPUTMIDI;
  gui->si->cursor_y = gui->si->staffletter_y = mid_c_offset;
  gui->si->cursor_y += 7 * octave;
  insert_chordnote (gui);
  // shiftcursor(gui, mid_c_offset);
  setenshift (gui->si, enshift);
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
  if(Denemo.project->si->recording && (Denemo.project->si->recording->type==DENEMO_RECORDING_MIDI))
    {  
      //FIXME a better name for the mutex which originally was just for midi data, but will work for audio data too.
      recording = Denemo.project->si->recording;
      g_static_mutex_lock (&smfmutex);
      Denemo.project->si->recording = NULL;
      g_static_mutex_unlock (&smfmutex);
      g_free (recording->filename);
      g_free (recording);
      g_list_free_full (recording->notes, g_free);
     } 
  recording = (DenemoRecording *) g_malloc (sizeof (DenemoRecording));
  recording->type = DENEMO_RECORDING_MIDI;
  recording->samplerate = 44100;
  Denemo.project->si->recording = recording;
}

//Add the passed midi to a recording in Denemo.project->si
static void
record_midi (gchar * buf, gdouble time)
{
  smf_event_t *event = smf_event_new_from_pointer (buf, 3);
  if (event && smf_event_is_valid (event))
    {
      if (Denemo.project->si->recorded_midi_track && ((smf_track_t *) Denemo.project->si->recorded_midi_track)->smf)
        {
          smf_track_add_event_seconds (Denemo.project->si->recorded_midi_track, event, time);
          if(Denemo.project->si->recording && noteon_key(event))
            {
                DenemoRecordedNote *note = g_malloc0(sizeof(DenemoRecordedNote));
                note->timing = event->time_seconds * Denemo.project->si->recording->samplerate;
                notenum2enharmonic (noteon_key(event), &(note->mid_c_offset), &(note->enshift), &(note->octave));
                note->event = event;
                Denemo.project->si->recording->notes = g_list_append (Denemo.project->si->recording->notes, note);
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
{
  if ((Denemo.keyboard_state & ADDING_MASK) && (Denemo.keyboard_state & CHORD_MASK))
    {

      add_note_to_chord (mid_c_offset, enshift, notenum);
    }
  else
    {
      DenemoObject *curobj = NULL;
      //check for non-printing notes - back up to the first non-printing note.
      gboolean non_printing_note = FALSE;
      PushPosition (NULL, NULL);
      while (cursor_to_prev_note ())
        {
          curobj = Denemo.project->si->currentobject->data;
          if (!curobj->isinvisible)
            break;
          else
            non_printing_note = TRUE;
        }
      if (Denemo.project->si->currentobject)
        {
          curobj = Denemo.project->si->currentobject->data;
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
  if (Denemo.project->si->currentobject)
    {
      curObj = Denemo.project->si->currentobject->data;
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
  if (Denemo.project->si->currentobject)
    {
      if (Denemo.project->si->currentobject->prev)
        curObj = Denemo.project->si->currentobject->prev->data;
      else
        {
          if (Denemo.project->si->currentmeasure->prev && Denemo.project->si->currentmeasure->prev->data)
            {
              curObj = g_list_last (Denemo.project->si->currentmeasure->prev->data)->data;
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


/*  take an action for the passed note. Enter/edit/check the score following the mode and keyboard state. */
static gint
midiaction (gint notenum)
{

  DenemoProject *gui = Denemo.project;
  if (gui == NULL)
    return TRUE;
  if (gui->si == NULL)
    return TRUE;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  enharmonic enote, prevenote;
  gboolean have_previous;
  //g_print("Keyboard state %x, mask %x %x %x\n", Denemo.keyboard_state, CHECKING_MASK, GDK_CONTROL_MASK, GDK_MOD2_MASK);
  notenum2enharmonic (notenum, &enote.mid_c_offset, &enote.enshift, &enote.octave);
  if (Denemo.project->si->cursor_appending)
    have_previous = get_current (&prevenote);
  else
    have_previous = get_previous (&prevenote);

  if (!(Denemo.keyboard_state & CHECKING_MASK))
    stage_undo (gui->si, ACTION_STAGE_END);     //undo is a queue so this is the end :)

  if ((gui->mode & INPUTEDIT) || (Denemo.keyboard_state & CHECKING_MASK))
    {
      static gboolean beep = FALSE;
      gboolean is_tied = FALSE;
      gint measure = gui->si->currentmeasurenum;
      if (Denemo.project->si->currentobject)
        {
          DenemoObject *curObj = Denemo.project->si->currentobject->data;
          if (curObj->type == CHORD)
            {
              do
                {
                  curObj = Denemo.project->si->currentobject->data;
                  chord *thechord = (chord *) curObj->object;
                  is_tied = thechord->is_tied;

//#define check_midi_note(a,b,c,d) ((a->mid_c_offset==b)&&(a->enshift==c))?playnote(a,curstaffstruct->midi_channel):gdk_beep();

                  //g_print("check %d %d %d %d %d\n", a->mid_c_offset, a->enshift, b, c, d);
                  if ((Denemo.keyboard_state & CHECKING_MASK) && thechord->notes)
                    {
                      //later - find note nearest cursor and
                      note *thenote = (note *) thechord->notes->data;
//            check_midi_note(thenote, enote.mid_c_offset + 7 *(enote.octave), enote.enshift, enote.octave);
                      if ((!curObj->isinvisible) && (thenote->mid_c_offset == (enote.mid_c_offset + 7 * (enote.octave))) && (thenote->enshift == enote.enshift))
                        {
                          gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
                          play_note (DEFAULT_BACKEND, 0 /*port */ , curstaffstruct->midi_channel, midi, 300 /*duration */ , 0);
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
                  if (Denemo.project->si->cursor_appending)
                    break;
                }
              while ((!(Denemo.keyboard_state & ADDING_MASK)) && next_editable_note () && is_tied);
            }
          else //there is a current object that is not a chord
            {
              if (gui->si->cursor_appending) {
                do_one_note (enote.mid_c_offset, enote.enshift, enote.octave);
                next_editable_note ();//if we have gone back from an appending position after a non-chord we need this
                            }
              else
                gdk_beep ();
            }
          if (gui->mode & INPUTRHYTHM)
            {
              //g_print("measure was %d now %d with appending %d\n", measure, gui->si->currentmeasurenum, gui->si->cursor_appending);
              if (!beep && (measure != gui->si->currentmeasurenum) && !gui->si->cursor_appending)
                beep = TRUE;
              else if (beep)
                signal_measure_end (), beep = FALSE;
            }
        }
      else
        {                       // no current object
          do_one_note (enote.mid_c_offset, enote.enshift, enote.octave);
          next_editable_note ();//if we have gone back from an empty measure we need this.
        }
    }
  else
    {                           // not INPUTEDIT    
      action_note_into_score (enote.mid_c_offset, enote.enshift, enote.octave);
    }
  if (!(Denemo.keyboard_state & CHECKING_MASK))
    {
      stage_undo (gui->si, ACTION_STAGE_START);
    }
  gtk_widget_queue_draw (Denemo.scorearea);     //just for advancing the cursor.
  if (!(Denemo.keyboard_state & CHECKING_MASK))
    {
      if (Denemo.prefs.immediateplayback)
        {
          gint channel = curstaffstruct->midi_channel;

          if (have_previous && check_interval (enote.mid_c_offset, enote.enshift, prevenote.mid_c_offset, prevenote.enshift))
            channel = Denemo.prefs.pitchspellingchannel;

          play_note (DEFAULT_BACKEND, 0 /*port */ , channel, notenum, 300 /*duration */ , 0);
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
  if (Denemo.prefs.dynamic_compression == 100)
    {
      static gdouble times[0x7F];
      if (command == MIDI_NOTE_ON)
        {
          times[notenumber] = get_time ();
        }
      if (command == MIDI_NOTE_OFF)
        {
          //g_print("after %f seconds\n", get_time()-times[notenumber]);

          buf[0] = MIDI_NOTE_ON;        //or the channel here
          buf[2] = 60 / exp ((get_time () - times[notenumber]) * 1);    //scale according to the time
          return;
        }
    }

  if (command == MIDI_NOTE_ON)
    buf[2] = 127 - (gint) ((127 - buf[2]) * percent / 100.0);
}


void
process_midi_event (gchar * buf)
{
  if (command == MIDI_CONTROL_CHANGE && (notenumber == 0x40))
    {
      if (velocity == 0x7F)
        //PEDAL DOWN
        Denemo.keyboard_state |= ADDING_MASK;
      else
        {
          Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
          next_editable_note ();
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
      //g_print("queue emptied %d\n", g_queue_get_length(&midi_queue));
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
                      Denemo.keyboard_state |= ADDING_MASK;
                    }
                  else
                    {
                      Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
                      next_editable_note ();
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
  if ((Denemo.project->midi_destination & MIDIPLAYALONG) && Denemo.project->si->currentobject)
    {
      DenemoObject *obj = Denemo.project->si->currentobject->data;
      if (obj->type == CHORD)
        {
          chord *thechord = obj->object;
          if (thechord->notes)
            {
              play_until = obj->earliest_time - SHAVING;        //g_print("initial until %f\n", play_until);
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
  if (Denemo.project->si->currentobject)
    {
      DenemoObject *obj = Denemo.project->si->currentobject->data;
      if (obj->type != CHORD)
        if (cursor_to_next_chord ())
          obj = Denemo.project->si->currentobject->data;

      if (Denemo.project->si->currentobject && obj->type == CHORD)
        {
          chord *thechord = obj->object;
          if (thechord->notes)
            {
              note *thenote = thechord->notes->data;
              if (((buf[0] & 0xf0) == MIDI_NOTE_ON) && buf[2] && buf[1] == (dia_to_midinote (thenote->mid_c_offset) + thenote->enshift))
                {
                  gdouble thetime = get_time ();
                  Denemo.project->si->start_player = thetime - obj->earliest_time;

                  if (thechord->is_tied && cursor_to_next_note ())
                    {
                      obj = Denemo.project->si->currentobject->data;
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
                          obj = Denemo.project->si->currentobject->data;
                          thechord = obj->object;
                          play_until = obj->earliest_time - SHAVING;
                          //g_print("play until %f\n", play_until);
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
adjust_midi_channel (gchar * buf)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->si->currentstaff->data;
  gint channel = curstaffstruct->midi_channel;
  if ((buf[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_ON)
    {
      buf[0] = NOTE_ON | channel;
    }
  else if ((buf[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_OFF)
    {
      buf[0] = NOTE_OFF | channel;
    }
}

//adjusts the note-on volume by preferred dynamic compression and plays the passed event on default backend
void
play_adjusted_midi_event (gchar * buf)
{
  adjust_midi_velocity (buf, 100 - Denemo.prefs.dynamic_compression);
  adjust_midi_channel (buf);
  play_midi_event (DEFAULT_BACKEND, 0, (guchar*) buf);
}

#define EDITING_MASK (GDK_SHIFT_MASK)
void
handle_midi_event (gchar * buf)
{
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
      if ((Denemo.keyboard_state == (GDK_SHIFT_MASK | GDK_LOCK_MASK)) || Denemo.keyboard_state == (GDK_CONTROL_MASK) || Denemo.keyboard_state == (ADDING_MASK) || Denemo.keyboard_state == ((ADDING_MASK) | (CHORD_MASK)) || Denemo.keyboard_state == (GDK_CONTROL_MASK | GDK_LOCK_MASK) || (Denemo.keyboard_state == 0))
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
      infodialog (_("Not exiting the previous MIDI capture loop"));
      g_warning ("Cannot return to script");
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
      //g_print("getting from queue %x\n", *midi);
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
      gint tracknumber = Denemo.project->si->currentstaffnum - 1;
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
  if ((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_ON)
    return event->midi_buffer[1];
  return 0;
}
