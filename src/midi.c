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

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>





static volatile gboolean playing = FALSE;

static double last_draw_time;

// huh?
static gboolean midi_capture_on = FALSE;//any midi events not caught by midi_divert will be dropped if this is true

static  gdouble play_until = G_MAXDOUBLE;

/* MIDI in handling diversion to scheme scripts of MIDI in data */
static gint *divert_midi_event;
static gint divert_midi_id=0;//id of the DenemoGUI which wants to intercept midi events

static GQueue midi_queue = G_QUEUE_INIT;
static gint put_get_midiqueue(gint midi) {
  if(g_queue_is_empty(&midi_queue))
    return midi;
  g_queue_push_tail(&midi_queue, GINT_TO_POINTER(midi));
  return (gint)g_queue_pop_head(&midi_queue);
}
static void put_midiqueue(gint midi) {
  g_queue_push_tail(&midi_queue, GINT_TO_POINTER(midi));
}

static gint get_midiqueue(void) {
  return (gint)g_queue_pop_head(&midi_queue);
}
/*End of MIDI in handling diversion to scheme scripts of MIDI in data */

void update_position(smf_event_t *event) {
  DenemoScore *si = Denemo.gui->si;

  if (event) {
    if ((event->midi_buffer[0] & 0xf0) == MIDI_NOTE_ON &&
        event->time_seconds - last_draw_time > Denemo.prefs.display_refresh) {
      last_draw_time = event->time_seconds;
      queue_redraw_playhead(event);
    }
  } else {
    if(si) {
      si->playingnow = NULL;
      si->playhead = 0;
      queue_redraw_all();
    }
  }
}

static void safely_add_track(smf_t *smf, smf_track_t *track) {
  if(track->smf==NULL)
    smf_add_track(smf, track);
}

static void safely_track_remove_from_smf(smf_track_t *track) {
 if(track->smf!=NULL)
   smf_track_remove_from_smf(track);
}
static GString *callback_script = NULL;
void start_playing(gchar *callback) {
  smf_t *smf = Denemo.gui->si->smf;
  if(callback && *callback)
      callback_script = g_string_new(callback);
  if(Denemo.gui->si->recorded_midi_track)
    safely_add_track(Denemo.gui->si->smf, Denemo.gui->si->recorded_midi_track);

  set_start_and_end_objects_for_draw();
  smf_rewind(smf);

  int r = smf_seek_to_seconds(smf, Denemo.gui->si->start_time);

  initialize_until_time();

  initialize_playhead();

  playing = TRUE;
  last_draw_time = 0.0;
}
static gboolean
stop_play_callback(gchar *thescript) {
    call_out_to_guile(thescript);
    g_free(thescript);
    return FALSE;
}

static gboolean update_playbutton_callback(gboolean paused) {
  gdk_threads_enter();
  set_playbutton(paused);
  gdk_threads_leave();
  return FALSE;
}

void stop_playing() {
  update_position(NULL);
  g_idle_add_full(G_PRIORITY_HIGH_IDLE, (GSourceFunc)update_playbutton_callback, GINT_TO_POINTER(is_paused()), NULL);
  playing = FALSE;
  play_until = -G_MAXDOUBLE;
  if(Denemo.gui->si && Denemo.gui->si->recorded_midi_track) {
    safely_track_remove_from_smf(Denemo.gui->si->recorded_midi_track);
    finish_recording();
  }
  if(callback_script) {
    g_idle_add_full(G_PRIORITY_HIGH_IDLE, (GSourceFunc)stop_play_callback, g_string_free(callback_script, FALSE), NULL);
    callback_script = NULL;
  } 
}

void toggle_paused() {
  if(play_until<0.0)
    play_until = G_MAXDOUBLE;
  else
    play_until = -G_MAXDOUBLE;
}

gboolean is_playing() {
  return playing;
}
gboolean is_paused() {
  return play_until<0.0;
}

gdouble get_playuntil(void) {
return play_until;
}
void update_playback_start_time(double adjust) {
  if (Denemo.gui && Denemo.gui->si) {
    Denemo.gui->si->start_time += adjust;
  } 
}

double get_start_time() {
  if (Denemo.gui && Denemo.gui->si && (Denemo.gui->si->start_time>0.0)) {
    return Denemo.gui->si->start_time;
  } else {
    return 0.0;
  }
}


double get_end_time() {
  if (Denemo.gui && Denemo.gui->si && Denemo.gui->si->smf) {
    if(Denemo.gui->si->end_time<0.0)
      Denemo.gui->si->end_time =smf_get_length_seconds(Denemo.gui->si->smf);
    return Denemo.gui->si->end_time;
  } else {
    return 0.0;
  }
}


smf_event_t *get_smf_event(double until_time) {
  if(Denemo.gui==NULL || Denemo.gui->si==NULL || Denemo.gui->si->smf==NULL)
    return NULL;
  smf_t *smf = Denemo.gui->si->smf;

  if (until_time > Denemo.gui->si->end_time) {
    until_time = Denemo.gui->si->end_time;
  }

  for (;;) {
    smf_event_t *event = smf_peek_next_event(smf);

    if (event == NULL || event->time_seconds >= until_time) {
      return NULL;
    }

    if (smf_event_is_metadata(event)) {
      // consume metadata event and continue with the next one
      event = smf_get_next_event(smf);
      continue;
    }

    // consume the event
    event = smf_get_next_event(smf);
    if(event->midi_buffer_length > 3) {
      g_warning("Not Dropping event %d\n", event->midi_buffer_length);
      //continue;
    }	

    return event;
  }
}




gdouble get_time() {
  GTimeVal tv;
  double seconds;

  g_get_current_time(&tv);

  seconds = tv.tv_sec + tv.tv_usec / 1000000.0;
  return seconds;
}


void generate_midi() {
  if((Denemo.gui->si->smf==NULL) || (Denemo.gui->si->smfsync!=Denemo.gui->si->changecount)) {
    exportmidi(NULL, Denemo.gui->si, 0, 0);
  }

  if (Denemo.gui->si->smf == NULL) {
    g_critical("Loading SMF failed.");
  }
}


/* return the time of the last event on the list events */
gdouble get_midi_off_time(GList *events) {
  smf_event_t *event = g_list_last(events)->data;
return event->time_seconds;
}

/* return the time of the first event on the list events */
gdouble get_midi_on_time(GList *events) {
  smf_event_t *event = events->data;
  return event->time_seconds;
}



DenemoObject *get_obj_for_start_time(smf_t *smf, gdouble time) {
  if(time<0.0)
      time=0.0;
  static smf_event_t *event;
  smf_event_t *initial = smf_peek_next_event(smf);
  gdouble total = smf_get_length_seconds(smf);
  time = (time>total?total:time);
  gint error = smf_seek_to_seconds(smf, time);
  do {
      event = smf_get_next_event(smf);
  } while(event && (((event->midi_buffer[0]&0xF0)==MIDI_NOTE_OFF) || !event->user_pointer));
  if(initial)
      error = smf_seek_to_event(smf, initial);
  if(event)
    return (DenemoObject *)(event->user_pointer);
  return NULL;
}

DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time) {
  if(time<0.0)
      time=0.0;
  static smf_event_t *event = NULL;
  smf_event_t *initial = smf_peek_next_event(smf);
  gdouble total = smf_get_length_seconds(smf);
  time = (time>total?total:time);
  gint error = smf_seek_to_seconds(smf, time);
  do {
      event = smf_get_next_event(smf);
  } while(event && (((event->midi_buffer[0]&0xF0)==MIDI_NOTE_ON) || !event->user_pointer));
  if(initial)
      error = smf_seek_to_event(smf, initial);
  if(event)
    return (DenemoObject *)(event->user_pointer);
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
static void action_note_into_score (gint mid_c_offset, gint enshift, gint octave) {
  DenemoGUI *gui = Denemo.gui;
  gui->last_source = INPUTMIDI;
  gui->si->cursor_y = gui->si->staffletter_y = mid_c_offset;
  gui->si->cursor_y += 7*octave; 
  shiftcursor(gui, mid_c_offset);
  setenshift(gui->si, enshift);
  displayhelper (gui);
}
static void add_note_to_chord (gint mid_c_offset, gint enshift, gint octave) {
  DenemoGUI *gui = Denemo.gui;
  gui->last_source = INPUTMIDI;
  gui->si->cursor_y = gui->si->staffletter_y = mid_c_offset;
  gui->si->cursor_y += 7*octave; 
  insert_chordnote(gui);
  // shiftcursor(gui, mid_c_offset);
  setenshift(gui->si, enshift);
  displayhelper (gui);
}
typedef struct enharmonic
{
  gint mid_c_offset;
  gint enshift;
  gint octave;
}enharmonic;


//Add the passed midi to a recording in Denemo.gui->si
void record_midi(gchar *buf, gdouble time) {
  smf_event_t *event = smf_event_new_from_pointer(buf, 3);
  if(event && smf_event_is_valid(event)) {
    if(Denemo.gui->si->recorded_midi_track && ((smf_track_t *)Denemo.gui->si->recorded_midi_track)->smf ) {
      smf_track_add_event_seconds(Denemo.gui->si->recorded_midi_track, event, time);
    } else {
      smf_event_delete(event);
      gdk_beep();
    }
  }
}

static void 	      
do_one_note(gint mid_c_offset, gint enshift, gint notenum) {
  DenemoGUI *gui = Denemo.gui;
  if((Denemo.keyboard_state&ADDING_MASK) && (Denemo.keyboard_state&CHORD_MASK)) {
    
    add_note_to_chord(mid_c_offset, enshift, notenum);
  }
  else {
    DenemoObject *curobj = NULL;
    //check for non-printing notes - back up to the first non-printing note.
    gboolean non_printing_note = FALSE;
    PushPosition(NULL, NULL);
    while(cursor_to_prev_note()) {
      curobj=Denemo.gui->si->currentobject->data;
      if(!curobj->isinvisible)
	break;
      else
	non_printing_note = TRUE;
    }
    if(Denemo.gui->si->currentobject) {
        curobj=Denemo.gui->si->currentobject->data;
        if(non_printing_note) {
        if(!curobj->isinvisible)
            cursor_to_next_note();
        pop_position();
        } else 
        PopPosition(NULL, NULL);
    }   
    action_note_into_score(mid_c_offset, enshift, notenum);
    if(Denemo.keyboard_state&ADDING_MASK)
      Denemo.keyboard_state |= CHORD_MASK;
    set_midi_in_status();
  }
}


static gboolean get_current(enharmonic *enote) {
  DenemoObject *curObj=NULL;
   if(Denemo.gui->si->currentobject) {
    curObj = Denemo.gui->si->currentobject->data;
    if(curObj && curObj->type==CHORD) {
      chord *thechord = (chord *)  curObj->object;
      if(thechord->notes) {
        note *thenote = (note *) thechord->notes->data;
          enote->mid_c_offset = offsettonumber(thenote->mid_c_offset);
          enote->enshift = thenote->enshift;
          return TRUE;
      }
    }
   }
return FALSE;
}

static gboolean get_previous(enharmonic *enote) {
  DenemoObject *curObj=NULL;
   if(Denemo.gui->si->currentobject) {
     if(Denemo.gui->si->currentobject->prev) 
      curObj = Denemo.gui->si->currentobject->prev->data;
      else {
      if(Denemo.gui->si->currentmeasure->prev && Denemo.gui->si->currentmeasure->prev->data) {
        curObj = g_list_last(Denemo.gui->si->currentmeasure->prev->data)->data;
      }
    }
   }
  if(curObj && curObj->type==CHORD) {
    chord *thechord = (chord *)  curObj->object;
    if(thechord->notes) {
        note *thenote = (note *) thechord->notes->data;
        enote->mid_c_offset = offsettonumber(thenote->mid_c_offset);
        enote->enshift = thenote->enshift;
        return TRUE;
      }
    }
return FALSE;
}


/*  take an action for the passed note. Enter/edit/check the score following the mode and keyboard state. */
static gint midiaction(gint notenum) {

  DenemoGUI *gui = Denemo.gui;
  if(gui==NULL)
    return TRUE;
  if(gui->si==NULL)
    return TRUE;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  enharmonic enote, prevenote;
  gboolean have_previous;
  //g_print("Keyboard state %x, mask %x %x %x\n", Denemo.keyboard_state, CHECKING_MASK, GDK_CONTROL_MASK, GDK_MOD2_MASK);
  notenum2enharmonic (notenum, &enote.mid_c_offset, &enote.enshift, &enote.octave);

  if(Denemo.gui->si->cursor_appending)
    have_previous = get_current(&prevenote);
  else
    have_previous = get_previous(&prevenote);
    
  if( !(Denemo.keyboard_state&CHECKING_MASK))
   stage_undo(gui->si, ACTION_STAGE_END);//undo is a queue so this is the end :)

  if((gui->mode & INPUTEDIT) || (Denemo.keyboard_state&CHECKING_MASK))
    {
      static gboolean beep = FALSE;
      gboolean is_tied = FALSE;
      gint measure = gui->si->currentmeasurenum;
      if(Denemo.gui->si->currentobject) {
	DenemoObject *curObj = Denemo.gui->si->currentobject->data;
	if(curObj->type==CHORD) {
	  do {
	    curObj = Denemo.gui->si->currentobject->data;
	    chord *thechord = (chord *)  curObj->object;
	    is_tied = thechord->is_tied;
	    
#define check_midi_note(a,b,c,d) ((a->mid_c_offset==b)&&(a->enshift==c))?playnote(a,curstaffstruct->midi_channel):gdk_beep();

	    //g_print("check %d %d %d %d %d\n", a->mid_c_offset, a->enshift, b, c, d);
	    if( (Denemo.keyboard_state&CHECKING_MASK) && thechord->notes) {
	      //later - find note nearest cursor and
	      note *thenote = (note*)thechord->notes->data;
//	      check_midi_note(thenote, enote.mid_c_offset + 7 *(enote.octave), enote.enshift, enote.octave);
	      if((!curObj->isinvisible)&&(thenote->mid_c_offset== (enote.mid_c_offset + 7 *( enote.octave)))&&(thenote->enshift==enote.enshift)) {
		     gint midi = dia_to_midinote (thenote->mid_c_offset) + thenote->enshift;
		     play_note(DEFAULT_BACKEND, 0 /*port*/, curstaffstruct->midi_channel, midi, 300 /*duration*/, 0);
	      } else {
		gdk_beep();
		break;//do not move on to next note
	      }
	    }
	    else {

	      do_one_note(enote.mid_c_offset, enote.enshift, enote.octave);
		
	    }
	    if(Denemo.gui->si->cursor_appending)
	      break;
	  } while((!(Denemo.keyboard_state&ADDING_MASK)) && next_editable_note() && is_tied);
	} else {
	  if(gui->si->cursor_appending)
	    do_one_note(enote.mid_c_offset, enote.enshift, enote.octave);
	  else
	    gdk_beep();
	}
	if(gui->mode & INPUTRHYTHM) {
	  //g_print("measure was %d now %d with appending %d\n", measure, gui->si->currentmeasurenum, gui->si->cursor_appending);
	  if(!beep && (measure != gui->si->currentmeasurenum) && !gui->si->cursor_appending)
	    beep=TRUE;
	  else if(beep) signal_measure_end(), beep=FALSE;
	}
      } else {// no current object
	  do_one_note(enote.mid_c_offset, enote.enshift, enote.octave);
      }
    } else {// not INPUTEDIT    
      action_note_into_score(enote.mid_c_offset, enote.enshift, enote.octave);
  }
  if( !(Denemo.keyboard_state&CHECKING_MASK)) {
    stage_undo(gui->si, ACTION_STAGE_START);
  }
  gtk_widget_queue_draw (Denemo.scorearea);//just for advancing the cursor.
 if(!(Denemo.keyboard_state&CHECKING_MASK)) {
    if(Denemo.prefs.immediateplayback) {
      gint channel = curstaffstruct->midi_channel;

      if(have_previous && check_interval(enote.mid_c_offset, enote.enshift, prevenote.mid_c_offset, prevenote.enshift))
        channel = Denemo.prefs.pitchspellingchannel;

      play_note(DEFAULT_BACKEND, 0 /*port*/, channel, notenum, 300 /*duration*/, 0);
    }
  }

  return TRUE;
}


gboolean set_midi_capture(gboolean set) {
  gboolean ret = midi_capture_on;
  midi_capture_on = set;
  if(!set) divert_midi_id = 0;
  return ret;
}


#define command ((*buf)&0xF0)
#define notenumber ((*(buf+1))&0x7F)
#define velocity ((*(buf+2))&0x7F)
void
adjust_midi_velocity(gchar *buf, gint percent) {
  if(command==MIDI_NOTE_ON)
    buf[2]=127 - (gint)((127-buf[2])*percent/100.0);
} 


void process_midi_event(gchar *buf) {
   if (command==MIDI_CONTROL_CHANGE && (notenumber == 0x40)){
	if (velocity == 0x7F)
	  //PEDAL DOWN
	  Denemo.keyboard_state |= ADDING_MASK;
	else {
	  Denemo.keyboard_state &= ~(CHORD_MASK|ADDING_MASK);
	  next_editable_note();
	}
	set_midi_in_status();
	displayhelper(Denemo.gui);
      }
  if((0xFFFFFF & *(gint*)buf)==0) {
    set_midi_capture(FALSE);
    g_queue_clear(&midi_queue);
    if(divert_midi_event) {
      *divert_midi_event = 0;
      divert_midi_event = NULL;
      gtk_main_quit();
    }
    //g_print("queue emptied %d\n", g_queue_get_length(&midi_queue));
  } else {
    if(command==MIDI_NOTE_ON)
      midiaction(notenumber);
    else if(command==MIDI_CONTROL_CHANGE) {
      gchar *command_name = get_midi_control_command(notenumber, velocity);
      if(command_name) {  
        execute_callback_from_name(Denemo.map, command_name);
        g_free(command_name);
      } else {
        if (notenumber == 0x40){  //Foot Pedal
          if (velocity == 0x7F) {
            Denemo.keyboard_state |= ADDING_MASK;
        } else {
            Denemo.keyboard_state &= ~(CHORD_MASK|ADDING_MASK);
            next_editable_note();
        }
        set_midi_in_status();
        displayhelper(Denemo.gui);
        }
      } 
    } else if(command==MIDI_PITCH_BEND) {
      gchar *command_name = get_midi_pitch_bend_command((notenumber<<8) + velocity);
      if(command_name) {
        execute_callback_from_name(Denemo.map, command_name);
      g_free(command_name);
      }
    }
  }
}

#define SHAVING (0.01) //seconds to shave off a note start time to ensure stopping before noteon is sent, may depend of speed of machine??? FIXME

void initialize_until_time(void) {
  if((Denemo.gui->midi_destination & MIDIPLAYALONG) && Denemo.gui->si->currentobject ) {
    DenemoObject *obj = Denemo.gui->si->currentobject->data;
    if(obj->type==CHORD) {
      chord *thechord = obj->object;
      if(thechord->notes) {
	note *thenote = thechord->notes->data;      
	play_until = obj->earliest_time - SHAVING; //g_print("initial until %f\n", play_until);
      }
    }
  }
  else
    play_until = G_MAXDOUBLE;
}

//test if the midi event in buf is a note-on for the current note
//if so set play_until
//advance cursor to next note
static void advance_until_time(gchar *buf) {
  if(Denemo.gui->si->currentobject) {
    DenemoObject *obj = Denemo.gui->si->currentobject->data;
    if(obj->type!=CHORD) 
      if(cursor_to_next_chord()) 
	obj = Denemo.gui->si->currentobject->data;
    
    if(Denemo.gui->si->currentobject && obj->type==CHORD) {
      chord *thechord = obj->object;
      if(thechord->notes) {
	note *thenote = thechord->notes->data;
	if( ((buf[0]&0xf0)==MIDI_NOTE_ON) && buf[2] && buf[1] == (dia_to_midinote (thenote->mid_c_offset) + thenote->enshift)) {
	  gdouble thetime = get_time();
	  Denemo.gui->si->start_player = thetime -  obj->earliest_time;
	  
	  if(thechord->is_tied && cursor_to_next_note()) {
	    obj = Denemo.gui->si->currentobject->data;	   
	  }
	  //IF THE NEXT OBJ IS A REST ADVANCE OVER IT/THEM
	  do {
	    if(!cursor_to_next_note())	//if(!cursor_to_next_chord())	   	      
	      {
		play_until = G_MAXDOUBLE;
		break;
	      }
	    else {
	      obj = Denemo.gui->si->currentobject->data;
	      thechord = obj->object;
	      play_until = obj->earliest_time - SHAVING;
	      //g_print("play until %f\n", play_until);
	    }
	  } 
	  while(!thechord->notes);	    
	}
      }
    } else
      g_warning("Not on a chord");
  } else
    g_warning("Not on an object");
}

//adjusts the note-on volume by preferred dynamic compression and plays the passed event on default backend
void play_adjusted_midi_event(gchar *buf) {
    adjust_midi_velocity(buf, 100 - Denemo.prefs.dynamic_compression);
    play_midi_event(DEFAULT_BACKEND, 0, buf);
}

#define EDITING_MASK (GDK_SHIFT_MASK)  
void handle_midi_event(gchar *buf) {
  //g_print("%x : ready %d %x queue %d\n", midi_capture_on, divert_midi_event!=NULL, (0xFFFFFF & *(gint*)buf), g_queue_get_length(&midi_queue));
  if(midi_capture_on  &&  divert_midi_id==Denemo.gui->id){
    // this is only good for one endianness - FIXME ??
    if( divert_midi_event) {
      *divert_midi_event = (0xFFFFFF & put_get_midiqueue(*(gint*)buf));
      divert_midi_event = NULL;
      gtk_main_quit();
    } else {
      put_midiqueue(*(gint*)buf);
    }   
    return;//this *is* reached
  }
  if( (Denemo.gui->midi_destination & MIDIRECORD) ||
      (Denemo.gui->midi_destination & (MIDIPLAYALONG|MIDICONDUCT))) {
    if(Denemo.gui->midi_destination & MIDIRECORD)
      record_midi(buf,  get_playback_time());
    if(Denemo.gui->midi_destination & (MIDIPLAYALONG))
      advance_until_time(buf);
    else
      play_midi_event(DEFAULT_BACKEND, 0, buf);
  } else {
    if((Denemo.keyboard_state==(GDK_SHIFT_MASK|GDK_LOCK_MASK)) ||
       Denemo.keyboard_state==(GDK_CONTROL_MASK) ||
       Denemo.keyboard_state==(ADDING_MASK) ||
       Denemo.keyboard_state==((ADDING_MASK)|(CHORD_MASK)) ||
       Denemo.keyboard_state==(GDK_CONTROL_MASK|GDK_LOCK_MASK) ||
       (Denemo.keyboard_state==0))
      process_midi_event(buf);
    else
      if(Denemo.keyboard_state==(GDK_SHIFT_MASK) ||
	 Denemo.keyboard_state==(GDK_LOCK_MASK)) {
	  play_adjusted_midi_event(buf);
      }
  }
}


gboolean intercept_midi_event(gint *midi) {
  if(divert_midi_event) {
    infodialog("Not exiting the previous MIDI capture loop");
    g_warning("Cannot return to script");
   // divert_midi_event = NULL;
   // return FALSE;
    set_midi_capture(FALSE);
    g_queue_clear(&midi_queue);
   
  }
  if(g_queue_is_empty(&midi_queue)) {
  divert_midi_event = midi;
  divert_midi_id = Denemo.gui->id;
  set_midi_capture(TRUE);
  gtk_main();
  divert_midi_event = NULL;
  return TRUE;
  } else {
    *midi =  (0xFFFFFF & get_midiqueue());
     //g_print("getting from queue %x\n", *midi);
  }
}


gint get_midi_channel(DenemoStaff *staff) {
  if (!strcmp (staff->midi_instrument->str, "drums")) {
    return 9;
  } else {
    gint tracknumber = Denemo.gui->si->currentstaffnum-1;
    tracknumber = (tracknumber >= 9) ? tracknumber + 1 : tracknumber;
    return tracknumber&0xF;
  }
}


gint get_midi_prognum(DenemoStaff *staff) {
  if (staff->midi_channel == 9) {
    return 0;
  } else {
    return select_program (staff->midi_instrument->str);
  }
}


gint get_midi_port(DenemoStaff *staff) {
  return staff->midi_port;
}



/* change the MIDI output tuning */
void change_tuning(gdouble *cents) {
  gchar buffer[] = {
    0xF0, 0x7F,// 		Universal Real-Time SysEx header

    0x7F, //<device ID> 	ID of target device (7F = all devices)

    0x08,// 		sub-ID#1 = "MIDI Tuning Standard"
	
    0x08,// 		sub-ID#2 = "scale/octave tuning 1-byte form (Real-Time)"

    0x03, /*		channel/options byte 1
			bits 0 to 1 = channel 15 to 16
			bit 2 to 6 = reserved for future expansion*/

    0x7F, // 		channel byte 2 - bits 0 to 6 = channel 8 to 14

    0x7F, //		channel byte 3 - bits 0 to 6 = channel 1 to 7
    0,0,0,0,0,0,0,0,0,0,0,0,
    /*	[ss]		12 byte tuning offset of 12 semitones from C to B
				00H means -64 cents
				40H means 0 cents (equal temperament)
				7FH means +63 cents */

    0xF7	//	EOX

  };
  gint i;
  for(i=0;i<12;i++)
    buffer[i+8] = 64 + (cents[i]+0.5);
  play_midi_event(DEFAULT_BACKEND, 0, buffer);
}

//return the midi key of the passed event if note on, else 0 
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define NOTE_ON                 0x90
int noteon_key(smf_event_t *event) {
if((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_ON)
  return event->midi_buffer[1];
return 0;
}


