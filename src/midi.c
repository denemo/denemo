/* midi.c
 * functions for direct output to /dev/sequencer
 * and direct input from /dev/midi
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Brian Delaney
 */

#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include "smf.h"


#define MIDI_NOTEOFF		0x80
#define MIDI_NOTEON		0x90
#define MIDI_KEY_PRESSURE	0xA0

#define MIDI_CTL_CHANGE		0xB0
#define MIDI_PGM_CHANGE		0xC0
#define MIDI_CHN_PRESSURE	0xD0
#define MIDI_PITCH_BEND		0xE0

#define MIDI_SYSTEM_PREFIX	0xF0


//#include <sys/types.h>
//#include <sys/stat.h>
//#include <fcntl.h>
#include <gtk/gtk.h>

#include <denemo/denemo.h>
#include "draw.h"
#include "audio.h"
#include "jackmidi.h"
#include "instrumentname.h"

static double
midi2hz(int midinum)
{
  double argument = (midinum - 69);
  double expon = (argument / 12);
  return 440 * pow(2, expon);
}



/* 
 *  get the midi channel of the currently selected staff
 */
gint get_midi_channel()
{
  gint tracknumber;
  gint channel;
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (!strcmp (curstaffstruct->midi_instrument->str, "drums"))
    channel = 9;
  else
    {
      tracknumber = Denemo.gui->si->currentstaffnum-1;
      tracknumber = (tracknumber >= 9) ? tracknumber + 1 : tracknumber;
      channel = tracknumber&0xF;
    }
  return channel ; //staff struct uses encoding 0-15
}

gint get_midi_prognum()
{
  gint prognum;
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaffstruct->midi_channel == 9)
    prognum = 0;
  else
    prognum = select_program (curstaffstruct->midi_instrument->str);  
  return prognum;
}

gint get_midi_port()
{
  gint portnumber;
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;

  portnumber = curstaffstruct->midi_port;
  return portnumber; 
}


static gboolean sequencer_absent = TRUE;
/**
 * Initialise the sequencer device ready for immediate playback
 *
 */
gint
midi_init ()
{

  return 0;
}

void playpitch(double pitch, double duration, double volume, int channel) {
  if(!Denemo.prefs.immediateplayback)
    return;
  play_pitch(pitch, duration, volume, channel);
}

void play_midikey(gint key, double duration, double volume, gint channel){
  if (Denemo.prefs.midi_audio_output == Portaudio)
    playpitch(midi2hz(key), duration, volume, channel);
  else if (Denemo.prefs.midi_audio_output == Jack)
    jack_playpitch(key, 1000 /*duration*/);
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    fluid_playpitch(key, 1000 /*duration*/, channel, (int)(0x7f&(int)(volume*127)));
}

/**
 *  Used to play each note in the given chord
 *  (a g_list_foreach function)
 */
static void
playnote (gpointer tone, gpointer chord, int channel)
{
  gint offset;
  gchar key;
  gint voice;
  /* Because mid_c_offset is a measure of notes and we need a measure of
   * half-steps, this array will help */
  const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

  offset = ((note *) tone)->mid_c_offset;

  /* 60 is middle-C in MIDI keys */
  key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
  key += ((note *) tone)->enshift;
  voice = g_list_index ((GList *) chord, tone);

  play_midikey(key, 0.3, 0.5/*Denemo.prefs.pcmvolume*/, channel);

}

/** 
 * This version of the function opens and closes /dev/sequencer w/ each
 * write, as a separate process for performance-type reasons 
 */
void
playnotes (gboolean doit, chord chord_to_play, int channel)
{
  //g_print("playnotes called for channel %d\n", channel);
  if (doit && (sequencer_absent) && chord_to_play.notes) {
    playnote( chord_to_play.notes->data, chord_to_play.notes, channel);
    return;
  }

}

// MIDI input
#include <string.h> /*for memcpy */
#include <math.h>
#include <glib.h>
static  GIOChannel* channel;/* a channel to access /dev/midi by */

#define MAX_MIDI_NOTES (20)
static gint midi_notes[MAX_MIDI_NOTES];
static volatile int count;

static void store_midi_note(gint pitch)
{ 
  if( ++count < MAX_MIDI_NOTES)
    midi_notes[count] = pitch;
}

gint get_midi_note(void) {
  gint ret=-1;
  if(count) {
    ret=midi_notes[count];
    count = 0;
  }
  return ret;
}

void safely_add_track(smf_t *smf, smf_track_t *track) {
  if(track->smf==NULL)
    smf_add_track(smf, track);
}

void safely_track_remove_from_smf(smf_track_t *track) {
 if(track->smf!=NULL)
   smf_track_remove_from_smf(track);
}
/**
 * enter_midi_note_in_score
 * @mid_c_offset enters the midi note steps above/below mid-c
 * @enshift enharmonic adjustment -1 is one flat etc.. 
 */
static void enter_midi_note_in_score (DenemoGUI *gui, gint mid_c_offset, gint enshift, gint octave) {
  gui->last_source = INPUTMIDI;
  gui->si->cursor_y = gui->si->staffletter_y = mid_c_offset;
  gui->si->cursor_y += 7*octave; 
  shiftcursor(gui, mid_c_offset);
  setenshift(gui->si, enshift);
  displayhelper (gui);
}

typedef struct enharmonic
{
  gint mid_c_offset;
  gint enshift;
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

/* look for a new note played into midi input, if
   present insert it into the score/edit the score following the mode */
gint midientry(void) {
  
  DenemoGUI *gui = Denemo.gui;
  if(gui==NULL)
    return TRUE;
  if(gui->si==NULL)
    return TRUE;
  gint notenum;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  notenum = get_midi_note();
  if(notenum < 0) 
    return TRUE;
  enharmonic enote;
  notenum2enharmonic (notenum, &enote.mid_c_offset, &enote.enshift);
  if (Denemo.prefs.midi_audio_output == Portaudio)
    playpitch(midi2hz(notenum), 0.3, 0.5, 0);
  if (Denemo.prefs.midi_audio_output == Jack)
    jack_playpitch(notenum, 300 /*duration*/);
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    fluid_playpitch(notenum, 300 /*duration*/,  curstaffstruct->midi_channel, 0);
 
  if(gui->mode & INPUTEDIT)
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
	    enter_midi_note_in_score(gui, enote.mid_c_offset, enote.enshift, notenum/12 - 5);
	    if(Denemo.gui->si->cursor_appending)
	      break;
	  } while(next_editable_note() && is_tied);
	} else 
	  gdk_beep();
	if(gui->mode & INPUTRHYTHM) {
	  if(measure != gui->si->currentmeasurenum)
	    beep=TRUE;
	  else if(beep) signal_measure_end(), beep=FALSE;
	}
      } else
	enter_midi_note_in_score(gui, enote.mid_c_offset, enote.enshift, notenum/12 - 5);
    } else
    enter_midi_note_in_score(gui, enote.mid_c_offset, enote.enshift, notenum/12 - 5);
  return TRUE;
}

static guint midi_timer;// timer id
#define DEFAULT_TIMER_RATE (5)

void start_midi_input(void) {
  
  if(midi_timer)
    g_source_remove(midi_timer);

  midi_timer = g_timeout_add (DEFAULT_TIMER_RATE, (GSourceFunc)midientry, NULL);

  if(midi_timer==0)
    g_error("Timer id 0 - if valid the code needs re-writing (documentation not clear)");
}







static gint *divert_midi_event;
static gint divert_midi_id=0;//id of the DenemoGUI which wants to intercept midi events

static gboolean midi_capture_on = FALSE;//any midi events not caught by midi_divert will be dropped if this is true

gboolean set_midi_capture(gboolean set) {
  gboolean ret = midi_capture_on;
  midi_capture_on = set;
  return ret;
}

#define command ((*buf)&0xFF)
#define notenumber ((*(buf+1))&0xFF)
#define velocity ((*(buf+2))&0xFF)
void process_midi_event(gchar *buf) {
  //g_print("process midi (%s) %x %x %x\n",divert_midi_event?"diverted":"straight", command, notenumber, velocity);
  if(divert_midi_event &&  divert_midi_id==Denemo.gui->id){
    // this is only good for one endianness - FIXME
    *divert_midi_event = 0;//clear 4th byte
    memcpy(divert_midi_event, buf, 3);//midi events are up to three bytes long
    gtk_main_quit();
    return;// not reached
  }
  if(midi_capture_on) {
    gdk_beep();
    g_warning("MIDI event dropped");
  } else {
    if(command==MIDI_NOTEON && velocity==0) {//Zero velocity NOTEON is used as NOTEOFF by some MIDI controllers
      buf[0]=MIDI_NOTEOFF;
      buf[2]=128;
    }
    if(command==MIDI_NOTEON)
      store_midi_note(notenumber);
  }
}

gboolean intercept_midi_event(gint *midi) {
  if(divert_midi_event) {
    infodialog("Recursive midi capture not possible!");/* we could make a stack of them instead ... */
    divert_midi_event = NULL;
    return FALSE;
  }
  divert_midi_event = midi;
  divert_midi_id = Denemo.gui->id;
  gtk_main();
  divert_midi_event = NULL;
  return TRUE;
}
static int
process_callback (GIOChannel *source, GIOCondition condition, gchar * data)
{
  GError *error=NULL;
  gsize bytes_read;
  static gchar buf[3];
  if(channel==NULL)
    return FALSE;//shutdown
  if(channel!=source)
    return FALSE;//shutdown

  //  g_print("Channel %p source %p is %d\n", channel, source, source->is_readable);
  g_io_channel_read_chars (source, buf, 1, &bytes_read, &error);

  if(command==MIDI_SYSTEM_PREFIX) {
    while(command!=0xF7)
      g_io_channel_read_chars (source, buf, 1, &bytes_read, &error);
    return TRUE;
  }
  if(command)
    switch(command) {
    case MIDI_NOTEON:
     
    case MIDI_NOTEOFF:
    case MIDI_KEY_PRESSURE:
    case MIDI_CTL_CHANGE:
    case MIDI_PITCH_BEND:
    case 0xF2:
      {
	g_io_channel_read_chars (source, buf+1, 1, &bytes_read, &error);
	g_io_channel_read_chars (source, buf+2, 1, &bytes_read, &error);            
      }
      if(command==MIDI_NOTEON && velocity==0) {//Zero velocity NOTEON is used as NOTEOFF by some MIDI controllers
	buf[0]=MIDI_NOTEOFF;
	buf[2]=128;
      }
      process_midi_event(buf);
      break;
    case MIDI_PGM_CHANGE:
    case MIDI_CHN_PRESSURE:
    case 0xF3:
      g_io_channel_read_chars (source, buf+1, 1, &bytes_read, &error);
      break;
    default:
      break; 
    }
  return TRUE; 
}


gint init_midi_input(void) {
  gint ret = -1;
  if (Denemo.prefs.midi_audio_output == Jack)
    ret = jackmidi_server_running() ? 0 : -1;
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    ret = fluid_start_midi_in();
  else if (Denemo.prefs.midi_audio_output == Portaudio){
    GError *error = NULL;
    if(!channel)
      channel =  g_io_channel_new_file (Denemo.prefs.midi_in->str,"r", &error);
    if(error)
      ret = -1;
    g_io_channel_set_encoding       (channel,NULL/* raw binary */,
                                             &error);
    if(error)
      ret = -2;
    g_io_add_watch_full(channel, G_PRIORITY_HIGH,G_IO_IN|G_IO_PRI, (GIOFunc) process_callback,NULL, NULL);
    //  g_io_add_watch (channel,G_IO_IN, (GIOFunc) process_callback,NULL, NULL);
    ret = 0;
  }
  start_midi_input();
  return ret;
}

gint stop_midi_input(void) {
  if (Denemo.prefs.midi_audio_output == Jack)
    stop_jack();
  else if (Denemo.prefs.midi_audio_output == Fluidsynth)
    return fluid_stop_midi_in();
  else if (Denemo.prefs.midi_audio_output == Portaudio){
    GError *error = NULL;
    if(channel)
      g_io_channel_shutdown(channel, FALSE, &error);
    if(error)
      g_warning("%s", error->message);
    else
      channel = NULL; 
  }
return 0;
}


/* returns the system time in seconds */
gdouble get_time(void)
{
  GTimeVal tv;
  
  double          seconds;
  g_get_current_time(&tv);
  
  seconds = tv.tv_sec + tv.tv_usec / 1000000.0;
  return seconds;
}


gdouble generate_midi(void) {
  return exportmidi(NULL, Denemo.gui->si, 0, 0);
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
  smf_event_t *event;
  smf_event_t *initial = smf_peek_next_event(smf);
  if(time<0.0)
    return NULL;
  gdouble total = smf_get_length_seconds(smf);
  time = (time>total?total:time);
  smf_seek_to_seconds(smf, time);
  do {
    event = smf_get_next_event(smf);
  } while(event && (!(event->midi_buffer[0] & MIDI_NOTEON) || !event->user_pointer));
  if(initial)
    smf_seek_to_event(smf, initial);
  if(event)
    return (DenemoObject *)(event->user_pointer);
  return NULL;
}

DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time) {
  smf_event_t *event;
  smf_event_t *initial = smf_peek_next_event(smf);
  if(time<0.0)
    return NULL;
  gdouble total = smf_get_length_seconds(smf);
  time = (time>total?total:time);
  smf_seek_to_seconds(smf, time);
  do {
    event = smf_get_next_event(smf);
  } while(event && (!(event->midi_buffer[0] & MIDI_NOTEOFF) || !event->user_pointer));
  if(initial)
    smf_seek_to_event(smf, initial);
  if(event)
    return (DenemoObject *)(event->user_pointer);
  return NULL;
}
