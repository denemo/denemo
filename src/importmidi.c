/*importmidi.c
 * midi file import functions
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005 AJAnderson
 *
 * 	TODO
 *  
 *  check to see if memory used by malloc is freed
 *  clean up/optimise code
 *  add functions to add notes to chords
 *  create a define to initialize mididata->*?
 *
 *  change lastoff name to cursor_time
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
#include "importmidi.h"
#include "staffops.h"
#include "smf.h"
#include "view.h"
#include "utils.h"
#include "file.h"
#include "commandfuncs.h"
#include "processstaffname.h"

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
//0x0F ??

typedef struct notetype
{
	gint notetype;
	gint numofdots;
	gint tied;
}notetype;

typedef struct nstack 
{
	gint pitch;	/* base pitch or lowest pitch possibly redundant because of chordnotes */
     	gint timeon;  /* tick number when the note starts*/
	gint on_delta_time; /* Is this NEEDED? this is time between last event and this notes start */
     	gint duration; /* length of time in ticks */ 
	gint tracknum;	
}nstack;

typedef struct mididata
{
	nstack currentnote; /* current note being processed */
	gint leftover; /* note/rest value that is leftover across the measure */
	gint bartime; /* time relative to barlength 0-barlength */
	gint delta_time; /* distance between notes */
	gint barlength; /* amount of time in measure */
	gint lastoff; /* starttime + duration. The time when the note is finished */
	gint event_number; /* smf event number that is currently being processed */
	gint track; /* the current track that is being read */
	smf_t *smf;
	smf_track_t *selected_track;
}mididata;

typedef struct harmonic
{
	gint pitch;
	gint enshift;
}harmonic;

static struct harmonic enharmonic(gint input, gint key);
static int readtrack();
void AddStaff();
void dotimesig(gint numerator, gint denominator);
void dokeysig(gint key, gint isminor);
void dotempo(gint tempo);
void dotrackname(gchar *name);
void doinstrname(gchar *name);
void donoteon(gint pitchon, gint velocity, gint timeon);
void donoteoff();
struct notetype ConvertLength(gint duration);
void process_list();
static gint Get_Smf_Note_OFF (gint pitch, gint timeon, gint delta_time);


mididata mdata;	
	
static void
note_from_int(char *buf, int note_number)
{
	int note, octave;
	char *names[] = {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"};

	octave = note_number / 12 - 1;
	note = note_number % 12;

	sprintf(buf, "%s%d", names[note], octave);
}

static int
cmd_load(char *file_name)
{
	if (mdata.smf != NULL)
		smf_delete(mdata.smf);

	mdata.selected_track = NULL;

	mdata.smf = smf_load(file_name);
	if (mdata.smf == NULL) {
		g_critical("Couldn't load '%s'.", file_name);

		mdata.smf = smf_new();
		if (mdata.smf == NULL) {
			g_critical("Cannot initialize smf_t.");
			return -1;
		}

		return -2;
	}

	g_message("File '%s' loaded.", file_name);
	g_message("%s.", smf_decode(mdata.smf));

	mdata.selected_track = smf_get_track_by_number(mdata.smf, 1);

	return 0;
}

#define BUFFER_SIZE 1024

void
decode_metadata(const smf_event_t *event)
{
	int off = 0, mspqn, flats, isminor;
	char *buf;

	static const char *const major_keys[] = {"Fb", "Cb", "Gb", "Db", "Ab",
		"Eb", "Bb", "F", "C", "G", "D", "A", "E", "B", "F#", "C#", "G#"};

	static const char *const minor_keys[] = {"Dbm", "Abm", "Ebm", "Bbm", "Fm",
		"Cm", "Gm", "Dm", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m", "A#m", "E#m"};

	//assert(smf_event_is_metadata(event));

	switch (event->midi_buffer[1]) {
		case TEXT:
			//return smf_event_decode_textual(event, "Text");

		case COPYRIGHT:
			//return smf_event_decode_textual(event, "Copyright");

		case META_TRACK_NAME:
			//return smf_event_decode_textual(event, "Sequence/Track Name");
			dotrackname(smf_event_extract_text(event));

		case META_INSTR_NAME:
			//printf("\nInstrument text = %s\n", smf_string_from_event(event));
			doinstrname(smf_event_extract_text(event));
		case 0x05:
			//return smf_event_decode_textual(event, "Lyric");

		case 0x06:
			//return smf_event_decode_textual(event, "Marker");

		case 0x07:
			//return smf_event_decode_textual(event, "Cue Point");

		case 0x08:
			//return smf_event_decode_textual(event, "Program Name");

		case 0x09:
			//return smf_event_decode_textual(event, "Device (Port) Name");

		default:
			break;
	}

	buf = malloc(BUFFER_SIZE);
	if (buf == NULL) {
		g_critical("smf_event_decode_metadata: malloc failed.");
	}

	switch (event->midi_buffer[1]) {
		case 0x00:
			off += snprintf(buf + off, BUFFER_SIZE - off, "Sequence number");
			break;

		/* http://music.columbia.edu/pipermail/music-dsp/2004-August/061196.html */
		case 0x20:
			if (event->midi_buffer_length < 4) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			off += snprintf(buf + off, BUFFER_SIZE - off, "Channel Prefix: %d.", event->midi_buffer[3]);
			break;

		case 0x21:
			if (event->midi_buffer_length < 4) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			off += snprintf(buf + off, BUFFER_SIZE - off, "Midi Port: %d.", event->midi_buffer[3]);
			break;

		case 0x2F:
			off += snprintf(buf + off, BUFFER_SIZE - off, "End Of Track");
			break;

		case META_TEMPO:
			if (event->midi_buffer_length < 6) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			mspqn = (event->midi_buffer[3] << 16) + (event->midi_buffer[4] << 8) + event->midi_buffer[5];
		        
			dotempo(mspqn);

			break;

		case 0x54:
			off += snprintf(buf + off, BUFFER_SIZE - off, "SMPTE Offset");
			break;

		case META_TIMESIG:
			if (event->midi_buffer_length < 7) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			dotimesig(event->midi_buffer[3], (int)pow(2, event->midi_buffer[4]));
			break;

		case META_KEYSIG:
			if (event->midi_buffer_length < 5) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			flats = event->midi_buffer[3];
			isminor = event->midi_buffer[4];

			if (isminor != 0 && isminor != 1) {
				g_critical("smf_event_decode_metadata: last byte of the Key Signature event has invalid value %d.", isminor);
				goto error;
			}

			dokeysig(isminor, flats);
			break;

		case 0x7F:
			off += snprintf(buf + off, BUFFER_SIZE - off, "Proprietary (aka Sequencer) Event, length %d",
				event->midi_buffer_length);
			break;

		default:
			goto error;
	}

error:
	free(buf);
}

void 
decode_midi_event(const smf_event_t *event)
{
	int off = 0, channel;
	char note[5];

	/* + 1, because user-visible channels used to be in range <1-16>. */
	channel = (event->midi_buffer[0] & 0x0F) + 1;

	switch (event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) {
		case NOTE_OFF:
			note_from_int(note, event->midi_buffer[1]);
			g_debug("\nNote Off channel %d note %s velocity %d\n", 
					channel, note, event->midi_buffer[2]);
			donoteoff ();
			break;

		case NOTE_ON:
			note_from_int(note, event->midi_buffer[1]);
			g_debug("\nNote On channel %d note %s velocity %d\n", 
					channel, note, event->midi_buffer[2]);

                        if (0 == event->midi_buffer[2]) { // actually note off
			    donoteoff ();
                        } // actually note off
                        else { // really note on
			    mdata.delta_time = event->delta_time_pulses;
			    mdata.event_number = event->event_number;
			    donoteon(event->midi_buffer[1],  event->midi_buffer[2], event->time_pulses);
                        } // really note on
			break;

		case AFTERTOUCH:
			note_from_int(note, event->midi_buffer[1]);
			g_debug("\nAftertouch channel %d note %s velocity %d\n", 
					channel, note, event->midi_buffer[2]);
			break;

		case CTRL_CHANGE:
			g_debug("\nController channel %d controller %d value %d\n", 
					channel, event->midi_buffer[1], event->midi_buffer[2]);
			break;

		case PGM_CHANGE:
			g_debug("\nProgram Change channel %d controller %d\n", 
					channel, event->midi_buffer[1]);
			break;

		case CHNL_PRESSURE:
			g_debug("\nChannel Pressure channel %d pressure %d\n", 
					channel, event->midi_buffer[1]);
			break;

		case PCH_WHEEL:
			g_debug("\nPitch Wheel channel %d value %d\n", 
					channel, ((int)event->midi_buffer[2] << 7) | (int)event->midi_buffer[2]);
			break;

		default:
			break;
	}
}

static int
process_midi(smf_event_t *event)
{
	int off = 0, i;

	if (smf_event_is_metadata(event))
		decode_metadata(event);
	else
		decode_midi_event(event);
	
	return 0;
}

int
process_track(smf_track_t *track) //TODO remove mdata here
{
	smf_event_t *event;

  	while (event = smf_track_get_next_event(track)) {
	  process_midi(event); 
	}
}

static int
readtrack()
{
	smf_event_t *event;

	if (mdata.selected_track == NULL) {
		g_critical("No track");
		return -1;
	}

	smf_rewind(mdata.smf);
       
        while (mdata.track <= mdata.smf->number_of_tracks){	
	  mdata.selected_track = smf_get_track_by_number(mdata.smf, mdata.track);
	  while ((event = smf_track_get_next_event(mdata.selected_track)) != NULL) {
		/* Do something with the event */
		process_track(mdata.selected_track);
	  }
	  mdata.track++;
	  if (mdata.track+1 <= mdata.smf->number_of_tracks)
	    AddStaff();
	}
	smf_rewind(mdata.smf);

	return 0;
}

static void
insert_note_into_score(notetype length)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  gint i;

  /* 0-8 accepted bellow */
  gchar *proc = g_strdup_printf("(d-Insert%d)", length.notetype);
  call_out_to_guile(proc);
  g_free(proc);
  
  /* get correct note name */
  gint key = curstaffstruct->keysig.number;
  harmonic enote = enharmonic (mdata.currentnote.pitch, (gint) key);
  gchar *name =  mid_c_offsettolily (enote.pitch, enote.enshift);
  
  /* Rename note to the correct note */
  gchar *accidental = g_strdup_printf("(d-ChangeChordNotes \"%s\")", name);
  call_out_to_guile(accidental);
  g_free(accidental);

  /* Add dots */
  for (i=0;i<length.numofdots;i++)
    add_dot_key (gui);

  /* insert tie */
  if (length.tied)
    call_out_to_guile("(d-ToggleTie)");
}	

static void
insert_rest_into_score(notetype length)
{
  DenemoGUI *gui = Denemo.gui;
  gint i;

  switch (length.notetype)
  {
     case 0:
       insert_rest_0key (gui);
       break;
     case 1:
       insert_rest_1key (gui);
       break;
     case 2:
       insert_rest_2key (gui);
       break;
     case 3:
       insert_rest_3key (gui);
       break;
     case 4:
       insert_rest_4key (gui);
       break;
     case 5:
       insert_rest_5key (gui);
       break;
     case 6:
       insert_rest_6key (gui);
       break;
     default:
       break;
  }
  displayhelper (gui);

  /* add dots */
  for (i=0;i<length.numofdots;i++)
    add_dot_key (gui);
  
  /* Insert tie */
  if (length.tied)
    call_out_to_guile("(d-ToggleTie)");

}
/**
 * Insert time signature into current staff 
 *
 */
void
dotimesig (gint numerator, gint denominator)
{
  DenemoGUI *gui = Denemo.gui;
  /*only does initial TS */
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;

  curstaffstruct->timesig.time1 = numerator;
  curstaffstruct->timesig.time2 = denominator;

  mdata.barlength = mdata.smf->ppqn * 4 * numerator / denominator;
}

/**
 * Insert key signature into the current staff
 *
 */
void
dokeysig (gint isminor, gint key)
{
  DenemoGUI *gui = Denemo.gui;
  if (key > 7)
    key = key - 256;		/*get flat key num, see keysigdialog.cpp */
#ifdef DEBUG
  g_print("\nkey = %d\n", key); 
#endif
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  curstaffstruct->keysig.number = key;
  curstaffstruct->keysig.isminor = isminor;
  dnm_setinitialkeysig (curstaffstruct, key, isminor);
}

void
dotempo (gint tempo)
{ 
  DenemoGUI *gui = Denemo.gui;
  gui->si->tempo = (gint) (6.0e7 / (double) tempo);
}

void
dotrackname (gchar *name)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  
  curstaffstruct->denemo_name->str = g_strdup(name);
  //set_lily_name (curstaffstruct->denemo_name, curstaffstruct->lily_name);
}

void
doinstrname (gchar* name)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;

  curstaffstruct->midi_instrument->str = g_strdup(name);
}

static nstack 
stack (gint pitch, gint timeon, gint delta_time, gint duration, gint tracknum)
{
  nstack mystack;
  mystack.pitch = pitch;
  mystack.timeon = timeon;
  mystack.on_delta_time = delta_time;
  mystack.duration = duration;
  mystack.tracknum = tracknum;
  return mystack;
}

/**
 * Process note on command 
 */
void
donoteon (gint pitchon, gint velocity, gint timeon)
{
  gint delta_time = mdata.delta_time; /*is this needed????*/
  
  /* add a note to the stack */
  Get_Smf_Note_OFF(pitchon, timeon, delta_time);
#ifdef DEBUG
  g_print ("\npitchon = %d timeon = %d event = %d\n", (gint) pitchon, (gint) timeon, (gint) mdata.event_number);
#endif
}

/**
 * Process note off command
 */
void
donoteoff ()
{
   /*process the note found*/
   process_list(mdata);	
}

/** 
 * extremely simple quantizer that rounds 
 * to the closest granule size
 */
static int
round2granule(int tick)
{
  gint smallestgrain = 48;
  gdouble div = ((gdouble) tick / (gdouble) smallestgrain);
  return smallestgrain * (gint) round(div);
}

static gint
Get_Smf_Note_OFF (gint pitch, gint timeon, gint delta_time){
  gint event_number = mdata.event_number;
  smf_event_t *event;
  gint starttime;
  gint duration;
  gint tracknum = mdata.track;
  gboolean chordtone = FALSE;

  while ((event = smf_track_get_event_by_number(mdata.selected_track, event_number)) != NULL){
    if (   (((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_OFF) 
		    && (event->midi_buffer[1] == (int) pitch))
        || (((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_ON) 
		    && (event->midi_buffer[1] == (int) pitch)
		    && (0 == event->midi_buffer[2]))
       ){
	duration = round2granule(event->time_pulses - timeon); 
	starttime = round2granule(timeon); //simple quantize timeon
	g_print("\nFound corresponding note off to pitch %d timeon = %d duration = %d\n", (gint) pitch, timeon, (gint) duration);
    	break;
    }
    event_number++;
  }
  return 0;
}

static harmonic
enharmonic (gint input, gint key)
{
  harmonic local;
  local.pitch = (input / 12) - 5;
  local.enshift = input % 12;
  
  switch (local.enshift)
    {
    case 0:			//c
      {
	local.pitch = (key > 6) ? (-1 + local.pitch * 7) : (local.pitch * 7);
	local.enshift = (key > 6) ? (1) : (0);
	break;
      }
    case 1:			//c#
      {
	local.pitch = (key < -3) ? (1 + local.pitch * 7) : (local.pitch * 7);
	local.enshift = (key < -3) ? (-1) : (1);
	break;
      }
    case 2:			//D
      {
	local.pitch = 1 + local.pitch * 7;
	local.enshift = 0;
	break;
      }
    case 3:			//D#
      {
	local.pitch =
	  (key < -1) ? (2 + local.pitch * 7) : (1 + local.pitch * 7);
	local.enshift = (key < -1) ? (-1) : (1);
	break;
      }
    case 4:			//E
      {
	local.pitch =
	  (key < -6) ? (3 + local.pitch * 7) : (2 + local.pitch * 7);
	local.enshift = (key < -6) ? (-1) : (0);
	break;
      }
    case 5:			//F
      {
	local.pitch =
	  (key > 5) ? (2 + local.pitch * 7) : (3 + local.pitch * 7);
	local.enshift = (key > 5) ? (1) : (0);
	break;
      }
    case 6:			//F#
      {
	local.pitch =
	  (key < -4) ? (4 + local.pitch * 7) : (3 + local.pitch * 7);
	local.enshift = (key < -4) ? (-1) : (1);
	break;
      }
    case 7:			//G
      {
	local.pitch = 4 + local.pitch * 7;
	local.enshift = 0;
	break;
      }
    case 8:			//G#
      {
	local.pitch =
	  (key < -2) ? (5 + local.pitch * 7) : (4 + local.pitch * 7);
	local.enshift = (key < -2) ? (-1) : (1);
	break;
      }
    case 9:			//A
      {
	local.pitch = 5 + local.pitch * 7;
	local.enshift = 0;
	break;
      }
    case 10:			//A#
      {
	local.pitch =
	  (key < 0) ? (6 + local.pitch * 7) : (5 + local.pitch * 7);
	local.enshift = (key < 0) ? (-1) : (1);
	break;
      }
    case 11:			//B
      {
	local.pitch =
	  (key < -5) ? (7 + local.pitch * 7) : (6 + local.pitch * 7);
	local.enshift = (key < -5) ? (-1) : (0);
	break;
      }
    };
  return local;
}
int
ConvertNoteType2ticks(notetype *gnotetype){
  gint ticks;
  gint PPQN = mdata.smf->ppqn;
  gint notetype = (int) gnotetype->notetype;
  gint numofdots = (int) gnotetype->numofdots;
  gint dsq = (4 * PPQN);
  gint i = 0;

  ticks = dsq >> notetype;
  while (i++ < numofdots) 
    ticks += dsq >> (notetype + 1);

  return ticks;
}

notetype ConvertLength(gint duration){
 /*convert length to 2 = quarter, 1 = half, 0 = whole etc...... */
	/* quarter = 384, half = 768, whole = 1536*/
  notetype gnotetype;
  gint PPQN = mdata.smf->ppqn;
  gint notetype = 0;
  gint numofdots = 0;
  gint tied = 0;
  gint leftover = 0;
  gint dsq = (4 * PPQN);
  g_debug("\nDuration = %d ticks\n", duration);
  
  while ((dsq >> notetype) > duration)
	  notetype++;
	  
  leftover = duration - (dsq >> notetype); 

  while (leftover >= (dsq >> (notetype +1))){
  	leftover -= (dsq >> (notetype +1));
	numofdots++;
  }
  
  gnotetype.notetype = notetype;
  gnotetype.numofdots = numofdots;
  gnotetype.tied = leftover;
  return gnotetype;
}

void AddStaff(){
  call_out_to_guile("(d-AddAfter)");
  mdata.lastoff = 0;
  mdata.bartime = 0;
}

/** 
 * check
 * to see if we need to add a new measure 
 */
void MeasureCheck(){
  DenemoGUI *gui = Denemo.gui;
  if (mdata.bartime >= mdata.barlength)	
  {			/* mdata.bartime >= barlenth will be true if there are rests  or notes
			   going over end of measures. */
    if (!gui->si->currentmeasure->next)
      call_out_to_guile("(d-AddMeasure)");
    else
      call_out_to_guile("(d-MeasureRight)");
    mdata.bartime = 0;
  }
}

void RestCheck(){
  gint rest = 0;
  gint ticks;
  gint starttime = (int) mdata.currentnote.timeon;
  gint duration = (int) mdata.currentnote.duration;
  gint on_delta_time = (int) mdata.currentnote.on_delta_time;

  if (duration == 0)
    return;
  if (starttime > mdata.lastoff){
    rest = starttime - mdata.lastoff;
    g_debug("\nRest = %d\n",rest);
    if (mdata.bartime + on_delta_time  >= (mdata.barlength)){
      g_debug("\nmdata.bartime + on_delta_time  >= mdata.barlength\n");
      while(mdata.barlength - mdata.bartime){
        rest = mdata.barlength - mdata.bartime;
        struct notetype length = ConvertLength(rest);
        insert_rest_into_score(length);
	ticks = ConvertNoteType2ticks(&length);
        mdata.bartime += ticks;
        mdata.lastoff += ticks;
      } 
      MeasureCheck(mdata);		          
      rest = 0;/* I am not sure if this is the best choice here*/
    }
    while (rest){
        struct notetype length = ConvertLength(rest);
	insert_rest_into_score(length);
	ticks = ConvertNoteType2ticks(&length);
	mdata.bartime += ticks;
	mdata.lastoff += ticks;
	rest -= ticks;
	g_debug("\nbartime = %d, lastoff = %d\n", mdata.bartime, mdata.lastoff);
    }
  }
}

/**
 * This takes a note and calculates if the note is to big to fit in the measure. 
 * If it is it fits what it can fit in the measure and ties it to the 
 * remainder in the next measure.
 */
void TiedNoteCheck(){
	gint duration = (int) mdata.currentnote.duration;
  	gint barlength = (int) mdata.barlength;
  
	if ((mdata.bartime + duration) > barlength)
	{
	  	mdata.leftover = (int) (duration - (barlength - mdata.bartime));	/*tied over bar line */
	  	duration = (int) (barlength - mdata.bartime); /*new value that fits in bar*/
		mdata.currentnote.duration = duration; 
	}
      	else 
		mdata.leftover = (int) 0;
}

void ProcessNote() {
	DenemoGUI *gui = Denemo.gui;
	DenemoScore *si = gui->si;
	gint starttime = (int) mdata.currentnote.timeon;
	gint lastoff = mdata.lastoff;
	gint duration = (int) mdata.currentnote.duration;
	gint pitch = (int) mdata.currentnote.pitch;
	if (duration == 0)
  	  return;
        g_debug("\nProcessing pitch %d\n",pitch);	        
	/*if a noteon event happends just after anouther note finishes just add the next note */
	if (starttime == mdata.lastoff) {
		notetype length = ConvertLength(duration);
		length.tied = mdata.leftover;
		insert_note_into_score(length);
		mdata.lastoff = starttime + duration;
		mdata.bartime += duration;
	}
	if (mdata.leftover){
		MeasureCheck();
		notetype tied_length = ConvertLength(mdata.leftover);
		insert_note_into_score(tied_length);
		mdata.lastoff += mdata.leftover;
		mdata.bartime += mdata.leftover;
		mdata.leftover = 0;
	}
	/* if the starttime of noteon is not were we left off we need to do something */
	if ((starttime != mdata.lastoff) && (mdata.leftover !=0)){
		mdata.leftover = 0;
		mdata.currentnote.timeon = mdata.lastoff;
		process_list();
	}
}

/**
 * Add notes to the current staff
 */
void
process_list()
{
  /* check to see if we need to add a measure */
  MeasureCheck();  	
  /*check for rests*/
  RestCheck();
  /*check for notes tied across measure*/
  TiedNoteCheck();	
  /* note processing stuff */
  ProcessNote();  
}

gint
importMidi (gchar *filename, DenemoGUI *gui)
{
  mdata.selected_track = NULL;
  mdata.smf = NULL;
  mdata.bartime = 0;
  mdata.lastoff = 0;
  mdata.track = 1;
  gint ret = 0;	// (-1 on failure)

  /* delete old data in the score */
  deletescore (NULL, gui);

  /* load the file */
  ret = cmd_load(mdata, filename);
  /* Read Track Data */ 
  readtrack(mdata);

  return ret;
}
