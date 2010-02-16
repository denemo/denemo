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
	GList *chordnotes; /* list of notes that go along with that object */
	gint pitch;	/* base pitch or lowest pitch possibly redundant because of chordnotes */
     	gint timeon;  /* tick number when the note starts*/
	gint on_delta_time; /* Is this NEEDED? this is time between last event and this notes start */
     	gint duration; /* length of time in ticks */ 
	gint tracknum;	
}nstack;

typedef struct midicallback
{
	DenemoGUI *gui;
	nstack *currentnote; /* current note being processed */
	GList *notestack; /* polyphonic voices notes that need reprocessing */
	gint leftover; /* note/rest value that is leftover across the measure */
	gint bartime; /* time relative to barlength 0-barlength */
	gint delta_time; /* distance between notes */
	gint barlength; /* amount of time in measure */
	gint lastoff; /* starttime + duration. The time when the note is finished */
	gint event_number; /* smf event number that is currently being processed */
	gint key;   /* current key sig */
	gint track; /* the current track that is being read */
	smf_t *smf;
	smf_track_t *selected_track;
}midicallback;

typedef struct harmonic
{
	gint pitch;
	gint enshift;
}harmonic;

static struct harmonic enharmonic(gint input, gint key);
static int readtrack(midicallback *mididata);
void dotimesig(gint numerator, gint denominator, midicallback *mididata);
void dokeysig(gint key, gint isminor, midicallback *mididata);
void dotempo(gint tempo,  midicallback *mididata);
void dotrackname(gchar *name, midicallback *mididata);
void doinstrname(gchar *name, midicallback *mididata);
void donoteon(midicallback *mididata, gint pitchon, gint velocity, gint timeon);
void donoteoff(midicallback *mididata);
struct notetype ConvertLength(gint duration, midicallback *mididata);
void ProcessNoteStack(midicallback *mididata);
void process_list(midicallback *mididata);
static gint Get_Smf_Note_OFF (gint pitch, gint timeon, gint delta_time, midicallback *mididata);

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
cmd_load(midicallback *mididata, char *file_name)
{
	if (mididata->smf != NULL)
		smf_delete(mididata->smf);

	mididata->selected_track = NULL;

	mididata->smf = smf_load(file_name);
	if (mididata->smf == NULL) {
		g_critical("Couldn't load '%s'.", file_name);

		mididata->smf = smf_new();
		if (mididata->smf == NULL) {
			g_critical("Cannot initialize smf_t.");
			return -1;
		}

		return -2;
	}

	g_message("File '%s' loaded.", file_name);
	g_message("%s.", smf_decode(mididata->smf));

	mididata->selected_track = smf_get_track_by_number(mididata->smf, 1);

	return 0;
}

#define BUFFER_SIZE 1024

void
decode_metadata(const smf_event_t *event, midicallback *mididata)
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
			dotrackname(smf_event_extract_text(event),mididata);

		case META_INSTR_NAME:
			//printf("\nInstrument text = %s\n", smf_string_from_event(event));
			doinstrname(smf_event_extract_text(event), mididata);
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
		        
			dotempo(mspqn, mididata);

			break;

		case 0x54:
			off += snprintf(buf + off, BUFFER_SIZE - off, "SMPTE Offset");
			break;

		case META_TIMESIG:
			if (event->midi_buffer_length < 7) {
				g_critical("smf_event_decode_metadata: truncated MIDI message.");
				goto error;
			}

			dotimesig(event->midi_buffer[3], (int)pow(2, event->midi_buffer[4]), mididata);
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

			dokeysig(isminor, flats, mididata);
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
decode_midi_event(const smf_event_t *event, midicallback *mididata)
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
			donoteoff (mididata);
			break;

		case NOTE_ON:
			note_from_int(note, event->midi_buffer[1]);
			g_debug("\nNote On channel %d note %s velocity %d\n", 
					channel, note, event->midi_buffer[2]);

                        if (0 == event->midi_buffer[2]) { // actually note off
			    donoteoff (mididata);
                        } // actually note off
                        else { // really note on
			    mididata->delta_time = event->delta_time_pulses;
			    mididata->event_number = event->event_number;
			    donoteon(mididata,  event->midi_buffer[1],  event->midi_buffer[2], event->time_pulses);
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
process_midi(smf_event_t *event, midicallback *mididata)
{
	int off = 0, i;

	if (smf_event_is_metadata(event))
		decode_metadata(event, mididata);
	else
		decode_midi_event(event, mididata);
	
	return 0;
}

static int
readtrack(midicallback *mididata)
{
	smf_event_t *event;

	if (mididata->selected_track == NULL) {
		g_critical("No track");
		return -1;
	}

	smf_rewind(mididata->smf);
       
        while (mididata->track <= mididata->smf->number_of_tracks){	
	  /* process polyphonic voices */
	  if (mididata->notestack != NULL)
		ProcessNoteStack(mididata);
	  mididata->selected_track = smf_get_track_by_number(mididata->smf, mididata->track);
	  while ((event = smf_track_get_next_event(mididata->selected_track)) != NULL) {
		/* Do something with the event */
		process_midi(event, mididata);
	  }
	  mididata->track++;
	}
	smf_rewind(mididata->smf);

	return 0;
}

/* TODO consolidate this into one function */
void
new_dnm_object(midicallback *mididata, nstack *currentnote, notetype length, gboolean is_note){
	DenemoObject *mudela_obj_new;
	mudela_obj_new = dnm_newchord (length.notetype, length.numofdots, length.tied);
	if (is_note){
	  harmonic enote = enharmonic ((int) mididata->currentnote->pitch,mididata->key);
	  dnm_addtone (mudela_obj_new, enote.pitch, enote.enshift, mididata->gui->si->cursorclef);
	  while (mididata->currentnote->chordnotes){
		  struct harmonic enote = enharmonic ((intptr_t) 
				 mididata->currentnote->chordnotes->data,mididata->key);
	          dnm_addtone (mudela_obj_new, enote.pitch, enote.enshift, mididata->gui->si->cursorclef);		
		  mididata->currentnote->chordnotes = g_list_remove_link(mididata->currentnote->chordnotes, 
				  g_list_first (mididata->currentnote->chordnotes));
	  }
	}
	object_insert (mididata->gui, mudela_obj_new);	
}

static void
insert_note_into_score(midicallback *mididata, notetype length)
{
  DenemoGUI *gui = Denemo.gui;
  gint i;

  /* 0-8 accepted bellow */
  gchar *proc = g_strdup_printf("(d-Insert%d)", length.notetype);
  call_out_to_guile(proc);
  g_free(proc);
  
  /* get correct note name */
  harmonic enote = enharmonic ((int) mididata->currentnote->pitch,mididata->key);
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
insert_rest_into_score(midicallback *mididata, notetype length)
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
dotimesig (gint numerator, gint denominator, midicallback *mididata)
{
  /*only does initial TS */
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;

  curstaffstruct->timesig.time1 = numerator;
  curstaffstruct->timesig.time2 = denominator;

  mididata->barlength = mididata->smf->ppqn * 4 * numerator / denominator;
}

/**
 * Insert key signature into the current staff
 *
 */
void
dokeysig (gint isminor, gint key, midicallback *mididata)
{

  if (key > 7)
    key = key - 256;		/*get flat key num, see keysigdialog.cpp */
#ifdef DEBUG
  g_print("\nkey = %d\n", key); 
#endif
  mididata->key = key;
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;
  curstaffstruct->keysig.number = key;
  curstaffstruct->keysig.isminor = isminor;
  dnm_setinitialkeysig (curstaffstruct, key, isminor);
}

void
dotempo (gint tempo, midicallback *mididata)
{ 
  mididata->gui->si->tempo = (gint) (6.0e7 / (double) tempo);
}

void
dotrackname (gchar *name, midicallback *mididata)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;
  
  curstaffstruct->denemo_name->str = g_strdup(name);
  //set_lily_name (curstaffstruct->denemo_name, curstaffstruct->lily_name);
}

void
doinstrname (gchar* name,  midicallback *mididata)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;

  curstaffstruct->midi_instrument->str = g_strdup(name);
}

static nstack *
stack (gint pitch, gint timeon, gint delta_time, gint duration, gint tracknum)
{
  nstack *mystack = (nstack *)g_malloc0(sizeof(nstack));
  mystack->pitch = pitch;
  mystack->timeon = timeon;
  mystack->on_delta_time = delta_time;
  mystack->duration = duration;
  mystack->tracknum = tracknum;
  mystack->chordnotes = NULL;
  return mystack;
}

/**
 * Process note on command 
 */
void
donoteon (midicallback *mididata, gint pitchon, gint velocity, gint timeon)
{
  gint delta_time = mididata->delta_time; /*is this needed????*/
  
  /* add a note to the stack */
  Get_Smf_Note_OFF(pitchon, timeon, delta_time, mididata);
#ifdef DEBUG
  g_print ("\npitchon = %d timeon = %d event = %d\n", (gint) pitchon, (gint) timeon, (gint) mididata->event_number);
#endif
}

/**
 * Process note off command
 */
void
donoteoff (midicallback *mididata)
{
  nstack *currentnote = mididata->currentnote;
  if (currentnote != NULL){
	/*process the note found*/
	process_list(mididata);	
	g_free(mididata->currentnote);
  }
  mididata->currentnote = NULL; 
}

gboolean
ChordToneCheck(midicallback *mididata, gint pitch, gint timeon, gint delta_time, gint duration){
  nstack *currentnote = mididata->currentnote;	
  gint tracknum = (int) mididata->track;

  if ((currentnote->timeon == timeon) &&
     (currentnote->duration == duration)){
	  g_print("\n***Same Duration and timeon***\n");
  	  /* append note to GList */
	  currentnote->chordnotes = g_list_append(currentnote->chordnotes, (gpointer)(intptr_t)pitch);
	  return TRUE;
  }
  else if ((currentnote->timeon <= timeon) && (timeon <= ((int) currentnote->timeon + (int) currentnote->duration))){
	g_print("\n***NOTE_ON colliding between a NOTE_ON and NOTE_OFF tracknum = %d***\n",tracknum);	
  	/* append to voice glist */
	nstack *noteon = stack(pitch, timeon, delta_time, duration, tracknum);
      	/* store noteon in mididata->notestack */
	mididata->notestack = g_list_append(mididata->notestack, noteon);
	return FALSE;
  }
  return FALSE;
}

static gint
Get_Smf_Note_OFF (gint pitch, gint timeon, gint delta_time, midicallback *mididata){
  gint event_number = mididata->event_number;
  smf_event_t *event;
  gint duration;
  gint tracknum = mididata->track;
  gboolean chordtone = FALSE;

  while ((event = smf_track_get_event_by_number(mididata->selected_track, event_number)) != NULL){
    if (   (((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_OFF) 
		    && (event->midi_buffer[1] == (int) pitch))
        || (((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1) == NOTE_ON) 
		    && (event->midi_buffer[1] == (int) pitch)
		    && (0 == event->midi_buffer[2]))
       ){
	duration = event->time_pulses - timeon; 
	if (mididata->currentnote != NULL) 
	   chordtone = ChordToneCheck(mididata, pitch, timeon, delta_time, duration);
	if (!chordtone){
      	  /* store note in mididata->currentnote */
	  mididata->currentnote = stack(pitch, timeon, delta_time, duration, tracknum);
	}
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
ConvertNoteType2ticks(midicallback *mididata, notetype *gnotetype){
  gint ticks;
  gint PPQN = mididata->smf->ppqn;
  gint notetype = (int) gnotetype->notetype;
  gint numofdots = (int) gnotetype->numofdots;
  gint dsq = (4 * PPQN);
  gint i = 0;

  ticks = dsq >> notetype;
  while (i++ < numofdots) 
    ticks += dsq >> (notetype + 1);

  return ticks;
}

notetype ConvertLength(gint duration, midicallback *mididata){
 /*convert length to 2 = quarter, 1 = half, 0 = whole etc...... */
	/* quarter = 384, half = 768, whole = 1536*/
  notetype gnotetype;
  gint PPQN = mididata->smf->ppqn;
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

/**
 * check to see if a new staff needs to be added
 */
void StaffCheck(midicallback *mididata){
  DenemoScore *si = mididata->gui->si;
  gint track = (int) mididata->currentnote->tracknum;
  gint currentstaffnum = si->currentstaffnum;

  if (track > currentstaffnum) /*if not first track add track */
    {
      si->currentstaffnum++;
      si->currentstaff = g_list_first (si->thescore); /* set the first track to be copied */
      newstaff (mididata->gui, ADDFROMLOAD, DENEMO_NONE); /* add track */
      si->currentstaff = g_list_last (si->thescore); /* make last staff the current */
      mididata->lastoff = 0;
      si->cursor_x = 0;
      mididata->bartime = 0;
      si->currentmeasurenum = 1;
    }
}

/** 
 * check
 * to see if we need to add a new measure 
 */
void MeasureCheck(midicallback *mididata){
  if (mididata->bartime >= mididata->barlength)	
  {			/* mididata->bartime >= barlenth will be true if there are rests  or notes
			   going over end of measures. */
    if (!mididata->gui->si->currentmeasure->next)
      /* Add a measure and make it currentmeasure */
      mididata->gui->si->currentmeasure =
      dnm_addmeasures (mididata->gui->si, mididata->gui->si->currentmeasurenum, 1, 1);
    else
      mididata->gui->si->currentmeasure = mididata->gui->si->currentmeasure->next;
    /* Now the stuff that needs to be done for each case */
    mididata->gui->si->currentmeasurenum++;
    mididata->gui->si->currentobject = NULL;
    mididata->gui->si->cursor_x = 0;
    memcpy (mididata->gui->si->cursoraccs, mididata->gui->si->nextmeasureaccs, SEVENGINTS);
    memcpy (mididata->gui->si->curmeasureaccs, mididata->gui->si->nextmeasureaccs, SEVENGINTS);
    mididata->gui->si->curmeasureclef = mididata->gui->si->cursorclef;
    mididata->bartime = 0;
  }
}

void RestCheck(midicallback *mididata){
  gint rest = 0;
  gint ticks;
  gint PPQN = mididata->smf->ppqn;
  gint starttime = (int) mididata->currentnote->timeon;
  gint duration = (int) mididata->currentnote->duration;
  gint on_delta_time = (int) mididata->currentnote->on_delta_time;
  gint dsq = (4 * mididata->smf->ppqn);

  if (duration == 0)
    return;
  if (starttime > mididata->lastoff){
    rest = starttime - mididata->lastoff;
    g_debug("\nRest = %d\n",rest);
    if (mididata->bartime + on_delta_time  >= (mididata->barlength)){
      g_debug("\nmididata->bartime + on_delta_time  >= mididata->barlength\n");
      while(mididata->barlength - mididata->bartime){
        rest = mididata->barlength - mididata->bartime;
        struct notetype length = ConvertLength(rest, mididata);
        insert_rest_into_score(mididata, length);
	//new_dnm_object(mididata, mididata->currentnote, length, FALSE);
	ticks = ConvertNoteType2ticks(mididata, &length);
        mididata->bartime += ticks;
        mididata->lastoff += ticks;
      } 
      process_list(mididata);		          
      rest = 0;/* I am not sure if this is the best choice here*/
    }
    while (rest){
        struct notetype length = ConvertLength(rest, mididata);
	insert_rest_into_score(mididata, length);
	//new_dnm_object(mididata, mididata->currentnote, length, FALSE);
	ticks = ConvertNoteType2ticks(mididata, &length);
	mididata->bartime += ticks;
	mididata->lastoff += ticks;
	rest -= ticks;
	g_debug("\nbartime = %d, lastoff = %d\n", mididata->bartime, mididata->lastoff);
    }
  }
}

/**
 * This takes a note and calculates if the note is to big to fit in the measure. 
 * If it is it fits what it can fit in the measure and ties it to the 
 * remainder in the next measure.
 */
void TiedNoteCheck(midicallback *mididata){
	gint duration = (int) mididata->currentnote->duration;
  	gint barlength = (int) mididata->barlength;
  
	if ((mididata->bartime + duration) > barlength)
	{
	  	mididata->leftover = (int) (duration - (barlength - mididata->bartime));	/*tied over bar line */
	  	duration = (int) (barlength - mididata->bartime); /*new value that fits in bar*/
		mididata->currentnote->duration = duration; 
	}
      	else 
		mididata->leftover = (int) 0;
}

void ProcessNote(midicallback *mididata) {
	DenemoScore *si = mididata->gui->si;
	gint starttime = (int) mididata->currentnote->timeon;
	gint lastoff = mididata->lastoff;
	gint duration = (int) mididata->currentnote->duration;
	gint pitch = (int) mididata->currentnote->pitch;
	if (duration == 0)
  	  return;
        g_debug("\nProcessing pitch %d\n",pitch);	        
	/*if a noteon event happends just after anouther note finishes just add the next note */
	if (starttime == mididata->lastoff) {
		notetype length = ConvertLength(duration, mididata);
		length.tied = mididata->leftover;
		insert_note_into_score(mididata, length);
		//new_dnm_object (mididata, mididata->currentnote, length, TRUE);
		mididata->lastoff = starttime + duration;
		mididata->bartime += duration;
	}
	if (mididata->leftover){
		MeasureCheck(mididata);
		notetype tied_length = ConvertLength(mididata->leftover, mididata);
		insert_note_into_score(mididata, tied_length);
		//new_dnm_object (mididata, mididata->currentnote, tied_length, TRUE);
		mididata->lastoff += mididata->leftover;
		mididata->bartime += mididata->leftover;
		mididata->leftover = 0;
	}
	/* if the starttime of noteon is not were we left off we need to do something */
	if ((starttime != mididata->lastoff) && (mididata->leftover !=0)){
		mididata->leftover = 0;
		mididata->currentnote->timeon = mididata->lastoff;
		process_list(mididata);
	}
}

void
ProcessNoteStack(midicallback *mididata){
  	while (mididata->notestack){
		/* Assign the note to currentnote */
		mididata->currentnote = mididata->notestack->data; 
		/* send the note on for processing */
		process_list(mididata);
		/* remove note from list */
		mididata->notestack = g_list_remove_link(mididata->notestack,
		g_list_first (mididata->notestack));
  	}
}
/**
 * Add notes to the current staff
 */
void
process_list(midicallback *mididata)
{
  /* check to see if we need to add a new staff */
  StaffCheck(mididata);
  /* check to see if we need to add a measure */
  MeasureCheck(mididata);  	
  /*check for rests*/
  RestCheck(mididata);
  /*check for notes tied across measure*/
  TiedNoteCheck(mididata);	
  /* note processing stuff */
  ProcessNote(mididata);  
}

gint
importMidi (gchar *filename, DenemoGUI *gui)
{
  midicallback *mididata = (midicallback *)g_malloc0(sizeof(midicallback));
  mididata->notestack = NULL;
  mididata->selected_track = NULL;
  mididata->smf = NULL;
  mididata->gui = gui;
  mididata->bartime = 0;
  mididata->lastoff = 0;
  mididata->track = 1;
  gint ret = 0;	// (-1 on failure)

  /* delete old data in the score */
  deletescore (NULL, gui);

  /* load the file */
  ret = cmd_load(mididata, filename);
  /* Read Track Data */ 
  readtrack(mididata);

  g_free(mididata);
  return ret;
}
