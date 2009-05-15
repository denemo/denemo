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
#include <math.h>
#include <string.h>
#include <denemo/denemo.h>
#include "importmidi.h"
#include "staffops.h"
#include "smf.h"

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

typedef struct notetype
{
	gint notetype;
	gint numofdots;
	gint tied;
}notetype;

typedef struct nstack 
{
	gint *pitch;
   	gint *measure; /*can this be calculated on noteoff?*/
     	gint *timeon;
	gint *on_delta_time; /* this is time between last event and this notes start */
     	gint *duration; 
	gint *staffnum;	
}nstack;

typedef struct midicallback
{
	GList *notestack;
	GList *chordnotes;
	GList *currentnote;
	DenemoGUI *gui;
	gint leftover; /* note/rest value that is leftover across the measure */
	gint PPQN;
	gint bartime; /* time relative to barlength 0-barlength */
	gint delta_time; /* distance between notes */
	gint barlength; /* amount of time in measure */
	gint lastoff; /* starttime + duration. The time when the note is finished */
	gint trackplus;
	gint number_of_tracks;
	gint midi_format;
	gint key;
	gint track;
	smf_t *smf;
}midicallback;

typedef struct harmonic
{
	gint pitch;
	gint enshift;
}harmonic;


void ProcessNote(GList *list, midicallback *mididata); 
void process_list(GList *list, midicallback *mididata);
void MeasureCheck(GList *list, midicallback *mididata);
struct harmonic enharmonic(gint input, gint key);
gint readheader(midicallback *mididata);
void readtrack(midicallback *mididata);
void dotimesig(gint numerator, gint denominator, midicallback *mididata);
void dokeysig(gint key, gint isminor, midicallback *mididata);
void dotempo(gint tempo,  midicallback *mididata);
void dotrackname(gchar *name, midicallback *mididata);
void doinstrname(gchar *name, midicallback *mididata);
void donoteon(midicallback *mididata, gint *pitchon, gint *velocity, gint *timeon);
void donoteoff(midicallback *mididata, gint *pitchoff, gint *timeoff);
void restcheck(GList *tmp, midicallback *mididata);
struct notetype ConvertLength(gint endnote, midicallback *mididata);

smf_track_t *selected_track = NULL;
smf_event_t *selected_event = NULL;
static smf_t *smf = NULL;

static void
log_handler(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer notused)
{
	fprintf(stderr, "%s: %s\n", log_domain, message);
}

static int cmd_track(char *arg);

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
	/*
	if (file_name == NULL) {
		if (last_file_name == NULL) {
			g_critical("Please specify file name.");
			return -1;
		}

		file_name = last_file_name;
	}
	*/

	if (smf != NULL)
		smf_delete(smf);

	selected_track = NULL;
	selected_event = NULL;

	//last_file_name = strdup(file_name);
	smf = smf_load(file_name);
	if (smf == NULL) {
		g_critical("Couldn't load '%s'.", file_name);

		smf = smf_new();
		if (smf == NULL) {
			g_critical("Cannot initialize smf_t.");
			return -1;
		}

		return -2;
	}

	g_message("File '%s' loaded.", file_name);
	g_message("%s.", smf_decode(smf));

	cmd_track("1");

	return 0;
}

static int
cmd_format(char *new_format)
{
	int tmp;
	char *end;

	if (new_format == NULL) {
		g_message("Format is %d.", smf->format);
	} else {
		tmp = strtol(new_format, &end, 10);
		if (end - new_format != strlen(new_format)) {
			g_critical("Invalid format value, garbage characters after the number.");
			return -1;
		}

		if (tmp < 0 || tmp > 2) {
			g_critical("Invalid format value, valid values are in range 0 - 2, inclusive.");
			return -2;
		}

		if (smf_set_format(smf, tmp)) {
			g_critical("smf_set_format failed.");
			return -3;
		}

		g_message("Forma changed to %d.", smf->format);
	}
	
	return 0;
}

static int
cmd_tracks(char *notused)
{
	if (smf->number_of_tracks > 0)
		g_message("There are %d tracks, numbered from 1 to %d.", smf->number_of_tracks, smf->number_of_tracks);
	else
		g_message("There are no tracks.");

	return 0;
}

static int
parse_track_number(const char *arg)
{
	int num;
	char *end;

	if (arg == NULL) {
		if (selected_track == NULL) {
			g_message("No track currently selected and no track number given.");
			return -1;
		} else {
			return selected_track->track_number;
		}
	}

	num = strtol(arg, &end, 10);
	if (end - arg != strlen(arg)) {
		g_critical("Invalid track number, garbage characters after the number.");
		return -1;
	}

	if (num < 1 || num > smf->number_of_tracks) {
		if (smf->number_of_tracks > 0) {
			g_critical("Invalid track number specified; valid choices are 1 - %d.", smf->number_of_tracks);
		} else {
			g_critical("There are no tracks.");
		}

		return -1;
	}

	return num;
}

static int
cmd_track(char *arg)
{
	int num;

	if (arg == NULL) {
		if (selected_track == NULL)
			g_message("No track currently selected.");
		else
			g_message("Currently selected is track number %d, containing %d events.",
				selected_track->track_number, selected_track->number_of_events);
	} else {
		if (smf->number_of_tracks == 0) {
			g_message("There are no tracks.");
			return -1;
		}

		num = parse_track_number(arg);
		if (num < 0)
			return -1;

		selected_track = smf_get_track_by_number(smf, num);
		if (selected_track == NULL) {
			g_critical("smf_get_track_by_number() failed, track not selected.");
			return -3;
		}

		selected_event = NULL;

		g_message("Track number %d selected; it contains %d events.",
				selected_track->track_number, selected_track->number_of_events);
	}

	return 0;
}


#define BUFFER_SIZE 1024

static int
show_event(smf_event_t *event)
{
	int off = 0, i;
	char *decoded, *type;

	if (smf_event_is_metadata(event))
		type = "Metadata";
	else
		type = "Event";
	
	decoded = smf_event_decode(event);

	if (decoded == NULL) {
		decoded = malloc(BUFFER_SIZE);
		if (decoded == NULL) {
			g_critical("show_event: malloc failed.");
			return -1;
		}

		off += snprintf(decoded + off, BUFFER_SIZE - off, "Unknown event:");

		for (i = 0; i < event->midi_buffer_length && i < 5; i++)
			off += snprintf(decoded + off, BUFFER_SIZE - off, " 0x%x", event->midi_buffer[i]);
	}

	g_message("%d: %s: %s, %f seconds, %d pulses, %d delta pulses", event->event_number, type, decoded,
		event->time_seconds, event->time_pulses, event->delta_time_pulses);

	free(decoded);

	return 0;
}

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
		case 0x01:
			//return smf_event_decode_textual(event, "Text");

		case 0x02:
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
		return NULL;
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
			break;

		case NOTE_ON:
			note_from_int(note, event->midi_buffer[1]);
			g_debug("\nNote On channel %d note %s velocity %d\n", 
					channel, note, event->midi_buffer[2]);
			donoteon(mididata, event->midi_buffer[1], event->midi_buffer[2], );
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
cmd_events(midicallback *mididata)
{
	smf_event_t *event;

	if (selected_track == NULL) {
		g_critical("No track selected - please use 'track [number]' command first.");
		return -1;
	}

	g_message("List of events in track %d follows:", selected_track->track_number);

	smf_rewind(smf);

	while ((event = smf_track_get_next_event(selected_track)) != NULL) {
#ifdef DEBUG
		/* print midi event */
		//show_event(event);
#endif
		/* Do something with the event */
		process_midi(event, mididata);
	}

	smf_rewind(smf);

	return 0;
}

static int
parse_event_number(const char *arg)
{
	int num;
	char *end;

	if (selected_track == NULL) {
		g_critical("You need to select track first (using 'track <number>').");
		return -1;
	}

	if (arg == NULL) {
		if (selected_event == NULL) {
			g_message("No event currently selected and no event number given.");
			return -1;
		} else {
			return selected_event->event_number;
		}
	}

	num = strtol(arg, &end, 10);
	if (end - arg != strlen(arg)) {
		g_critical("Invalid event number, garbage characters after the number.");
		return -1;
	}

	if (num < 1 || num > selected_track->number_of_events) {
		if (selected_track->number_of_events > 0) {
			g_critical("Invalid event number specified; valid choices are 1 - %d.", selected_track->number_of_events);
		} else {
			g_critical("There are no events in currently selected track.");
		}

		return -1;
	}

	return num;
}

static int
cmd_event(char *arg)
{
	int num;

	if (arg == NULL) {
		if (selected_event == NULL) {
			g_message("No event currently selected.");
		} else {
			g_message("Currently selected is event %d, track %d.", selected_event->event_number, selected_track->track_number);
			show_event(selected_event);
		}
	} else {
		num = parse_event_number(arg);
		if (num < 0)
			return -1;

		selected_event = smf_track_get_event_by_number(selected_track, num);
		if (selected_event == NULL) {
			g_critical("smf_get_event_by_number() failed, event not selected.");
			return -2;
		}

		g_message("Event number %d selected.", selected_event->event_number);
		show_event(selected_event);
	}

	return 0;
}

static int
decode_hex(char *str, unsigned char **buffer, int *length)
{
	int i, value, midi_buffer_length;
	char buf[3];
	unsigned char *midi_buffer = NULL;
	char *end = NULL;

	if ((strlen(str) % 2) != 0) {
		g_critical("Hex value should have even number of characters, you know.");
		goto error;
	}

	midi_buffer_length = strlen(str) / 2;
	midi_buffer = malloc(midi_buffer_length);
	if (midi_buffer == NULL) {
		g_critical("malloc() failed.");
		goto error;
	}

	for (i = 0; i < midi_buffer_length; i++) {
		buf[0] = str[i * 2];
		buf[1] = str[i * 2 + 1];
		buf[2] = '\0';
		value = strtoll(buf, &end, 16);

		if (end - buf != 2) {
			g_critical("Garbage characters detected after hex.");
			goto error;
		}

		midi_buffer[i] = value;
	}

	*buffer = midi_buffer;
	*length = midi_buffer_length;

	return 0;

error:
	if (midi_buffer != NULL)
		free(midi_buffer);

	return -1;
}

DenemoObject * new_dnm_object(notetype length){
	DenemoObject *mudela_obj_new;
	mudela_obj_new = dnm_newchord (length.notetype, length.numofdots, length.tied);
	return mudela_obj_new;
}

/**
 * Read Midi file header
 *
 */
gint
readheader (midicallback *mididata)
{
  mididata->midi_format = smf->format;
  mididata->number_of_tracks = smf->number_of_tracks;
  mididata->PPQN = smf->ppqn;

#ifdef DEBUG
  printf ("\nMidi format is %d\n", mididata->midi_format);
  printf ("\nNo. of Tracks is %d\n", mididata->number_of_tracks);
  printf ("\nPPQN is %d\n", mididata->PPQN);
#endif
  
  return 1;
}

/**
 * Read track from midi file
 *
 */
void
readtrack (midicallback *mididata)
{
  /* Get a listing of events 
   and decide what to do with them */  
  cmd_events(mididata);
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

  mididata->barlength = mididata->PPQN * 4 * numerator / denominator;
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
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;
  curstaffstruct->keysig.number = key;
  curstaffstruct->keysig.isminor = isminor;
  /* is this needed ?*/
  //dnm_setinitialkeysig (curstaffstruct, flats, isminor);
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
  dnm_set_lily_name (curstaffstruct->denemo_name, curstaffstruct->lily_name);
  //g_string_free (temp, FALSE);
}

void
doinstrname (gchar* name,  midicallback *mididata)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) mididata->gui->si->currentstaff->data;

  curstaffstruct->midi_instrument->str = g_strdup(name);
  //g_string_free (temp, FALSE);
}

static nstack *
stack (gint *pitch, gint *timeon, gint *delta_time, gint *duration, gint *measure, gint *staffnum)
{
  nstack *mystack = (nstack *)g_malloc0(sizeof(nstack));
  mystack->pitch = pitch;
  mystack->timeon = timeon;
  mystack->on_delta_time = delta_time;
  return mystack;
}

/**
 * Process note on command 
 */

void
donoteon (midicallback *mididata, gint *pitchon, gint *velocity, gint *timeon)
{
  nstack *noteon;
  int delta_time = mididata->delta_time;
  if (attack == 0)
    {
      donoteoff (mididata, pitchon, timeon);
    }
  else
    {
      /* add a note to the stack */
      noteon = stack(pitchon,timeon,delta_time,NULL,NULL, NULL);
      mididata->notestack = g_list_append(mididata->notestack, noteon);
#ifdef DEBUG
	      printf ("\npitchon = %i timeon = %i\n", pitchon, timeon);
#endif
    }
}

gint CompareNotes(GList *list, gint *pitchoff)
{
        if ((((nstack *) &list->data)->pitch == pitchoff) &&
               (((nstack *) &list->data)->duration == (int) NULL))
               return 0;
        else
               return 1;
}

void findchordtones(GList *list, midicallback *mididata){
	/*list*/
	gint *l_timeon = ((nstack *) list->data)->timeon;
	gint *l_staffnum = ((nstack *) list->data)->staffnum;
	gint *l_duration = ((nstack *) list->data)->duration;
	gint *l_pitch = ((nstack *) list->data)->pitch;
	/*current note*/
	gint *c_timeon = ((nstack *) mididata->currentnote->data)->timeon;
	gint *c_staffnum = ((nstack *) mididata->currentnote->data)->staffnum;
	gint *c_duration = ((nstack *) mididata->currentnote->data)->duration;
	gint *c_pitch = ((nstack *) mididata->currentnote->data)->pitch;
        //1 2	
	if ((int) l_timeon <= (int) c_timeon < ((int) l_timeon + (int) l_duration)){
	  if ((l_timeon == c_timeon) /*check for notes that belong in the chord*/
		&& (l_duration == c_duration)
		&& (l_staffnum == c_staffnum)
		&& (l_pitch != c_pitch)){
				mididata->chordnotes = g_list_append(mididata->chordnotes, (int *) l_pitch);
	  	printf("\n!!! Chord Tone !!!\n");
	  }
	  else if (((gint) l_timeon == (gint) c_timeon)
			 && (l_pitch != c_pitch) 
			 && ((gint) l_duration != (gint) c_duration)) 
            printf("\n!!! possible voice !!!\n");
	  else if (((gint) l_timeon < (gint) c_timeon)
			 && ((((int) l_timeon + (int) l_duration) > (int) c_timeon)))
		  printf("\n!!! possible voice !!!\n");
	}

}

void findvoicesandchords(GList *list, midicallback *mididata){
	/*list*/
	gint *l_timeon = ((nstack *) list->data)->timeon;
	gint *l_staffnum = ((nstack *) list->data)->staffnum;
	gint *l_duration = ((nstack *) list->data)->duration;
	gint *l_pitch = ((nstack *) list->data)->pitch;
	/*current note*/
	gint *c_timeon = ((nstack *) mididata->currentnote->data)->timeon;
	gint *c_staffnum = ((nstack *) mididata->currentnote->data)->staffnum;
	gint *c_duration = ((nstack *) mididata->currentnote->data)->duration;
	gint *c_pitch = ((nstack *) mididata->currentnote->data)->pitch;
	//0 3 / 1 4
}


/**
 * Process note off command
 */
void
donoteoff (midicallback *mididata, gint *pitchoff, gint *timeoff)
{
  gint duration = 0;
  gint starttime = 0;
  GList *list = NULL;
  list = g_list_find_custom (mididata->notestack, pitchoff, (GCompareFunc) (int) CompareNotes);
  if (list != NULL)
      	starttime = (int) ((nstack *) list->data)->timeon;
      	duration = ((gint) timeoff) - starttime; /*can this actually be obtained from the delta_time?*/
#ifdef DEBUG
   	g_printf("\nduration = %i\n",duration);
#endif
  	((nstack *) list->data)->duration = (int *) duration;
	/* should I get currentmeasure from gui->si or calculate that here?*/
	((nstack *) list->data)->measure = (int *) mididata->gui->si->currentmeasurenum;
	((nstack *) list->data)->staffnum = (int *) mididata->track;
        /*process the found note*/
	process_list(list,mididata);	
	/*once note is found and applied remove it from the stack*/ 	
	mididata->notestack = g_list_remove (mididata->notestack, list);
}


void ProcessNote(GList *list, midicallback *mididata) {
	DenemoScore *si = mididata->gui->si;
	gint starttime = (int) ((nstack *) list->data)->timeon;
	gint lastoff = mididata->lastoff;
	gint duration = (int) ((nstack *) list->data)->duration;
	gint track = (int) ((nstack *) list->data)->staffnum;
	gint pitch = (int) ((nstack *) list->data)->pitch;
	nstack note;
	DenemoObject *mudela_obj_new;
	
	
	//while (mididata->chordnotes){
	//	   mididata->chordnotes = g_list_remove_link(mididata->chordnotes, g_list_first (mididata->chordnotes));
	//}
	
	/* call function to return a list of chord tones */
	//mididata->currentnote = g_list_copy(list);
	//g_list_foreach(mididata->final_list, (GFunc) findchordtones, mididata);
	
	/*if a noteon event happends just after anouther note finishes just add the next note */
	if (starttime == mididata->lastoff) {
		notetype length = ConvertLength(duration, mididata);
		length.tied = mididata->leftover;
		mudela_obj_new = new_dnm_object (length);
		struct harmonic enote = enharmonic ((int) ((nstack *) list->data)->pitch,mididata->key);
		dnm_addtone (mudela_obj_new, enote.pitch, enote.enshift, mididata->gui->si->cursorclef);
		/*while (mididata->chordnotes){
		  struct harmonic enote = enharmonic ((int) 
				 mididata->chordnotes->data,mididata->key);
	          dnm_addtone (mudela_obj_new, enote.pitch, enote.enshift, mididata->gui->si->cursorclef);		
		  mididata->chordnotes = g_list_remove_link(mididata->chordnotes, g_list_first (mididata->chordnotes));
		}*/
		mididata->lastoff = starttime + duration;
		object_insert (mididata->gui, mudela_obj_new);
		mididata->bartime += duration;
	}
	if (mididata->leftover){
		MeasureCheck(list,mididata);
		notetype tied_length = ConvertLength(mididata->leftover, mididata);
		mudela_obj_new = new_dnm_object (tied_length);
		struct harmonic enote = enharmonic ((int) ((nstack *) list->data)->pitch,mididata->key);
		dnm_addtone (mudela_obj_new, enote.pitch, enote.enshift, mididata->gui->si->cursorclef);
		mididata->lastoff += mididata->leftover;
		object_insert (mididata->gui, mudela_obj_new);
		mididata->bartime += mididata->leftover;
		mididata->leftover = 0;
	}
	/* if the starttime of noteon is not were we left off we need to do something */
	if ((starttime != mididata->lastoff) && (mididata->leftover !=0)){
		mididata->leftover = 0;
		((nstack *) list->data)->timeon = (int *) mididata->lastoff;
		process_list(list, mididata);
	}
#ifdef DEBUG
	printf("\nNotelist pitch = %i, timeon = %i, duration = %i, staffnum = %i\n", pitch, starttime, duration, track);
#endif
}

void restcheck(GList *list, midicallback *mididata){
  gint rest = 0;
  gint PPQN = mididata->PPQN;
  gint starttime = (int) ((nstack *) list->data)->timeon;
  gint duration = (int) ((nstack *) list->data)->duration;
  gint on_delta_time = (int) ((nstack *) list->data)->on_delta_time;
  DenemoObject *mudela_obj_new;
  if (starttime > mididata->lastoff){
    rest = starttime - mididata->lastoff;
    if (mididata->bartime + on_delta_time  >= (mididata->barlength)){
      while(mididata->barlength - mididata->bartime){
        rest = mididata->barlength - mididata->bartime;
        struct notetype length = ConvertLength(rest, mididata);
	length.tied = 0;
        mudela_obj_new = new_dnm_object(length);
        object_insert (mididata->gui, mudela_obj_new);
        mididata->bartime += rest;
        mididata->lastoff += rest;
      } 
      process_list(list,mididata);		          
      rest = 0;/* I am not sure if this is the best choice here*/
    }
    if (rest){
        struct notetype length = ConvertLength(rest, mididata);
	length.tied = 0;
	mudela_obj_new = new_dnm_object(length);
	object_insert (mididata->gui, mudela_obj_new);
	mididata->bartime = mididata->bartime + (rest);
	mididata->lastoff += rest;
	rest = 0;
    }
  }
}

notetype ConvertLength(gint endnote, midicallback *mididata){
 /*convert length to 2 = quarter, 1 = half, 0 = whole etc...... */
	/* quarter = 384, half = 768, whole = 1536*/
  notetype gnotetype;
  gint PPQN = mididata->PPQN;
  gint notetype = 0;
  gint numofdots = 0;
  gint tied = 0;
  gint leftover = 0;
  gint dsq = (4 * PPQN);

  do notetype++;
  while ((dsq >> notetype) > endnote);
	  
  /* TODO add a do while or something to find more dots */

  if (dsq % endnote){
    leftover = (endnote - (dsq >> notetype));            //if leftovers
      if (leftover == (dsq >> (notetype +1))){
        numofdots = 1;
        leftover = 0;
      }	
    }

  gnotetype.notetype = notetype;
  gnotetype.numofdots = numofdots;
  gnotetype.tied = leftover;
  return gnotetype;
}

/**
 * check to see if a new staff needs to be added
 */
void StaffCheck(GList *list, midicallback *mididata){
  DenemoScore *si = mididata->gui->si;
  gint track = (int) ((nstack *) list->data)->staffnum;
  gint currentstaffnum = si->currentstaffnum;

  if ((track + 1) > currentstaffnum) /*if not first track add track */
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
void MeasureCheck(GList *list, midicallback *mididata){
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

/**
 * This takes a note and calculates if the note is to big to fit in the measue. 
 * If it is it fits what it can fit in the measure and ties it to the 
 * remainder in the next measure.
 */
void TiedNoteCheck(GList *list, midicallback *mididata){
	gint endnote = (int) ((nstack *) list->data)->duration;
  	gint barlength = (int) mididata->barlength;
  
	if ((mididata->bartime + endnote) > barlength)
	{
	  	mididata->leftover = (int) (endnote - (barlength - mididata->bartime));	/*tied over bar line */
	  	endnote = (int) (barlength - mididata->bartime); /*new value that fits in bar*/
		((nstack *) list->data)->duration = (int *) endnote; 
	}
      	else 
		mididata->leftover = (int) 0;
}

/**
 * Add notes to the current staff
 */
void
process_list(GList *list, midicallback *mididata)
{
  /* check to see if we need to add a new staff */
  StaffCheck(list, mididata);
  /* check to see if we need to add a measure */
  MeasureCheck(list, mididata);  	
  /*check for rests*/
  restcheck(list,mididata);
  /*check for notes tied across measure*/
  TiedNoteCheck(list, mididata);	
  /* note processing stuff */
  ProcessNote(list, mididata);  

}

harmonic
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

gint
importMidi (gchar *filename, DenemoGUI *gui)
{
  midicallback *mididata = (midicallback *)g_malloc0(sizeof(midicallback));
  mididata->notestack = NULL;
  mididata->chordnotes = NULL;
  mididata->gui = gui;
  mididata->PPQN = 200;
  mididata->bartime = 0;
  mididata->lastoff = 0;
  mididata->track = 0;
  gint ret = 0;	// (-1 on failure)

  /* delete old data in the score */
  dnm_deletescore (NULL, gui);

  /* load the file */
  ret = cmd_load(filename);
  /* get header info */
  readheader(mididata);
  /* Read Track Data */ 
  readtrack(mididata);

  g_free(mididata);
  g_free(filename);
  return ret;
}
