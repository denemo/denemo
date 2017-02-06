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
#include "importmidi.h"
#include "command/staff.h"
#include "smf.h"
#include "core/view.h"
#include "core/utils.h"
#include "export/file.h"
#include "command/commandfuncs.h"
#include "command/processstaffname.h"

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

typedef struct notetype
{
  gint notetype;
  gint numofdots;
} notetype;

typedef struct harmonic
{
  gint pitch;
  gint enshift;
} harmonic;


static void
note_from_int (char *buf, int note_number)
{
  int note, octave;
  char *names[] = { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };

  octave = note_number / 12 - 1;
  note = note_number % 12;

  sprintf (buf, "%s%d", names[note], octave);
}

static harmonic
enharmonic (gint input, gint key)
{
  harmonic local;
  local.pitch = (input / 12) - 5;
  local.enshift = input % 12;

  switch (local.enshift)
    {
    case 0:                    //c
      {
        local.pitch = (key > 6) ? (-1 + local.pitch * 7) : (local.pitch * 7);
        local.enshift = (key > 6) ? (1) : (0);
        break;
      }
    case 1:                    //c#
      {
        local.pitch = (key < -3) ? (1 + local.pitch * 7) : (local.pitch * 7);
        local.enshift = (key < -3) ? (-1) : (1);
        break;
      }
    case 2:                    //D
      {
        local.pitch = 1 + local.pitch * 7;
        local.enshift = 0;
        break;
      }
    case 3:                    //D#
      {
        local.pitch = (key < -1) ? (2 + local.pitch * 7) : (1 + local.pitch * 7);
        local.enshift = (key < -1) ? (-1) : (1);
        break;
      }
    case 4:                    //E
      {
        local.pitch = (key < -6) ? (3 + local.pitch * 7) : (2 + local.pitch * 7);
        local.enshift = (key < -6) ? (-1) : (0);
        break;
      }
    case 5:                    //F
      {
        local.pitch = (key > 5) ? (2 + local.pitch * 7) : (3 + local.pitch * 7);
        local.enshift = (key > 5) ? (1) : (0);
        break;
      }
    case 6:                    //F#
      {
        local.pitch = (key < -4) ? (4 + local.pitch * 7) : (3 + local.pitch * 7);
        local.enshift = (key < -4) ? (-1) : (1);
        break;
      }
    case 7:                    //G
      {
        local.pitch = 4 + local.pitch * 7;
        local.enshift = 0;
        break;
      }
    case 8:                    //G#
      {
        local.pitch = (key < -2) ? (5 + local.pitch * 7) : (4 + local.pitch * 7);
        local.enshift = (key < -2) ? (-1) : (1);
        break;
      }
    case 9:                    //A
      {
        local.pitch = 5 + local.pitch * 7;
        local.enshift = 0;
        break;
      }
    case 10:                   //A#
      {
        local.pitch = (key < 0) ? (6 + local.pitch * 7) : (5 + local.pitch * 7);
        local.enshift = (key < 0) ? (-1) : (1);
        break;
      }
    case 11:                   //B
      {
        local.pitch = (key < -5) ? (7 + local.pitch * 7) : (6 + local.pitch * 7);
        local.enshift = (key < -5) ? (-1) : (0);
        break;
      }
    };
  return local;
}

static void
ConvertLength (gint ppqn, gint duration, notetype * pnotetype)
{
  /*convert length to 2 = quarter, 1 = half, 0 = whole etc...... */
  /* quarter = 384, half = 768, whole = 1536 */

  gint notetype = 0;
  gint numofdots = 0;
  gint leftover = 0;
  gint dsq = (4 * ppqn);
  g_debug ("\nDuration = %d ticks\n", duration);

  while ((dsq >> notetype) > duration)
    notetype++;

  leftover = duration - (dsq >> notetype);
  if ((dsq >> (notetype + 1)) > 0)
    while (leftover >= (dsq >> (notetype + 1)))
      {
        leftover -= (dsq >> (notetype + 1));
        numofdots++;
      }

  pnotetype->notetype = notetype;
  pnotetype->numofdots = numofdots;
}

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
  gui->movement->tempo = (gint) (6.0e7 / (double) tempo);
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

static void
insert_rest_into_score (notetype length)
{
  DenemoProject *gui = Denemo.project;
  gint i;

  switch (length.notetype)
    {
    case 0:
      insert_rest_0key (NULL, NULL);
      break;
    case 1:
      insert_rest_1key (NULL, NULL);
      break;
    case 2:
      insert_rest_2key (NULL, NULL);
      break;
    case 3:
      insert_rest_3key (NULL, NULL);
      break;
    case 4:
      insert_rest_4key (NULL, NULL);
      break;
    case 5:
      insert_rest_5key (NULL, NULL);
      break;
    case 6:
      insert_rest_6key (NULL, NULL);
      break;
    default:
      insert_rest_2key (NULL, NULL);
      break;
    }
  displayhelper (gui);

  /* add dots */
  for (i = 0; i < length.numofdots; i++)
    add_dot_key (NULL, NULL);
}

static gint
ConvertNoteType2ticks (gint ppqn, notetype * gnotetype)
{
  gint ticks;
  gint notetype = (int) gnotetype->notetype;
  gint numofdots = (int) gnotetype->numofdots;
  gint dsq = (4 * ppqn);
  gint i = 0;

  ticks = dsq >> notetype;
  while (i++ < numofdots)
    ticks += dsq >> (notetype + 1);

  return ticks;
}

static void
AddRest (gint ppqn, gint duration)
{
  gint rest;
  gint ticks;
  if (duration == 0)
    return;
  rest = duration;

  while (rest)
    {
      notetype length;
      ConvertLength (ppqn, rest, &length);
      insert_rest_into_score (length);
      ticks = ConvertNoteType2ticks (ppqn, &length);
      rest -= ticks;
    }
}

static void
insert_note_into_score (gint pitch, notetype length)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  gint i;

  /* 0-8 accepted bellow */
  DenemoScriptParam param;
  highlight_duration (gui, length.notetype);

  switch (length.notetype)
    {
    case 0:
      insert_chord_0key (NULL, NULL);
      break;
    case 1:
      insert_chord_1key (NULL, NULL);
      break;
    case 2:
      insert_chord_2key (NULL, NULL);
      break;
    case 3:
      insert_chord_3key (NULL, NULL);
      break;
    case 4:
      insert_chord_4key (NULL, NULL);
      break;
    case 5:
      insert_chord_5key (NULL, NULL);
      break;
    case 6:
      insert_chord_6key (NULL, NULL);
      break;
    case 7:
      insert_chord_7key (NULL, NULL);
      break;
    default:
      insert_chord_8key (NULL, NULL);
      g_warning ("Cannot handle size %d", length.notetype);
      break;
    }

  score_status (gui, TRUE);
  displayhelper (gui);

  g_debug ("DenemoScriptParam = %d", param.status);
  /* get correct note name */
  gint key = curstaffstruct->keysig.number;
  harmonic enote = enharmonic (pitch, (gint) key);
  gchar *name = mid_c_offsettolily (enote.pitch, enote.enshift);

  /* Rename note to the correct note */
  gchar *accidental = g_strdup_printf ("(d-ChangeChordNotes \"%s\")", name);
  call_out_to_guile (accidental);
  g_free (accidental);
  /* Add dots */
  for (i = 0; i < length.numofdots; i++)
    add_dot_key (NULL, NULL);

  displayhelper (gui);
}

static void
AddMidiNote (gint ppqn, gint pitch, gint duration)
{
  if (duration == 0)
    return;
  notetype length;
  ConvertLength (ppqn, duration, &length);
  insert_note_into_score (pitch, length);
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

void
decode_metadata (const smf_event_t * event)
{
  int off = 0, mspqn, flats, isminor;
  char *buf;
/*
  static const char *const major_keys[] = { "Fb", "Cb", "Gb", "Db", "Ab",
    "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B", "F#", "C#", "G#"
  };

  static const char *const minor_keys[] = { "Dbm", "Abm", "Ebm", "Bbm", "Fm",
    "Cm", "Gm", "Dm", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m", "A#m", "E#m"
  };
*/
  //assert(smf_event_is_metadata(event));

  switch (event->midi_buffer[1])
    {
    case TEXT:
      //return smf_event_decode_textual(event, "Text");

    case COPYRIGHT:
      //return smf_event_decode_textual(event, "Copyright");

    case META_TRACK_NAME:
      //return smf_event_decode_textual(event, "Sequence/Track Name");
      dotrackname (smf_event_extract_text (event));

    case META_INSTR_NAME:
      //printf("\nInstrument text = %s\n", smf_string_from_event(event));
      doinstrname (smf_event_extract_text (event));
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

  buf = malloc (BUFFER_SIZE);
  if (buf == NULL)
    {
      g_critical ("smf_event_decode_metadata: malloc failed.");
    }

  switch (event->midi_buffer[1])
    {
    case 0x00:
      off += snprintf (buf + off, BUFFER_SIZE - off, "Sequence number");
      break;

      /* http://music.columbia.edu/pipermail/music-dsp/2004-August/061196.html */
    case 0x20:
      if (event->midi_buffer_length < 4)
        {
          g_critical ("smf_event_decode_metadata: truncated MIDI message.");
          goto error;
        }

      off += snprintf (buf + off, BUFFER_SIZE - off, "Channel Prefix: %d.", event->midi_buffer[3]);
      break;

    case 0x21:
      if (event->midi_buffer_length < 4)
        {
          g_critical ("smf_event_decode_metadata: truncated MIDI message.");
          goto error;
        }

      off += snprintf (buf + off, BUFFER_SIZE - off, "Midi Port: %d.", event->midi_buffer[3]);
      break;

    case 0x2F:
      off += snprintf (buf + off, BUFFER_SIZE - off, "End Of Track");
      break;

    case META_TEMPO:
      if (event->midi_buffer_length < 6)
        {
          g_critical ("smf_event_decode_metadata: truncated MIDI message.");
          goto error;
        }

      mspqn = (event->midi_buffer[3] << 16) + (event->midi_buffer[4] << 8) + event->midi_buffer[5];

      dotempo (mspqn);

      break;

    case 0x54:
      off += snprintf (buf + off, BUFFER_SIZE - off, "SMPTE Offset");
      break;

    case META_TIMESIG:
      if (event->midi_buffer_length < 7)
        {
          g_critical ("smf_event_decode_metadata: truncated MIDI message.");
          goto error;
        }

      dotimesig (event->midi_buffer[3], (int) pow (2, event->midi_buffer[4]));
      break;

    case META_KEYSIG:
      if (event->midi_buffer_length < 5)
        {
          g_critical ("smf_event_decode_metadata: truncated MIDI message.");
          goto error;
        }

      flats = event->midi_buffer[3];
      isminor = event->midi_buffer[4];

      if (isminor != 0 && isminor != 1)
        {
          g_critical ("smf_event_decode_metadata: last byte of the Key Signature event has invalid value %d.", isminor);
          goto error;
        }

      dokeysig (isminor, flats);
      break;

    case 0x7F:
      off += snprintf (buf + off, BUFFER_SIZE - off, "Proprietary (aka Sequencer) Event, length %d", event->midi_buffer_length);
      break;

    default:
      goto error;
    }

error:
  free (buf);
}

/**
 * extremely simple quantizer that rounds
 * to the closest granule size
 */
static gint
round2granule (gint tick)
{
  gint smallestgrain = 48;
  gdouble div = ((gdouble) tick / (gdouble) smallestgrain);
  return smallestgrain * (gint) round (div);
}

/**
 * Process note off command
 */
void
donoteoff (const smf_event_t * event)
{
  gint duration;
  gint ppqn;

  ppqn = event->track->smf->ppqn;
  duration = event->delta_time_pulses;
  duration = round2granule (duration);
  AddMidiNote (ppqn, event->midi_buffer[1], duration);
}

/**
 * Process note on command
 */
void
donoteon (const smf_event_t * event)
{
  gint ppqn;
  ppqn = event->track->smf->ppqn;
  if (event->delta_time_pulses)
    AddRest (ppqn, round2granule (event->delta_time_pulses));
}

void
decode_midi_event (const smf_event_t * event)
{
  gint channel;
  gchar note[5];

  /* + 1, because user-visible channels used to be in range <1-16>. */
  channel = (event->midi_buffer[0] & 0x0F) + 1;

  switch (event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)
    {
    case NOTE_OFF:
      note_from_int (note, event->midi_buffer[1]);
      g_debug ("\nNote Off channel %d note %s velocity %d\n", channel, note, event->midi_buffer[2]);
      donoteoff (event);
      break;

    case NOTE_ON:
      note_from_int (note, event->midi_buffer[1]);
      g_debug ("\nNote On channel %d note %s velocity %d\n", channel, note, event->midi_buffer[2]);

      if (event->midi_buffer[2])
        donoteon (event);
      else
        donoteoff (event);
      break;

    case AFTERTOUCH:
      note_from_int (note, event->midi_buffer[1]);
      g_debug ("\nAftertouch channel %d note %s velocity %d\n", channel, note, event->midi_buffer[2]);
      break;

    case CTRL_CHANGE:
      g_debug ("\nController channel %d controller %d value %d\n", channel, event->midi_buffer[1], event->midi_buffer[2]);
      break;

    case PGM_CHANGE:
      g_debug ("\nProgram Change channel %d controller %d\n", channel, event->midi_buffer[1]);
      break;

    case CHNL_PRESSURE:
      g_debug ("\nChannel Pressure channel %d pressure %d\n", channel, event->midi_buffer[1]);
      break;

    case PCH_WHEEL:
      g_debug ("\nPitch Wheel channel %d value %d\n", channel, ((int) event->midi_buffer[2] << 7) | (int) event->midi_buffer[2]);
      break;

    default:
      break;
    }
}

static int
process_midi (smf_event_t * event)
{
  if (smf_event_is_metadata (event))
    decode_metadata (event);
  else
    decode_midi_event (event);

  return 0;
}

gint
process_track (smf_track_t * track)
{
  smf_event_t *event;
  smf_t *smf = track->smf;      //tracks can't be processed except inside an smf_t so create one if needed
  gboolean delete_smf_after = FALSE;
  if (smf == NULL)
    {
      delete_smf_after = TRUE;
      smf = smf_new ();
      smf_add_track (smf, track);
      smf_rewind (smf);
    }
  while ((event = smf_track_get_next_event (track)) && event)
    process_midi (event);
  if (delete_smf_after)
    {
      smf_track_remove_from_smf (track);
      smf_delete (smf);
    }
  return 1;
}

void
AddStaff ()
{
  call_out_to_guile ("(d-AddAfter)");
}

static gint
readtrack (smf_t * smf)
{
  smf_track_t *selected_track;
  gint track;

  smf_rewind (smf);

  for (track = 1; track <= smf->number_of_tracks; track++)
    {
      selected_track = smf_get_track_by_number (smf, track);
      process_track (selected_track);
      if (track + 1 <= smf->number_of_tracks)
        AddStaff ();
    }
  return 0;
}

gint
importMidi (gchar * filename)
{
  smf_t *smf;
  gint ret = 0;                 // (-1 on failure)
  gboolean save = Denemo.prefs.immediateplayback;
  Denemo.prefs.immediateplayback = FALSE;
  /* load the file */
  smf = cmd_load (filename);
  if (!smf)
    return -1;
  /* Read Track Data */
  if (readtrack (smf))
    return -1;
  /* Delete data...Does it free it? */
  smf_delete (smf);
  Denemo.prefs.immediateplayback = save;
  return ret;
}
