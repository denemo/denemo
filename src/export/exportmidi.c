/*
 * exportmidi.c
 *
 * Functions for exporting a Standard Midi file
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C) 2001, 2002 Per Andersson, 2009, 2010, 2011, 2012 Richard Shann
 *
 * License: this file may be used under the FSF GPL version 3 or later
 */

#define EXPORTMIDI_VERSION  "1.2"

/*
 * The function exportmidi() writes a "Standard MIDI file" to disk.
 * The file can then be played by programs like playmidi
 * or TiMidity, externally or by the denemo playback command.
 *
 * See www.wotsit.org for some info on midi and the standard file format
 *
 * The ambition is to honour as many musical directives (tempo, slurs, ties,
 * dynamics, staccato ...) as possible, and to make the output as musical
 * as possible.  Efforts have been made to try to handle empty or incomplete
 * measures, unbalanced slurs and other strange input.
 *
 * Exportmidi() has a velocity modulation feature to make the sound
 * less mechanical, but this is still under development.
 * Timing modulation is planned.
 *
 * Call exportmidi() from some file menu or as a part of the playback command.
 * It has the same parameters as exportmudela().
 * There is a code fragment for this in the file fragment.c.
 * The makefile needs the four files exportmidi.[ch] and instrumentname.[ch].
 *
 * Environment variable is used for user preferences. This should be replaced
 * by some musical "style sheet" in the future, since the values depend on
 * the genre (and tempo) of the piece, as well as personal taste.
 * Information should be stored in the mudela file, somehow.
 * Values may change within the piece, and ...
 * There are many issues to resolve here!
 *
 * This software is tested with denemo 0.5.5.
 *
 *
 *
 *     Per Andersson
 *     Artifex consulting
 *
 *     email to: artifex@europe.com
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>

#include <denemo/denemo.h>
#include "export/exportmidi.h"
#include "audio/instrumentname.h"
#include "audio/audiointerface.h"
#include "core/view.h"
#include "printview/svgview.h"
#include "smf.h"
/*
 * only for developers
 */

static int debug = 0;

/****************************************************************/


/**
 * some important midi command bytes
 */

#define MIDI_NOTE_OFF       0x80
#define MIDI_NOTE_ON        0x90
#define MIDI_PROG_CHANGE    0xc0
#define MAX_TRACKS      16


/*
 * support macros for exportmidi()
 */

/* length, in ticks, of a quarter note */
#define MIDI_RESOLUTION     384

/* tick conversion */
#define ticks2bars(t,u,l) (t/(MIDI_RESOLUTION*4*u/l))
#define bars2ticks(t,u,l) (t*(MIDI_RESOLUTION*4*u/l))
#define ticks2beats(t,u,l) (t/(MIDI_RESOLUTION*4/l))
#define beats2ticks(t,u,l) (t*(MIDI_RESOLUTION*4/l))

/****************************************************************/

/**
 * dynamic request handling
 */

enum dynamics
{
  DYN_TACET = 0,
  DYN_PPP,
  DYN_PP,
  DYN_P,
  DYN_MP,
  DYN_MF,
  DYN_F,
  DYN_FF,
  DYN_FFF,
  DYN_MAX
};

static char *dyn_strings[DYN_MAX] = {
  "tacet",
  "ppp",
  "pp",
  "p",
  "mp",
  "mf",
  "f",
  "ff",
  "fff",
};

static int dyn_vol[DYN_MAX] = { 1, 16, 32, 48, 64, 80, 96, 112, 127 };

/**
 * convert dynamic string to midi velocity 0 .. 127
 */

static int
string_to_vol (char *dynamic, int default_vol)
{
  int i;

  for (i = 0; i < DYN_MAX; i++)
    {
      if (!strcmp (dynamic, dyn_strings[i]))
        {
          return dyn_vol[i];
        }
    }
  return default_vol;
}

/****************************************************************/

/**
 *  unbiased random generator,
 *
 *  returns a value within +- maxdev
 */
/* UNUSED
static int
i_random (int *accumulate, int maxdev)
{
  int rnd;

  rnd = maxdev - 2 * maxdev * (rand () >> 4) / (RAND_MAX >> 4) - *accumulate;

  *accumulate += rnd / 2;

  if (debug >= 10)
    {
      fprintf (stderr, "random=%d\n", rnd / 2);
    }

  return rnd / 2;
}
*/
/****************************************************************/

/**
 *  limit a value to be within limits
 *  (simple first approach)
 */
/* UNUSED
static int
compress (int range, int invalue)
{
  int outvalue;

  outvalue = invalue;

  if (invalue >= range)
    {
      outvalue = range - 1;
    }

  if (invalue != outvalue)
    {
      fprintf (stderr, "compress %d %d\n", invalue, outvalue);
    }

  return outvalue & 0x7F;
}
*/
/****************************************************************/

/**
 * integer base 2 logarithm used in time-sig code
 */

static int
twolog (int x)
{
  int answer = 0;

  /* should not happen! */
  if (x < 1)
    return 0;

  while (x > 1)
    {
      x >>= 1;
      answer++;
    }

  return answer;
}


#define midi_meta_text(a,b) smf_event_new_textual(a,b)


/**
 * almost only used for program change
 */

static smf_event_t *
midi_change_event (int type, int chan, int val)
{
  smf_event_t *event = smf_event_new ();
  guchar *buf = malloc (2);
  event->midi_buffer = buf;
  event->midi_buffer_length = 2;
  *buf++ = type | chan;
  *buf++ = val;
  return event;
}

/**
 * meta event: set time signature, time scale and metronome FF 58 04 nn dd cc bb Time Signature
 */

static smf_event_t *
midi_timesig (int upper, int lower)
{
  smf_event_t *event = smf_event_new ();
  guchar *buf = malloc (7);
  event->midi_buffer = buf;
  event->midi_buffer_length = 7;
  div_t n;
  int click = 24;

  if (lower != 0)
    {
      n = div (4, lower);
      click = 24 * ((float) n.quot + (float) n.rem);
    }

  *buf++ = 0xff, *buf++ = 0x58, *buf++ = 4;
  *buf++ = upper, *buf++ = twolog (lower), *buf++ = click, *buf++ = 8;
  return event;
}

/**
 * meta event: set key signature
 */

static smf_event_t *
midi_keysig (gint key, gint isminor)
{
  smf_event_t *event = smf_event_new ();
  guchar *buf = malloc (5);
  event->midi_buffer = buf;
  event->midi_buffer_length = 5;
  *buf++ = 0xff;
  *buf++ = 0x59;
  *buf++ = 2;
  *buf++ = key;
  *buf++ = isminor;
  return event;
}

/**
 * meta event: set system clock speed
 */

static smf_event_t *
midi_tempo (long tempo)
{
  long midi_tempo;
  smf_event_t *event = smf_event_new ();
  guchar *buf = malloc (6);
  event->midi_buffer = buf;
  event->midi_buffer_length = 6;
  if (tempo == 0)
    {
      tempo = 1;
    }
  *buf++ = 0xff;
  *buf++ = 0x51;
  *buf++ = 3;
  midi_tempo = 60000000 / tempo;
  *buf++ = (midi_tempo >> 16) & 255;
  *buf++ = (midi_tempo >> 8) & 255/*, (midi_tempo >> 0) & 255*/;
  return event;
}

/**
 * compute midi chromatic note number from diatonic offset
 */

int
dia_to_midinote (int offs)
{
  int table[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

  int tone, octave;
  int midinum;

  octave = offs / 7;
  tone = offs % 7;
  midinum = 60 + 12 * octave + table[tone + 6];
  return midinum;
}

/****************************************************************/

/**
 * slur handling code:
 *
 * decide what to do about slurs and things, using a note status table
 * describing each note and a status variable for the general situation
 */

/*
 * constants used in note status table
 */

#define FLG_CONNECT_B       0x01
#define FLG_CONNECT_F       0x02
#define FLG_SLUR_BEGIN      0x04
#define FLG_SLUR_END        0x08

#define FLG_NOTE_ON     0x10
#define FLG_NOTE_OFF        0x20
#define FLG_STACCATO        0x40
#define FLG_STACCATISSIMO   0x80

#define FLG_TIED_F      0x100
#define FLG_TIED_B      0x200

/**
 * debug print-out of slur table
 */

static char *
fmt_ticks (long t)
{
  static char answer[12];
  long sig = 4;
  long res = 4;
  long count;

  count = t * res / MIDI_RESOLUTION / sig;

  sprintf (answer, "%ld = %ld+%ld/%ld", t, count / res, count % res, res);
  return answer;
}

#if slurdebug
/**
 * Output slur descriptions to the given file
 *
 */
static int
print_slurs (FILE * fd, int *tab, int status, int t_read, int t_written, char *txt)
{
  int i;

  fprintf (fd, "=== slurs: (state=%d) %s  @ ", status, txt);
  fprintf (fd, "r=%s ", fmt_ticks (t_read));
  fprintf (fd, "w=%s\n", fmt_ticks (t_written));

  for (i = 0; i < 128; i++)
    {
      if (tab[i])
        {
          fprintf (fd, "%2d: %3x  %s%s%s%s%s%s%s%s%s%s\n",
                   i, tab[i], tab[i] & FLG_CONNECT_B ? "connect-backward " : "", tab[i] & FLG_CONNECT_F ? "connect-forward " : "", tab[i] & FLG_SLUR_BEGIN ? "begin " : "", tab[i] & FLG_SLUR_END ? "end " : "", tab[i] & FLG_STACCATO ? "staccato " : "", tab[i] & FLG_STACCATISSIMO ? "staccatissimo " : "", tab[i] & FLG_NOTE_ON ? "note_is_on " : "", tab[i] & FLG_NOTE_OFF ? "note_is_off " : "", tab[i] & FLG_TIED_B ? "tied_backward " : "", tab[i] & FLG_TIED_F ? "tied_forward " : "");
        }
    }

  /* not used */
  return 0;
}
#endif

#define STATE_NONE  0
#define STATE_FIRST 1
#define STATE_LAST  2
#define STATE_THROUGH   3

/**
 * reset note status table and slur status
 *
 * called once before each track
 */

static void
slur_erase (int *table, int *state)
{
  int i;

  for (i = 0; i < 128; i++)
    {
      table[i] = 0;
    }
  *state = STATE_NONE;
}

/**
 * prepare note status table for next chord
 *
 * only keep notes that are still playing
 *
 * called once after each chord
 */

static void
slur_shift (int *table)
{
  int i;

  for (i = 0; i < 128; i++)
    {
      if (table[i] & FLG_CONNECT_F)
        {
          table[i] |= FLG_CONNECT_B;
        }
      else
        {
          table[i] &= ~FLG_CONNECT_B;
        }
      if (table[i] & FLG_TIED_F)
        {
          table[i] |= FLG_TIED_B;
        }
      else
        {
          table[i] &= ~FLG_TIED_B;
        }
      table[i] &= (FLG_TIED_B | FLG_CONNECT_B);
    }
}

/**
 * update slur status (begin/middle/end of slurs, or no slur)
 */

static void
slur_update (int *state, int begin, int end)
{
  /* prepare flag bits */
  begin = begin ? FLG_SLUR_BEGIN : 0;
  end = end ? FLG_SLUR_END : 0;

  /* look at: previous state, begin and end, then set new state */
  switch (*state)
    {
    case STATE_FIRST:
      switch (begin + end)
        {
        case FLG_SLUR_BEGIN:
          fprintf (stderr, "warning: strange slur: extra begin\n");
          *state = STATE_THROUGH;
          break;
        case 0:
        case FLG_SLUR_BEGIN + FLG_SLUR_END:
          *state = STATE_THROUGH;
          break;
        case FLG_SLUR_END:
          *state = STATE_LAST;
          break;
        }
      break;
    case STATE_LAST:
      switch (begin + end)
        {
        case FLG_SLUR_BEGIN:
          *state = STATE_FIRST;
          break;
        case FLG_SLUR_BEGIN + FLG_SLUR_END:
          *state = STATE_THROUGH;
          fprintf (stderr, "strange slur: extra end\n");
          break;
        case 0:
          *state = STATE_NONE;
          break;
        case FLG_SLUR_END:
          fprintf (stderr, "strange slur: extra end\n");
          *state = STATE_LAST;
          break;
        }
      break;
    case STATE_NONE:
      switch (begin + end)
        {
        case FLG_SLUR_BEGIN:
          *state = STATE_FIRST;
          break;
        case 0:
          *state = STATE_NONE;
          break;
        case FLG_SLUR_BEGIN + FLG_SLUR_END:
          fprintf (stderr, "strange slur: missing begin\n");
          *state = STATE_THROUGH;
          break;
        case FLG_SLUR_END:
          *state = STATE_LAST;
          break;
        }
      break;
    case STATE_THROUGH:
      switch (begin + end)
        {
        case FLG_SLUR_BEGIN:
          fprintf (stderr, "strange slur: extra begin\n");
          *state = STATE_THROUGH;
          break;
        case 0:
        case FLG_SLUR_BEGIN + FLG_SLUR_END:
          *state = STATE_THROUGH;
          break;
        case FLG_SLUR_END:
          *state = STATE_LAST;
          break;
        }
      break;
    }
}

/**
 * insert a note and its properties in the table
 */

static void
slur_note (int *table, int state, int notenum, int staccato, int staccatissimo, int tied)
{
  int running;

  /* see if note is already playing */
  running = table[notenum] & FLG_CONNECT_B;

  /* set flags for directives */
  staccato = staccato ? FLG_STACCATO : 0;
  staccatissimo = (staccatissimo ? FLG_STACCATISSIMO : 0);

  table[notenum] &= FLG_TIED_B;

  /* set note on/off according to the slur state */
  switch (state)
    {
    case STATE_LAST:
      if (!running)
        {
          table[notenum] |= FLG_NOTE_ON + FLG_NOTE_OFF;
        }
      else
        {
          table[notenum] |= FLG_NOTE_OFF;
        }
      break;
    case STATE_NONE:
      /* normal note, no slur */
      if (running)
        {
          fprintf (stderr, "warning: note should not be on (0)\n");
        }
      table[notenum] |= FLG_NOTE_ON + FLG_NOTE_OFF;
      break;
    case STATE_THROUGH:
      /* note may be started before */
      if (!running)
        {
          table[notenum] |= FLG_NOTE_ON + FLG_CONNECT_F;
        }
      else
        {
          table[notenum] |= FLG_CONNECT_F;
        }
      break;
    case STATE_FIRST:
      /* first note of slur */
      if (running)
        {
          fprintf (stderr, "warning: note should not be on (1)\n");
          table[notenum] |= FLG_CONNECT_F;
        }
      else
        {
          table[notenum] |= FLG_NOTE_ON + FLG_CONNECT_F;
        }
      break;
    default:
      fprintf (stderr, "this shouldn't happen!\n");
      exit (3);
    }

  if (tied)
    {
      table[notenum] |= FLG_TIED_F;
      table[notenum] &= ~FLG_NOTE_OFF;
    }

  /* only relevant in normal notes or at end of slur */
  table[notenum] += staccato + staccatissimo;
}

/*
 * some predicates used in exportmidi
 */

static int
slur_on_p (int *table, int notenum)
{
  return (table[notenum] & FLG_NOTE_ON) && !(table[notenum] & FLG_TIED_B);
}

static int
slur_kill_p (int *table, int notenum)
{
  return ((table[notenum] & FLG_CONNECT_B) && !(table[notenum] & FLG_NOTE_ON)) || ((table[notenum] & FLG_TIED_B) && !(table[notenum] & FLG_NOTE_ON));
}

static int
slur_off_p (int *table, int notenum)
{
  return (table[notenum] & FLG_NOTE_OFF);
}
/* UNUSED
static int
slur_staccato_p (int *table, int notenum)
{
  return !(table[notenum] & FLG_STACCATO);
}

static int
slur_staccatissimo_p (int *table, int notenum)
{
  return !(table[notenum] & FLG_STACCATISSIMO);
}
*/
/****************************************************************/

/**
 * compute the amount of extra velocity to be added to a note
 */
/* UNUSED
static int
compute_beat (long ticks, long ticks_in_a_beat, long ticks_in_a_measure, int length, int factor)
{
  // give extra beat only to short notes
  if (length < ticks_in_a_measure / 2)
    {
      if (ticks == 0)
        {
          // put more emphasis on the first beat
          return factor;
        }
      else if (ticks % ticks_in_a_beat)
        {
          // put less emphasis on off beat notes
          return -factor;
        }
      else
        {
          // leave the rest alone
          return 0;
        }
    }
  else
    {
      return 0;
    }
}
*/
/****************************************************************/


/* convert denemo time values to ticks */

#define internaltoticks(i) ((MIDI_RESOLUTION*4)>>i)


/* this is a real high-tech macro */

#define percent(t,p)    ((t*p)/100)

static int
is_status_byte(const unsigned char status)
{
    return (status & 0x80);
}

/* puts an event into track if buffer contains valid midi message.
   and frees buffer. Returns the event, or NULL if invalid buffer.
 */
static smf_event_t *
put_event (gchar * buffer, gint numbytes,  smf_track_t * track)
{
  smf_event_t *event = NULL;
  if (numbytes && is_status_byte (buffer[0]))
    event = smf_event_new_from_pointer (buffer, numbytes);
 if (event && smf_event_is_valid (event))
    {
      smf_track_add_event_delta_pulses (track, event, 0);
    }
  g_free (buffer);
  return event;
}

static gint
directive_get_midi_override (DenemoDirective * directive)
{
  return (directive->override & DENEMO_OVERRIDE_HIDDEN) | (directive->override & DENEMO_MIDI_MASK);
}

static gint
directive_get_midi_interpretation (DenemoDirective * directive)
{
  return directive->override & DENEMO_MIDI_INTERPRETATION_MASK;
}

static gint
directive_get_midi_action (DenemoDirective * directive)
{
  return directive->override & DENEMO_MIDI_ACTION_MASK;
}

gchar *
substitute_midi_values (gchar * str, gint channel, gint volume)
{
  gchar *bytes = g_strdup (str);
  gchar *c;
  for (c = bytes; *c; c++)
    {
      if (*c == '$')
        *c = '0' + channel;
      if (*c == '%' && *(c + 1) == '%' && *(c + 2) == '%')
        sprintf (c, "%3d", volume);     //*c = itoa(volume);
    }
  //  g_debug("We have transformed %s to %s\n", str, bytes);
  return bytes;
}

static gchar *
directive_get_midi_buffer (DenemoDirective * directive, gint * pnumbytes, gint channel, gint volume)
{
   errno = 0;
  *pnumbytes = 0;
  if (directive->midibytes)
    {
      gchar *bytes;
      bytes = substitute_midi_values (directive->midibytes->str, channel, volume);
      //g_print("Got %s as midi bytes\n", bytes);
      char *next;
      gint i, numbytes;
    for (i = 0, next = bytes; *next; i++)
        {
            gchar *last = next;
          strtol (next, &next, 0);
          if(errno || (last==next))
            {
                g_free(bytes);
                return NULL;
            }
        }
      gchar *buf = (gchar *) g_malloc0 (i);
      for (i=0, next = bytes; *next; i++)
        {
          buf[i] = (char) strtol (next, &next, 0);
          //g_print("byte %x\n", buf[i]);
        }
      *pnumbytes = i;
      g_free (bytes);
      return buf;
    }
  return NULL;
}

static gint
directive_get_midi_val (DenemoDirective * directive)
{
  gint val = 0;
  if (directive->midibytes)
    {
      gchar *bytes;
      bytes = directive->midibytes->str;
      errno = 0;
      val = strtol (bytes, NULL, 0);
      if (errno)
        {
          g_warning ("String %s is bad format for MIDI value", bytes);
          return 0;
        }
    }
  return val;
}

/* change the volume according to the values passed in */
static void
change_volume (gint * volume, gint midi_val, gint midi_interpretation, gint midi_action)
{
  gdouble val;
  // g_debug("midi_val %d cur_volume %d\n", midi_val, cur_volume);
  val = (gdouble) midi_val;
  if (midi_interpretation & DENEMO_OVERRIDE_PERCENT)
    val = *volume * (midi_val / 100.0);
  if (midi_action == DENEMO_OVERRIDE_ONCE)
    g_warning ("Scripting error, ONCE for standalone directive is meaningless");
  if (midi_action == DENEMO_OVERRIDE_RAMP)
    g_warning ("Not implemented ramp yet");
  if (midi_action == DENEMO_OVERRIDE_STEP)
    {
      if (midi_interpretation & DENEMO_OVERRIDE_RELATIVE)
        *volume += val;
      else
        *volume = val;
    }
  if (*volume > 127)
    *volume = 127;
  if (*volume < 0)
    *volume = 0;
}


/* change the channel according to the values passed in */
static void
change_channel (gint * channel, gint midi_val, gint midi_interpretation, gint midi_action)
{
  gdouble val;
  //g_debug("midi_val %d cur_channel %d\n", midi_val, cur_channel);
  val = (gdouble) midi_val;
  if (midi_interpretation & DENEMO_OVERRIDE_PERCENT)
    g_warning ("Percent meaningless with channel change");
  if (midi_action == DENEMO_OVERRIDE_ONCE)
    g_warning ("Scripting error, ONCE for standalone directive is meaningless");
  if (midi_action == DENEMO_OVERRIDE_RAMP)
    g_warning ("Ramp meaningless with channel change");
  if (midi_action == DENEMO_OVERRIDE_STEP)
    {
      *channel = val;
    }
  if (*channel > 127)
    *channel = 127;
  if (*channel < 0)
    *channel = 0;
  g_message ("After channel change channel = %d", *channel);
}




/* change the tempo according to the values passed in */
static void
change_tempo (gint * tempo, gint midi_val, gint midi_interpretation, gint midi_action)
{
  gdouble val;
  val = (gdouble) midi_val;
  if (midi_interpretation & DENEMO_OVERRIDE_PERCENT)
    val = *tempo * (midi_val / 100.0);
  if (midi_action == DENEMO_OVERRIDE_ONCE)
    g_warning ("Not implemented change of tempo for one chord");
  if (midi_action == DENEMO_OVERRIDE_RAMP)
    g_warning ("Not implemented ramp yet");
  if (midi_action == DENEMO_OVERRIDE_STEP)
    {
      if (midi_interpretation & DENEMO_OVERRIDE_RELATIVE)
        *tempo += val;
      else
        *tempo = val;
    }
}

static void load_smf ( DenemoMovement *si, smf_t *smf)
  {
    if (si->smf==smf)
       return;
    gboolean midi_track = FALSE;


    g_mutex_lock (&smfmutex);
    if (Denemo.project->movement->recorded_midi_track)
      {
        if (si->smf && (((smf_track_t *) Denemo.project->movement->recorded_midi_track)->smf == si->smf))
          {
            smf_track_remove_from_smf (Denemo.project->movement->recorded_midi_track);
            midi_track = TRUE;
          }
      }
    free_midi_data (si);
    si->smf = smf;
    if (midi_track)
      smf_add_track (smf, Denemo.project->movement->recorded_midi_track);


    si->smfsync = si->changecount;
    g_mutex_unlock (&smfmutex);
  }


static void save_smf_to_file (smf_t *smf, gchar *thefilename)
{
  if (thefilename)
    {
      if (Denemo.project->movement->recorded_midi_track)
        smf_add_track (smf, Denemo.project->movement->recorded_midi_track);
      if(smf_save (smf, (const char *) thefilename))
        {
          if (Denemo.non_interactive)
            g_debug("smf_save failed");
          else
            {
              gchar *msg = g_strdup_printf ("%s%s", _("Failed to save MIDI file: "), thefilename);
              warningdialog (msg);
              g_free (msg);
            }
        }
       else if (!Denemo.non_interactive) 
            {
              gchar *msg = g_strdup_printf ("%s%s", _("Saved MIDI file: "), thefilename);
              infodialog (msg);
              g_free (msg);
            } 
      if (Denemo.project->movement->recorded_midi_track)
        smf_track_remove_from_smf (Denemo.project->movement->recorded_midi_track);
    }
}


gdouble load_lilypond_midi (gchar * outfile, gboolean keep) {
    smf_t *saved = NULL;
    gchar *midi_file = Denemo.printstatus->printname_midi[Denemo.printstatus->cycle];
    smf_t *smf = smf_load (midi_file);
    if (smf)
        {
        if (!attach_timings ())
            g_warning ("Attaching timings to objects failed\n");
        if (outfile)
            save_smf_to_file (smf, outfile);
        if (!keep )
            load_smf (Denemo.project->movement, smf);
        else
            smf_delete (smf);
        return smf_get_length_seconds (Denemo.project->movement->smf);
    } else
    {
        g_warning ("midi file %s not loaded", midi_file);
    }
    return 0.0;
}


static void do_tempo (smf_track_t* track, gint cur_tempo)
{
     smf_event_t* event = midi_tempo (cur_tempo);
     smf_track_add_event_delta_pulses (track, event, 0);
}
  
  
static void generate_midi_from_recorded_notes (smf_t *smf)
{
	smf_track_t* track;
	track = smf_track_new ();
	smf_add_track (smf, track);
	
	do_tempo (track, Denemo.project->movement->tempo);
	GList *g;
	for (g=Denemo.project->movement->recording->notes; g; g=g->next)
		{
			DenemoRecordedNote *note = g->data;
			if (note->timing>=0)
				{
					smf_event_t* event = smf_event_new_from_pointer (note->midi_event, 3);
					smf_track_add_event_seconds (track, event, note->timing/(double)Denemo.project->movement->recording->samplerate);
			}
		}
}




/*
 * the main midi output system (somewhat large)
 */

/****************************************************************/
/****************************************************************/
/****************************************************************/

/**
 * the main midi output system (somewhat large)
 * return the duration in seconds of the music stored
 */
gdouble
exportmidi (gchar * thefilename, DenemoMovement * si)
{
  /* variables for reading and decoding the object list */
  smf_event_t *event = NULL;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  measurenode *curmeasure;
  objnode *curobjnode;
  DenemoObject *curobj;
  gint curmeasurenum;
  chord chordval;
  gint duration, numdots;
  gint enshift;
  gint mid_c_offset;
  GList *curtone;
  gboolean no_recorded_midi_track = TRUE;

  gint measurenum, last = 0;

  /* variables for generating music */
  //int i;
  int d, n;

  long ticks_read;
  long ticks_written;
  long ticks_at_bar = 0;
  int cur_volume;
  gdouble master_volume;
  gboolean override_volume;
  int cur_transposition = 0;
  int global_transposition = 0;

  int midi_channel = (-1);
  int tracknumber = 0;
  int timesigupper = 4;
  int timesiglower = 4;
  int notenumber;
  int prognum;

  int width = 0;
  int notes_in_chord = 0;
  int note_status[128];
  int slur_status;
  gboolean measure_is_empty;
  int measure_has_odd_tuplet;
  int measurewidth;

  /* output velocity and timing modulation */
  //int rand_sigma = 0;
  //int rand_delta;
  //int beat = 0;

  /* to handle user preferences */
  char *envp;
  int vel_randfact = 5;
  int vel_beatfact = 10;
  int pref_width = 100;
  int pref_staccato = 25;
  int pref_staccatissimo = 10;

  /* tuplets */
  int tuplet = 0;               //level of tuplet nesting only 0 and 1 are supported
  tupopen tupletnums;
  tupopen savedtuplet;

  /* used to insert track sizes in the midi file */
  //long track_start_pos[MAX_TRACKS];
  //long track_end_pos[MAX_TRACKS];

  /* statistics */
  time_t starttime;
  //time_t endtime;

  call_out_to_guile ("(InitializeMidiGeneration)");

  /* get user preferences, if any */
  envp = getenv ("EXP_MIDI_VEL");
  if (envp)
    {
      sscanf (envp, "%d %d", &vel_randfact, &vel_beatfact);
      fprintf (stderr, "VELOCITY parameters are: %d %d\n", vel_randfact, vel_beatfact);
    }

  envp = getenv ("EXP_MIDI_PERCENT");
  if (envp)
    {
      sscanf (envp, "%d %d %d", &pref_width, &pref_staccato, &pref_staccatissimo);

      fprintf (stderr, "PERCENT parameters are: normal=%d staccato=%d %d\n", pref_width, pref_staccato, pref_staccatissimo);
    }

  /* just curious */
  time (&starttime);
 
  smf_t *smf = smf_new ();
  if(smf_set_ppqn (smf, MIDI_RESOLUTION))
    g_debug("smf_set_ppqn failed");

/*
 * end of headers and meta events, now for some real actions
 */

//play recorded MIDI if the top (click track) staff is unmuted
  if (Denemo.project->movement->recording  && Denemo.project->movement->recording->type == DENEMO_RECORDING_MIDI && !((DenemoStaff *) si->thescore->data)->mute)
	{
		generate_midi_from_recorded_notes (smf); 
		no_recorded_midi_track = FALSE;
	}


  //fraction = 1 / g_list_length (si->thescore);

  /* iterate over all staffs in movement */
  //printf ("\nsi->stafftoplay in exportmidi = %i", si->stafftoplay);
  curstaff = si->thescore;
  if (si->stafftoplay > 0)
    {
      int z = si->stafftoplay;
      while (--z)
        curstaff = curstaff->next;
    }
  if (Denemo.project->lilycontrol.directives)
            {
              GList *g = Denemo.project->lilycontrol.directives;
              DenemoDirective *directive = NULL;
              for (; g; g = g->next)
                {
                  directive = (DenemoDirective *) g->data;
                  if (!strcmp(directive->tag->str, "TransposeOnPrint"))
                    global_transposition = directive->minpixels;
                }
            }
            
 
		
  //for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
  while (curstaff)
    {
      /* handle one track */
      curstaffstruct = (DenemoStaff *) curstaff->data;

      /* select a suitable track number */
      tracknumber++;
      smf_track_t *track = smf_track_new ();
      smf_add_track (smf, track);

      /* track name */
      event = midi_meta_text (3, curstaffstruct->lily_name->str);
      smf_track_add_event_delta_pulses (track, event, 0);

      /* tempo */
      gint cur_tempo = si->tempo;

      if (no_recorded_midi_track && (tracknumber == 1))
        do_tempo (track, cur_tempo);//Do not set the tempo for later tracks as there may be tempo directives at the start of the first measure which libsmf will muddle up with any done here
 
      /* Midi Client/Port */
//      track->user_pointer = (DevicePort *) device_manager_get_DevicePort(curstaffstruct->device_port->str);

      /* The midi instrument */
      if (curstaffstruct->midi_instrument && curstaffstruct->midi_instrument->len)
        {
          event = midi_meta_text (4, curstaffstruct->midi_instrument->str);
          smf_track_add_event_delta_pulses (track, event, 0);
        }
      midi_channel = curstaffstruct->midi_channel;
      prognum = curstaffstruct->midi_prognum;

      /* set selected midi program */
      //g_message ("Using channel %d prognum %d", midi_channel, prognum);
      event = midi_change_event (MIDI_PROG_CHANGE, midi_channel, prognum);
      smf_track_add_event_delta_pulses (track, event, 0);

      /*key signature */

      event = midi_keysig (curstaffstruct->keysig.number, curstaffstruct->keysig.isminor);
      smf_track_add_event_delta_pulses (track, event, 0);

      /* Time signature */
      timesigupper = curstaffstruct->timesig.time1;
      //printf("\nstime1 = %i\n", timesigupper);

      timesiglower = curstaffstruct->timesig.time2;
      //printf("\nstime2 = %i\n", timesiglower);

      event = midi_timesig (timesigupper, timesiglower);
      smf_track_add_event_delta_pulses (track, event, 0);

      /* set a default velocity value */
      cur_volume = curstaffstruct->volume;

      master_volume = curstaffstruct->volume / 127.0;   /* new semantic for staff volume as fractional master volume */
      override_volume = curstaffstruct->override_volume;        /* force full volume output if set */
      cur_transposition = global_transposition + curstaffstruct->transposition;


      if (tuplet > 0)
        g_warning ("Unterminated tuplet at end of voice %d", tracknumber - 1);
      //Reset to  tuplets nesting level 0, in case unbalanced tuplet start end in last staff
      tuplet = 0;
      //Now that we have the channel and volume we can interpret any score and staff-wide directives for midi
      if (curstaffstruct->staff_directives)
        {
          GList *g = curstaffstruct->staff_directives;
          DenemoDirective *directive = NULL;
          for (; g; g = g->next)
            {
              gint numbytes;
              directive = (DenemoDirective *) g->data;
              gint midi_override = directive_get_midi_override (directive);
              gchar *buf = directive_get_midi_buffer (directive, &numbytes, midi_channel, cur_volume);
              if (!(midi_override & DENEMO_OVERRIDE_HIDDEN))
                if (buf)
                  if (NULL == put_event (buf, numbytes, track))
                    g_warning ("Invalid midi bytes in staff directive");
            }
        }

      if (tracknumber == 1)
        {
          if (Denemo.project->lilycontrol.directives)
            {
              //FIXME repeated code
              GList *g = Denemo.project->lilycontrol.directives;
              DenemoDirective *directive = NULL;

              for (; g; g = g->next)
                {
                  gint numbytes;
                  directive = (DenemoDirective *) g->data;
                  gint midi_override = directive_get_midi_override (directive);
                  gchar *buf = directive_get_midi_buffer (directive, &numbytes, midi_channel, cur_volume);
                  if (!(midi_override & DENEMO_OVERRIDE_HIDDEN))
                    if (buf)
                      if (NULL == put_event (buf, numbytes, track))
                        g_warning ("Invalid midi bytes in score directive"); 
                }
            }

          if (Denemo.project->movement->movementcontrol.directives)
            {
              GList *g = Denemo.project->movement->movementcontrol.directives;
              DenemoDirective *directive = NULL;
              for (; g; g = g->next)
                {
                  gint numbytes;
                  directive = (DenemoDirective *) g->data;
                  gint midi_override = directive_get_midi_override (directive);
                  gchar *buf = directive_get_midi_buffer (directive, &numbytes, midi_channel, cur_volume);
                  if (!(midi_override & DENEMO_OVERRIDE_HIDDEN))
                    if (buf)
                      if (NULL == put_event (buf, numbytes,  track))
                        g_warning ("Invalid midi bytes in movement directive");
                }
            }

        }





      /* reset measure */
      curmeasurenum = 0;
      curmeasure = curstaffstruct->themeasures;

      /* reset tick counters */
      ticks_read = 0;
      ticks_written = 0;

      /* reset slur system */
      slur_erase (note_status, &slur_status);

      /* set boundries */

      last = g_list_length (curmeasure);


      /* iterate for over measures in track */
      for (measurenum = 1; curmeasure && measurenum <= last; curmeasure = curmeasure->next, measurenum++)
        {
          /* start of measure */
          curmeasurenum++;
          measure_is_empty = TRUE;
          measure_has_odd_tuplet = 0;
          ticks_at_bar = ticks_read;
		
		  ((DenemoMeasure*)curmeasure->data)->earliest_time = ticks_read * 60.0 / (cur_tempo * MIDI_RESOLUTION);
		  
          /* iterate over objects in measure */
          for (curobjnode = (objnode *) ((DenemoMeasure*)curmeasure->data)->objects; curobjnode; curobjnode = curobjnode->next)
            {
              curobj = (DenemoObject *) curobjnode->data;
              curobj->earliest_time = ticks_read * 60.0 / (cur_tempo * MIDI_RESOLUTION);        //smf_get_length_seconds(smf);
              if (curobj->durinticks)
                  measure_is_empty = FALSE;

        /*******************************************
     *  huge switch:
     *  here we handle every kind of object
     *  that seems relevant to us
     *******************************************/
              int tmpstaccato = 0, tmpstaccatissimo = 0;
              gboolean skip_midi = FALSE;

              switch (curobj->type)
                {
                case CHORD:
          /********************
       * one or more notes
       ********************/
                  if (debug)
                    fprintf (stderr, "=============================== chord at %s\n", fmt_ticks (ticks_read));
                  chordval = *(chord *) curobj->object;
                  if (chordval.directives)
                    {
                      GList *g = chordval.directives;
                      DenemoDirective *directive = NULL;
                      for (; g; g = g->next)
                        {
                          gint numbytes;
                          directive = (DenemoDirective *) g->data;
                          gchar *buf = directive_get_midi_buffer (directive, &numbytes, midi_channel, cur_volume);
                          gint midi_override = directive_get_midi_override (directive);
                          gint midi_interpretation = directive_get_midi_interpretation (directive);
                          gint midi_action = directive_get_midi_action (directive);
                          gint midi_val = directive_get_midi_val (directive);
                          /* handle all types of MIDI overrides attached to chord here */
                          switch (midi_override)
                            {
                            case DENEMO_OVERRIDE_VOLUME:
                              if (midi_val)
                                change_volume (&cur_volume, midi_val, midi_interpretation, midi_action);
                              else
                                skip_midi = TRUE;
                              break;
                            case DENEMO_OVERRIDE_TRANSPOSITION:
                              cur_transposition = global_transposition + midi_val;
                              break;

                            case DENEMO_OVERRIDE_CHANNEL:
                              change_channel (&midi_channel, midi_val, midi_interpretation, midi_action);
                              break;
                            case DENEMO_OVERRIDE_TEMPO:
                              change_tempo (&cur_tempo, midi_val, midi_interpretation, midi_action);
                              if (cur_tempo)
                                {
                                  event = midi_tempo (cur_tempo);
                                  smf_track_add_event_delta_pulses (track, event, 0);
                                }
                              else
                                g_warning ("Tempo change to 0 bpm is illegal");
                              break;
                              //etc
                            default:
                              if (!(midi_override & DENEMO_OVERRIDE_HIDDEN))
                                if (buf)
                                  {
                                    if (NULL == put_event (buf, numbytes, track))
                                      g_warning ("Invalid midi bytes in chord directive");
                                  }
                              break;
                            }
                        }       //for each directive attached to the chord
                    }           //if there are directives
                  /* FIXME sound grace notes either simultaneously for shorter duration or steal time .... */
                  if (curobj->durinticks == 0) 
                  //if (chordval.is_grace)
                    {
                        curobj->latest_time = curobj->earliest_time;
                        break;
                    }

          /***********************************
       * compute nominal duration of note
       ***********************************/

                  numdots = chordval.numdots;
                  duration = 0;
                  if (chordval.baseduration >= 0)
                    {
                      for (d = 0; d <= numdots; d++)
                        {


                          duration += internaltoticks (chordval.baseduration) >> d;
                        }
                      if (tuplet >= 1)
                        {
                          duration *= tupletnums.numerator;
                          duration /= tupletnums.denominator;
                          if (MIDI_RESOLUTION % tupletnums.denominator)
                            {
                              measure_has_odd_tuplet = 1;
                            }
                        }

                      if (tuplet >= 2)
                        {
                          if (MIDI_RESOLUTION % tupletnums.denominator * savedtuplet.denominator)
                            {
                              measure_has_odd_tuplet = 1;
                            }
                          duration *= savedtuplet.numerator;
                          duration /= savedtuplet.denominator;
                        }
                    }
                  else
                    duration = curobj->durinticks;

          /********************************
       * compute real duration of note
       ********************************/
#if 0
                  //this is not working - it causes the delta to be -ve later
                  for (tmp = chordval.ornamentlist; tmp; tmp = tmp->next)
                    {
                      if (*(enum ornament *) tmp->data == (enum ornament) STACCATISSIMO)
                        {
                          tmpstaccatissimo = 1;
                          width = percent (duration, pref_staccatissimo);
                        }
                      else if (*(enum ornament *) tmp->data == (enum ornament) STACCATO)
                        {
                          width = percent (duration, pref_staccato);
                          tmpstaccato = 1;
                        }

                      else
                        {
                          width = percent (duration, pref_width);
                        }

                    }
                  if (debug)
                    fprintf (stderr, "duration is %s\n", fmt_ticks (duration));
#else
                  width = 0;
#endif

                  if (!chordval.notes)
                    {
                      //MUST GIVE OFF TIME FOR RESTS HERE
                      curobj->latest_time = curobj->earliest_time + duration * 60.0 / (cur_tempo * MIDI_RESOLUTION);
                      //g_debug("Adding Dummy event for rest %d %d %d\n", duration, ticks_read, ticks_written);
                      event = midi_meta_text (1 /* comment */ , "rest");
                      smf_track_add_event_delta_pulses (track, event, duration);

                      ticks_written += duration;
                      event->user_pointer = curobj;
                      //g_debug("rest of %f seconds at %f\n", duration/(double)MIDI_RESOLUTION, curobj->latest_time);
                    }

                  if (chordval.notes)
                    {

                      gint tmp_channel = midi_channel;
                      if (curobj->isinvisible)
                        midi_channel = 9;

            /**************************
         * prepare for note output
         **************************/

                      notes_in_chord = 0;
                      if (debug)
                        fprintf (stderr, "this is a chord\n");

                     // slur_update (&slur_status, chordval.slur_begin_p, chordval.slur_end_p);
 slur_update (&slur_status, 0, 0);

                      /* compute beat to add to note velocity */
                      //beat = compute_beat (ticks_read - ticks_at_bar, beats2ticks (1, timesigupper, timesiglower), bars2ticks (1, timesigupper, timesiglower), duration, vel_beatfact);

            /************************
         * begin chord read loop
         ************************/

                      for (curtone = chordval.notes; curtone; curtone = curtone->next)
                        {
                          note *thenote = (note *) curtone->data;

#ifdef NOTE_MIDI_OVERRIDES_IMPLEMENTED
                          for (g = thenote->directives; g; g = g->next)
                            {
                              DenemoDirective *directive = g->data;
                              gint midi_override = directive_get_midi_override (directive);
                              if (midi_override)
                                skip_midi = TRUE;       //TODO if it *is* overriden take action e.g. increase volume etc. For now we just drop it
                            }
#endif
                          if (!skip_midi)
                            {
                              if (chordval.has_dynamic)
                                {
                                  //g_debug ("\nThis chord has a dynamic marking attatched\n");
                                  GList *dynamic = g_list_first (chordval.dynamics);
                                  cur_volume = string_to_vol (((GString *) dynamic->data)->str, cur_volume);
                                }
                              mid_c_offset = thenote->mid_c_offset;
                              enshift = thenote->enshift;
                              notenumber = dia_to_midinote (mid_c_offset) + enshift;
                              notenumber += cur_transposition;

                              if (notenumber > 127)
                                {
                                  g_warning ("Note out of range: %d", notenumber = 60);
                                }
                              slur_note (note_status, slur_status, notenumber, tmpstaccato, tmpstaccatissimo, chordval.is_tied);
                            }

                        }
                      /* End chord read loop */
#if slurdebug
                      print_slurs (stderr, note_status, slur_status, ticks_read, ticks_written, "after chord read");
#endif
            /****************************
         * start note-on output loop
         ****************************/

                      /* kill old slurs and ties */
                      /* start new notes */

                      notes_in_chord = 0;
                      /* write delta */
                      for (n = 0; n < 128; n++)
                        {
                          gint mididelta;
                          if (slur_on_p (note_status, n) || slur_kill_p (note_status, n))
                            {
                              if (notes_in_chord++ == 0)
                                {
                                  mididelta = ticks_read - ticks_written;
                                  ticks_written = ticks_read;
                                }
                              else
                                {
                                  mididelta = 0;
                                }
                            }

                          /* compute velocity delta */
                          //rand_delta = i_random (&rand_sigma, vel_randfact);
                          /* write note on/off */
                          if (slur_on_p (note_status, n))
                            {
                              // int mix = cur_volume? compress(128, cur_volume + rand_delta + beat) : 0;
                              // FIXME the function compress is returning large values.
                              event = smf_event_new_from_bytes (MIDI_NOTE_ON | midi_channel, n,(curstaffstruct->mute)? 0: (override_volume ? 127 : (gint) (master_volume * cur_volume /*FIXME as above, mix */ )));
                              smf_track_add_event_delta_pulses (track, event, mididelta);
                              event->user_pointer = curobj;

                              curobj->earliest_time = event->time_seconds;
                              curobj->latest_time = curobj->earliest_time + duration * 60.0 / (cur_tempo * MIDI_RESOLUTION);

                              //g_debug ("'%d len %d'", event->event_number, event->midi_buffer_length);
                              //printf ("volume = %i\n", (override_volume ? 0:mix));
                            }
                          else if (slur_kill_p (note_status, n))
                            {
                              event = smf_event_new_from_bytes (MIDI_NOTE_OFF | midi_channel, n, 0);
                              //g_debug("{%d}", event->event_number);
                              smf_track_add_event_delta_pulses (track, event, mididelta);
                              //g_debug("Note  off for track %x at delta (%d) %.1f for cur_tempo %d\n", track, mididelta, event->time_seconds, cur_tempo);
                              event->user_pointer = curobj;

                              curobj->latest_time = event->time_seconds;
                              curobj->earliest_time = curobj->latest_time - duration * 60.0 / (cur_tempo * MIDI_RESOLUTION);
                              //g_debug("event off lur kill %f\n", event->time_seconds);
                            }
                        }
                      /* end of first chord output loop */

#if slurdebug
                      print_slurs (stderr, note_status, slur_status, ticks_read, ticks_written, "after loop1");
#endif
            /*****************************
         * start note-off output loop
         *****************************/

                      /* kill untied notes */

                      notes_in_chord = 0;

                      /* start second chord output loop */

                      /* write delta */
                      for (n = 0; n < 128; n++)
                        {
                          if (slur_off_p (note_status, n))
                            {
                              gint mididelta;
                              if (notes_in_chord++ == 0)
                                {
                                  width += ticks_read - ticks_written;
                                  mididelta = duration + width;
                                  ticks_written += duration + width;
                                  if (ticks_written > ticks_read + duration)
                                    {
                                      fprintf (stderr, "BAD WIDTH %d so delta %d\n" "(should not happen!)", width, mididelta);
                                      mididelta = 0;
                                    }
                                }
                              else
                                {
                                  mididelta = 0;
                                }
                              /* write note off */
                              event = smf_event_new_from_bytes (MIDI_NOTE_OFF | midi_channel, n, 60);
                              //g_debug("smf length before %d %f mididelta %d",smf_get_length_pulses(smf), smf_get_length_seconds(smf),mididelta);
                              smf_track_add_event_delta_pulses (track, event, mididelta);
                              //g_debug("Note  off for track %x at delta (%d) %.1f for cur_tempo %d\n", track, mididelta, event->time_seconds, cur_tempo);
                              //g_debug("smf length after %d %f mididelta %d", smf_get_length_pulses(smf), smf_get_length_seconds(smf),mididelta);
                              event->user_pointer = curobj;

                              curobj->latest_time = event->time_seconds;
                              curobj->earliest_time = curobj->latest_time - duration * 60.0 / (cur_tempo * MIDI_RESOLUTION);
                              //g_debug("event off %f mididelta %d duration %d for curobj->type = %d\n", event->time_seconds, mididelta, duration, curobj->type);

                            }
                        }
                      /* end of second chord output loop */
                      midi_channel = tmp_channel;
                    }           //end of for notes in chord. Note that rests have no MIDI representation, of course.
                  width = 0;
#if slurdebug
                  print_slurs (stderr, note_status, slur_status, ticks_read, ticks_written, "after loop2");
#endif
                  /* prepare for next event */

                  ticks_read += duration;
                  slur_shift (note_status);
#if slurdebug
                  print_slurs (stderr, note_status, slur_status, ticks_read, ticks_written, "after shift");
#endif
                  if (debug)
                    fprintf (stderr, "chord end\n");
                  break;

                case TIMESIG:

          /************************
       * time signature change
       ************************/

                  if (ticks_read != ticks_at_bar)
                    {
                      fprintf (stderr, "error: can only change time" " signature at beginning of a measure\n");
                    }
                  timesigupper = ((timesig *) curobj->object)->time1;
                  timesiglower = ((timesig *) curobj->object)->time2;
                  if (debug)
                    {
                      fprintf (stderr, "timesig change to %d:%d\n", timesigupper, timesiglower);
                    }

                  event = midi_timesig (timesigupper, timesiglower);
                  smf_track_add_event_delta_pulses (track, event, 0);
                  event->user_pointer = curobj;

                  curobj->earliest_time = curobj->latest_time = event->time_seconds;    //= smf_get_length_seconds(smf);
                  break;

                case TUPOPEN:

          /***************
       * tuplet begin
       ***************/

                  switch (tuplet)
                    {
                    default:
                      fprintf (stderr, "too complicated tuplets\n");
                      break;
                    case 1:
                      savedtuplet.numerator = tupletnums.numerator;
                      savedtuplet.denominator = tupletnums.denominator;
                      tupletnums.numerator = ((tupopen *) curobj->object)->numerator;
                      tupletnums.denominator = ((tupopen *) curobj->object)->denominator;
                      break;
                    case 0:
                      tupletnums.numerator = ((tupopen *) curobj->object)->numerator;
                      tupletnums.denominator = ((tupopen *) curobj->object)->denominator;
                      break;
                    }
                  tuplet++;
                  curobj->earliest_time = curobj->latest_time = event->time_seconds;    //the last event
                  break;
                case TUPCLOSE:

          /*************
       * tuplet end
       *************/

                  tuplet--;
                  switch (tuplet)
                    {
                    case 2:
                    case 3:
                    case 4:
                    case 5:
                    case 6:
                    case -1:
                      fprintf (stderr, "too complicated tuplets\n");
                      break;
                    case 1:
                      tupletnums.numerator = savedtuplet.numerator;
                      tupletnums.denominator = savedtuplet.denominator;
                      break;
                    case 0:
                      break;
                    }
                  curobj->earliest_time = curobj->latest_time = event->time_seconds;    //the last event
                  break;

                case DYNAMIC:

          /********************
       * dynamic directive
       ********************/

                  cur_volume = string_to_vol (((dynamic *) curobj->object)->type->str, cur_volume);
                  curobj->earliest_time = curobj->latest_time = event->time_seconds;    //the last event
                  break;

                case KEYSIG:
                  // curobj->object
                  //  ((keysig *) theobj->object)->number; referenced in src/measure.cpp
                  //printf("\nKEYSIG type = %d\n", ((keysig *) curobj->object)->number);
                  event = midi_keysig ((((keysig *) curobj->object)->number), curstaffstruct->keysig.isminor);
                  smf_track_add_event_delta_pulses (track, event, 0);
                  event->user_pointer = curobj;

                  curobj->earliest_time = curobj->latest_time = event->time_seconds;    //= smf_get_length_seconds(smf);
                  break;

                case CLEF:

          /***********
       * ignored!
       ***********/

                  break;

                case LILYDIRECTIVE:
                  {
                    gint theduration = curobj->durinticks;
                    if (!(((DenemoDirective *) curobj->object)->override & DENEMO_OVERRIDE_HIDDEN))
                      {
                        gint numbytes;
                        gchar *buf = directive_get_midi_buffer (curobj->object, &numbytes, midi_channel, cur_volume);
                        gint midi_override = directive_get_midi_override (curobj->object);
                        gint midi_interpretation = directive_get_midi_interpretation (curobj->object);
                        gint midi_action = directive_get_midi_action (curobj->object);
                        gint midi_val = directive_get_midi_val (curobj->object);
                        switch (midi_override)
                          {
                          case DENEMO_OVERRIDE_VOLUME:
                            change_volume (&cur_volume, midi_val, midi_interpretation, midi_action);
                            break;
                          case DENEMO_OVERRIDE_TRANSPOSITION:
                            cur_transposition = global_transposition + midi_val;
                            break;

                          case DENEMO_OVERRIDE_CHANNEL:
                            change_channel (&midi_channel, midi_val, midi_interpretation, midi_action);
                            break;
                          case DENEMO_OVERRIDE_TEMPO:
                            change_tempo (&cur_tempo, midi_val, midi_interpretation, midi_action);
                            if (cur_tempo)
                              {
                                event = midi_tempo (cur_tempo);
                                smf_track_add_event_delta_pulses (track, event, 0);     //!!!!!!!!!! if rests precede this it is not at the right time...
                              }
                            else
                              {
                                g_warning ("Tempo change to 0 bpm is illegal - re-setting.");
                                cur_tempo = 120;
                              }
                            break;
                          case DENEMO_OVERRIDE_DURATION:
                            theduration = midi_interpretation;
                            //g_debug ("Duration is %d", theduration);
                            break;
                          default:
                            if (!(midi_override & DENEMO_OVERRIDE_HIDDEN))
                              if (buf)
                                {g_print ("putting numbytes %d", numbytes);
                                  if (NULL == put_event (buf, numbytes, track))
                                    g_warning ("Directive has invalid MIDI bytes");
                                }
                            break;
                          }
                      }



                    curobj->earliest_time = event->time_seconds;        // taking the last one...
                    curobj->latest_time = curobj->earliest_time + theduration * 60.0 / (cur_tempo * MIDI_RESOLUTION);
                    ticks_read += theduration;
                  }
                  break;
                default:
#if DEBUG
                  fprintf (stderr, "midi ignoring type %d\n", curobj->type);
#endif
                  break;
                }

              //g_debug("Object type  0x%x Starts at %f Finishes %f\n",curobj->type, curobj->earliest_time, curobj->latest_time);
            } // end of objects in measure
		
	 // ((DenemoMeasure*)curmeasure->data)->latest_time = ticks_read * 60.0 / (cur_tempo * MIDI_RESOLUTION);
	  
    //  ((DenemoMeasure*)curmeasure->data)->earliest_time =
    //       curmeasure->prev? ((DenemoMeasure*)curmeasure->prev->data)->latest_time : 0.0;
//g_print ("staff %d measure earliest %f latest %f\n", tracknumber, ((DenemoMeasure*)curmeasure->data)->earliest_time, ((DenemoMeasure*)curmeasure->data)->latest_time);
      /*******************
       * Do some checking
       *******************/

          measurewidth = bars2ticks (1, timesigupper, timesiglower);

          if (measure_is_empty) // was (((DenemoMeasure*)curmeasure->data)->objects == NULL) //An empty measure - treat as whole measure silence
            ticks_read = ticks_at_bar + measurewidth;
          if (ticks_at_bar + measurewidth != ticks_read)
            {
              if ((!measure_is_empty) && curmeasure->next)
                {
                  g_warning ("warning: overfull measure in %s " "measure %d from %ld to %ld " "\n%sdifference is %ld, measure began at %ld)", curstaffstruct->lily_name->str, measurenum, ticks_read, ticks_at_bar + measurewidth, measure_has_odd_tuplet ? "(after unusual tuplet: " : "(", ticks_at_bar + measurewidth - ticks_read, ticks_at_bar);
                }
              if (debug)
                {
                  printf ("\nmeasure is empty = %d", measure_is_empty);
                  printf ("\nticks_at_bar %ld  + measurewidth %d != ticks_read %ld\n", ticks_at_bar, measurewidth, ticks_read);
                  printf ("\ninternal ticks = %d\n", internaltoticks (0));
                }

              //ticks_read = ticks_at_bar + measurewidth;//+ internaltoticks (0);
            }
          else
            {
              ;                 //fprintf (stderr, "[%d]", measurenum);
            }
          fflush (stdout);

      /*************************
       * Done with this measure
       *************************/
		

        }                       /* Done with this staff */

    /***********************
     * Done with this track
     ***********************/

      //fprintf (stderr, "[%s done]\n", curstaffstruct->lily_name->str);
      fflush (stdout);
      /*gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(si->progressbar), fraction +
         gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(si->progressbar))); */
      if (si->stafftoplay == 0)
        curstaff = curstaff->next;
      else
        break;
    }
    
    
  
  
   

#if 0
{
  smf_event_t *event;
  smf_rewind (smf);
  while ((event=smf_get_next_event (smf)))
    g_print ("generated 0x%hhX 0x%hhX 0x%hhX\n", *(event->midi_buffer+0), *(event->midi_buffer+1), *(event->midi_buffer+2));
}
#endif


  /********
   * Done!
   ********/
  save_smf_to_file (smf, thefilename);


  load_smf (si, smf);//frees the old Denemo.project->movement->smf and points it to this one



  if (si->start_time < 0.0)
    si->start_time = 0.0;
  if (si->end_time < 0.0 || (si->end_time > smf_get_length_seconds (smf)))
    si->end_time = smf_get_length_seconds (smf);
  g_print ("Start time %f end time %f\n", si->start_time, si->end_time);

  call_out_to_guile ("(FinalizeMidiGeneration)");
  return smf_get_length_seconds (smf);
}

void
free_midi_data (DenemoMovement * si)
{
  if (si->smf)
    {
      smf_t *temp = si->smf;
      si->smf = NULL;
      smf_delete (temp);
    }
}


/*
 * That's all, folks!
 *
 * End of exportmidi.c
 */
