/* 
 * exportmidi.cpp 
 *
 * Functions for exporting a Standard Midi file
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C) 2001, 2002 Per Andersson
 *
 * License: this file may be used under the FSF GPL version 2
 */

#define EXPORTMIDI_VERSION	"1.2"

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

#include "instrumentname.h"
#include <denemo/denemo.h>


/* 
 * only for developers
 */

int debug = 0;

/****************************************************************/

/*
 * support macros for exportmidi()
 */

/* length, in ticks, of a quarter note */
#define MIDI_RESOLUTION		384

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

char *dyn_strings[DYN_MAX] = {
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

int dyn_vol[DYN_MAX] = { 1, 16, 32, 48, 64, 80, 96, 112, 127 };

/**
 * convert dynamic string to midi velocity 0 .. 127
 */

int
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
 *	unbiased random generator,
 *	
 *	returns a value within +- maxdev
 */

int
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

/****************************************************************/

/**
 *	limit a value to be within limits
 *	(simple first approach)
 */

int
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

  return outvalue;
}

/****************************************************************/

/**
 * integer base 2 logarithm used in time-sig code
 */

int
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

/****************************************************************/

/*
 * midi file basic output functions
 */

/**
 * Output a byte to the file
 */
void
midi_byte (FILE * fd, int c0)
{
  putc (c0, fd);
}

/**
 * Output a 2 bytes to the file
 */
void
midi_2_bytes (FILE * fd, int c0, int c1)
{
  putc (c0, fd);
  putc (c1, fd);
}

/**
 * Output a three bytes to the file
 */
void
midi_3_bytes (FILE * fd, int c0, int c1, int c2)
{
  putc (c0, fd);
  putc (c1, fd);
  putc (c2, fd);
}

/**
 * Output a four bytes to the file
 */
void
midi_4_bytes (FILE * fd, int c0, int c1, int c2, int c3)
{
  putc (c0, fd);
  putc (c1, fd);
  putc (c2, fd);
  putc (c3, fd);
}

/**
 * Output short int to the file
 */
void
midi_short (FILE * fd, int amount)
{
  midi_2_bytes (fd, (amount >> 8) & 255, (amount >> 0) & 255);
}

/**
 *
 */
void
midi_medium (FILE * fd, long amount)
{
  midi_3_bytes (fd,
		(amount >> 16) & 255,
		(amount >> 8) & 255, (amount >> 0) & 255);
}

/**
 * Output long int to the file
 */
void
midi_long (FILE * fd, long amount)
{
  if (amount > 4000)
    fprintf (stderr, "[midilong:%ld=%lx]", amount, amount);

  midi_4_bytes (fd,
		(amount >> 24) & 255,
		(amount >> 16) & 255,
		(amount >> 8) & 255, (amount >> 0) & 255);
}

/****************************************************************/

/*
 * now some complex midi output functions
 *
 * most meta event functions write their own
 * (zero) delta time code
 */

/**
 * standard midi file specific variable length time diff encoding
 *
 * one delta is required before each event
 *
 * the lower seven bits is a number 0 .. 127
 * the high bit means the are bytes more to come
 */

int
midi_delta (FILE * fd, long delta, int more)
{
  char x;
  int sz = 0;

  if (delta < 0)
    {
      fprintf (stderr, " Error: negative delta! (%ld)\n", delta);
      midi_byte (fd, 0);
      return 0;
    };

  if (delta > 127)
    {
      sz += midi_delta (fd, delta / 128, 1);
    }

  x = (more ? 128 : 0) | (delta % 128);

  midi_byte (fd, x);
  sz++;

  return sz;
}

/**
 * this is used by several meta events
 */

void
midi_meta_text (FILE * fd, int metatype, char *string)
{
  int len;

  midi_delta (fd, 0, 0);

  len = strlen (string);

  /* meta event */
  midi_byte (fd, 0xff);

  /* meta type */
  midi_byte (fd, metatype);

  /* meta size */
  midi_byte (fd, len);

  /* meta text */
  fprintf (fd, string);
}

/**
 * put two strings and a number in a midi comment (debug)
 */

void
midi_comment (FILE * fd, char *txt1, long number, char *txt2)
{
  char buf[200];
  if (number)
    {
      sprintf (buf, "%s %ld %s", txt1, number, txt2);
      midi_meta_text (fd, 1, buf);
    }
  else
    {
      sprintf (buf, "%s%s", txt1, txt2);
      midi_meta_text (fd, 1, buf);
    }
}

/**
 *  put a string and two numbers in a midi comment (debug)
 */

void
midi_com2 (FILE * fd, char *txt1, long number1, long number2)
{
  char buf[200];

  sprintf (buf, "%s %ld %ld", txt1, number1, number2);
  midi_meta_text (fd, 1, buf);
}

/**
 * used for most midi events: note on, note off, aftertouch ...
 */

void
midi_channel_event (FILE * fd, int type, int chan, int note, int val)
{
  midi_3_bytes (fd, type | chan, note, val);
  if (debug)
    printf ("mchev: %x %d %d %d\n", type, chan + 1, note + 1, val);

}

/**
 * almost only used for program change
 */

void
midi_change_event (FILE * fd, int type, int chan, int val)
{
  midi_2_bytes (fd, type | chan, val);
}

/**
 * meta event: required after every track
 */

void
midi_end_of_track (FILE * fd)
{
  midi_4_bytes (fd, 0, 0xff, 0x2f, 0);
}

/**
 * meta event: set time signature, time scale and metronome
 */

void
midi_timesig (FILE * fd, int upper, int lower)
{
  div_t n;	
  int click = 24;

  if (lower !=0){
    n = div(4, lower);
    click = 24 * ((float) n.quot + (float) n.rem);
  }

  midi_4_bytes (fd, 0, 0xff, 0x58, 4);
  midi_4_bytes (fd, upper, twolog (lower), click, 8);
}

/**
 * meta event: set key signature
 */

void
midi_keysig (FILE * fd, gint key, gint isminor)
{
  midi_4_bytes (fd, 0, 0xff, 0x59, 2);
  midi_2_bytes (fd, key, isminor);
}

/**
 * meta event: set system clock speed
 */

void
midi_tempo (FILE * fd, long tempo)
{
  long midi_tempo;

  if (tempo == 0)
    {
      tempo = 1;
    }
  midi_delta (fd, 0, 0);
  midi_3_bytes (fd, 0xff, 0x51, 3);
  midi_tempo = 60000000 / tempo;
  midi_medium (fd, midi_tempo);
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

#define FLG_CONNECT_B		0x01
#define FLG_CONNECT_F		0x02
#define FLG_SLUR_BEGIN		0x04
#define FLG_SLUR_END		0x08

#define FLG_NOTE_ON		0x10
#define FLG_NOTE_OFF		0x20
#define FLG_STACCATO		0x40
#define FLG_STACCATISSIMO	0x80

#define FLG_TIED_F		0x100
#define FLG_TIED_B		0x200

/**
 * debug print-out of slur table
 */

char *
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

/**
 * Output slur descriptions to the given file
 * 
 */
int
print_slurs (FILE * fd, int *tab, int status,
	     int t_read, int t_written, char *txt)
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
		   i, tab[i],
		   tab[i] & FLG_CONNECT_B ? "connect-backward " : "",
		   tab[i] & FLG_CONNECT_F ? "connect-forward " : "",
		   tab[i] & FLG_SLUR_BEGIN ? "begin " : "",
		   tab[i] & FLG_SLUR_END ? "end " : "",
		   tab[i] & FLG_STACCATO ? "staccato " : "",
		   tab[i] & FLG_STACCATISSIMO ? "staccatissimo " : "",
		   tab[i] & FLG_NOTE_ON ? "note_is_on " : "",
		   tab[i] & FLG_NOTE_OFF ? "note_is_off " : "",
		   tab[i] & FLG_TIED_B ? "tied_backward " : "",
		   tab[i] & FLG_TIED_F ? "tied_forward " : "");
	}
    }

  /* not used */
  return 0;
}

#define STATE_NONE	0
#define STATE_FIRST	1
#define	STATE_LAST	2
#define STATE_THROUGH	3

/**
 * reset note status table and slur status
 * 
 * called once before each track
 */

void
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

void
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

void
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
 * insert a note and it's properties in the table
 */

void
slur_note (int *table, int state, int notenum,
	   int staccato, int staccatissimo, int tied)
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

int
slur_on_p (int *table, int notenum)
{
  return (table[notenum] & FLG_NOTE_ON) && !(table[notenum] & FLG_TIED_B);
}

int
slur_kill_p (int *table, int notenum)
{
  return
    ((table[notenum] & FLG_CONNECT_B) && !(table[notenum] & FLG_NOTE_ON))
    || ((table[notenum] & FLG_TIED_B) && !(table[notenum] & FLG_NOTE_ON));
}

int
slur_off_p (int *table, int notenum)
{
  return (table[notenum] & FLG_NOTE_OFF);
}

int
slur_staccato_p (int *table, int notenum)
{
  return !(table[notenum] & FLG_STACCATO);
}

int
slur_staccatissimo_p (int *table, int notenum)
{
  return !(table[notenum] & FLG_STACCATISSIMO);
}

/****************************************************************/

/**
 * compute the amount of extra velocity to be added to a note
 */

int
compute_beat (long ticks, long ticks_in_a_beat,
	      long ticks_in_a_measure, int length, int factor)
{
  /* give extra beat only to short notes */
  if (length < ticks_in_a_measure / 2)
    {
      if (ticks == 0)
	{
	  /* put more emphasis on the first beat */
	  return factor;
	}
      else if (ticks % ticks_in_a_beat)
	{
	  /* put less emphasis on off beat notes */
	  return -factor;
	}
      else
	{
	  /* leave the rest alone */
	  return 0;
	}
    }
  else
    {
      return 0;
    }
}

/****************************************************************/

/**
 * some important midi command bytes
 */

#define MIDI_NOTE_OFF		0x80
#define MIDI_NOTE_ON		0x90
#define MIDI_PROG_CHANGE	0xc0
#define	MAX_TRACKS		16

/* convert denemo time values to ticks */

#define internaltoticks(i) ((MIDI_RESOLUTION*4)>>i)


/* this is a real high-tech macro */

#define percent(t,p)	((t*p)/100)

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
exportmidi (gchar * thefilename, DenemoScore * si, gint start, gint end)
{
  /* variables for reading and decoding the object list */
  FILE *fd;
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
  gdouble fraction;
  GString *filename = g_string_new (thefilename);
  gint measurenum, last = 0;

  /* variables for generating music */
  int i;
  int d, n;

  long ticks_read;
  long ticks_written;
  long ticks_at_bar = 0;
  int cur_volume;
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
  int measure_is_empty;
  int measure_has_odd_tuplet;
  int measurewidth;

  /* output velocity and timing modulation */
  int rand_sigma = 0;
  int rand_delta;
  int beat = 0;

  /* to handle user preferences */
  char *envp;
  int vel_randfact = 5;
  int vel_beatfact = 10;
  int pref_width = 100;
  int pref_staccato = 25;
  int pref_staccatissimo = 10;

  /* tuplets */
  int tuplet = 0;
  tupopen tupletnums;
  tupopen savedtuplet;

  /* used to insert track sizes in the midi file */
  long track_start_pos[MAX_TRACKS];
  long track_end_pos[MAX_TRACKS];

  /* statistics */
  time_t starttime;
  time_t endtime;

  /* get user preferences, if any */
  envp = getenv ("EXP_MIDI_VEL");
  if (envp)
    {
      sscanf (envp, "%d %d", &vel_randfact, &vel_beatfact);
      fprintf (stderr, "VELOCITY parameters are: %d %d\n",
	       vel_randfact, vel_beatfact);
    }

  envp = getenv ("EXP_MIDI_PERCENT");
  if (envp)
    {
      sscanf (envp, "%d %d %d",
	      &pref_width, &pref_staccato, &pref_staccatissimo);

      fprintf (stderr,
	       "PERCENT parameters are: normal=%d staccato=%d %d\n",
	       pref_width, pref_staccato, pref_staccatissimo);
    }

  /* just curious */
  time (&starttime);

  /* Append .mid to the filename */
  /*g_string_append (filename, ".midi"); */

  /* Now open the file */
  fd = fopen (filename->str, "w");

  /* verify file descriptor */
  if (!fd)
    {
      perror (filename->str);
      fprintf (stderr, "Can't open midi file %s for writing!\n",
	       filename->str);
      return;
    }

  /* MIDI file header */
  midi_4_bytes (fd, 'M', 'T', 'h', 'd');
  midi_long (fd, 6);

  /* midi file type */
  midi_short (fd, 1);

  /* tracks in the file */
  /* this gets overwritten before closing the file */
  midi_short (fd, 0);

  /* ticks in a quarter note */
  midi_short (fd, MIDI_RESOLUTION);

/*
 * end of headers and meta events, now for some real actions
 */
  fraction = 1 / g_list_length (si->thescore);

  /* iterate over all tracks in file */
  printf ("\nsi->stafftoplay in exportmidi = %i", si->stafftoplay);
  curstaff = si->thescore;
  if (si->stafftoplay > 0)
    {
      int z = si->stafftoplay;
      while (--z)
	curstaff = curstaff->next;
    }
  //for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
  while (curstaff)
    {
      /* handle one track */
      curstaffstruct = (DenemoStaff *) curstaff->data;

      /* select a suitable track number */
      tracknumber++;
      if (!strcmp (curstaffstruct->midi_instrument->str, "drums"))
	{
	  midi_channel = 9;
	}
      else
	{
	  midi_channel = (tracknumber >= 9) ? tracknumber + 1 : tracknumber;
	}

      /* MIDI track header */
      midi_4_bytes (fd, 'M', 'T', 'r', 'k');

      /* remember track size (to be filled in later, when we know the size) */
      midi_long (fd, 0);
      track_start_pos[tracknumber] = ftell (fd);

      /* track name */
      midi_meta_text (fd, 3, curstaffstruct->lily_name->str);

      /* tempo */
      midi_tempo (fd, si->tempo);

      /* The midi instrument */
      midi_meta_text (fd, 4, curstaffstruct->midi_instrument->str);

      if (curstaffstruct->midi_prognum_override != TRUE){
      /* drums is a special case, else try to match an instrument name */
      	      if (midi_channel == 9)
		{
		  prognum = 0;
		}
	      else
		{
		  prognum = select_program (curstaffstruct->midi_instrument->str);
		}
      }
      else{
	      prognum = curstaffstruct->midi_prognum;
	      midi_channel = curstaffstruct->midi_channel;
      }

      /* set selected midi program */
      midi_delta (fd, 0, 0);
      midi_change_event (fd, MIDI_PROG_CHANGE, midi_channel, prognum);

      /*key signature */
      midi_keysig (fd, curstaffstruct->skey, curstaffstruct->skey_isminor);

      /* Time signature */
      timesigupper = curstaffstruct->stime1;
      //printf("\nstime1 = %i\n", timesigupper);

      timesiglower = curstaffstruct->stime2;
      //printf("\nstime2 = %i\n", timesiglower);

      midi_timesig (fd, timesigupper, timesiglower);

      /* set a default velocity value */
      cur_volume = dyn_vol[DYN_MF];

      /* reset measure */
      curmeasurenum = 0;
      curmeasure = curstaffstruct->measures;

      /* reset tick counters */
      ticks_read = 0;
      ticks_written = 0;

      /* reset slur system */
      slur_erase (note_status, &slur_status);

      /* set boundries */
      if (!end)
	last = g_list_length (curmeasure);
      else
	last = end;
      if (start)
	curmeasure = g_list_nth (curmeasure, start - 1);
      /* iterate for over measures in track */
      for (measurenum = MAX (start, 1); curmeasure && measurenum <= last;
	   curmeasure = curmeasure->next, measurenum++)
	{
	  //printf
	  //  ("\nMAX start,1, curmeasurenum, last in export midi is = %i, %i, %i\n",
	  //   MAX (start, 1), curmeasurenum, last);
	  //printf("\ncurmeasure && measurenum in export midi is = %i\n", (curmeasure && measurenum));
	  //printf("\ncurmeasurenum in export midi is = %i\n", curmeasurenum);
	  /* start of measure */
	  curmeasurenum++;
	  measure_is_empty = 1;
	  measure_has_odd_tuplet = 0;
	  ticks_at_bar = ticks_read;

	  /* iterate for over objects in measure */
	  for (curobjnode = (objnode *) curmeasure->data; curobjnode;
	       curobjnode = curobjnode->next)
	    {
	      curobj = (DenemoObject *) curobjnode->data;

	/*******************************************
	 *	huge switch:
	 * 	here we handle every kind of object
	 * 	that seems relevant to us
	 *******************************************/
	      int tmpstaccato = 0, tmpstaccatissimo = 0;
	      GList *tmp;

	      switch (curobj->type)
		{
		case CHORD:
	  /******************** 
	   * one or more notes
	   ********************/


		  measure_is_empty = 0;
		  if (debug)
		    fprintf (stderr,
			     "=============================== chord at %s\n",
			     fmt_ticks (ticks_read));
		  chordval = *(chord *) curobj->object;

		  if(chordval.is_grace)
		    break;

	  /***********************************
	   * compute nominal duration of note 
	   ***********************************/

		  numdots = chordval.numdots;
		  duration = 0;
		  for (d = 0; d <= numdots; d++)
		    {
		      duration +=
			internaltoticks (chordval.baseduration) >> d;
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
		      if (MIDI_RESOLUTION % tupletnums.denominator *
			  savedtuplet.denominator)
			{
			  measure_has_odd_tuplet = 1;
			}
		      duration *= savedtuplet.numerator;
		      duration /= savedtuplet.denominator;
		    }

	  /********************************
	   * compute real duration of note
	   ********************************/
		  for (tmp = chordval.ornamentlist; tmp; tmp = tmp->next)
		    {
		      if (*(enum ornament *) tmp->data ==
			  (enum ornament) STACCATISSIMO)
			{
			  tmpstaccatissimo = 1;
			  width = percent (duration, pref_staccatissimo);
			}
		      else if (*(enum ornament *) tmp->data ==
			       (enum ornament) STACCATO)
			{
			  width = percent (duration, pref_staccato);
			  tmpstaccato = 1;
			}
#if 0
		      else
			{
			  width = percent (duration, pref_width);
			}
#endif
		    }
		  if (debug)
		    fprintf (stderr, "duration is %s\n",
			     fmt_ticks (duration));
		  if (chordval.notes)
		    {

	    /**************************
	     * prepare for note output
	     **************************/

		      notes_in_chord = 0;
		      if (debug)
			fprintf (stderr, "this is a chord\n");

		      slur_update (&slur_status,
				   chordval.slur_begin_p,
				   chordval.slur_end_p);

		      /* compute beat to add to note velocity */
		      beat = compute_beat (ticks_read - ticks_at_bar,
					   beats2ticks (1, timesigupper,
							timesiglower),
					   bars2ticks (1, timesigupper,
						       timesiglower),
					   duration, vel_beatfact);

	    /************************
	     * begin chord read loop 
	     ************************/

		      for (curtone = chordval.notes; curtone;
			   curtone = curtone->next)
			{
			  gint tmp = cur_volume;
			  if (curobj->isinvisible)
			    cur_volume = 0;
			  else
			    cur_volume = tmp;
			  mid_c_offset =
			    ((note *) curtone->data)->mid_c_offset;
			  enshift = ((note *) curtone->data)->enshift;
			  //printf("mid_c_offset = %i\n",mid_c_offset);
			  //printf("enshift = %i\n",enshift);
			  notenumber =
			    dia_to_midinote (mid_c_offset) + enshift;
			  //printf ("transposition = %i\n",
			  //	  curstaffstruct->transposition);
			  notenumber += curstaffstruct->transposition;
			  //printf ("notenumber = %i\n", notenumber);
			  slur_note (note_status,
				     slur_status,
				     notenumber,
				     tmpstaccato,
				     tmpstaccatissimo, chordval.is_tied);

			}
		      /* End chord read loop */
#if slurdebug
		      print_slurs (stderr,
				   note_status, slur_status,
				   ticks_read, ticks_written,
				   "after chord read");
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
			  if (slur_on_p (note_status, n)
			      || slur_kill_p (note_status, n))
			    {
			      if (notes_in_chord++ == 0)
				{
				  midi_delta (fd, ticks_read - ticks_written,
					      0);
				  ticks_written = ticks_read;
				}
			      else
				{
				  midi_delta (fd, 0, 0);
				}
			    }

			  /* compute velocity delta */
			  rand_delta = i_random (&rand_sigma, vel_randfact);

			  /* write note on/off */
			  if (slur_on_p (note_status, n))
			    {
			      midi_channel_event (fd, MIDI_NOTE_ON,
						  midi_channel, n,
						  compress (128,
							    cur_volume +
							    rand_delta +
							    beat));
			      // printf ("n = %i\n", n);
			    }
			  else if (slur_kill_p (note_status, n))
			    {
			      midi_channel_event (fd, MIDI_NOTE_OFF,
						  midi_channel, n, 12820);
			    }
			}
		      /* end of first chord output loop */

#if slurdebug
		      print_slurs (stderr,
				   note_status, slur_status,
				   ticks_read, ticks_written, "after loop1");
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
			      if (notes_in_chord++ == 0)
				{
				  width += ticks_read - ticks_written;
				  midi_delta (fd, duration + width, 0);
				  ticks_written += duration + width;
				  if (ticks_written > ticks_read + duration)
				    {
				      fprintf (stderr, "BAD WIDTH %d\n"
					       "(should not happen!)", width);
				    }
				}
			      else
				{
				  midi_delta (fd, 0, 0);
				}

			      /* write note off */
			      midi_channel_event (fd, MIDI_NOTE_OFF,
						  midi_channel, n, 60);
			    }
			}
		      /* end of second chord output loop */

		    }
		  width = 0;
#if slurdebug
		  print_slurs (stderr,
			       note_status, slur_status,
			       ticks_read, ticks_written, "after loop2");
#endif
		  /* prepare for next event */

		  ticks_read += duration;
		  slur_shift (note_status);
#if slurdebug
		  print_slurs (stderr,
			       note_status, slur_status,
			       ticks_read, ticks_written, "after shift");
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
		      fprintf (stderr, "error: can only change time"
			       " signature at beginning of a measure\n");
		    }
		  timesigupper = ((timesig *) curobj->object)->time1;
		  timesiglower = ((timesig *) curobj->object)->time2;
		  if (debug)
		    {
		      fprintf (stderr, "timesig change to %d:%d\n",
			       timesigupper, timesiglower);
		    }
		  printf ("\nchange to timesigupper = %i\n", timesigupper);
		  printf ("\nchange to timesiglower = %i\n", timesiglower);

		  midi_timesig (fd, timesigupper, timesiglower);
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
		      tupletnums.numerator =
			((tupopen *) curobj->object)->numerator;
		      tupletnums.denominator =
			((tupopen *) curobj->object)->denominator;
		      break;
		    case 0:
		      tupletnums.numerator =
			((tupopen *) curobj->object)->numerator;
		      tupletnums.denominator =
			((tupopen *) curobj->object)->denominator;
		      break;
		    }
		  tuplet++;

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

		  break;

		case DYNAMIC:

	  /********************
	   * dynamic directive
	   ********************/

		  cur_volume =
		    string_to_vol (((dynamic *) curobj->object)->type->str,
				   cur_volume);
		  break;

		case KEYSIG:
		  // curobj->object
		  //  ((keysig *) theobj->object)->number; referenced in src/measureops.cpp       
		  //printf("\nKEYSIG type = %d\n", ((keysig *) curobj->object)->number);
		  midi_keysig (fd, (((keysig *) curobj->object)->number),
			       curstaffstruct->skey_isminor);

		case CLEF:

	  /***********
	   * ignored!
	   ***********/

		  break;
		default:
		  fprintf (stderr, "ignoring type %d\n", curobj->type);
		  break;
		}
	    }

      /*******************
       * Do some checking
       *******************/

	  measurewidth = bars2ticks (1, timesigupper, timesiglower);
	  if (ticks_at_bar + measurewidth != ticks_read)
	    {
	      if ((!measure_is_empty) && curmeasure->next)
		{
		  fprintf (stderr,
			   "warning: ticks_read reset in %s "
			   "measure %d from %ld to %ld "
			   "\n%sdifference is %ld, measure began at %ld)\n",
			   curstaffstruct->lily_name->str,
			   measurenum,
			   ticks_read,
			   ticks_at_bar + measurewidth,
			   measure_has_odd_tuplet ?
			   "(after unusual tuplet: " : "(",
			   ticks_at_bar + measurewidth - ticks_read,
			   ticks_at_bar);
		}
 		if(debug){
		  printf("\nmeasure is empty = %d", measure_is_empty);
		  printf("\nticks_at_bar(%d) + measurewidth(%d)!= "
				  "ticks_read(%d)\n",
				  ticks_at_bar, measurewidth, ticks_read);
		  printf("\ninternal ticks = %d\n", internaltoticks(0));
		}

	      ticks_read = ticks_at_bar + measurewidth;//+ internaltoticks (0);
	    }
	  else
	    {
	      fprintf (stderr, "[%d]", measurenum);
	    }
	  fflush (stdout);

      /*************************
       * Done with this measure
       *************************/


	  /* end the track (required!) */
	  if ((!curmeasure->next)
	      || (MAX (start, 1) + curmeasurenum == last + 1))
	    {
	      midi_end_of_track (fd);
	      printf ("\nend of track\n");
	    }
	  track_end_pos[tracknumber] = ftell (fd);

	}			/* Done with this staff */

    /***********************
     * Done with this track
     ***********************/

      fprintf (stderr, "[%s done]\n", curstaffstruct->lily_name->str);
      fflush (stdout);
      /*gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(si->progressbar), fraction + 
         gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(si->progressbar))); */
      if (si->stafftoplay == 0)
	curstaff = curstaff->next;
      else
	break;
    }

  /*********************
   * Insert track sizes
   *********************/

  /* insert track count in file header */
  fseek (fd, 10, SEEK_SET);
  midi_short (fd, tracknumber);
  fflush (fd);

  /* insert track byte count in track headers */
  for (i = 1; i <= tracknumber; i++)
    {
      fseek (fd, track_start_pos[i] - 4, SEEK_SET);
      midi_long (fd, track_end_pos[i] - track_start_pos[i]);
      fflush (fd);

      if (debug)
	fprintf (stderr,
		 "track %d from: %ld to %ld (%ld at %ld)\n", i,
		 track_start_pos[i], track_end_pos[i],
		 (track_end_pos[i] - track_start_pos[i]), ftell (fd) - 4);
    }

  /********
   * Done!
   ********/

  /* we are done */
  fclose (fd);

  /* just curious again
  time (&endtime);
  fprintf (stderr,
	   "exportmidi %s done:\nfile: %s %s %s\ntotal elapsed time is %ld\n",
	   EXPORTMIDI_VERSION, filename->str, __DATE__, __TIME__,
	   endtime - starttime);
 */
  //g_print("total duration %f seconds\n", 60.0*ticks_read/(MIDI_RESOLUTION*(gdouble)si->tempo));

  g_string_free (filename, FALSE);
  return  60.0*ticks_read/(MIDI_RESOLUTION*(double)si->tempo);
}

/*
 * That's all, folks!
 *
 * End of exportmidi.c
 */
