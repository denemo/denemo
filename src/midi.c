/* midi.cpp
 * functions for direct output to /dev/sequencer
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Brian Delaney
 */

#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifdef HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <gtk/gtk.h>
#ifndef G_OS_WIN32
#include <sys/ioctl.h>
#endif
#include <denemo/denemo.h>
#include "draw.h"


#define SEQ_DEV    "/dev/sequencer"
#define SEQ_DEV_N  0

#ifdef HAVE_SYS_SOUNDCARD_H
struct synth_info card_info;

SEQ_DEFINEBUF (128);
/*Prototype for function */
void seqbuf_dump ();
#endif

static int sequencer_fd = -1;
static gint ttag;
static gboolean shouldremove = FALSE;

/**
 * Close the sequencer device
 *
 */
void
midi_cleanup ()
{
  (void) close (sequencer_fd);
}

/**
 * Initialise the sequencer device ready for immediate playback
 *
 */
gint
midi_init ()
{
#ifdef HAVE_SYS_SOUNDCARD_H
  if ((sequencer_fd = open ("/dev/sequencer", O_WRONLY)) == -1)
    {
      perror (_("Error opening sequencer"));
      return -1;
    }

  card_info.device = 0;

  if (ioctl (sequencer_fd, SNDCTL_SYNTH_INFO, &card_info) == -1)
    {
      perror (_("Cannot get info on soundcard"));
      return -1;
    }

  printf (_("Synthesizer detected: %s\n"), card_info.name);
  printf (_("Synthesizer supports %d voices.\n"), card_info.nr_voices);

  /* Reset the sequencer */
  if (ioctl (sequencer_fd, SNDCTL_SEQ_RESET) == -1)
    {
      perror (_("Error resetting sequencer"));
      return -1;
    }

  SEQ_DUMPBUF ();
  close (sequencer_fd);
  sequencer_fd = -1;

#endif
  return 0;
}

#ifdef HAVE_SYS_SOUNDCARD_H
/**
 * Dump the global buffer to the sequencer device
 *
 */
void
seqbuf_dump ()
{
  if (_seqbufptr)
    if (write (sequencer_fd, _seqbuf, _seqbufptr) == -1)
      {
	perror (_("Error during seqbuf_dump"));
	exit (-1);
      }
  _seqbufptr = 0;
}

/**
 *  Used to play each tone in the given chord
 *  (a g_list_foreach function)
 */
static void
playtone (gpointer tone, gpointer chord, int prognum)
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

  SEQ_SET_PATCH (SEQ_DEV_N, voice, prognum);

  SEQ_START_NOTE (SEQ_DEV_N, voice, key, 127);
  SEQ_DUMPBUF ();
}

/**
 *  Used to stop each tone in the given chord
 *  (a g_list_foreach function)
 */
static void
stoptone (gpointer tone, gpointer chord)
{
  gint offset;
  gint voice;
  gchar key;
  /* Because mid_c_offset is a measure of notes and we need a measure of
   * half-steps, this array will help */
  const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

  offset = ((note *) tone)->mid_c_offset;

  /* 60 is middle-C in MIDI keys */
  key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
  key += ((note *) tone)->enshift;
  voice = g_list_index ((GList *) chord, tone);

  SEQ_STOP_NOTE (0, voice, key, 127);
  SEQ_DUMPBUF ();
}

#endif

/**
 * Close the sequencer device.  This is used by playnotes
 *
 */
static gint
close_seqfd (gpointer data)
{
  close (sequencer_fd);
  sequencer_fd = -1;
  shouldremove = FALSE;
  return FALSE;			/* Timeout function won't be called again */
}

/** 
 * This version of the function opens and closes /dev/sequencer w/ each
 * write, as a separate process for performance-type reasons 
 */
void
playnotes (gboolean doit, chord chord_to_play, int prognum)
{

#ifdef HAVE_SYS_SOUNDCARD_H
  if (doit)
    if (sequencer_fd != -1
	|| (sequencer_fd = open ("/dev/sequencer", O_WRONLY)) != -1)
      {
	GList *tone;
	SEQ_START_TIMER ();
	/*
	   g_list_foreach (chord_to_play.notes,
	   (GFunc) playtone, chord_to_play.notes);
	 */
	tone = chord_to_play.notes;
	while (tone)
	  {
	    playtone (tone->data, chord_to_play.notes, prognum);
	    tone = tone->next;
	  }
	SEQ_DELTA_TIME (50);


	g_list_foreach (chord_to_play.notes,
			(GFunc) stoptone, chord_to_play.notes);
	if (shouldremove)
	  gtk_timeout_remove (ttag);
	ttag = gtk_timeout_add (1000, close_seqfd, NULL);
	/*shouldremove = TRUE; */
      }
#endif
}

gint
playsong (DenemoScore * si)
{

  return 0;
}
