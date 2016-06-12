/*
 * pitchrecog.c
 * upgraded to AUBIO 4  Paul Brossier  2014
  Hacked from aubionotes.c for denemo by Richard Shann (c) 2007
   Copyright (C) 2003 Paul Brossier

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef _HAVE_PORTAUDIO_
#include "audio/audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <math.h>
#define AUBIO_UNSTABLE 1
#ifdef DISABLE_AUBIO
#else
#include <aubio/aubio.h>
#include <glib.h>
#include "audio/pitchrecog.h"

typedef int (*aubio_process_func_t) (smpl_t ** input, smpl_t ** output, int nframes);




static void send_noteon (smpl_t pitch, int velo);






#define MAX_PITCHES (20)

static smpl_t pitches[MAX_PITCHES];

static int usejack = 1;
static int usedoubled = 1;


/* energy,specdiff,hfc,complexdomain,phase */
static smpl_t threshold = 0.3;
static smpl_t silence = -90.;
static uint_t buffer_size = 1024;       //512; //1024;
static uint_t overlap_size = 512;       //256; //512;
static uint_t samplerate = 44100;

static fvec_t *ibuf;

static aubio_onset_t *o;
static fvec_t *onset;
static uint_t isonset;

/* pitch objects */
static aubio_pitch_t *p;
static fvec_t *pitch;
static uint_t median = 6;

static fvec_t *note_buffer = NULL;
static fvec_t *note_buffer2 = NULL;
static smpl_t curlevel = 0.;



static smpl_t curnote = 0.;     // should not be global
//smpl_t newnote = 0.;
static uint_t isready = 0;      // should not be global, is static within pitchrecog()

static void
init_aubio (void)
{

  ibuf = new_fvec (overlap_size);

  {
    p = new_aubio_pitch ("default", buffer_size * 4, overlap_size, samplerate);
    aubio_pitch_set_tolerance (p, 0.7);
    aubio_pitch_set_unit (p, "freq");

    if (median)
      {
        note_buffer = new_fvec (median);
        note_buffer2 = new_fvec (median);
      }
  }
  o = new_aubio_onset ("default", buffer_size, overlap_size, samplerate);
  onset = new_fvec (2);
  pitch = new_fvec (1);

}


static void
aubio_finish (void)
{
  {
    send_noteon (curnote, 0);
    if (median)
      {
        if (note_buffer) del_fvec (note_buffer);
        if (note_buffer2) del_fvec (note_buffer2);
      }
  }
  if (o) del_aubio_onset (o);
  if (p) del_aubio_pitch (p);
  if (onset) del_fvec (onset);
  if (pitch) del_fvec (pitch);
  aubio_cleanup ();
}

static volatile int count;


static void
send_noteon (smpl_t pitch, int velo)
{
  if (velo)
    if (++count < MAX_PITCHES)
      pitches[count] = pitch;
}


/** append new note candidate to the note_buffer and return filtered value. we
 * need to copy the input array as fvec_median destroy its input data.*/
static void
note_append (fvec_t * note_buffer, smpl_t anote)
{
  uint_t i = 0;
  for (i = 0; i < note_buffer->length - 1; i++)
    {
      note_buffer->data[i] = note_buffer->data[i + 1];
    }
  note_buffer->data[note_buffer->length - 1] = anote;
  return;
}

static uint_t
get_note (fvec_t * note_buffer, fvec_t * note_buffer2)
{
  fvec_copy(note_buffer, note_buffer2);
  return fvec_median (note_buffer2);
}


static int Stop;

  int
pitchrecog (float **input, float **output, int nframes)
{
  unsigned int pos = 0;         /*frames%dspblocksize */
  unsigned int j;               /*frames */
  if (Stop)
    return Stop;
  for (j = 0; j < (unsigned) nframes; j++)
  {
    if (usejack)
    {
      DENEMO_SAMPLE_TYPE *in = (DENEMO_SAMPLE_TYPE *) * input;
      ibuf->data[pos] = *(in + j);   /* need threshold higher - say 0.5 to avoid repeated note detection when using this higher precision data */
    }
    /*when pos reaches overlap size it is time for fft to look for a note */
    if (pos == overlap_size - 1)
    {
      /* block loop */
      aubio_onset_do (o, ibuf, onset);
      aubio_pitch_do (p, ibuf, pitch);

      isonset = onset->data[0];

      if (median)
      {
        note_append (note_buffer, pitch->data[0]);
      }

      /* curlevel is negatif or 1 if silence */
      curlevel = aubio_level_detection (ibuf, silence);

      if (isonset)
      {
        if (curlevel == 1)
        {
          isonset = 0;
          if (median)
            isready = 0;
          /* send note off */
          send_noteon (curnote, 0);
        }
        else
        {                   // not silent
          if (median)
          {
            isready = 1;
          }
          else
          {
            /* kill old note */
            send_noteon (curnote, 0);
            /* get and send new one */
            curnote = pitch->data[0];
            send_noteon (curnote, 1);
          }


        }
      }
      else
      {                       //not onset
        if (median)
        {
          if (isready > 0)
            isready++;
          if (isready == median)
          {
            /* kill old note */
            send_noteon (curnote, 0);

            curnote = get_note (note_buffer, note_buffer2);
            /* get and send new one */
            if (curnote > 45)
            {           //FIXME
              send_noteon (curnote, 1);
            }
          }
        } // if median

      }
      /* end of block loop */
      pos = -1;                 /* so it will be zero next j loop */

    }
    pos++;
  }

  return Stop;
}

extern int pa_main (aubio_process_func_t process_func);


#define START  init_aubio();return pa_main(pitchrecog);
#define STOP   (void)pa_main(NULL);aubio_finish();

int
set_silence (double shh)
{
  silence = shh;
  return 0;
}

int
set_threshold (double thresh)
{                               /* threshold requires memory allocation */
  STOP threshold = thresh;
START}

int
set_smoothing (double smooth)
{                               /* median requires memory allocation */
  STOP median = (unsigned) smooth;
START}

int
set_onset_type (unsigned onset)
{
  /* changing onset type requires memory allocation */
#if 0
  if (onset >= sizeof (onset_types) / sizeof (aubio_onsetdetection_type))
    return 0;
  STOP type_onset = onset_types[onset];
#endif
START}

int
initialize_pitch_recognition (void)
{
  Stop = 0;
  init_aubio ();
  return pa_main (pitchrecog);
}


int
terminate_pitch_recognition (void)
{
  g_print ("Terminating portaudio and aubio\n");
  (void) pa_main (NULL);
  aubio_finish ();
  return 0;
}

double
get_pitch (void)
{
  double ret = 0.0;
  if (count)
    {
      //printf("count is %d\n", count);
      ret = pitches[count];
      count = 0;
    }

  return ret;
}

void
store_pitch (double pitch)
{
  send_noteon (pitch, 1);
  // return count<MAX_PITCHES; no point in returning the status, look at it outside of interrupts.
}
#endif // _HAVE_PORTAUDIO_
#endif // DISABLE_AUBIO
