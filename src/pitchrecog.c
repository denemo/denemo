/*
 * pitchrecog.c
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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <getopt.h>
#include <unistd.h>
#include <math.h>
#include <aubio/aubio.h>
#include <audio.h>
#include <glib.h>
#include "pitchrecog.h"

#ifdef HAVE_C99_VARARGS_MACROS
#define debug(...)              if (verbose) fprintf (stderr, __VA_ARGS__)
#define errmsg(...)             fprintf (stderr, __VA_ARGS__)
#define outmsg(...)             fprintf (stdout, __VA_ARGS__)
#else
#define debug(format, args...)  if (verbose) fprintf(stderr, format , ##args)
#define errmsg(format, args...) fprintf(stderr, format , ##args)
#define outmsg(format, args...) fprintf(stdout, format , ##args)
#endif






typedef int (*aubio_process_func_t) (smpl_t ** input, smpl_t ** output, int nframes);



static void send_noteon (smpl_t pitch, int velo);






#define MAX_PITCHES (20)

static smpl_t pitches[MAX_PITCHES];

static int usejack = 1;
static int usedoubled = 1;


/* energy,specdiff,hfc,complexdomain,phase */
static aubio_onsetdetection_type type_onset = aubio_onset_kl;
static aubio_onsetdetection_type type_onset2 = aubio_onset_complex;
static smpl_t threshold = 0.3;
static smpl_t silence = -90.;
static uint_t buffer_size = 1024;       //512; //1024;
static uint_t overlap_size = 512;       //256; //512;
static uint_t channels = 1;
static uint_t samplerate = 44100;

static aubio_pvoc_t *pv;
static fvec_t *ibuf;
static fvec_t *obuf;
static cvec_t *fftgrain;

static aubio_onsetdetection_t *o;
static aubio_onsetdetection_t *o2;
static fvec_t *onset;
static fvec_t *onset2;
static int isonset = 0;
static aubio_pickpeak_t *parms;


/* pitch objects */
static smpl_t pitch = 0.;
static aubio_pitchdetection_t *pitchdet;
static aubio_pitchdetection_type type_pitch = aubio_pitch_yinfft;       // aubio_pitch_mcomb
static aubio_pitchdetection_mode mode_pitch = aubio_pitchm_freq;
static uint_t median = 6;

static fvec_t *note_buffer = NULL;
static fvec_t *note_buffer2 = NULL;
static smpl_t curlevel = 0.;



static smpl_t curnote = 0.;     // should not be global
//smpl_t newnote = 0.;
static uint_t isready = 0;      // should not be global, is static within pitchrecog()


static aubio_onsetdetection_type onset_types[] = { aubio_onset_energy,
  aubio_onset_specdiff,
  aubio_onset_hfc,
  aubio_onset_complex,
  aubio_onset_complex,
  aubio_onset_phase,
  aubio_onset_mkl,
  aubio_onset_kl
};





static void
init_aubio (void)
{

  ibuf = new_fvec (overlap_size, channels);
  obuf = new_fvec (overlap_size, channels);
  fftgrain = new_cvec (buffer_size, channels);

  {
    pitchdet = new_aubio_pitchdetection (buffer_size * 4, overlap_size, channels, samplerate, type_pitch, mode_pitch);
    aubio_pitchdetection_set_yinthresh (pitchdet, 0.7);

    if (median)
      {
        note_buffer = new_fvec (median, 1);
        note_buffer2 = new_fvec (median, 1);
      }
  }
  /* phase vocoder */
  pv = new_aubio_pvoc (buffer_size, overlap_size, channels);
  /* onsets */
  parms = new_aubio_peakpicker (threshold);
  o = new_aubio_onsetdetection (type_onset, buffer_size, channels);
  onset = new_fvec (1, channels);
  if (usedoubled)
    {
      o2 = new_aubio_onsetdetection (type_onset2, buffer_size, channels);
      onset2 = new_fvec (1, channels);
    }

}


static void
aubio_finish (void)
{
  {
    send_noteon (curnote, 0);
    del_aubio_pitchdetection (pitchdet);
    if (median)
      {
        del_fvec (note_buffer);
        del_fvec (note_buffer2);
      }
  }
  if (usedoubled)
    {
      del_aubio_onsetdetection (o2);
      del_fvec (onset2);
    }
  del_aubio_onsetdetection (o);
  del_aubio_peakpicker (parms);
  del_aubio_pvoc (pv);
  del_fvec (obuf);
  del_fvec (ibuf);
  del_cvec (fftgrain);
  del_fvec (onset);
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
 * need to copy the input array as vec_median destroy its input data.*/
static void
note_append (fvec_t * note_buffer, smpl_t anote)
{
  uint_t i = 0;
  for (i = 0; i < note_buffer->length - 1; i++)
    {
      note_buffer->data[0][i] = note_buffer->data[0][i + 1];
    }
  note_buffer->data[0][note_buffer->length - 1] = anote;
  return;
}

static uint_t
get_note (fvec_t * note_buffer, fvec_t * note_buffer2)
{
  uint_t i = 0;
  for (i = 0; i < note_buffer->length; i++)
    {
      note_buffer2->data[0][i] = note_buffer->data[0][i];
    }
  return vec_median (note_buffer2);
}


static int calls;


static int Stop;

int
pitchrecog (float **input, float **output, int nframes)
{
  unsigned int pos = 0;         /*frames%dspblocksize */
  unsigned int i;               /*channels */
  unsigned int j;               /*frames */
  calls++;
  if (Stop)
    return Stop;
  for (j = 0; j < (unsigned) nframes; j++)
    {
      if (usejack)
        {
          for (i = 0; i < channels; i++)
            {
              DENEMO_SAMPLE_TYPE *in = (DENEMO_SAMPLE_TYPE *) * input;
              ibuf->data[i][pos] = *(in + j);   /* need threshold higher - say 0.5 to avoid repeated note detection when using this higher precision data */
            }
        }
      /*when pos reaches overlap size it is time for fft to look for a note */
      if (pos == overlap_size - 1)
        {
          /* block loop */
          aubio_pvoc_do (pv, ibuf, fftgrain);
          aubio_onsetdetection (o, fftgrain, onset);
          if (usedoubled)
            {
              aubio_onsetdetection (o2, fftgrain, onset2);
              onset->data[0][0] *= onset2->data[0][0];
            }
          isonset = aubio_peakpick_pimrt (onset, parms);

          pitch = aubio_pitchdetection (pitchdet, ibuf);

          if (median)
            {
              note_append (note_buffer, pitch);
            }

          /* curlevel is negatif or 1 if silence */
          curlevel = aubio_level_detection (ibuf, silence);

          if (isonset)
            {
              /* test for silence */
#ifndef SUSPICIOUS_CODE
              if (curlevel == 1.)
                {
#else
              //if (curlevel <= 1.) {

#endif

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
                  send_noteon (pitch, 1);
                  curnote = pitch;
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
            }                   // if median

        }
      /* end of block loop */
      pos = -1;                 /* so it will be zero next j loop */
    }                           /* end of if pos==overlap_size-1 */
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

/* FIXME consider controlling usedoubled parameter as well*/
int
set_onset_type (unsigned onset)
{
  /* changing onset type requires memory allocation */
  if (onset >= sizeof (onset_types) / sizeof (aubio_onsetdetection_type))
    return 0;
  STOP type_onset = onset_types[onset];
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
  g_message ("Terminating portaudio and aubio");
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
#endif
