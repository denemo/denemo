/*
 * audiocapture.c
 * capture audio data using portaudio library api
 * analyse data to determine frequency of a note close to a target note.
 * Hacked from patest_record.c
 * Record input into an array.
 * Save array to a file.
 * Playback recorded data.
 *
 * Author: Phil Burk  http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
#ifdef _HAVE_PORTAUDIO_
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <portaudio.h>
#include <glib.h>
#include "audio/audiocapture.h"
#include "audio/audio.h"
#ifndef paNonInterleaved
#undef PA_VERSION_19
#else
#define PA_VERSION_19
#endif

// #define SAMPLE_RATE  (17932) // Test failure to open with this value.
#define SAMPLE_RATE  DENEMO_SAMPLE_RATE
#define NUM_SECONDS     (10)


#define PA_SAMPLE_TYPE  paFloat32       /*paUInt8 */
typedef DENEMO_SAMPLE_TYPE SAMPLE;


static AubioCallback *aubio_routine;
typedef struct
{
  int frameIndex;               /* Index into sample array. */
  int maxFrameIndex;
  int samplesPerFrame;
  SAMPLE *recordedSamples;
} paTestData;

typedef struct
{
  int frameIndex;               /* Index into sample array. */
  int maxFrameIndex;
  double pitch;
  double volume;
  int channel;
  SAMPLE *recordedSamples;
} OutData;


static paTestData data;
static OutData out_data;
static int tuning = 0;          /* copy data for instrument tuning routines */





#define TABLE_SIZE (20000)
#ifndef PA_VERSION_19
static PortAudioStream *out_stream = NULL;
#define StreamActive Pa_StreamActive
#else
static PaStream *out_stream = NULL;
PaStreamParameters inputParameters, outputParameters;
#define StreamActive Pa_IsStreamActive
#endif

static int
init_audio_out (void)
{
  PaError err;
  int i;
  err = Pa_Initialize ();
  if (err != paNoError)
    {
      g_warning ("Error initializing portaudio library");
      return 0;
    }
  out_data.frameIndex = 0;
  out_data.recordedSamples = (SAMPLE *) malloc (TABLE_SIZE * sizeof (SAMPLE));
  if (out_data.recordedSamples == NULL)
    {
      printf ("Could not allocate record array.\n");
      return 0;
    }
  for (i = 0; i < TABLE_SIZE; i++)
    out_data.recordedSamples[i] = sin (2.0 * M_PI * i / (double) TABLE_SIZE);
  play_pitch (440.0, 0.0, 0.0, 0);      /* to start the stream */
  return 1;
}

/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int
playCallback (void *inputBuffer, void *outputBuffer, unsigned long framesPerBuffer,
#ifndef PA_VERSION_19
              PaTimestamp outTime,
#else
              PaStreamCallbackTimeInfo * outTime, PaStreamCallbackFlags status,
#endif
              void *userData)
{

  SAMPLE *wptr = (SAMPLE *) outputBuffer;
  unsigned int i;

  (void) inputBuffer;           /* Prevent unused variable warnings. */
  (void) outTime;
  for (i = 0; i < framesPerBuffer; i++)
    {
      if (out_data.frameIndex >= out_data.maxFrameIndex)
        wptr[i] = 0;
      else
        {
          wptr[i] = out_data.volume * out_data.recordedSamples[out_data.frameIndex % TABLE_SIZE];
          if (out_data.channel && wptr[i] < 0)
            wptr[i] = -wptr[i]; //distort sound for other channels

          out_data.frameIndex += TABLE_SIZE * out_data.pitch / SAMPLE_RATE;
        }
    }
  return 0;
  //      return out_data.frameIndex >= out_data.maxFrameIndex;
}

void
play_pitch (double pitch, double duration, double volume, int channel)
{
  //g_debug("playing");
  if (out_data.recordedSamples == NULL && !init_audio_out ())
    {
      fprintf (stderr, "Could not initialize audio out\n");
      return;
    }                           //else
  //g_debug("already initialized");
  if (out_stream && StreamActive (out_stream))
    {
      out_data.maxFrameIndex = duration * TABLE_SIZE * pitch /*SAMPLE_RATE */ ;
      out_data.pitch = pitch;
      out_data.volume = volume;
      out_data.channel = channel;
      out_data.frameIndex = 0;
      return;
    }
  //g_debug("starting stream ...");

#ifdef PA_VERSION_19
  outputParameters.device = Pa_GetDefaultOutputDevice ();       /* default output device */
  if (outputParameters.device == paNoDevice)
    {
      g_critical ("Error: No default output device.");
      return;
    }
  outputParameters.channelCount = 1;    /* mono output */
  outputParameters.sampleFormat = PA_SAMPLE_TYPE;
  outputParameters.suggestedLatency = Pa_GetDeviceInfo (outputParameters.device)->defaultLowInputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;
#endif


  out_data.maxFrameIndex = duration * SAMPLE_RATE;
  out_data.pitch = pitch;
  PaError err;
  out_data.frameIndex = 0;
  out_stream = NULL;
  err = Pa_OpenStream (&out_stream,
#ifndef PA_VERSION_19
                       paNoDevice, 0,   /* NO input */
                       PA_SAMPLE_TYPE, NULL, Pa_GetDefaultOutputDeviceID (), 1, /* mono output */
                       PA_SAMPLE_TYPE, NULL, SAMPLE_RATE, 1024, /* frames per buffer */
                       0,       /* number of buffers, if zero then use default minimum */
                       paClipOff,       /* we won't output out of range samples so don't bother clipping them */
                       playCallback, &out_data);
#else
                       NULL,    /* input parameters */
                       &outputParameters, SAMPLE_RATE, 1024,    /* frames per buffer */
                       paClipOff,       /* we won't output out of range samples so don't bother clipping them */
                       (PaStreamCallback *) playCallback, &out_data);
#endif
  if (err != paNoError)
    {
      g_critical ("Error opening stream");
      return;
    }
  if (out_stream)
    err = Pa_StartStream (out_stream);
  if (err != paNoError)
    {
      g_critical ("Error starting stream");
      out_stream = NULL;
      return;
    }
}

void
stop_audio (void)
{
  if (out_stream != NULL)
    Pa_CloseStream (out_stream);
  out_stream = NULL;
}

/* This routine will be called by the PortAudio engine when audio is needed.
** It may be called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/


static int
recordCallback (const void *inputBuffer, const void *outputBuffer, unsigned long framesPerBuffer,
#ifndef PA_VERSION_19
                PaTimestamp outTime,
#else
                PaStreamCallbackTimeInfo * outTime, PaStreamCallbackFlags status,
#endif
                void *userData)
{

  SAMPLE *rptr = (SAMPLE *) inputBuffer;

/* 	copy data to data.recordedSamples */
  if (tuning)
    {
      float *wptr = &data.recordedSamples[data.frameIndex * data.samplesPerFrame];
      long framesToCalc;
      long i;
      unsigned long framesLeft = data.maxFrameIndex - data.frameIndex;

      (void) outputBuffer;      /* Prevent unused variable warnings. */
      (void) outTime;

      if (framesLeft < framesPerBuffer)
        {
          framesToCalc = framesLeft;
        }
      else
        {
          framesToCalc = framesPerBuffer;
        }
      if (inputBuffer == NULL)
        {
          for (i = 0; i < framesToCalc; i++)
            {
              *wptr++ = 0;      /* left */
            }
        }
      else
        {
          for (i = 0; i < framesToCalc; i++)
            {
              *wptr++ = *rptr++;        /* left */
            }
        }
      data.frameIndex += framesToCalc;
    }
  rptr = (SAMPLE *) inputBuffer;
  return aubio_routine (&rptr, NULL, framesPerBuffer);
}


int
collect_data_for_tuning (int ok)
{
  tuning = ok;
  return 1;
}

/******************************************************************
with param FN non null, start audio capture with FN as callback
else shutdown audio capture.
return 0 for success
*/

int
pa_main (AubioCallback * fn)
{
#ifndef PA_VERSION_19
  static PortAudioStream *stream;
#else
  static PaStream *stream;
  PaStreamParameters inputParameters;
#endif
  PaError err = -1;
  static int last_tried;
  int totalFrames;
  int numSamples;
  int numBytes;

  if ((fn == NULL) && (stream))
    {
      err = Pa_StopStream (stream);
      if (err != paNoError)
        goto error;
      err = Pa_CloseStream (stream);
      if (err != paNoError)
        goto error;
      Pa_Terminate ();
      return 0;
    }
  if (fn == NULL)
    return -1;
  aubio_routine = fn;
  err = Pa_Initialize ();
  if (err != paNoError)
    goto error;
  data.maxFrameIndex = totalFrames = NUM_SECONDS * SAMPLE_RATE; /* Record for a few seconds. */
  data.frameIndex = 0;
  data.samplesPerFrame = 1;     /*mono */
  numSamples = totalFrames * data.samplesPerFrame;

  numBytes = numSamples * sizeof (float);
  data.recordedSamples = (float *) malloc (numBytes);   //FIXME multiple inits, memory leak...

#ifdef PA_VERSION_19
  inputParameters.device = Pa_GetDefaultInputDevice (); /* default input device */
  if (inputParameters.device == paNoDevice)
    {

      //g_debug("Number of devices %d now trying = %d\n", Pa_GetDeviceCount(), last_tried);
      inputParameters.device = last_tried++;    // guess
    }
  inputParameters.channelCount = 1;     /* mono input */
  inputParameters.sampleFormat = PA_SAMPLE_TYPE;
  const PaDeviceInfo *info = Pa_GetDeviceInfo (inputParameters.device);
  if (info)
    inputParameters.suggestedLatency = info->defaultLowInputLatency;
  else
    goto error;
  inputParameters.hostApiSpecificStreamInfo = NULL;
#endif



/* Record some audio. -------------------------------------------- */
  err = Pa_OpenStream (&stream,
#ifndef PA_VERSION_19
                       Pa_GetDefaultInputDeviceID (), 1,        /* mono input */
                       PA_SAMPLE_TYPE, NULL, paNoDevice, 0, PA_SAMPLE_TYPE, NULL, SAMPLE_RATE, 1024,    /* frames per buffer */
                       0,       /* number of buffers, if zero then use default minimum */
                       paClipOff,       /* we won't output out of range samples so don't bother clipping them */
                       recordCallback, NULL);
#else
                       &inputParameters, NULL,  /* output parameters */
                       SAMPLE_RATE, 1024,       /* frames per buffer */
                       paClipOff,       /* we won't output out of range samples so don't bother clipping them */
                       (PaStreamCallback *) recordCallback, NULL);
#endif
  if (err != paNoError)
    goto error;

  err = Pa_StartStream (stream);
  if (err != paNoError)
    goto error;
  last_tried--;
  printf ("Now recording!!\n");
  fflush (stdout);
  return 0;


error:
  Pa_Terminate ();
  fprintf (stderr, "An error occurred while using the portaudio stream\n");
  fprintf (stderr, "Error number: %d\n", err);
  fprintf (stderr, "Error message: %s\n", Pa_GetErrorText (err));
  return -1;
}





/********************** code from accordeur ****************************/

static int Autocorrelation (float mData[],      // In
                            int mDataLen,       // In
                            // int mWindowSize,    In
                            float *processed[]  // Out
                            //, int* mProcessedSize Out
  );

static float bestPeak2 (float *mProcessed,      // IN
                        int mProcessedSize,     // IN
                        float mRate     // IN
  );

static float Freq2Pitch (float freq);



#define OCTAVE 4
static unsigned long WindowSize = 16384 >> OCTAVE;


int Pitch = 69 /*       69 */ ;
/* 69 = 5 octaves *12 + 9th step = A, so A 440 */
#define MIN_DB  (-96)
#define BACKGROUND_DB  (-54)    // This seemed to work okay
#define INF_DB (MIN_DB - 1)

static float
level2db (SAMPLE level)
{
  const SAMPLE maxLevel = 1;
  //  wxASSERT((0 < level) && (level <= maxLevel));
  return 20 * log10 (level / maxLevel);
}

float
PitchToFreq (float pitch)
{
  return 440.0 * pow (2, (pitch - 69.0) / 12.0);
}





/* void setTargetPitch(int pitch) { */
void
setTuningTarget (double note)
{
  int pitch = (int) (Freq2Pitch (note) + 0.5);
  Pitch = pitch;
  WindowSize = 16384 >> ((Pitch - 12) / 12);
  if (WindowSize < 1024)
    WindowSize = 1024;
}

static double PARTDERNIER = 0.25;
void
set_frequency_smoothing (double m)
{
  PARTDERNIER = m;
  fprintf (stderr, "smoothing %f\n", PARTDERNIER);
}

double
determine_frequency (void)
{
  static float *autocorr = 0;
  static float *autocorr2;
  /* initialize if needed */
  if (autocorr == NULL)
    {
      autocorr = calloc (sizeof (float), 16384);
      autocorr2 = calloc (sizeof (float), 16384);
    }

  // Analyze the audio
  SAMPLE avg_abs, *average_abs;

  int numSamples = data.frameIndex * data.samplesPerFrame;
  /* Measure average absolute amplitude. */
  SAMPLE val;
  average_abs = &avg_abs;
  *average_abs = 0;
  int i;
  for (i = 0; i < numSamples; i++)
    {
      val = data.recordedSamples[i];
      if (val < 0)
        val = -val;             /* ABS */
      *average_abs += val;
    }
  *average_abs /= numSamples;
  int gotSound = (numSamples > 0) && (*average_abs > 0) && Autocorrelation (data.recordedSamples, numSamples, &autocorr);
  /* Reset the frame index to 0, so we keep going with only new sound */
  data.frameIndex = 0;
  /* smooth the autocorrelation */
  /*#define PARTDERNIER 0.25 */
  for (i = 0; i < WindowSize; i++)
    {
      autocorr2[i] = (autocorr[i]) * PARTDERNIER + autocorr2[i] * (1 - PARTDERNIER);
    }
  for (; i < 16384; i++)
    {
      autocorr2[i] = autocorr2[i] * (1 - PARTDERNIER);
    }

  float db = level2db (avg_abs);
  gotSound = gotSound && (db > BACKGROUND_DB);

  if (gotSound)
    {
#define INTERVAL (1)            /*(2) */
      double m_sample_rate = 44100.0;
      int lower = lround (m_sample_rate / PitchToFreq (Pitch - INTERVAL));      /* lowest allowed value of Pitch is 12 */
      int upper = lround (m_sample_rate / PitchToFreq (Pitch + INTERVAL));
      double bestpeak_x = upper + bestPeak2 (&autocorr2[upper], lower - upper, m_sample_rate);
      double psd;
      for (psd = 0.0, i = upper; i < lower; i++)
        psd += autocorr[i];
      psd *= (Pitch * Pitch * Pitch * Pitch / (400000000.0));
      if (psd < 1.0)
        return -1.0;
      double bestpeak_freq2 = m_sample_rate / bestpeak_x;
      int pitch = -1;

      if (db > BACKGROUND_DB + 6)
        {
          pitch = (int) (Freq2Pitch (bestpeak_freq2) + 0.5);    // note found this time
        }                       // loud enough
      else
        return -4.0;
      if (pitch > 0)
        {
          return bestpeak_freq2;
        }
      return -3.0;
    }                           // if gotPitch
  return -2.0;
}




/**********************************************************************

  from FFT.cpp  Dominic Mazzoni  September 2000
**********************************************************************/
#define	M_PI		3.14159265358979323846  /* pi */
#define false 0
#define true 1
#define bool int
static int **gFFTBitTable = NULL;
static const int MaxFastBits = 16;

static int
IsPowerOfTwo (int x)
{
  if (x < 2)
    return false;

  if (x & (x - 1))              /* Thanks to 'byang' for this cute trick! */
    return false;

  return true;
}

static int
NumberOfBitsNeeded (int PowerOfTwo)
{
  int i;

  if (PowerOfTwo < 2)
    {
      fprintf (stderr, "Error: FFT called with size %d\n", PowerOfTwo);
      exit (1);
    }

  for (i = 0;; i++)
    if (PowerOfTwo & (1 << i))
      return i;
}

static int
ReverseBits (int index, int NumBits)
{
  int i, rev;

  for (i = rev = 0; i < NumBits; i++)
    {
      rev = (rev << 1) | (index & 1);
      index >>= 1;
    }

  return rev;
}

static void
InitFFT ()
{
  gFFTBitTable = malloc (sizeof (int *) * MaxFastBits);

  int len = 2;
  int b;
  for (b = 1; b <= MaxFastBits; b++)
    {

      gFFTBitTable[b - 1] = malloc (sizeof (int) * len);
      int i;
      for (i = 0; i < len; i++)
        gFFTBitTable[b - 1][i] = ReverseBits (i, b);

      len <<= 1;
    }
}

static inline int
FastReverseBits (int i, int NumBits)
{
  if (NumBits <= MaxFastBits)
    return gFFTBitTable[NumBits - 1][i];
  else
    return ReverseBits (i, NumBits);
}

/*
 * Complex Fast Fourier Transform
 */

static void
FFT (int NumSamples, bool InverseTransform, float *RealIn, float *ImagIn, float *RealOut, float *ImagOut)
{
  int NumBits;                  /* Number of bits needed to store indices */
  int i, j, k, n;
  int BlockSize, BlockEnd;

  double angle_numerator = 2.0 * M_PI;
  float tr, ti;                 /* temp real, temp imaginary */

  if (!IsPowerOfTwo (NumSamples))
    {
      fprintf (stderr, "%d is not a power of two\n", NumSamples);
      exit (1);
    }

  if (!gFFTBitTable)
    InitFFT ();

  if (InverseTransform)
    angle_numerator = -angle_numerator;

  NumBits = NumberOfBitsNeeded (NumSamples);

  /*
   **   Do simultaneous data copy and bit-reversal ordering into outputs...
   */

  for (i = 0; i < NumSamples; i++)
    {
      j = FastReverseBits (i, NumBits);
      RealOut[j] = RealIn[i];
      ImagOut[j] = (ImagIn == NULL) ? 0.0 : ImagIn[i];
    }

  /*
   **   Do the FFT itself...
   */

  BlockEnd = 1;
  for (BlockSize = 2; BlockSize <= NumSamples; BlockSize <<= 1)
    {

      double delta_angle = angle_numerator / (double) BlockSize;

      float sm2 = sin (-2 * delta_angle);
      float sm1 = sin (-delta_angle);
      float cm2 = cos (-2 * delta_angle);
      float cm1 = cos (-delta_angle);
      float w = 2 * cm1;
      float ar0, ar1, ar2, ai0, ai1, ai2;

      for (i = 0; i < NumSamples; i += BlockSize)
        {
          ar2 = cm2;
          ar1 = cm1;

          ai2 = sm2;
          ai1 = sm1;

          for (j = i, n = 0; n < BlockEnd; j++, n++)
            {
              ar0 = w * ar1 - ar2;
              ar2 = ar1;
              ar1 = ar0;

              ai0 = w * ai1 - ai2;
              ai2 = ai1;
              ai1 = ai0;

              k = j + BlockEnd;
              tr = ar0 * RealOut[k] - ai0 * ImagOut[k];
              ti = ar0 * ImagOut[k] + ai0 * RealOut[k];

              RealOut[k] = RealOut[j] - tr;
              ImagOut[k] = ImagOut[j] - ti;

              RealOut[j] += tr;
              ImagOut[j] += ti;
            }
        }

      BlockEnd = BlockSize;
    }

  /*
   **   Need to normalize if inverse transform...
   */

  if (InverseTransform)
    {
      float denom = (float) NumSamples;

      for (i = 0; i < NumSamples; i++)
        {
          RealOut[i] /= denom;
          ImagOut[i] /= denom;
        }
    }
}

/*
 * Windowing Functions
 */

static void
WindowFunc (int whichFunction, int NumSamples, float *in)
{
  int i;

  if (whichFunction == 1)
    {
      // Bartlett (triangular) window
      for (i = 0; i < NumSamples / 2; i++)
        {
          in[i] *= (i / (float) (NumSamples / 2));
          in[i + (NumSamples / 2)] *= (1.0 - (i / (float) (NumSamples / 2)));
        }
    }

  if (whichFunction == 2)
    {
      // Hamming
      for (i = 0; i < NumSamples; i++)
        in[i] *= 0.54 - 0.46 * cos (2 * M_PI * i / (NumSamples - 1));
    }

  if (whichFunction == 3)
    {
      // Hanning
      for (i = 0; i < NumSamples; i++)
        in[i] *= 0.50 - 0.50 * cos (2 * M_PI * i / (NumSamples - 1));
    }
}


// This is Audacity's FreqWindow::Recalc(), but shaved down to
//   1) be enhanced auto-correlation only
//   2) take parameters, and return values.
static bool
Autocorrelation (float mData[], // In
                 int mDataLen,  // In
                 float *processed[]     // Out
  )
{
  if (mDataLen < WindowSize)
    {
      // Not enough data to get even one window
      return false;
    }

//   float *mProcessed = NULL;
  float *mProcessed = *processed;

//   mProcessed = new float[WindowSize];

  int i;
  for (i = 0; i < WindowSize; i++)
    mProcessed[i] = 0.0;
  int half = WindowSize / 2;

  float *in = malloc (sizeof (float) * WindowSize);
  float *out = malloc (sizeof (float) * WindowSize);
  float *out2 = malloc (sizeof (float) * WindowSize);

  int start = 0;
  int windows = 0;
  while (start + WindowSize <= mDataLen)
    {
      // Copy stuff into in
      for (i = 0; i < WindowSize; i++)
        in[i] = mData[start + i];

      // Window the data to lose crazy artifacts
      // due to finite-length window
      WindowFunc (2 /* Hamming */ , WindowSize, in);

      // Enhanced AC

      // Take FFT
      FFT (WindowSize, false, in, NULL, out, out2);

      // Compute power
      for (i = 0; i < WindowSize; i++)
        in[i] = (out[i] * out[i]) + (out2[i] * out2[i]);

      // Tolonen and Karjalainen recommend taking the cube root
      // of the power, instead of the square root

      for (i = 0; i < WindowSize; i++)
        in[i] = pow (in[i], 1.0 / 3.0);

      // Take FFT
      FFT (WindowSize, false, in, NULL, out, out2);

      // Take real part of result
      for (i = 0; i < half; i++)
        mProcessed[i] += out[i];

      start += half;
      windows++;
    }

  // Enhanced Autocorrelation
  for (i = 0; i < half; i++)
    mProcessed[i] = mProcessed[i] / windows;

  // Peak Pruning as described by Tolonen and Karjalainen, 2000

  // Clip at zero, copy to temp array
  for (i = 0; i < half; i++)
    {
      if (mProcessed[i] < 0.0)
        mProcessed[i] = 0.0;
      out[i] = mProcessed[i];
    }

  // Subtract a time-doubled signal (linearly interp.) from the original
  // (clipped) signal
  for (i = 0; i < half; i++)
    if ((i % 2) == 0)
      mProcessed[i] -= out[i / 2];
    else
      mProcessed[i] -= ((out[i / 2] + out[i / 2 + 1]) / 2);

  // Clip at zero again
  for (i = 0; i < half; i++)
    if (mProcessed[i] < 0.0)
      mProcessed[i] = 0.0;

  /*  *mProcessedSize = half; */

  free (in);
  free (out);
  free (out2);

//   *processed = mProcessed;

  return true;
}

static float
Freq2Pitch (float freq)
{
  return (69.0 + 12.0 * (log (freq / 440.0) / log (2.0)));
}

float
CubicMaximize (float y0, float y1, float y2, float y3, float *maxyVal)
{
  // Find coefficients of cubic
  float a, b, c, d;

  a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
  b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
  c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
  d = y0;

  // Take derivative
  float da, db, dc;

  da = 3 * a;
  db = 2 * b;
  dc = c;

  // Find zeroes of derivative using quadratic equation
  float discriminant = db * db - 4 * da * dc;
  if (discriminant < 0.0)
    return -1.0;                // error

  float x1 = (-db + sqrt (discriminant)) / (2 * da);
  float x2 = (-db - sqrt (discriminant)) / (2 * da);

  // The one which corresponds to a local _maximum_ in the
  // cubic is the one we want - the one with a negative
  // second derivative
  float dda = 2 * da;
  float ddb = db;

#define CUBIC(x,a,b,c,d)  (a*x*x*x + b*x*x + c*x + d)

  if (dda * x1 + ddb < 0)
    {
      *maxyVal = CUBIC (x1, a, b, c, d);
      return x1;
    }
  else
    {
      *maxyVal = CUBIC (x2, a, b, c, d);
      return x2;
    }

#undef CUBIC
}




static float
Parabole (float *y, int nb, float *maxyVal)
{
  int i;
  float mx4 = 0, mx3 = 0, mx2 = 0, mx = 0;
  float mx2y = 0, mxy = 0, my = 0;
  float a, b, c;
  float highX;
  for (i = 0; i < nb; i++)
    {
      mx += i;
      mx2 += i * i;
      mx3 += i * i * i;
      mx4 += i * i * i * i;
      mxy += i * y[i];
      mx2y += i * i * y[i];
      my += y[i];
    }
  mx /= nb;
  mx2 /= nb;
  mx3 /= nb;
  mx4 /= nb;
  my /= nb;
  mxy /= nb;
  mx2y /= nb;
  a = ((mx2y - mx2 * my) * (mx2 - mx * mx) - (mxy - mx * my) * (mx3 - mx2 * mx)) / ((mx4 - mx2 * mx2) * (mx2 - mx * mx) - (mx3 - mx * mx2) * (mx3 - mx * mx2));
  b = (mxy - mx * my - a * (mx3 - mx * mx2)) / (mx2 - mx * mx);
  c = my - a * mx2 - b * mx;
  highX = (-b / (2 * a));
  *maxyVal = a * highX * highX + b * highX + c;
  return (-b / (2 * a));
}

static float
bestPeak2 (float *mProcessed,   // IN
           int mProcessedSize,  // IN
           float mRate          // IN
  )
{
  float highestpeak_y = 0;
  int iMaxX = 0;
  int bin;

  bool up = (mProcessed[1] > mProcessed[0]);
  for (bin = 2; bin < mProcessedSize; bin++)
    {
      bool nowUp = mProcessed[bin] > mProcessed[bin - 1];
      if (!nowUp && up)
        {
          if (mProcessed[bin - 1] > highestpeak_y)
            {
              highestpeak_y = mProcessed[bin - 1];
              iMaxX = bin - 1;
            }
        }
      up = nowUp;
    }
  // cherche le pic par recherche de la parabole la plus proche.
  int leftbin = iMaxX - 1;
  while (leftbin > 1 && leftbin > (iMaxX - 20) && mProcessed[leftbin - 1] > mProcessed[iMaxX] / 2)
    leftbin--;
  int nb = (iMaxX - leftbin) * 2 + 1;
  float thispeak_y;
  float max = leftbin + Parabole (&mProcessed[leftbin], nb, &thispeak_y);
  return max;
}
#endif
