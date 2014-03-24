/* audio.h
 * common header file for pitchentry, portaudio and aubio to share definitions
 *

 * (c) 2007 Richard Shann
 */
#ifndef AUDIO_H
#define AUDIO_H


#define DENEMO_SAMPLE_RATE (44100)
#define DENEMO_SAMPLE_TYPE float        /*unsigned char */


typedef int (AubioCallback) (void *inputBuffer, void *outputBuffer, unsigned long framesPerBuffer);

/* get approximate pitch in Hz of new note from audio input or 0 if none */
double get_pitch (void);
/* set the pitch as target for accurate pitch detection */
void setTuningTarget (double pitch);
/* measure the frequency of a peak in the spectrum close to the target frequency */
double determine_frequency (void);
void set_frequency_smoothing (double fraction);
int initialize_pitch_recognition (void);
int terminate_pitch_recognition (void);
void play_pitch (double pitch, double duration, double volume, int channel);
#endif //AUDIO_H
