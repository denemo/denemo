/* jackmidi.h
 * function prototypes for interface to midi in
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c)2008 Jeremiah Benham, Richard Shann
 */
#ifndef JACKMIDI_H
#define JACKMIDI_H

int init_jack(void);
int jackmidi(void);
void  jackstop(void);
void jack_midi_playback(GtkAction *action, gpointer param);
void stop_jack_midi_playback(GtkAction *action, gpointer param);

//void jack_playnotes (gboolean doit, chord chord_to_play,int prognum);

#endif //JACKMIDI_H
