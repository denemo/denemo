/* jackmidi.h
 * function prototypes for interface to midi in
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c)2008 Jeremiah Benham, Richard Shann
 */
#ifndef JACKMIDI_H
#define JACKMIDI_H
#include<lash/lash.h>

int init_jack(void);
int jackmidi(void);
void  jackstop(void);
void jack_midi_playback_control(gboolean start);
int jack_kill_timer(void);
void start_init_lash(lash_client_t *client);

#endif //JACKMIDI_H
