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
void jack_midi_playback_control(gboolean start);
int jack_kill_timer(void);
int create_jack_midi_port(char *port_name);
int rename_jack_midi_port(int port_number, char *port_name);
int remove_jack_midi_port(int port_number);
void remove_all_jack_midi_ports(void); 
#ifdef WITH_LASH
#include<lash/lash.h>
void start_init_lash(lash_client_t *client);
#endif



#endif //JACKMIDI_H
