/* midi.h
 * header file for Brian Delaney's direct output to /dev/sequencer
 * and input from /dev/midi
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>
#include "smf.h"

void generate_midi();
gdouble get_time();
gdouble get_midi_on_time(GList *events);
gdouble get_midi_off_time(GList *events);
DenemoObject *get_obj_for_start_time(smf_t *smf, gdouble time);
DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time);


void start_playing();
void stop_playing();
gboolean is_playing();
gboolean get_smf_event(unsigned char *event_buffer, size_t *event_length, double *event_time, double until_time);


//void midi_cleanup ();
//
//gint midi_init ();
//gint get_midi_channel();
//gint get_midi_prognum();
//gint get_midi_port();
//void playnotes (gboolean doit, chord *chord_to_play,int prognum);

//void play_midikey(gint key, double duration, double volume, gint channel);
//void process_midi_event(gchar *buf);
//void playpitch(double pitch, double duration, double volume, int channel);
//void start_midi_input(void);
