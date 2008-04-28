/*
 * midiseq: a simple midi player/recorder
 * for Denemo, a gtk+ frontend to GNU Lilypond.
 * (c) 2006 Benoit Rouits <brouits@free.fr>
 */

#ifndef MIDISEQ_H
#define MIDISEQ_H

/*
 * midi seqencer types
 */

#ifdef HAVEALSA
#include "alsaseq.h"
typedef alsa_seq midi_seq;
typedef alsa_ev midi_ev;
typedef alsa_val midi_val;
typedef alsa_dur midi_dur;
typedef alsa_type midi_type;
#else
typedef void midi_seq;
typedef void midi_ev;
typedef unsigned char midi_val;
typedef unsigned int  midi_dur;
typedef unsigned char midi_type;
#endif

/*
 * sequencer initialization and termination
 */

extern midi_seq      *
midi_seq_new(const char *name);
/*
 * creates a midi sequencer
 */

extern int
midi_seq_delete(midi_seq *player);
/*
 * closes and deletes the midi sequencer
 */

extern int
midi_seq_add_port(midi_seq *player, int capa);
/*
 * adds a new port with given capacities
 * (returns port id)
 */
    
extern int
midi_seq_plug_to(midi_seq *player, int dh, int dp);
/*
 * plugs to another midi host:port
 */

extern int
midi_seq_unplug_to(midi_seq * player, int dh, int dp);
/*
 * disconnect the player to a midi host:port
 */

/*
 * instant playback
 */

extern int
midi_seq_instant_set_pgm (midi_seq *player, midi_val channel,
midi_val program);

extern int
midi_seq_instant_play_note (midi_seq *player, midi_val channel, midi_val key, midi_val velocity, midi_dur duration);

extern int
midi_seq_instant_play_event (midi_seq *player, midi_ev event);

/*
 * sequence creation
 */

extern int
midi_seq_tempo(midi_seq *player, int t);
/*
 * sets the tempo of the sequence
 */

extern int
midi_seq_put_note
(midi_seq *p, midi_val channel, midi_val key, midi_val press, unsigned int dur, midi_val rel);
/*
 * puts a complete note event sequence to the midi channel
 */

extern int
midi_seq_put_note_on(midi_seq *p, midi_val channel, midi_val key, midi_val press);
/*
 * puts a note ON event to the midi sequence
 */

extern int
midi_seq_put_tick(midi_seq *player, midi_val channel, unsigned int tick);
/*
 * put a timestamp in the channel
 */

extern int
midi_seq_put_note_off(midi_seq *p, midi_val channel, midi_val key, midi_val rel);
/*
 * puts a note OFF event to the midi sequence
 */

extern int
midi_seq_put_control(midi_seq *p, midi_val channel, midi_val c, midi_val v);
/*
 * puts a midi control to the midi sequence
 * e.g:
 * - fullpress sustain pedal is MIDI_SUSTAIN, 127
 * - release sustain pedal is MIDI_SUSTAIN, 0
 */

#if 0
extern int
midi_seq_put_raw(midi_seq *p, char* data, int len);
/*
 * puts a raw midi event made by hand (experts only)
 */
#endif

/*
 * playback controls
 */

extern int
midi_seq_play(midi_seq *player);
/*
 * start to play the midi sequence
 */

extern int
midi_seq_stop(midi_seq *player);
/*
 * stops the midi sequence
 */

extern void
midi_seq_flush(midi_seq *player);
/*
 * flushes buffered events of the sequence
 */

extern void
midi_seq_rewind(midi_seq *player);
/*
 * rewinds the midi event sequence
 */

/*
 * record control
 */

extern midi_ev *
midi_seq_rec_ev(midi_seq *player);
/*
 * return the next event in the sequence from readable plugs
 */

/*
 * midi event parsing
 */

extern midi_type
midi_seq_ev_get_type(midi_ev *event);
/*
 * returns the midi type of the given event
 */

/* note */

extern midi_val
midi_note_get_key(midi_ev *note);
/*
 * if the event is a note, returns the key
 */

extern midi_val
midi_note_get_pressvel(midi_ev *note);
/*
 * if the event is a note, returns the pressure velocity
 */

extern midi_val
midi_note_get_relvel(midi_ev *note);
/*
 * if the event is a note, returns the release velocity
 */

extern midi_val
midi_note_get_channel(midi_ev *note);
/*
 * if the event is a note, returns the channel (0-15)
 */

/* control */

extern midi_val
midi_control_get_val(midi_ev *control);
/*
 * if the event is a control (e.g: pedal), returns the value (0-127)
 */

#endif /* MIDISEQ_H */
