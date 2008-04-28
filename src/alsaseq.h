/*
 * alsaseq a simple midi sequencer based on ALSA
 * for Denemo, a gtk+ frontend to GNU Lilypond.
 * (c) 2006 Benoit Rouits <brouits@free.fr>
 * 
 * Useful code to read to learn the ALSA api:
 * - miniArp.c by Matthias Nagorni
 *   (http://www.suse.de/~mana/alsa090_howto.html)
 *   
 * - midirecord.cc by Tuomas Airaksinen
 *   (http://people.jyu.fi/~tuma/home/progs.php)
 *
 * - midibus.cpp and event.cpp of seq24 by Rob C. Buse
 *   (http://www.filter24.org/seq24/)
 *   
 * - /usr/include/alsa/seqmid.h by the ALSA Team
 *
 */
#ifndef ALSASEQ_H
#define ALSASEQ_H

#include <alsa/asoundlib.h>

typedef struct _plug {
        int id; /* the easy id of the plug */
        int dh; /* the destination host    */
        int dp; /* the destination port    */
} plug;

typedef struct _alsa_seq {
    snd_seq_t      *seq;        /* the actual ALSA sequencer       */
    int             queue;      /* the event queue descriptor      */
    int*            ports;      /* my local i/o Midi ports         */
    plug           *plugs;      /* Midi host:port destination list */
} alsa_seq;

typedef snd_seq_event_t alsa_ev;

typedef snd_seq_event_type_t alsa_type;

typedef unsigned char alsa_val;
typedef unsigned int  alsa_dur;

/* function definition is useless since people use midiseq.h abstraction */

#endif /* ALSASEQ_H */

