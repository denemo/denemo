/*
 * alsaseq: a simple midi sequencer based on ALSA
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
 * - /usr/include/alsa/seqmid.h
 * - /usr/share/doc/libasound2-doc/html/index.html
 *
 */
#ifdef HAVEALSA
#include <alsa/asoundlib.h>

/* the default midi port references (not ALSA) */
#define OUTPORT 0
#define INPORT  1

typedef struct _plug
{
  int id;			/* the easy id of the plug */
  int dh;			/* the destination host    */
  int dp;			/* the destination port    */
} plug;

typedef struct _alsa_seq
{
  snd_seq_t *handle;    /* the actual ALSA sequencer   */
  int queue;            /* the event queue descriptor  */
  int *ports;           /* my local i/o Midi ports     */
  plug *plugs;          /* Midi host:port destinations */
} alsa_seq;

typedef snd_seq_event_t alsa_ev;

typedef snd_seq_event_type_t alsa_type;

typedef unsigned char alsa_val; /* storage for any [0-255] MIDI value */
typedef unsigned int alsa_dur;  /* storage for time in miliseconds    */

/*
 * sequencer initialization and termination
 */

alsa_seq *
midi_seq_new (const char *name)
{
  alsa_seq *seq;

  seq = (alsa_seq *) malloc (sizeof (alsa_seq));
  if (!seq)
    goto failure;
  /* return NULL; */

  /* create an empty plug list */
  seq->plugs = (plug *) malloc (sizeof (plug));
  if (!seq->plugs)
    {
      goto freeseq;
      /* return NULL; */
    }
  seq->plugs[0].id = 0;
  seq->plugs[0].dh = -1;
  seq->plugs[0].dp = -1;

  if (snd_seq_open (&(seq->handle), "default", SND_SEQ_OPEN_DUPLEX, 0) < 0)
    {
      fprintf (stderr, "Error opening ALSA sequencer.\n");
      goto freeplugs;
      /* return NULL; */
    }

  snd_seq_set_client_name (seq->handle, name);

  /* 2 ports => 16 channels for INPORT, 16 channels for OUTPORT */
  /* NOTE: INPORT number is expected to be greater than OUTPORT */
  seq->ports = (int *) malloc ((INPORT + 2) * sizeof (int));
  if (!seq->ports)
    {
      goto freehandle;
      /* return NULL; */
    }
  seq->ports[OUTPORT] = 0;
  seq->ports[INPORT] = 0;
  /* end of list: -1 */
  seq->ports[INPORT + 1] = -1;

  if ((seq->ports[OUTPORT] =
       snd_seq_create_simple_port (seq->handle, "default Playback",
				   SND_SEQ_PORT_CAP_READ |
				   SND_SEQ_PORT_CAP_SUBS_READ,
                   SND_SEQ_PORT_TYPE_MIDI_GENERIC |
				   SND_SEQ_PORT_TYPE_APPLICATION)) < 0)
    {
      fprintf (stderr, "Error creating sequencer default output port (%d).\n",
              OUTPORT);
      goto freeportlist;
      /* return NULL; */
    }

  if ((seq->ports[INPORT] =
       snd_seq_create_simple_port (seq->handle, "default Record",
                  SND_SEQ_PORT_CAP_WRITE |
                  SND_SEQ_PORT_CAP_SUBS_WRITE,
                  SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                  SND_SEQ_PORT_TYPE_APPLICATION)) < 0)
    {
      fprintf (stderr, "Error creating sequencer default input port (%d).\n",
              INPORT);
      goto freeoutport;
      /* return NULL; */
    }
  seq->queue = snd_seq_alloc_queue (seq->handle);
  if (seq->queue == -1)
    {
      fprintf (stderr, "Error creating sequencer queue.\n");
      goto freeinport;
      /* return NULL; */
    }
  /* success: */
  return (seq);
  /* failures: */
freeinport:
  snd_seq_delete_simple_port (seq->handle, seq->ports[INPORT]);
freeoutport:
  snd_seq_delete_simple_port (seq->handle, seq->ports[OUTPORT]);
freeportlist:
  free (seq->ports);
freehandle:
  snd_seq_close (seq->handle);
freeplugs:
  free (seq->plugs);
freeseq:
  free (seq);
failure:
  return NULL;
}

int
midi_seq_delete (alsa_seq * player)
{
  int err, i;

  for (i = 0; player->plugs[i].id; i++)
    {
      snd_seq_disconnect_to (player->handle, player->ports[OUTPORT],
			     player->plugs[i].dh, player->plugs[i].dp);
    }
  snd_seq_delete_simple_port (player->handle, player->ports[OUTPORT]);
  snd_seq_delete_simple_port (player->handle, player->ports[INPORT]);
  snd_seq_free_queue (player->handle, player->queue);
  err = snd_seq_close (player->handle);
  free (player->plugs);
  free (player);
  return err;
}

int
midi_seq_add_port (alsa_seq * player, int capa)
{
  int i, *list = NULL;

  for (i = 0; player->ports[i] >= 0; i++)
    {;
    }
  i++;				/* take EndOfList element in account */
  list = (int *) realloc (player->ports, (i + 1) * sizeof (int));
  if (!list)
    {
      fprintf (stderr, "Error expanding sequencer port list (to %d).\n", i);
      return -1;
    }
  else
    {
      player->ports = list;
    }

  if ((player->ports[i] = snd_seq_create_simple_port (player->handle, "User",
                  capa,
                  SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                  SND_SEQ_PORT_TYPE_APPLICATION)) < 0)
    {
        fprintf (stderr, "Error creating sequencer port number %d.\n", i);
        /* player->ports[i] = -1; already done by ALSA */
    }
  return player->ports[i];	/* the port identifier */
}

/*
int
midi_seq_rem_port(alsa_seq *player, int port) {
}
*/

int
midi_seq_plug_to (alsa_seq * player, int dh, int dp)
{
  int err, i;

  /* connect with ALSA */
  err = snd_seq_connect_to (player->handle, player->ports[OUTPORT], dh, dp);
  if (err == -1)		/* FIXME: be sure (err==-1) means an alsa error */
    return -1;

  /* append plug id to the list */
  for (i = 0; player->plugs[i].id; i++)
    {;
    }
  player->plugs = (plug *) realloc (player->plugs, (i + 2) * sizeof (plug));
  player->plugs[i].id = i;
  player->plugs[i].dh = dh;
  player->plugs[i].dp = dp;
  player->plugs[i + 1].id = 0;
  player->plugs[i + 1].dh = -1;
  player->plugs[i + 1].dp = -1;
  return i;			/* this is the plug id */
}

/* de-insert a plug id in the list:
 * this local function is used by midi_seq_unplug_to()
 */
static void
plugs_rem (plug * plugs, int id)
{
  int i;

/* FIXME: as it is internal, we could pass 'i' directly instead
 * of 'id', then avoiding the first for() search loop.
 */

  for (i = 0; plugs[i].id != id; i++)
    {;
    }
  for (; plugs[i].id; i++)
    {
      plugs[i].id = plugs[i + 1].id;
      plugs[i].dh = plugs[i + 1].dh;
      plugs[i].dp = plugs[i + 1].dp;
    }
/* trailing empty plugs will be freed on next realoc() */
}

int
midi_seq_unplug_to (alsa_seq * player, int id)
{
  int err, i;

  for (i = 0; player->plugs[i].id != id && player->plugs[i].id; i++)
    {;
    }
  if (!player->plugs[i].id)
    return -1;

  err =
    snd_seq_disconnect_to (player->handle, player->ports[OUTPORT],
			   player->plugs[i].dh, player->plugs[i].dp);
  if (err == -1)		/* FIXME: be sure (err == -1) means an alsa error */
    return -1;
  plugs_rem (player->plugs, id);
  return 0;
}

/* 
 * instant play (no queue)
 */

int midi_seq_instant_set_pgm (alsa_seq * player, alsa_val channel,
        alsa_val program)
{
    snd_seq_event_t ev;
    
    snd_seq_ev_clear(&ev);
    snd_seq_ev_set_pgmchange (&ev, channel, program);
    return midi_seq_instant_play_event(player, ev);
}

/* FIXME: use a tiny ticked queue and let ALSA drain it */
int
midi_seq_instant_play_note (alsa_seq * player, alsa_val channel,
		    alsa_val key, alsa_val velocity, alsa_dur duration)
{
    snd_seq_event_t ev;
    
    snd_seq_ev_clear(&ev);
#if 0
    snd_seq_ev_set_note(&ev, channel, key, velocity, duration);
/*  wtf is set_note? maybe for queued events? */
#endif
    snd_seq_ev_set_noteon(&ev, channel, key, velocity);
    midi_seq_instant_play_event (player, ev);
    sleep(duration/1000);
    snd_seq_ev_clear(&ev);
    snd_seq_ev_set_noteoff(&ev, channel, key, velocity);
    return midi_seq_instant_play_event (player, ev);
}

int
midi_seq_instant_play_event (alsa_seq * player, alsa_ev ev)
{
    int err = 0;
    
    snd_seq_ev_set_source(&ev, player->ports[OUTPORT]);
    snd_seq_ev_set_subs(&ev); /* send through available plugs */
/*    snd_seq_ev_set_broadcast(&ev); */
    snd_seq_ev_set_direct(&ev); /* no queue */
    err = snd_seq_event_output_direct(player->handle, &ev); /* drained */
#ifdef DEBUG
    fprintf (stderr,"instant_play_event [type %u]\n", ev.type);
    if (err < 0) fprintf (stderr, "\terror sending event\n");
#endif
    return err;
}

/*
 * midi sequence creation
 */

int
midi_seq_put_note (alsa_seq * p, ...)
{
/*
 * puts a note to the midi sequence
 */
  return 0;
}

int
midi_seq_put_control (alsa_seq * p, char c, char v)
{
/*
 * puts a midi control to the midi sequence
 * e.g:
 * - fullpress  pedal is MIDI_PEDAL, 127
 * - fullrelease pedal is MIDI_PEDAL, 0
 */
  return 0;
}

int
midi_seq_put_raw (alsa_seq * p, char *data, int len)
{
/*
 * puts a raw midi event made by hand (experts only)
 */
  return 0;
}

/*
 * queue playback controls
 */

int
midi_seq_play (alsa_seq * player)
{
/*
 * start to play the midi sequence
 */
  return 0;
}

int
midi_seq_stop (alsa_seq * player)
{
/*
 * stops the midi sequence
 */
  return 0;
}

void
midi_seq_flush (alsa_seq * player)
{
/*
 * flushes buffered events of the sequence
 */
  return;
}

/*
 * recording controls
 */

alsa_ev *
midi_seq_rec_ev (alsa_seq * player)
{
/*
 * return the next event in the sequence from readable plugs
 */
  return NULL;
}

/*
 * midi event parsing
 */

alsa_type
midi_seq_ev_get_type (alsa_ev * event)
{
/*
 * returns the midi type of the given event
 */
  return 0;
}

/* note */

alsa_val
midi_note_get_key (alsa_ev * note)
{
/*
 * if the event is a note, returns the key
 */
  return 0;
}

alsa_val
midi_note_get_pressvel (alsa_ev * note)
{
/*
 * if the event is a note, returns the pressure velocity
 */
  return 0;
}

alsa_val
midi_note_get_relvel (alsa_ev * note)
{
/*
 * if the event is a note, returns the release velocity
 */
  return 0;
}

alsa_val
midi_note_get_channel (alsa_ev * event)
{
/*
 * if the event is a note, returns the channel (0-15)
 */
  return 0;
}

/* control */

alsa_val
midi_control_get_val (alsa_ev * control)
{
/*
 * if the event is a control (e.g: pedal), returns the value (0-127)
 */
  return 0;
}
#endif /* HAVEALSA */
