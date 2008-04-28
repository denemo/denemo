
#include "alsaplayback.h"
#ifdef HAVEALSA

snd_seq_t *seq_handle;
int out_ports[1];
int num_out = 1;


int
open_seq ()
{
  int l1;
  char portname[64];


  if (snd_seq_open (&seq_handle, "default", SND_SEQ_OPEN_DUPLEX, 0) < 0)
    {
      g_print ("Error opening Alsa Sequencer.\n");
      return -1;
    }

  snd_seq_set_client_name (seq_handle, "Denemo Playback");
  for (l1 = 0; l1 < num_out; l1++)
    {

      sprintf (portname, "Denemo OUT %d", l1);
      if ((out_ports[l1] = snd_seq_create_simple_port (seq_handle, portname,
						       SND_SEQ_PORT_CAP_READ |
						       SND_SEQ_PORT_CAP_SUBS_READ,
						       SND_SEQ_PORT_TYPE_MIDI_GENERIC
						       |
						       SND_SEQ_PORT_TYPE_APPLICATION))
	  < 0)
	{
	  g_print ("Error creating sequencer port.\n");
	  return -1;
	}
    }
  return 0;
}


int
close_seq ()
{
  snd_seq_close (seq_handle);
  return 0;
}


void
alsaplayback (GtkAction * action, DenemoGUI * gui)
{


  snd_seq_event_t ev;


  /*alsa midi parser */
  snd_midi_event_t *midi_ev;

  /* temp for midi data */
  unsigned char buffer[3];

  /* fill buffer and set midi channel */
  buffer[0] = 0x90;
  buffer[0] += (1 & 0x0F);

  buffer[1] = 60;
  buffer[2] = 128;

  snd_midi_event_new (10, &midi_ev);

  /* clear event */
  snd_seq_ev_clear (&ev);
  snd_midi_event_encode (midi_ev, buffer, 3, &ev);

  snd_midi_event_free (midi_ev);

  /* set source */
  snd_seq_ev_set_source (&ev, out_ports[0]);
  snd_seq_ev_set_subs (&ev);
  // its immediate 
  snd_seq_ev_set_direct (&ev);

  snd_seq_event_output (seq_handle, &ev);
  snd_seq_drain_output (seq_handle);
  sleep (2);



  /* fill buffer and set midi channel */
  buffer[0] = 0x80;
  buffer[0] += (1 & 0x0F);

  buffer[1] = 60;
  buffer[2] = 128;

  snd_midi_event_new (10, &midi_ev);

  /* clear event */
  snd_seq_ev_clear (&ev);
  snd_midi_event_encode (midi_ev, buffer, 3, &ev);

  snd_midi_event_free (midi_ev);

  /* set source */
  snd_seq_ev_set_source (&ev, out_ports[0]);
  snd_seq_ev_set_subs (&ev);
  // its immediate 
  snd_seq_ev_set_direct (&ev);

  snd_seq_event_output (seq_handle, &ev);
  snd_seq_drain_output (seq_handle);


}
#endif
