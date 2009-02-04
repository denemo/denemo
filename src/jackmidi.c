 



#ifdef _HAVE_JACK_
#include <jack/jack.h>
#include <jack/midiport.h>
#include "midi.h"


#include <glib.h>
#include <math.h>
#include "pitchentry.h"
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define SYS_EXCLUSIVE_MESSAGE1  0xF0

#define INPUT_PORT_NAME         "midi_in"
#define OUTPUT_PORT_NAME	"midi_out"
#define PROGRAM_NAME            "denemo"



jack_client_t   *jack_client = NULL;
jack_port_t     *input_port;
jack_port_t	*output_port;
//unsigned char* note_frqs; 
unsigned char note_frqs;
//GList *note_frqs;
gint note_start;
gint note_length;
jack_nframes_t num_notes;
jack_nframes_t loop_nsamp;
jack_nframes_t loop_index;

int process_midi_output(jack_nframes_t nframes)
{
  int i,j;
  void* port_buf = jack_port_get_buffer(output_port, nframes);
  unsigned char* buffer;
  jack_midi_clear_buffer(port_buf);
  //g_print("\nProcessing midi output to jack!!!\n");
  
  for(i=0; i<nframes; i++)
  {
    for(j=0; j<num_notes; j++)
    {
      if(note_start == loop_index)
      {
	buffer = jack_midi_event_reserve(port_buf, i, 3);
	buffer[2] = 64; /* velocity */
	//buffer[1] = note_frqs[j];
        buffer[1] = note_frqs;	
	buffer[0] = 0x90; /*note on*/
	g_print("\njust sent midi note on to jack midi key = %d\n",note_frqs);
      }
      else if(note_start + note_length == loop_index)
      {
	buffer = jack_midi_event_reserve(port_buf, i, 3);
	buffer[2] = 64;
	//buffer[1] = note_frqs[j];
	buffer[1] = note_frqs;
	buffer[0] = 0x80; /* note off */
	g_print("\njust sent midi note off to jack midi key = %d\n",note_frqs);
      }
    }
    loop_index = loop_index+1 >= loop_nsamp ? 0 : loop_index+1;
    //g_print("\nloop index = %d\n", (int) loop_index);
  }
  return 0;
}

void
static process_midi_input(jack_nframes_t nframes)
{
  int read, events, i, channel;
  void           *port_buffer;
  jack_midi_event_t event;
  int             last_frame_time;
  static int      time_of_first_event = -1;

  last_frame_time = jack_last_frame_time(jack_client);
  port_buffer = jack_port_get_buffer(input_port, nframes);
  events = jack_midi_get_event_count(port_buffer);
  for (i = 0; i < events; i++) {
    read = jack_midi_event_get(&event, port_buffer, i);
    process_midi_event(event.buffer);
  }
}
 
static int
process_callback(jack_nframes_t nframes, void *notused)
{
  process_midi_input(nframes);
  process_midi_output(nframes);
  return 0;
}

void 
stop_jack(void){
  jack_deactivate(jack_client);
}

int
init_jack(void){
  int err = 0;
  jack_client = jack_client_open(PROGRAM_NAME, JackNullOption, NULL);
  if (jack_client == NULL) {
    g_critical("Could not connect to the JACK server; run jackd first?");
    //exit(EX_UNAVAILABLE);
  }

  err = jack_set_process_callback(jack_client, process_callback, 0);
  if (err) {
    g_critical("Could not register JACK process callback.");
    //exit(EX_UNAVAILABLE);
  }

  input_port = jack_port_register(jack_client, INPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE,
	                  JackPortIsInput, 0);

  if (input_port == NULL) {
    g_critical("Could not register JACK input port.");
    //exit(EX_UNAVAILABLE);
  }

  output_port = jack_port_register(jack_client, OUTPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);
  
  if (output_port == NULL) {
    g_critical("Could not register JACK output port '%s'.", OUTPUT_PORT_NAME);
    //  exit(EX_UNAVAILABLE);
  }


  if (jack_activate(jack_client)) {
    g_critical("Cannot activate JACK client.");
    //exit(EX_UNAVAILABLE);
  }
  return err;
}

/**
 *  Used to play each tone in the given chord
 *  (a g_list_foreach function)
 */
static void
jack_playtone (gpointer tone, gpointer chord, int prognum)
{
  gint offset, i;
  gchar key;
  gint voice;
  /* Because mid_c_offset is a measure of notes and we need a measure of
   * half-steps, this array will help */
  const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

  offset = ((note *) tone)->mid_c_offset;

  /* 60 is middle-C in MIDI keys */
  key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
  key += ((note *) tone)->enshift;
  voice = g_list_index ((GList *) chord, tone);
  
  //g_print("\nI see a noteon for processing key = %d\n", key);
  //note_frqs = g_list_append (note_frqs, key);
  note_frqs = key;
  num_notes = 1;
  loop_nsamp = 8010;
  note_start = 0;
  note_length = 8000;
}

/**
 *  Used to stop each tone in the given chord
 *  (a g_list_foreach function)
 */
static void
jack_stoptone (gpointer tone, gpointer chord)
{
  gint offset;
  gint voice;
  gchar key;
  /* Because mid_c_offset is a measure of notes and we need a measure of
   * half-steps, this array will help */
  const gint key_offset[] = { -10, -8, -7, -5, -3, -1, 0, 2, 4, 5, 7, 9, 11 };

  offset = ((note *) tone)->mid_c_offset;

  /* 60 is middle-C in MIDI keys */
  key = 60 + 12 * (offset / 7) + key_offset[offset % 7 + 6];
  key += ((note *) tone)->enshift;
  voice = g_list_index ((GList *) chord, tone);

  g_print("\nsending jack noteoff %d\n", key); 
}

void
jack_playnotes (gboolean doit, chord chord_to_play, int prognum)
{
  if (doit && chord_to_play.notes) {
    jack_playtone( chord_to_play.notes->data, chord_to_play.notes, 0);
    return;
  }
  if (doit){
	GList *tone;
	tone = chord_to_play.notes;
	while (tone)
	  {
	    jack_playtone (tone->data, chord_to_play.notes, prognum);
	    tone = tone->next;
	  }
	/* delta time or length of chord */

	g_list_foreach (chord_to_play.notes,
			(GFunc) jack_stoptone, chord_to_play.notes);
  }
}


#endif // _HAVE_JACK_

