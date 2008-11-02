#ifdef _HAVE_JACK_

#include <jack/jack.h>
#include <jack/midiport.h>
#include <glib.h>
#include "pitchentry.c"
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define SYS_EXCLUSIVE_MESSAGE1  0xF0

#define INPUT_PORT_NAME         "midi_in"
#define PROGRAM_NAME            "denemo"




jack_client_t   *jack_client = NULL;
jack_port_t     *input_port;

double
midi2hz(int midinum)
{
  return 440 * pow(2, ((midinum - 69)/12));
}

void
process_midi_input(jack_nframes_t nframes)
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
    if (event.buffer[0] = (SYS_EXCLUSIVE_MESSAGE1 & NOTE_ON))
      store_pitch(midi2hz(event.buffer[1]));
    if (event.buffer[0] = (SYS_EXCLUSIVE_MESSAGE1 & NOTE_OFF))
      /* cannot do I/O during interrupts printf("\nmidi note off = %d\n", event.buffer[1])*/;
  }
}
 
static int
process_callback(jack_nframes_t nframes, void *notused)
{
  process_midi_input(nframes);
  return 0;
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

  if (jack_activate(jack_client)) {
    g_critical("Cannot activate JACK client.");
    //exit(EX_UNAVAILABLE);
  }
  return err;
}
#endif

void 
jackstop(){
#ifdef _HAVE_JACK_
  jack_deactivate(jack_client);
#endif
}

int jackmidi(){
#ifdef _HAVE_JACK_
return  init_jack();
#else
 return -1;
#endif
}


