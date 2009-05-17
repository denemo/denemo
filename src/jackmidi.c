
#ifdef _HAVE_JACK_
#include <jack/jack.h>
#include <jack/midiport.h>
#include "midi.h"

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include "pitchentry.h"
#include "smf.h"
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define MIDI_CONTROLLER         0xB0
#define MIDI_ALL_SOUND_OFF      120
#define MAX_NUMBER_OF_TRACKS    128

#define INPUT_PORT_NAME         "midi_in"
#define PROGRAM_NAME            "denemo"
jack_client_t   *jack_client = NULL;
jack_port_t     *input_port;
jack_port_t	*output_ports[MAX_NUMBER_OF_TRACKS];

static gint timeout_id = 0, kill_id=0;
static gdouble duration;

smf_t           *smf = NULL;
double          rate_limit = 0;
int             just_one_output = 0;
int             start_stopped = 0;
int             use_transport = 0;
int             be_quiet = 1;

int    playback_started = -1, song_position = 0, stop_midi_output = 0;

double 
get_time(void)
{
	double		seconds;
	int		ret;
	struct timeval	tv;

	ret = gettimeofday(&tv, NULL);

	if (ret) {
		perror("gettimeofday");
	}

	seconds = tv.tv_sec + tv.tv_usec / 1000000.0;

	return seconds;
}

double
get_delta_time(void)
{
	static double	previously = -1.0;
	double		now;
	double		delta;

	now = get_time();

	if (previously == -1.0) {
		previously = now;

		return 0;
	}

	delta = now - previously;
	previously = now;

	assert(delta >= 0.0);

	return delta;
}

static gboolean
warning_async(gpointer s)
{
	const char *str = (const char *)s;

	g_warning(str);

	return FALSE;
}

static void
warn_from_jack_thread_context(const char *str)
{
	g_idle_add(warning_async, (gpointer)str);
}

static double
nframes_to_ms(jack_nframes_t nframes)
{
	jack_nframes_t sr;

	sr = jack_get_sample_rate(jack_client);

	assert(sr > 0);

	return (nframes * 1000.0) / (double)sr;
}

static double
nframes_to_seconds(jack_nframes_t nframes)
{
	return nframes_to_ms(nframes) / 1000.0;
}

static jack_nframes_t
ms_to_nframes(double ms)
{
	jack_nframes_t sr;

	sr = jack_get_sample_rate(jack_client);

	assert(sr > 0);

	return ((double)sr * ms) / 1000.0;
}

static jack_nframes_t
seconds_to_nframes(double seconds)
{
	return ms_to_nframes(seconds * 1000.0);
}

static void
send_all_sound_off(void *port_buffers[MAX_NUMBER_OF_TRACKS], jack_nframes_t nframes)
{
	int i, channel;
	unsigned char *buffer;
	g_debug("\nSending all sound off!!!\n");
	for (i = 0; i < smf->number_of_tracks; i++) {
		for (channel = 0; channel < 16; channel++) {
#ifdef JACK_MIDI_NEEDS_NFRAMES
			buffer = jack_midi_event_reserve(port_buffers[i], 0, 3, nframes);
#else
			buffer = jack_midi_event_reserve(port_buffers[i], 0, 3);
#endif
			if (buffer == NULL) {
				warn_from_jack_thread_context("jack_midi_event_reserve failed, cannot send All Sound Off.");
				break;
			}

			buffer[0] = MIDI_CONTROLLER | channel;
			buffer[1] = MIDI_ALL_SOUND_OFF;
			buffer[2] = 0;
		}

		if (just_one_output)
			break;
	}
}
static gint move_on(DenemoGUI *gui){
	  if(timeout_id==0)
		      return FALSE;
	    set_currentmeasurenum (gui, gui->si->currentmeasurenum+1);

	      return TRUE;
}

gint jack_kill_timer(void){
if(timeout_id>0)
  g_source_remove(timeout_id);
timeout_id= 0;
if(kill_id)
  g_source_remove(kill_id);
kill_id = 0;
}

static void
process_midi_output(jack_nframes_t nframes)
{
	int		i, t, bytes_remaining, track_number;
	unsigned char  *buffer, tmp_status;
	void		*port_buffers[MAX_NUMBER_OF_TRACKS];
	jack_nframes_t	last_frame_time;
	jack_transport_state_t transport_state;
	static jack_transport_state_t previous_transport_state = JackTransportStopped;
	for (i = 0; i < smf->number_of_tracks; i++) {
		port_buffers[i] = jack_port_get_buffer(output_ports[i], nframes);

		if (port_buffers[i] == NULL) {
			warn_from_jack_thread_context("jack_port_get_buffer failed, cannot send anything.");
			return;
		}

#ifdef JACK_MIDI_NEEDS_NFRAMES
		jack_midi_clear_buffer(port_buffers[i], nframes);
#else
		jack_midi_clear_buffer(port_buffers[i]);
#endif

		if (just_one_output)
			break;
	}
 
	if (use_transport) {
		transport_state = jack_transport_query(jack_client, NULL);
		if (transport_state == JackTransportStopped) {
			if (previous_transport_state == JackTransportRolling)
				send_all_sound_off(port_buffers, nframes);

			previous_transport_state = transport_state;

			return;
		}

		previous_transport_state = transport_state;
	}
	
	last_frame_time = jack_last_frame_time(jack_client);
	/* End of song already? */
	if (playback_started < 0)
		return;
	/* We may push at most one byte per 0.32ms to stay below 31.25 Kbaud limit. */
	bytes_remaining = nframes_to_ms(nframes) * rate_limit;

	for (;;) {
		smf_event_t *event = smf_peek_next_event(smf);

		if (event == NULL) {
			if (!be_quiet)
				g_debug("End of song.");
			playback_started = -1;

			if (!use_transport)
				stop_midi_output = 1;

			break;
		}

	        if (stop_midi_output > 0) {
			send_all_sound_off(port_buffers, nframes);
			jack_transport_stop(jack_client);	
			return;
		}
		
		/* Skip over metadata events. */
		if (smf_event_is_metadata(event)) {
			char *decoded = smf_event_decode(event);
			if (decoded && !be_quiet)
				g_debug("Metadata: %s", decoded);

			smf_get_next_event(smf);
			continue;
		}

		bytes_remaining -= event->midi_buffer_length;
		if (rate_limit > 0.0 && bytes_remaining <= 0) {
			warn_from_jack_thread_context("Rate limiting in effect.");
			break;
		}

		t = seconds_to_nframes(event->time_seconds) + playback_started - song_position + nframes - last_frame_time;

		/* If computed time is too much into the future, we'll need
		   to send it later. */
		if (t >= (int)nframes)
			break;

		/* If computed time is < 0, we missed a cycle because of xrun. */
		if (t < 0)
			t = 0;

		assert(event->track->track_number >= 0 && event->track->track_number <= MAX_NUMBER_OF_TRACKS);

		/* We will send this event; remove it from the queue. */
		smf_get_next_event(smf);

		/* First, send it via midi_out. */
		track_number = 0;

#ifdef JACK_MIDI_NEEDS_NFRAMES
		buffer = jack_midi_event_reserve(port_buffers[track_number], t, event->midi_buffer_length, nframes);
#else
		buffer = jack_midi_event_reserve(port_buffers[track_number], t, event->midi_buffer_length);
#endif

		if (buffer == NULL) {
			warn_from_jack_thread_context("jack_midi_event_reserve failed, NOTE LOST.");
			break;
		}

		memcpy(buffer, event->midi_buffer, event->midi_buffer_length);

		/* Ignore per-track outputs? */
		if (just_one_output)
			continue;

		/* Send it via proper output port. */
		track_number = event->track->track_number -1;

#ifdef JACK_MIDI_NEEDS_NFRAMES
		buffer = jack_midi_event_reserve(port_buffers[track_number], t, event->midi_buffer_length, nframes);
#else
		buffer = jack_midi_event_reserve(port_buffers[track_number], t, event->midi_buffer_length);
#endif

		if (buffer == NULL) {
			warn_from_jack_thread_context("jack_midi_event_reserve failed, NOTE LOST.");
			break;
		}

		/* Before sending, reset channel to 0. XXX: Not very pretty. */
		assert(event->midi_buffer_length >= 1);

		tmp_status = event->midi_buffer[0];

		if (event->midi_buffer[0] >= 0x80 && event->midi_buffer[0] <= 0xEF)
			event->midi_buffer[0] &= 0xF0;

		memcpy(buffer, event->midi_buffer, event->midi_buffer_length);

		event->midi_buffer[0] = tmp_status;
	}
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
#ifdef MEASURE_TIME
	if (get_delta_time() > MAX_TIME_BETWEEN_CALLBACKS) {
		warn_from_jack_thread_context("Had to wait too long for JACK callback; scheduling problem?");
	}
#endif

	/* Check for impossible condition that actually happened to me, caused by some problem between jackd and OSS4. */
	if (nframes <= 0) {
		warn_from_jack_thread_context("Process callback called with nframes = 0; bug in JACK?");
		return 0;
	}

	process_midi_input(nframes);
	if ((smf != NULL) && (output_ports != NULL)){
	  process_midi_output(nframes);
	}
#ifdef MEASURE_TIME
	if (get_delta_time() > MAX_PROCESSING_TIME) {
		warn_from_jack_thread_context("Processing took too long; scheduling problem?");
	}
#endif

	return 0;
}

static int
sync_callback(jack_transport_state_t state, jack_position_t *position, void *notused)
{
	assert(jack_client);

	/* XXX: We should probably adapt to external tempo changes. */

	if (state == JackTransportStarting) {
		song_position = position->frame;
		smf_seek_to_seconds(smf, nframes_to_seconds(position->frame));

		if (!be_quiet)
			g_debug("Seeking to %f seconds.", nframes_to_seconds(position->frame));

		playback_started = jack_frame_time(jack_client);

	} else if (state == JackTransportStopped) {
		playback_started = -1;
	}

	return TRUE;
}

/* returns the jack midi port number that
 *  has been assigned
 */
int 
create_jack_midi_port(char* port_name){
  if (jack_client != NULL){
	gint i;
	jack_nframes_t nframes = jack_get_buffer_size(jack_client);
	/* only assign it if the port has not been assigned already */	
	for (i=0;i <= MAX_NUMBER_OF_TRACKS;i++){

	  if (output_ports[i] == NULL){
	    output_ports[i] = jack_port_register(jack_client, 
					port_name, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);

		if (output_ports[i] == NULL) {
			g_critical("Could not register JACKMIDI output_port[%d] '%s'.",i, port_name);
		}
  
		/* clear buffer */
		if (output_ports[i]){	
		  jack_midi_clear_buffer(jack_port_get_buffer(output_ports[i], nframes));
		}
	
	 	if (output_ports[i] != NULL)	
		  g_debug("\nassigning jackmidi port output_port[%d]\n", i);
		return i;
	  }
	}
  }
  else 
    return -1;
}

void
create_jack_midi_ports_from_score(){
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  curstaff = Denemo.gui->si->thescore;
  
  while (curstaff)
  {
    curstaffstruct = (DenemoStaff *) curstaff->data;
    g_debug("\nStaff name = %s\n", curstaffstruct->denemo_name->str);
    /* Create port and assign jack port number */
    curstaffstruct->jack_midi_out_port = create_jack_midi_port(curstaffstruct->denemo_name->str);
    curstaff = curstaff->next;
  }
}

int 
remove_jack_midi_port(int port_number){
	int err;
	err = 0;
	if (output_ports[port_number] != NULL){
		err = jack_port_unregister(jack_client, output_ports[port_number]);
		output_ports[port_number] = NULL;
#ifdef DEBUG
		g_debug("\nremove jackmidi port number = %d\n", port_number);
#endif
	}
	return err;
}

void
remove_all_jack_midi_ports(){
	int err,i;
	err = 0;

	
	for (i=0;i < MAX_NUMBER_OF_TRACKS;i++){

	  if (output_ports[i] != NULL){

		err = jack_port_unregister(jack_client, output_ports[i]);
		output_ports[i] = NULL;
#ifdef DEBUG
		g_debug("\nremoving jackmidi port number = %d\n", i);
#endif
	  }
	}
	return err;
}

int
rename_jack_midi_port(int port_number, char *port_name){
	int err = 0;
	if (output_ports[port_number] != NULL)
	  err = jack_port_set_name (output_ports[port_number], port_name);
	
	g_debug("Trying to rename JACK port output_ports[%d] to %s\n",port_number, port_name);

	if (err)
	  g_critical("Could not rename JACK port output_ports[%d] to %s",port_number, port_name);
	
	return err;	
}

void 
stop_jack(void){
  remove_all_jack_midi_ports();
  int err = jack_port_unregister(jack_client, input_port);
  jack_deactivate(jack_client);
  jack_client_close(jack_client);
}

int
init_jack(void){
  int err = 0;
  
  jack_client = jack_client_open(PROGRAM_NAME, JackNullOption, NULL);
 
  if (jack_client == NULL) {
    g_critical("Could not connect to the JACK server; run jackd first?");
  }
  err = jack_set_process_callback(jack_client, process_callback, 0);
  if (err) {
    g_critical("Could not register JACK process callback.");
  }
  
  input_port = jack_port_register(jack_client, INPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE,
	                  JackPortIsInput, 0);
   
  if (input_port == NULL) {
    g_critical("Could not register JACK input port.");
  }

  if (jack_activate(jack_client)) {
    g_critical("Cannot activate JACK client.");
  }
  return err;
}

void
jack_start_restart (void){
  g_debug("\nJack Start/Restart button pressed\n");
  if (jack_client == NULL){
    g_debug("\nStarting Jack\n");
    init_jack();
    create_jack_midi_ports_from_score();
  }
  if (jack_client != NULL){
    g_debug("\nRestarting Jack\n");
    stop_jack();
    init_jack();
    create_jack_midi_ports_from_score();
  }
}

void
jack_midi_player (gchar *file_name) {
  
  smf = smf_load(file_name);
  if (smf == NULL) {
     g_critical("Loading SMF file failed.");
  }
  if (!be_quiet)
     g_message("%s.", smf_decode(smf));

  if (smf->number_of_tracks > MAX_NUMBER_OF_TRACKS) { 
     g_warning("Number of tracks (%d) exceeds maximum for per-track output; implying '-s' option.", smf->number_of_tracks);
     just_one_output = 1;
  }
  g_debug("\nNumber of tracks = %d\n", smf->number_of_tracks);
 
  if (use_transport) {
    gint err = jack_set_sync_callback(jack_client, sync_callback, 0);
    if (err) {
      g_critical("Could not register JACK sync callback.");
    }
  }
  assert(smf->number_of_tracks >= 1);

  if (use_transport && !start_stopped) {
    jack_transport_locate(jack_client, 0);
    jack_transport_start(jack_client);
  }
  
  if (!use_transport)
    playback_started = jack_frame_time(jack_client);
}

void
jack_midi_playback_start()
{
  DenemoGUI *gui = Denemo.gui;
  gchar *mididata = NULL;
  playback_started = -1, song_position = 0, stop_midi_output = 0;
  /* set tranport on/off */
  use_transport = Denemo.prefs.jacktransport; 
  /* set tranport start_stopped */
  start_stopped = Denemo.prefs.jacktransport_start_stopped; 
  g_debug("\nTransport set to %d Transport start stopped = %d\n", 
		  use_transport, start_stopped);
  if (jack_client != NULL){
	  mididata = get_temp_filename ("denemoplayback.mid");
	  if(gui->si->markstaffnum)
	   duration = exportmidi (mididata, gui->si, gui->si->firstmeasuremarked, gui->si->lastmeasuremarked);
	  else 
	   if(gui->si->end)
	     exportmidi (mididata, gui->si, gui->si->start, gui->si->end);
	   else
	     duration = exportmidi (mididata, gui->si, gui->si->currentmeasurenum, 0/* means to end */);
	  /* execute jackmidi player function */ 
	  jack_midi_player(mididata);
	  g_free (mididata);
	  // first measure to play at start
	  
	    if(gui->si->markstaffnum)
	      set_currentmeasurenum (gui,gui->si->firstmeasuremarked);
	    else    
	      set_currentmeasurenum (gui, gui->si->currentmeasurenum);
	    if(gui->si->end==0) {//0 means not set, we move the cursor on unless the specific range was specified
	      DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
	      //FIXME add a delay before starting the timer.
	      timeout_id = g_timeout_add ( 4*((double)staff->timesig.time1/(double)staff->timesig.time2)/(gui->si->tempo/(60.0*1000.0)), 
				       (GSourceFunc)move_on, gui);
	      kill_id = g_timeout_add (duration*1000, (GSourceFunc)jack_kill_timer, NULL);
	    }
  }  
  return;
}

void
jack_midi_playback_stop ()
{
   stop_midi_output = 1;
   jack_transport_stop(jack_client);
}


#endif // _HAVE_JACK_

