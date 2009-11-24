
#ifdef _HAVE_JACK_
#include <jack/jack.h>
#include <jack/midiport.h>
#include "midi.h"
#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include "exportmidi.h"
#include "jackmidi.h"
#include "pitchentry.h"
#include "smf.h"
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define MIDI_CONTROLLER         0xB0
#define MIDI_ALL_SOUND_OFF      0x78 
#define MIDI_ALL_NOTE_OFF	0x7b
#define MAX_NUMBER_OF_TRACKS    128
#define MAX_NUMBER_OF_CLIENTS	50
#define BUFFER_MAX_INDEX	100
#define INPUT_PORT_NAME         "midi_in"
#define PROGRAM_NAME            "denemo"

jack_port_t     *input_port;

//maybe use this instead of looking through ports
//static gint jack_client_index = 0; 

struct midi_output_device
{
  jack_client_t *jack_client;	
  GList *port_names;
  jack_port_t *output_ports[MAX_NUMBER_OF_TRACKS];
};

struct midi_output_device midi_device[MAX_NUMBER_OF_CLIENTS];

static double start_player = 0.0;
static volatile gint BufferIndex;
static gint BufferFillIndex;
static volatile gboolean BufferEmpty = TRUE;
static gboolean playing_piece = FALSE;

struct midi_buffer
{
  unsigned char buffer[3];
  gint channel;
  gint jack_port;
};

struct midi_buffer global_midi_buffer[BUFFER_MAX_INDEX]; 

static gint timeout_id = 0, kill_id=0;
static gdouble playback_duration = 0.0;

static double          	rate_limit = 0;
static gboolean        	start_stopped = FALSE;
static gboolean        	use_transport = FALSE;

static double 		start_time = 0.0;//time in seconds to start at (from start of the smf)
static double 		end_time = 0.0;//time in seconds to end at (from start of the smf)

gint
maxnumber_of_clients(){
  return MAX_NUMBER_OF_CLIENTS;
}

gint
maxnumber_of_ports(){
  return MAX_NUMBER_OF_TRACKS;
}

double 
get_time(void)
{
  double seconds;
  int ret;
  struct timeval tv;

  ret = gettimeofday(&tv, NULL);

  if (ret) 
    perror("gettimeofday");
	
  seconds = tv.tv_sec + tv.tv_usec / 1000000.0;
  return seconds;
}

static gboolean
warning_async(gpointer s)
{
  gchar *str = (gchar *)s;
  g_warning("%s", str);
  return FALSE;
}

static void
warn_from_jack_thread_context(const char *str)
{
#ifdef DEBUG
  g_idle_add(warning_async, (gpointer)str);
#endif
}

static double
nframes_to_ms(jack_nframes_t nframes)
{
  jack_nframes_t sr;
  sr = jack_get_sample_rate(midi_device[0].jack_client);
  assert(sr > 0);
  return (nframes * 1000.0) / (double)sr;
}

static jack_nframes_t
ms_to_nframes(double ms)
{
  jack_nframes_t sr;
  sr = jack_get_sample_rate(midi_device[0].jack_client);
  assert(sr > 0);
  return ((double)sr * ms) / 1000.0;
}

static jack_nframes_t
seconds_to_nframes(double seconds)
{
  return ms_to_nframes(seconds * 1000.0);
}

/* this will be become obsolete when we have a playback bar*/ 
static gint 
move_on(DenemoGUI *gui){
  if(timeout_id==0)
    return FALSE;
  set_currentmeasurenum (gui, gui->si->currentmeasurenum+1);
  return TRUE;
}

gint 
jack_kill_timer(void){
  g_debug("Jack kill timer %d\n", timeout_id);
  if(timeout_id>0)
    g_source_remove(timeout_id);
  timeout_id = 0;
  if(kill_id)
    g_source_remove(kill_id);
  kill_id = 0;
  return 1;
}

static gboolean
timer_callback(gpointer bufferindex){
  gint index = (gint) bufferindex;
  global_midi_buffer[BufferFillIndex].buffer[0] = NOTE_OFF | global_midi_buffer[index].channel;
  global_midi_buffer[BufferFillIndex].buffer[1] = global_midi_buffer[index].buffer[1];
  global_midi_buffer[BufferFillIndex].buffer[2] = global_midi_buffer[index].buffer[2];
  global_midi_buffer[BufferFillIndex].jack_port = global_midi_buffer[index].jack_port;
  BufferFillIndex = BufferFillIndex+1 > BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
  BufferEmpty=FALSE;
  return FALSE;
}

void 
jack_playpitch(gint key, gint duration){
 if (BufferEmpty==TRUE){ 
    DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
    global_midi_buffer[BufferFillIndex].buffer[0] = NOTE_ON | get_midi_channel();
    global_midi_buffer[BufferFillIndex].buffer[1] = key; 
    global_midi_buffer[BufferFillIndex].buffer[2] = curstaffstruct->volume;
    global_midi_buffer[BufferFillIndex].channel = get_midi_channel();
    global_midi_buffer[BufferFillIndex].jack_port = get_midi_port();
    g_timeout_add(duration, timer_callback, (gpointer) BufferFillIndex);
    BufferFillIndex = BufferFillIndex+1 > BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
    BufferEmpty=FALSE;
  }
}

void 
jack_output_midi_event(unsigned char *buffer){
  if (BufferEmpty==TRUE){ 
    global_midi_buffer[BufferFillIndex].buffer[0] = buffer[0];
    global_midi_buffer[BufferFillIndex].buffer[1] = buffer[1];
    global_midi_buffer[BufferFillIndex].buffer[2] = buffer[2];
    BufferFillIndex = BufferFillIndex+1 > BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
    if (BufferFillIndex == BufferIndex)
      g_debug("\nBuffer Overrun\n");
    BufferEmpty=FALSE;
  }
  else {
    global_midi_buffer[BufferFillIndex].buffer[0] = buffer[0];
    global_midi_buffer[BufferFillIndex].buffer[1] = buffer[1];
    global_midi_buffer[BufferFillIndex].buffer[2] = buffer[2];
    BufferFillIndex = BufferFillIndex+1 > BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
  }
}

static void
send_midi_event(jack_nframes_t nframes){
  unsigned char *buffer;
  gint i=global_midi_buffer[BufferIndex].jack_port;
  void *port_buffers[MAX_NUMBER_OF_TRACKS];

  if (!midi_device[0].output_ports[i])//TODO check
    return;
  
  port_buffers[i] = jack_port_get_buffer(midi_device[0].output_ports[i], nframes);
  jack_midi_clear_buffer(port_buffers[i]);
  if (BufferEmpty==FALSE)
    while (BufferIndex != BufferFillIndex){
      buffer = jack_midi_event_reserve(port_buffers[i], 0, 3);
      if (buffer == NULL){
        warn_from_jack_thread_context("jack_midi_event_reserve_failed, NOTE_LOST.");
        return;
      }
      buffer[0] = global_midi_buffer[BufferIndex].buffer[0];
      buffer[1] = global_midi_buffer[BufferIndex].buffer[1];
      buffer[2] = global_midi_buffer[BufferIndex].buffer[2];
      BufferIndex = BufferIndex+1 > BUFFER_MAX_INDEX ? 0 : BufferIndex+1; 
    }
  BufferEmpty=TRUE;
}

gboolean 
jackmidi_read_smf_events(){

  smf_event_t *event = Denemo.gui->si->smf?smf_peek_next_event(Denemo.gui->si->smf):NULL;

  if (!midi_device[0].jack_client)
    return FALSE;

  if (!playing_piece)
    return FALSE;

  if (event == NULL || event->time_seconds>end_time){
    playing_piece = FALSE;
    return FALSE;
  }
  else
    playing_piece = TRUE;

  /* Skip over metadata events. */
  if (smf_event_is_metadata(event)) {
    event = smf_get_next_event(Denemo.gui->si->smf);
    return TRUE;
  }
  //TODO set a way to set client_number to use device $x
  //maybe something than selects that track before note is played?
  //or a function returning an int for device used on that track
  if ((get_time() - start_player) >= event->time_seconds){
    event = smf_get_next_event(Denemo.gui->si->smf);
    jack_output_midi_event(event->midi_buffer); //Are these in the correct order?
  }
  return TRUE;
}

void
static process_midi_input(jack_nframes_t nframes)
{
  int read, events, i, channel;
  void           *port_buffer;
  jack_midi_event_t event;
  int             last_frame_time;
  static int      time_of_first_event = -1;

  last_frame_time = jack_last_frame_time(midi_device[0].jack_client);
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
  if (nframes <= 0) {
    warn_from_jack_thread_context("Process callback called with nframes = 0; bug in JACK?");
    return 0;
  }
  if(Denemo.gui->input_source==INPUTMIDI && input_port)
    process_midi_input(nframes);
  if (Denemo.gui->si && midi_device[0].output_ports)
    send_midi_event(nframes);
  return 0;
}

/* returns the jack midi port number that
 *  has been assigned
 */
int 
create_jack_midi_port(gint client_number){
  if (!midi_device[client_number].jack_client)
    return -1;
  
  char port_name[10];  
  gint i;
  jack_nframes_t nframes = jack_get_buffer_size(midi_device[client_number].jack_client);
  
  /* only assign it if the port has not been assigned already */	
  for (i=0;i <= MAX_NUMBER_OF_TRACKS;i++)
    if (!midi_device[client_number].output_ports[i]){ 
      sprintf(port_name, "denemo:%d", i);
      midi_device[client_number].output_ports[i] = jack_port_register(midi_device[client_number].jack_client, 
	port_name, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);

      if (midi_device[client_number].output_ports[i]){
	/* clear buffer */
	jack_midi_clear_buffer(jack_port_get_buffer(midi_device[client_number].output_ports[i], nframes));
	g_debug("\nassigning jackmidi port output_port[%d]\n", i);
	return i;
      } else {
	 g_critical("Could not register JACKMIDI denemo:[%d]",i);
         return -1;
        }
    } 
  return -1;
}

void
create_default_jack_midi_ports(){
 
  gint i;
  gint default_number_of_ports = 10;

  for (i=0;i<default_number_of_ports;i++)
    create_jack_midi_port(0);

  for (i=0;i<default_number_of_ports;i++)
    create_jack_midi_port(1);
}

int
create_jack_midi_client(){
  gint i;
  char str[10];
  for (i=0;i <= MAX_NUMBER_OF_CLIENTS;i++)
    if (!midi_device[i].jack_client){
      sprintf(str, "%s:%d",PROGRAM_NAME, i); 
      midi_device[i].jack_client = jack_client_open(str, JackNullOption, NULL);

      if (midi_device[i].jack_client){
	jack_activate(midi_device[i].jack_client);
	g_debug("\nassigning jackmidi client %s\n", str); 
	return i;
      } 
      else
        g_critical("Could not connect to the JACK server; run jackd first?");
    }
  return -1;
}

int 
remove_jack_midi_port(int client_number){
  int err,i;
  err = 0;
  
  for (i=MAX_NUMBER_OF_TRACKS;i>=0;i--)
    if (midi_device[client_number].output_ports[i]){
      err = jack_port_unregister(midi_device[client_number].jack_client, midi_device[client_number].output_ports[i]);
      midi_device[client_number].output_ports[i] = NULL;
      g_debug("\nremove jackmidi port number = %d\n", i);
      return err;
    }
  return err;
}

void
remove_all_jack_midi_ports(int client_number){
  int err,i;
  err = 0;
	
  for (i=MAX_NUMBER_OF_TRACKS;i>=0;i--)
    if (midi_device[client_number].output_ports[i]){
      err = jack_port_unregister(midi_device[client_number].jack_client, midi_device[client_number].output_ports[i]);
      midi_device[client_number].output_ports[i] = NULL;
      g_debug("\nremoving jackmidi port number = %d\n", i);
    }
}

int
remove_jack_midi_client(){
  gint i;
  char str[10];
  for (i=MAX_NUMBER_OF_CLIENTS;i>=0;i--)
    if (midi_device[i].output_ports[i]){
      remove_all_jack_midi_ports(i);
      jack_deactivate(midi_device[i].jack_client);
      jack_client_close(midi_device[i].jack_client);
      midi_device[i].jack_client = NULL;
      return i;
    }
  return -1;
}

void
remove_all_jack_midi_clients(){
  while (remove_jack_midi_client() >= 0)
    g_debug("\nRemoving Jack Midi client\n");
}

int
rename_jack_midi_port(int client_number, int port_number, char *port_name){
  int err = 0;

  if (midi_device[client_number].output_ports[port_number] != NULL) //TODO needs changed
    err = jack_port_set_name (midi_device[client_number].output_ports[port_number], port_name); //TODO
  g_debug("Trying to rename JACK port output_ports[%d] to %s\n",port_number, port_name);
  if (err)
    g_critical("Could not rename JACK port output_ports[%d] to %s",port_number, port_name);
  return err;	
}

void 
stop_jack(void){

  if (midi_device[0].jack_client)
    jack_port_unregister(midi_device[0].jack_client, input_port);
  remove_all_jack_midi_clients();
}

int
init_jack(void){
  int err = 0;
  
  create_jack_midi_client();
  create_jack_midi_client();

  err = jack_set_process_callback(midi_device[0].jack_client, process_callback, 0);
  if (err) 
    g_critical("Could not register JACK process callback.");
  
  input_port = jack_port_register(midi_device[0].jack_client, INPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE,
	                  JackPortIsInput, 0);
   
  if (input_port == NULL) 
    g_critical("Could not register JACK input port.");
  
  return err;
}

void
jack_start_restart (void){
  if (midi_device[0].jack_client){ 
    g_debug("\nRestarting Jack\n");
    stop_jack();
    init_jack();
    create_default_jack_midi_ports();
  }
  else { 
    g_debug("\nStarting Jack\n");
    init_jack();
    create_default_jack_midi_ports();
  }
}

void
jack_midi_playback_start(){
  DenemoGUI *gui = Denemo.gui;
 
  start_player = get_time();
  playing_piece = TRUE;

  if (!midi_device[0].jack_client) //TODO check if this is correct
    return;

  /* set tranport on/off */
  use_transport = (gboolean)Denemo.prefs.jacktransport; 
  /* set transport start_stopped */
  start_stopped = (gboolean) Denemo.prefs.jacktransport_start_stopped; 

  if((gui->si->smf==NULL) || (gui->si->smfsync!=gui->si->changecount))
    exportmidi (NULL, gui->si, 1, 0/* means to end */);
	  
  if (Denemo.gui->si->smf == NULL) {
    g_critical("Loading SMF failed.");
  } else {
    smf_rewind(Denemo.gui->si->smf);
    g_idle_add(jackmidi_read_smf_events, NULL);
  }
  
  playback_duration = smf_get_length_seconds(gui->si->smf);
  
  DenemoObject *curobj;
  start_time = 0.0;
  curobj = get_mark_object();
  if(curobj==NULL && gui->si->currentobject)
    curobj = gui->si->currentobject->data;
  if(curobj && curobj->midi_events) {
    smf_event_t *event = curobj->midi_events->data;
    start_time = event->time_seconds;
    g_debug("\nsetting start %f\n", start_time);
  }
  end_time = playback_duration;
  curobj = NULL;
  curobj =  get_point_object();
  if(curobj && curobj->midi_events)/*is this ever true?*/ { 
    smf_event_t *event = g_list_last(curobj->midi_events)->data;
    end_time = event->time_seconds;
    g_debug("\nsetting end %f\n", end_time);	   
    //could investigate to see if event is NoteOn and g_warning("Setting stop time to a NoteOn event!");
  } 

  if(start_time>end_time) {
    gdouble temp = start_time;
    start_time = end_time;
    end_time = temp;
  }
  playback_duration = end_time - start_time;
  g_debug("\nstart %f for %f seconds\n",start_time, playback_duration);

  if(gui->si->end==0) {//0 means not set, we move the cursor on unless the specific range was specified
    DenemoStaff *staff = (DenemoStaff *) gui->si->currentstaff->data;
    //FIXME add a delay before starting the timer.
    timeout_id = g_timeout_add ( 4*((double)staff->timesig.time1/(double)staff->timesig.time2)/(gui->si->tempo/(60.0*1000.0)), (GSourceFunc)move_on, gui);
    // g_print("Setting end time to %f %u\n", duration*1000, (guint)(duration*1000));
    kill_id = g_timeout_add ((guint)(playback_duration*1000), (GSourceFunc)jack_kill_timer, NULL);
  }
  return;
}

void
jack_midi_playback_stop ()
{
   //if(jack_client)
     //jack_transport_stop(jack_client);
  playing_piece = FALSE;    
}
#else //If not _HAVE_JACK_
void jack_playpitch(int key, int duration){}
void jack_output_midi_event(unsigned char *buffer){}
int jack_kill_timer(void){}
void jack_midi_playback_stop (){}
void jack_midi_playback_start (){}
void remove_jack_midi_port (){}
void create_jack_midi_port (){}
void remove_jack_midi_client (){}
void create_jack_midi_client (){}
int maxnumber_of_clients(){ return 0;}
#endif 

