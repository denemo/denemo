
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
#include "device_manager.h"
#include "moveviewport.h"
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define SYS_EXCLUSIVE_MESSAGE1  0xF0
#define MIDI_CONTROLLER         0xB0
#define MIDI_ALL_SOUND_OFF      0x78 
#define MIDI_ALL_NOTE_OFF	0x7b

#define DENEMO_MAX_PORTS (128)

#define MAX_NUMBER_OF_TRACKS   DENEMO_MAX_PORTS 

//   #define BUFFER_MAX_INDEX	100  rename DENEMO_BUFFER_MAX_INDEX
#define INPUT_PORT_NAME         "midi_in"
#define OUTPUT_PORT_NAME         "midi_out"
#define CLIENT_NAME            "denemo"
#define MD Denemo.prefs.midi_device

jack_port_t     *input_port;

//maybe use this instead of looking through ports
//static gint jack_client_index = 0; 

//struct midi_output_device
//{
//  jack_client_t *jack_client;	
//  jack_port_t *output_ports[MAX_NUMBER_OF_TRACKS];
//  void *port_buffers[MAX_NUMBER_OF_TRACKS];//this is not needed, the value stored in it is looked up always
//};

//struct midi_output_device midi_device[DENEMO_MAX_DEVICES];

static double start_player = 0.0;
//static volatile gint BufferIndex;
//static gint BufferFillIndex;
//static volatile gboolean BufferEmpty = TRUE;
static gboolean playing_piece = FALSE;
static gboolean jack_transport = TRUE;
static gboolean jack_transport_master = TRUE;

float time_beats_per_bar = 4.0;
float time_beat_type = 0.25;
double time_ticks_per_beat = 1920.0;
double time_beats_per_minute = 120.0;
volatile int time_reset = 1;            /* true when time value change */

//struct midi_buffer
//{
//  unsigned char buffer[3];
  //gint device_number;
  //gint port_number;
//  gint channel;
//};

//struct midi_buffer global_midi_buffer[BUFFER_MAX_INDEX]; 

static gint timeout_id = 0, kill_id=0;
static gdouble playback_duration = 0.0;

static double          	rate_limit = 0;
static gboolean        	start_stopped = FALSE;
static gboolean		jack_server_running = TRUE;
static double 		start_time = 0.0;//time in seconds to start at (from start of the smf)
static double 		end_time = 0.0;//time in seconds to end at (from start of the smf)

gint
maxnumber_of_clients(){
  return DENEMO_MAX_DEVICES;
}

gint
maxnumber_of_ports(){
  return MAX_NUMBER_OF_TRACKS;
}

gchar *
jackmidi_default_client_name(){
  return CLIENT_NAME;
}

gchar *
jackmidi_default_port_name(){
  return OUTPUT_PORT_NAME;
}

gboolean
jackmidi_server_running(){
  return jack_server_running;
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

/* Change the tempo for the entire timeline, not just from the current
   location */
void com_tempo(gint tempo)
{
  time_beats_per_minute = tempo;
  time_reset = 1;
}

void com_locate(jack_nframes_t frame)
{
  jack_transport_locate(MD[0].jack_client, frame);
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
#if 0
  gint index = (gint) bufferindex;
  global_midi_buffer[BufferFillIndex].buffer[0] = NOTE_OFF | global_midi_buffer[index].channel;
  global_midi_buffer[BufferFillIndex].buffer[1] = global_midi_buffer[index].buffer[1];
  global_midi_buffer[BufferFillIndex].buffer[2] = global_midi_buffer[index].buffer[2];
  global_midi_buffer[BufferFillIndex].device_number = global_midi_buffer[index].device_number;
  global_midi_buffer[BufferFillIndex].port_number = global_midi_buffer[index].port_number;
  BufferFillIndex = BufferFillIndex+1 > DENEMO_BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
  BufferEmpty=FALSE;
#endif
  return FALSE;
}

void 
jack_playpitch(gint key, gint duration){
#if 0
  if (!jack_server_running)
    return;
  if (BufferEmpty==TRUE){ 
    DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
    global_midi_buffer[BufferFillIndex].buffer[0] = NOTE_ON | get_midi_channel();
    global_midi_buffer[BufferFillIndex].buffer[1] = key; 
    global_midi_buffer[BufferFillIndex].buffer[2] = curstaffstruct->volume;
    global_midi_buffer[BufferFillIndex].channel = get_midi_channel();
    //global_midi_buffer[BufferFillIndex].device_number = //TODO
    global_midi_buffer[BufferFillIndex].port_number = get_midi_port();
    g_timeout_add(duration, timer_callback, (gpointer) BufferFillIndex);
    BufferFillIndex = BufferFillIndex+1 > DENEMO_BUFFER_MAX_INDEX ? 0 : BufferFillIndex+1;
    BufferEmpty=FALSE;
  }
#endif
}

#define MDC MD[client_number].ports[port_number]
void 
jack_output_midi_event(unsigned char *buffer, gint client_number, gint port_number){
  DenemoScore *si = Denemo.gui->si;

  if (!jack_server_running)
    return; 
  gint velocity = ((gint)(si->master_volume * buffer[2]));
  if(velocity>0x7F) velocity = 0x7F;
  MDC.midi_buffer[ MDC.FillIndex].buffer[0] = buffer[0];
  MDC.midi_buffer[ MDC.FillIndex].buffer[1] = buffer[1];
  MDC.midi_buffer[ MDC.FillIndex].buffer[2] = velocity;
  MDC.FillIndex++;
  if(MDC.FillIndex > DENEMO_BUFFER_MAX_INDEX) 
    MDC.FillIndex = 0;
  if ( MDC.FillIndex ==  MDC.Index)
    g_debug("\nBuffer Overrun\n");
  MDC.BufferEmpty=FALSE;
  g_debug("output to client %d port %d\n", client_number, port_number);
}

static void
send_midi_event(jack_nframes_t nframes, gint client_number){
  unsigned char *buffer;
  gint port_number;
  if(MD[client_number].jack_client==NULL)
    return;
  if(MD[client_number].ports==NULL)
    return;
  for(port_number = 0;MDC.output_port;port_number++) {
  gchar *outbuffer  = jack_port_get_buffer(MDC.output_port, nframes);
  jack_midi_clear_buffer(outbuffer);
  if (MDC.BufferEmpty==FALSE)
    while (MDC.Index != MDC.FillIndex){
      buffer = jack_midi_event_reserve(outbuffer, 0, 3);
      if (buffer == NULL){
        warn_from_jack_thread_context("jack_midi_event_reserve_failed, NOTE_LOST.");
        return;
      }
      buffer[0] = MDC.midi_buffer[MDC.Index].buffer[0];
      buffer[1] = MDC.midi_buffer[MDC.Index].buffer[1];
      buffer[2] = MDC.midi_buffer[MDC.Index].buffer[2];
      MDC.Index++;
      if(MDC.Index > DENEMO_BUFFER_MAX_INDEX)
	MDC.Index=0; 
    }
  MDC.BufferEmpty=TRUE;
  }
}
#undef MDC

static gboolean finish_play(gchar *callback) {
  if(callback && *callback)
    call_out_to_guile (callback);
  return FALSE;
}

static gdouble 		last_draw_time;
static gdouble 		pause_time = -1.0;
static gboolean jackmidi_play_smf_event(gchar *callback)
{
  DevicePort *DP;
  DenemoScore *si = Denemo.gui->si;
  smf_event_t *event = si->smf?smf_peek_next_event(si->smf):NULL;
  
  if (!jack_server_running)
    return FALSE;
  if (!MD[0].jack_client)
    return FALSE;
  if (!playing_piece)
    return finish_play(callback);

  if (event == NULL || (event->time_seconds > si->end_time)){  
    si->playingnow = NULL;
    playing_piece = FALSE;
    pause_time = -1.0;
    toggle_playbutton();
    return  finish_play(callback);
  }
  else 
    playing_piece = TRUE;
  
   /* Skip over metadata events. */
  if (smf_event_is_metadata(event)) {
    event = smf_get_next_event(si->smf);
    return TRUE; 
  } 
  // g_print("rightmost %f event %f\n", si->rightmost_time, event->time_seconds);
  if(si->rightmost_time>0.0 && event->time_seconds>si->rightmost_time)
    page_viewport();
  gdouble thetime = get_time() - si->start_player;
  pause_time = thetime;
  //g_print("thetime %f\n", thetime);
  thetime -= si->tempo_change_time - si->start_player;
  thetime *= si->master_tempo;
  thetime +=  si->tempo_change_time - si->start_player;
  com_tempo(si->tempo * si->master_tempo);
  //g_print("transformed to %f\n", thetime);
  if (thetime > event->time_seconds){
     event = smf_get_next_event(si->smf);
     si->playingnow = event->user_pointer;
     si->playhead = event->time_seconds;//the time of the playhead used in draw.c
     DP = event->track->user_pointer;
     if (!DP)
       return FALSE;
     //g_print("current object %p %x\n", event->user_pointer,((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)) );
     if(((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_ON) &&
	event->time_seconds - last_draw_time>Denemo.prefs.display_refresh) {
       //       g_print("drawing because %f %f\n", event->time_seconds, last_draw_time);
       last_draw_time = event->time_seconds;
       
       gtk_widget_queue_draw (Denemo.gui->scorearea);
     }
    gint chan = (event->midi_buffer[0] & 0x0f);
    //g_print("message %x %x\n", event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1, PROGRAM_CHANGE);
    int success;
    switch((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1))
    {
      case NOTE_ON: {
        if (si->end_time - event->time_seconds > 0.01) //Do not turn notes on too close to the end
        jack_output_midi_event(event->midi_buffer, DP->device_number, DP->port_number);
      }
      si->playhead += 0.001;//Make sure playhead is inside duration of note
      break;
      case NOTE_OFF:
        jack_output_midi_event(event->midi_buffer, DP->device_number, DP->port_number);
	si->playhead -= 0.001;//Make sure playhead is inside duration of note
	break;
      default:
	jack_output_midi_event(event->midi_buffer, DP->device_number, DP->port_number);
	break;
    }

  }
  return TRUE;
}

static void handle_midi_event(gchar *buf) {
  if(Denemo.gui->midi_destination & MIDIRECORD)
    record_midi(buf,  get_time() - Denemo.gui->si->start_player);
    process_midi_event(buf);
}

#define MAX_MIDI (10)
static gchar midi_in_buf[MAX_MIDI][3]; 
static volatile gboolean midi_in_count = 0;
static midi_in_timer_id = 0;
static gboolean midi_in_timer_callback(void) {
  if(midi_in_count) {
    gint i;
    for(i=0;i<midi_in_count;i++)
      handle_midi_event(midi_in_buf[i+1]);
  }
  midi_in_count = 0;//if an event has arrived during the last note entry we will lose it here - it is not critical, if it were we would have to turn off interrupts while working on the counter...
  return TRUE;//timer keeps going
}

//Under Interrupt
static void load_midi_buf(gint type, gint key, gint vel) {
  midi_in_count++;
  if(midi_in_count<MAX_MIDI) {
  midi_in_buf[midi_in_count][0] = type;
  midi_in_buf[midi_in_count][1] = key;
  midi_in_buf[midi_in_count][2] = vel;
  } 
  else midi_in_count--;//over flow
}


static void
process_midi_input(jack_nframes_t nframes)
{
  int read, events, i, channel;
  void           *port_buffer;
  jack_midi_event_t event;
  int             last_frame_time;
  static int      time_of_first_event = -1;
  
  //last_frame_time = jack_last_frame_time(midi_device[0].jack_client);
  port_buffer = jack_port_get_buffer(input_port, nframes);
  events = jack_midi_get_event_count(port_buffer);
  for (i = 0; i < events; i++) {
    read = jack_midi_event_get(&event, port_buffer, i);
    //process_midi_event(event.buffer);
    load_midi_buf (event.buffer[0], event.buffer[1], event.buffer[2]);
  }
}

/* Jack timebase callback.    
 * Runs in the process thread.  Realtime, must not wait.
 * This code was from tranport.c transport example  
 */
void timebase(jack_transport_state_t state, jack_nframes_t nframes,
		                jack_position_t *pos, int new_pos, void *arg)
{
  double min;  /* minutes since frame 0 */
  long abs_tick;  /* ticks since frame 0 */
  long abs_beat;  /* beats since frame 0 */
  
  if (new_pos || time_reset) {
    pos->valid = JackPositionBBT;
    pos->beats_per_bar = time_beats_per_bar;
    pos->beat_type = time_beat_type;
    pos->ticks_per_beat = time_ticks_per_beat;
    pos->beats_per_minute = time_beats_per_minute;
    time_reset = 0;  /* time change complete */
    /* Compute BBT info from frame number.  This is relatively 	
     * simple here, but would become complex if we support tempo
     * or time signature change at specific locations in the
     * transport timeline. 
     */
    min = pos->frame / ((double) pos->frame_rate * 60.0);
    abs_tick = min * pos->beats_per_minute * pos->ticks_per_beat;
    abs_beat = abs_tick / pos->ticks_per_beat;
    pos->bar = abs_beat / pos->beats_per_bar;
    pos->beat = abs_beat - (pos->bar * pos->beats_per_bar) + 1;
    pos->tick = abs_tick - (abs_beat * pos->ticks_per_beat);
    pos->bar_start_tick = pos->bar * pos->beats_per_bar * pos->ticks_per_beat;
    pos->bar++;  /* adjust start to bar 1 */
  } else {
    /* Compute BBT info based on previous period. */
    pos->tick += nframes * pos->ticks_per_beat * pos->beats_per_minute / (pos->frame_rate * 60);
    while (pos->tick >= pos->ticks_per_beat) {
      pos->tick -= pos->ticks_per_beat;
      if (++pos->beat > pos->beats_per_bar) {
	pos->beat = 1;
	++pos->bar;
	pos->bar_start_tick += pos->beats_per_bar * pos->ticks_per_beat;
      }
    }
  }
}
					
static int 
process_callback(jack_nframes_t nframes, gint client_number)
{
  if (nframes <= 0) {
    warn_from_jack_thread_context("Process callback called with nframes = 0; bug in JACK?");
    return 0;
  }
  if(Denemo.gui->input_source==INPUTMIDI && input_port)
    process_midi_input(nframes);
   
  send_midi_event(nframes, client_number);
  return 0;
}

static void
register_jack_midi_port(gint client_number, gint port_number, gchar *port_name){
#if 0
  if (!midi_device[client_number].jack_client)
    return;
  if (!jack_server_running)
    return;

  midi_device[client_number].output_ports[port_number] = jack_port_register(midi_device[client_number].jack_client,
		          port_name, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);
  jack_nframes_t nframes = jack_get_buffer_size(midi_device[client_number].jack_client);
  jack_midi_clear_buffer(jack_port_get_buffer(midi_device[client_number].output_ports[port_number], nframes));
#endif
}
/* returns the jack midi port number that
 *  has been assigned
 */
int 
create_jack_midi_port(gint client_number){
#if 0
  if (!midi_device[client_number].jack_client)
    return -1;

  if (!jack_server_running)
    return -1;

  char port_name[12];  
  gint i;
  
  /* only assign it if the port has not been assigned already */	
  for (i=0;i <= MAX_NUMBER_OF_TRACKS;i++)
    if (!midi_device[client_number].output_ports[i]){ 
      sprintf(port_name, "%s:%d",OUTPUT_PORT_NAME, i);
      register_jack_midi_port(client_number,i, port_name);      
      if (midi_device[client_number].output_ports[i]){
	/* clear buffer */
	g_debug("\nassigning jackmidi port output_port[%d]\n", i);
        MD[client_number].port_names = g_list_append(MD[client_number].port_names, g_string_new(port_name));	
	return i;
      } else {
	 g_critical("Could not register JACKMIDI %s:[%d]",OUTPUT_PORT_NAME,i);
         return -1;
        }
    }

#endif 
  return -1;
}

int
create_jack_midi_client(){
#if 0
  gint i;
  gint err;
  char client_name[12];
  
  if (!jack_server_running)
    return -1;
  
  for (i=0;i <= DENEMO_MAX_DEVICES;i++)
    if (!midi_device[i].jack_client){
      sprintf(client_name, "%s:%d",CLIENT_NAME, i); 
      midi_device[i].jack_client = jack_client_open(client_name, JackNoStartServer, NULL);
      
      if (!midi_device[i].jack_client){
        jack_server_running = FALSE;
	return -1;
      }
      
      //  if (i==0)
        if (jack_set_process_callback(midi_device[i].jack_client, process_callback, i)){
	  jack_server_running = FALSE;
	  g_critical("Could not register JACK process callback.");
	  return -1;
	}
         
      /* activate client */
      if (midi_device[i].jack_client){
	jack_activate(midi_device[i].jack_client);
	g_debug("\nassigning jackmidi client %s\n", client_name);
	MD[i].client_name = g_string_new(client_name);
	MD[i].port_names = NULL;
	jack_client_index++;
	return i;
      } else {
        jack_server_running = FALSE;
	g_critical("Could not connect to the JACK server; run jackd first?");
      }
    }
#endif
  return -1;
}



int 
remove_jack_midi_port(int client_number, int port_number){
#if 0
  int err,i;
  err = 0;
  
    if (midi_device[client_number].output_ports[port_number]){
      err = jack_port_unregister(midi_device[client_number].jack_client, midi_device[client_number].output_ports[port_number]);
      midi_device[client_number].output_ports[port_number] = NULL;
      GList *n = g_list_nth(MD[client_number].port_names, port_number);
      MD[client_number].port_names = g_list_remove(MD[client_number].port_names, 
		      n->data);
      g_debug("\nremove jackmidi device number %d, port number = %d\n", client_number, port_number);
      return err;
    }
  return err;
#endif
  return -1;
}

int 
remove_last_jack_midi_port(int client_number){
#if 0
  int err,i;
  err = 0;
  
  for (i=MAX_NUMBER_OF_TRACKS;i>=0;i--)
    if (midi_device[client_number].output_ports[i]){
      err = jack_port_unregister(midi_device[client_number].jack_client, midi_device[client_number].output_ports[i]);
      midi_device[client_number].output_ports[i] = NULL;
      GList *n = g_list_nth(MD[client_number].port_names, (int) i);
      MD[client_number].port_names = g_list_remove(MD[client_number].port_names, 
		      n->data);
      g_debug("\nremove jackmidi port number = %d\n", i);
      return err;
    }
  return err;
#endif
  return -1;
}

void
remove_all_jack_midi_ports(int client_number){
#if 0
  int err,i;
  err = 0;

  for (i=MAX_NUMBER_OF_TRACKS;i>=0;i--)
    if (midi_device[client_number].output_ports[i]){
      jack_port_unregister(midi_device[client_number].jack_client, midi_device[client_number].output_ports[i]);
      midi_device[client_number].output_ports[i] = NULL;
      g_debug("\nremoving jackmidi port number = %d\n", i);
    }
#endif
}

int
remove_jack_midi_client(gint i){
#if 0
  remove_all_jack_midi_ports(i);
  jack_deactivate(midi_device[i].jack_client);
  jack_client_close(midi_device[i].jack_client);
  midi_device[i].jack_client = NULL;
  g_string_free(MD[i].client_name, TRUE);
  MD[i].client_name = NULL;
  jack_client_index--;
#endif
  return i;
}

void
remove_all_jack_midi_clients(){
#if 0
  while (jack_client_index)
    remove_jack_midi_client(jack_client_index-1);
#endif
}

#define MDC MD[client_number].ports[port_number]
int
rename_jack_midi_port(int client_number, int port_number, char *port_name){
  int err = -1;

  if (MD[client_number].ports[port_number].port_name) 
    err = jack_port_set_name (MDC.output_port, port_name);
  if (!err)
    MD[client_number].ports[port_number].port_name = g_string_new(port_name);
  else	  
    g_critical("Could not rename JACK device %d output_ports[%d] to %s",client_number, port_number, port_name);
  
  return err;
}
#undef MDC

void 
stop_jack(void){
#if 0
  if (midi_device[0].jack_client)
    jack_port_unregister(midi_device[0].jack_client, input_port);
  remove_all_jack_midi_clients();
#endif
}

int
init_jack(void){
  int i, err, port_number;
  i = err = port_number = 0;
  //DeviceManager();
  if(MD==NULL) {
    jack_server_running = FALSE;
    if(Denemo.prefs.midi_audio_output==Jack)
      g_warning("No devices in preferences, edit->preferences->MIDI add devices and re-start");
    return -1;}

  for (i=0;MD[i].client_name;i++){
    g_debug("\njack init *** client name == %s \n",MD[i].client_name->str);
    //create_jack_midi_client_from_load(MD[i].client_name->str);
    MD[i].jack_client = (gpointer)jack_client_open(MD[i].client_name->str, JackNoStartServer, NULL);
    if(MD[i].jack_client == NULL) {
      if(Denemo.prefs.midi_audio_output==Jack)
	g_warning("Could not open JACK client %s, no jack server running?", MD[i].client_name->str );
      return -1;
    }
    if (jack_set_process_callback(MD[i].jack_client, (JackProcessCallback)process_callback, (void *)i)){
      jack_server_running = FALSE;
      g_warning("Could not register JACK process callback.");
      return -1;
    }
    if (jack_transport_master)
      if (jack_set_timebase_callback(MD[i].jack_client, TRUE, timebase, NULL) != 0)
	g_warning("Unable to take over timebase.");

    if(MD[i].jack_client) {
      gint j;
      for(j=0; MD[i].ports && j<DENEMO_MAX_PORTS && MD[i].ports[j].port_name; j++) {
	MD[i].ports[j].output_port = 
	  (gpointer) jack_port_register(MD[i].jack_client,
					MD[i].ports[j].port_name->str, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0);
	MD[i].ports[j].Index = MD[i].ports[j].FillIndex = 0;
	MD[i].ports[j].BufferEmpty = TRUE;   
      }
      if (i==0 && MD[0].jack_client && input_port==NULL)
        input_port = jack_port_register(MD[0].jack_client, INPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE,
				      JackPortIsInput, 0);
      midi_in_timer_id = g_timeout_add (20, (GSourceFunc) midi_in_timer_callback, NULL);
      /* activate client */
      jack_activate(MD[i].jack_client);
    }
  }  
  return err;
}


void jack_midi_play(gchar *callback)
{
  DenemoGUI *gui = Denemo.gui;
  if (!jack_server_running)
    return;
  if((gui->si->smf==NULL) || (gui->si->smfsync!=gui->si->changecount))
    generate_midi();
  if (Denemo.gui->si->smf == NULL) {
    g_critical("Loading SMF failed.");
    return;
  }
  static GString *callback_string;

  if(callback_string==NULL)
    callback_string=g_string_new("");
  if(callback)
    g_string_assign(callback_string, callback);
  else
    g_string_assign(callback_string,"(display \"Stopped Playing\")");
  

  if(playing_piece) {
    // pause
    toggle_playbutton();
    playing_piece = FALSE;
    (void)finish_play(callback);
    return;
  }
  if(gui->si->end_time<0.0)
    gui->si->end_time = smf_get_length_seconds(Denemo.gui->si->smf);
  
  if(gui->si->start_time>gui->si->end_time)
    gui->si->start_time =  0.0;
  if(gui->si->start_time<0.0)
    gui->si->start_time = 0.0;
  if( (pause_time>0.0) && (pause_time<gui->si->end_time))
    gui->si->start_player = get_time() - pause_time;
  else
    pause_time = -1.0, gui->si->start_player = get_time() - gui->si->start_time;
  playing_piece = TRUE;
  toggle_playbutton();
  gui->si->tempo_change_time = gui->si->start_player;
  
  smf_rewind(Denemo.gui->si->smf);
  last_draw_time = 0.0;
  if (jack_transport)
    jack_transport_start(MD[0].jack_client);
  g_idle_add(jackmidi_play_smf_event, callback_string->str);
  smf_seek_to_seconds(gui->si->smf, pause_time>0.0? pause_time:gui->si->start_time);
}

void
jack_midi_playback_stop ()
{
  if(playing_piece)
    toggle_playbutton();
  if (jack_transport)
    jack_transport_stop(MD[0].jack_client);
  Denemo.gui->si->playingnow = NULL;
  playing_piece = FALSE;
  pause_time = -1.0;
}

void
jack_midi_panic()
{
  gint i,j,c;
  gint client_number, port_number;
  jack_midi_playback_stop ();
  unsigned char buffer[3];

  //flush buffers, stop callback?
#define MD Denemo.prefs.midi_device
  for (i=0;MD[i].client_name;i++)
    for(j=0;MD[i].ports[j].port_name;j++) 
      for (c=0;c<16;c++){
        buffer[0] = MIDI_CONTROLLER | c;
        buffer[1] = MIDI_ALL_SOUND_OFF;
        buffer[2] = 0x00;  
        jack_output_midi_event(buffer, i, j);
      }
  
  Denemo.gui->si->end_time = pause_time = -1.0;
  Denemo.gui->si->start_time =  0.0;
  displayhelper(Denemo.gui);
}

#else //If not _HAVE_JACK_
void jack_playpitch(){}
void jack_output_midi_event(){}
int jack_kill_timer(){}
void jack_midi_playback_stop (){}
void jack_midi_playback_start (){}
void jack_midi_play(){}
void remove_last_jack_midi_port (){}
void create_jack_midi_port (){}
void remove_jack_midi_client (){}
void remove_jack_midi_port (){}
void rename_jack_midi_port (){}

void create_jack_midi_client (){}
jackmidi_default_client_name(){}
jackmidi_default_port_name(){}
int maxnumber_of_ports(){ return 0;}
int maxnumber_of_clients(){ return 0;}
int jackmidi_server_running(){ return 0;}
void stop_jack(){}
void jack_midi_panic(){}
#endif 

