#ifdef _HAVE_FLUIDSYNTH_
#include <glib.h>
#include <fluidsynth.h>
#include <sys/time.h>
#include "midi.h"
#include "smf.h"
#include "fluid.h"
#include "moveviewport.h"
#include "playback.h"

#define MAX_NUMBER_OF_TRACKS    128
#define NOTE_OFF                0x80
#define NOTE_ON                 0x90
#define KEY_PRESSURE      	0xA0
#define CONTROL_CHANGE  	0xB0
#define PROGRAM_CHANGE  	0xC0
#define	CHANNEL_PRESSURE 	0xD0
#define PITCH_BEND      	0xE0
#define MIDI_SYSTEM_RESET       0xFF

/*********************
NOTE_OFF = 0x80,
46	  NOTE_ON = 0x90,
47	  KEY_PRESSURE = 0xa0,
48	  CONTROL_CHANGE = 0xb0,
49	  PROGRAM_CHANGE = 0xc0,
50	  CHANNEL_PRESSURE = 0xd0,
51	  PITCH_BEND = 0xe0,
case CONTROL_CHANGE:
         return fluid_synth_cc(synth, chan, event->midi_buffer[1], event->midi_buffer[2]); 
                             
       case PROGRAM_CHANGE:
         return fluid_synth_program_change(synth, chan,  event->midi_buffer[1]);
 
       case CHANNEL_PRESSURE:
       return fluid_synth_channel_pressure(synth, chan,  event->midi_buffer[1]);
 
       case PITCH_BEND:
         return fluid_synth_pitch_bend(synth, chan, event->midi_buffer[1] + (event->midi_buffer[2]<<8
				      );
 
       case MIDI_SYSTEM_RESET:
***********************/




#define SYS_EXCLUSIVE_MESSAGE1  0xF0

static fluid_settings_t* settings;
static fluid_synth_t* synth;
static fluid_audio_driver_t* adriver;



static gboolean playing_piece;


static gint start_fluid_settings()
{
  settings = new_fluid_settings();
  if (!settings){
    g_warning("\nFailed to create the settings\n");
    fluidsynth_shutdown();
    return 1;
  }
  return 0;
}

gchar * fluidsynth_get_default_audio_driver()
{
  if (!settings)
    start_fluid_settings(); 
  return fluid_settings_getstr_default(settings, "audio.driver");
}

int fluidsynth_init()
{ 
  g_debug("\nStarting FLUIDSYNTH\n"); 
  /* Create the settings. */
  if (!settings)
    start_fluid_settings(); 

  /* Change the settings if necessary*/
  fluid_settings_setstr(settings, "audio.jack.id", "Denemo");
  fluid_settings_setint(settings, "audio.jack.autoconnect", 1);

  fluid_settings_setstr(settings, "audio.driver", Denemo.prefs.fluidsynth_audio_driver->str);
  fluid_settings_setint(settings, "synth.reverb.active" , Denemo.prefs.fluidsynth_reverb?1:0);
  fluid_settings_setint(settings, "synth.chorus.active" , Denemo.prefs.fluidsynth_chorus?1:0);
  if(Denemo.prefs.fluidsynth_sample_rate>(22050-1)) {
    gint success = 
      fluid_settings_setnum(settings, "synth.sample-rate" ,(double) Denemo.prefs.fluidsynth_sample_rate);
    g_print("Setting sample rate %f %s\n",(double) Denemo.prefs.fluidsynth_sample_rate, success?"- success":"- FAILURE");
  }

  if(Denemo.prefs.fluidsynth_period_size>(64-1)) {
   fluid_settings_setint(settings, "audio.period-size" , Denemo.prefs.fluidsynth_period_size);
    g_print("Setting audio.period-size to %d\n", Denemo.prefs.fluidsynth_period_size);
  }

#ifdef G_OS_WIN32
  if(Denemo.prefs.fluidsynth_sample_rate<22050) {
    fluid_settings_setnum(settings, "synth.sample-rate" , 44100.0);
    g_print("Setting sample rate to %f Hz\n", 44100.0);
  }
  if(Denemo.prefs.fluidsynth_period_size<64) {
   fluid_settings_setint(settings, "audio.period-size" , 2048);
    g_print("Setting audio.period-size to %d\n", 2048);
  }
#endif


  /* Create the synthesizer. */
  synth = new_fluid_synth(settings);
  if (!synth){
    g_warning("\nFailed to create the settings\n");
    //fluidsynth_shutdown();
    delete_fluid_synth(synth);
    return 1;
  }

  /* Create the audio driver. */
  adriver = new_fluid_audio_driver(settings, synth);
  if (!adriver){
    g_warning("\nFailed to create the audio driver\n");
    fluidsynth_shutdown();
    return 1;
  }
  
  /* Load a SoundFont*/
gint  sfont_id = fluid_synth_sfload(synth, Denemo.prefs.fluidsynth_soundfont->str, 0);
  if (sfont_id == -1){
    g_warning("\nFailed to load the soundfont\n");
    fluidsynth_shutdown();
    return 1;
  }
  
  /* Select bank 0 and preset 0 in the SoundFont we just loaded on
  channel 0 */
  fluid_synth_program_select(synth, 0, sfont_id, 0, 0);


  gint i;
  for(i=0;i<16;i++)
    fluid_synth_program_change(synth, i, 0);
  if(Denemo.prefs.pitchspellingchannel)
    fluid_synth_program_change(synth, Denemo.prefs.pitchspellingchannel, Denemo.prefs.pitchspellingprogram);
  set_tuning();
  return 0;
}                                                                                                                              
                    
void fluidsynth_shutdown()
{
  /* Clean up */
  g_debug("\nStopping FLUIDSYNTH\n");
  if (adriver)
    delete_fluid_audio_driver(adriver);
  adriver = NULL;
  if (synth)
    delete_fluid_synth(synth);
  synth = NULL;
  if (settings)
    delete_fluid_settings(settings); 
  settings = NULL;
}

void
fluidsynth_start_restart (void){
  if (synth){
    fluidsynth_shutdown();
    fluidsynth_init();
  }
  else {
    fluidsynth_init();
  }
}

static gboolean noteoff_callback(gint notenum){
  gint key = notenum&0xFF;
  gint chan = notenum>>8;
  //g_print("turning off %d on channel %d\n", key, chan);
  if (synth)
    fluid_synth_noteoff(synth, chan, key);
  return FALSE;
}


void fluid_all_notes_off_channel(gint chan) {
  fluid_synth_cc(synth, chan, 123, 0);
}

void fluid_all_notes_off(void) {
  gint chan;
  for(chan=0;chan<16;chan++)
  fluid_all_notes_off_channel(chan);
}



// play the midipitch key for duration with volume (0-> default, 80) on channel

void fluid_playpitch(int key, int duration, int channel, int volume)
{
  /* Play a note */
  if (synth){
    //g_print("Emitting key %d\n", key);
    fluid_synth_noteon(synth, channel, key, (volume?volume:127)*Denemo.gui->si->master_volume);
    g_timeout_add(duration,(GSourceFunc) noteoff_callback, (gpointer)( (channel<<8) + key)); 
  }
}

void fluid_output_midi_event(unsigned char *buffer)
{
  if (synth){

    gint chan = (buffer[0] & 0x0f);
   
    int success;
    switch((buffer[0] & SYS_EXCLUSIVE_MESSAGE1))
      {
      case NOTE_ON: {
	gint velocity =  ((gint)(Denemo.gui->si->master_volume * buffer[2]));
	if(velocity>0x7F) velocity = 0x7F;
	fluid_synth_noteon(synth, chan,  buffer[1], velocity);
	//g_print("play %d on %d\n", chan, velocity);
      }
	break;
       case NOTE_OFF:
         fluid_synth_noteoff(synth, chan, buffer[1]);
	 //g_print("play %d off %f\n", chan, event->time_seconds);
	 break; 
       case CONTROL_CHANGE:
         fluid_synth_cc(synth, chan, buffer[1], buffer[2]);
	 break; 
                             
       case PROGRAM_CHANGE:
	 //g_print("changing on chan %d to prog? %d\n", chan,  event->midi_buffer[1]);
         success = fluid_synth_program_change(synth, chan,  buffer[1]);
	 //g_print("success = %d\n", success);
	 break;
 
	 //     case CHANNEL_PRESSURE:
	 //return fluid_synth_channel_pressure(synth, chan,  event->midi_buffer[1]);
 
       case PITCH_BEND:
       g_print("LSB %d MSB %d so %d\n", buffer[1], buffer[2], buffer[2]<<7);
         fluid_synth_pitch_bend(synth, chan, buffer[1] + (buffer[2]<<7)
				       /*I think! fluid_midi_event_get_pitch(event)*/);
	 break;
 
       case MIDI_SYSTEM_RESET:
         fluid_synth_system_reset(synth);
	 break;

      case SYS_EXCLUSIVE_MESSAGE1:
	if(FLUIDSYNTH_VERSION_MAJOR>=1 && FLUIDSYNTH_VERSION_MINOR>=1)
	  fluid_synth_sysex(synth, buffer+1, 19, NULL, 0, NULL, FALSE);
	else
	  g_warning("Not supported by this fluidsynth version use >=1.1");
	//char *response, int *response_len, int *handled, int dryrun)
	break;
	
	//FIXME - duplicate code below 
      default:
	g_warning("MIDI message Not handled");
      }
  }
}

/**
 * Select the soundfont to use for playback
 */
void
choose_sound_font (GtkWidget * widget, GtkWidget *fluidsynth_soundfont)
{
  GtkWidget *sf;
  GtkFileFilter *filter;

  sf = gtk_file_chooser_dialog_new (_("Choose SoundFont File"),
				     GTK_WINDOW (Denemo.window),
				    GTK_FILE_CHOOSER_ACTION_OPEN,
				    GTK_STOCK_CANCEL,
				    GTK_RESPONSE_REJECT,
				    GTK_STOCK_OPEN,
				    GTK_RESPONSE_ACCEPT, NULL);

  //TODO Should we filter????
  //filter = gtk_file_filter_new ();
  //gtk_file_filter_set_name (filter, "Soundfont file");
  //gtk_file_filter_add_pattern (filter, "*.sf");
  //gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (fs), filter);

  gtk_widget_show_all (sf);
  if (gtk_dialog_run (GTK_DIALOG (sf)) == GTK_RESPONSE_ACCEPT)
    {
      g_string_assign (Denemo.prefs.fluidsynth_soundfont,
		       gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (sf)));
  	/* this will only work for 1 sound font */
        gtk_entry_set_text (GTK_ENTRY (fluidsynth_soundfont), Denemo.prefs.fluidsynth_soundfont->str);

    }
  gtk_widget_destroy (sf);
}





static gboolean finish_play(gchar *callback) {
  if(callback && *callback)
    call_out_to_guile (callback);
  fluid_all_notes_off();
  if(Denemo.gui->si->recorded_midi_track) {
    safely_track_remove_from_smf(Denemo.gui->si->recorded_midi_track);
    finish_recording();
  }
  set_tempo();
  return FALSE;
}



static gdouble last_draw_time;
static gdouble pause_time = -1.0;
static  gdouble playalong_time = 0.0;
static  gdouble early_time = 0.0;


static gdouble GET_TIME (void) {
  DenemoScore *si = Denemo.gui->si;
  gdouble thetime = get_time();
  if((Denemo.gui->midi_destination & (MIDIPLAYALONG|MIDICONDUCT)) && (thetime > si->start_player + playalong_time))
    return playalong_time;
  return thetime - si->start_player;
}

//test if the midi event in buf is a note-on for the current note
//if so set a time delta so that a call to GET_TIME() will return get_time()- lost_time
//unless this time is greater than the start time of the next note when GET_TIME() will stick
//advance cursor to next note
static void advance_clock(gchar *buf) {
  if(Denemo.gui->si->currentobject) {
    DenemoObject *obj = Denemo.gui->si->currentobject->data;
    if(obj->type!=CHORD) 
      if(cursor_to_next_chord()) 
	obj = Denemo.gui->si->currentobject->data;
    
    if(Denemo.gui->si->currentobject && obj->type==CHORD) {
      chord *thechord = obj->object;
      if(thechord->notes) {
	note *thenote = thechord->notes->data;
	if( ((buf[0]&SYS_EXCLUSIVE_MESSAGE1)==NOTE_ON) && buf[2] && buf[1] == (dia_to_midinote (thenote->mid_c_offset) + thenote->enshift)) {
	  gdouble thetime = get_time();
	  Denemo.gui->si->start_player = thetime -  obj->earliest_time;
	  
	  if(thechord->is_tied && cursor_to_next_note()) {
	    obj = Denemo.gui->si->currentobject->data;	   
	  }
	  //playalong_time = obj->latest_time;
	  //IF THE NEXT OBJ IS A REST ADVANCE OVER IT/THEM
	  do {
	    if(!cursor_to_next_note())	//if(!cursor_to_next_chord())	   	      
	      {
		playalong_time = Denemo.gui->si->end_time + 1.0;
		break;
	      }
	    else {
	      obj = Denemo.gui->si->currentobject->data;
	      thechord = obj->object;
	      playalong_time = obj->earliest_time;
	    }
	  } 
	  while(!thechord->notes);	    
	}
      }
    } else
      g_warning("Not on a chord");
  } else
    g_warning("Not on an object");
}

void advance_time(gdouble seconds) {
  playalong_time += seconds;
}
static void initialize_clock(void) {
  if(Denemo.gui->si->currentobject ) {
    DenemoObject *obj = Denemo.gui->si->currentobject->data;
    if(obj->type==CHORD) {
      chord *thechord = obj->object;
      if(thechord->notes) {
	note *thenote = thechord->notes->data;      
	//gboolean thetime = get_time();
	//Denemo.gui->si->start_player =  thetime - obj->earliest_time;
	playalong_time = obj->latest_time; 
      }
    }
  }
}



static gboolean fluidsynth_play_smf_event(gchar *callback)
{
  DenemoScore *si = Denemo.gui->si;
  smf_event_t *event = si->smf?smf_peek_next_event(si->smf):NULL;
  
  if (!synth)
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
    page_viewport(), si->rightmost_time=-1;
  gdouble thetime = GET_TIME();//get_time() - si->start_player;
  pause_time = thetime;
  //g_print("thetime %f\n", thetime);
  thetime -= si->tempo_change_time - si->start_player;
  thetime *= si->master_tempo;
  thetime +=  si->tempo_change_time - si->start_player;
  //g_print("transformed to %f\n", thetime);
  if (thetime > event->time_seconds){
     event = smf_get_next_event(si->smf);
     si->playingnow = event->user_pointer;
     si->playhead = event->time_seconds;//the time of the playhead used in draw.c
     //g_print("current object %p %x\n", event->user_pointer,((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)) );
     if(((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_ON) &&
	event->time_seconds - last_draw_time>Denemo.prefs.display_refresh) {
       //       g_print("drawing because %f %f\n", event->time_seconds, last_draw_time);
       last_draw_time = event->time_seconds;
       //gtk_widget_queue_draw (Denemo.scorearea);
       region_playhead();//FIXME - need this to decide if the playhead has moved not just assume it has.!!!!
     }
    gint chan = (event->midi_buffer[0] & 0x0f);
    //g_print("message %x %x\n", event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1, PROGRAM_CHANGE);
    int success;
    switch((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1))
      {
      case NOTE_ON: {
	gint velocity =  ((gint)(si->master_volume * event->midi_buffer[2]));
	if(velocity>0x7F) velocity = 0x7F;
	if( si->end_time - event->time_seconds > 0.01)//Do not turn notes on too close to the end
	  fluid_synth_noteon(synth, chan,  event->midi_buffer[1], velocity);
	//g_print("play %d on %f\n", chan, event->time_seconds);
      }
	si->playhead += 0.001;//Make sure playhead is inside duration of note
	break;
       case NOTE_OFF:
         fluid_synth_noteoff(synth, chan,  event->midi_buffer[1]);
	 si->playhead -= 0.001;//Make sure playhead is inside duration of note
	 //g_print("play %d off %f\n", chan, event->time_seconds);
	 break; 
       case CONTROL_CHANGE:
         fluid_synth_cc(synth, chan, event->midi_buffer[1], event->midi_buffer[2]);
	 break; 
                             
       case PROGRAM_CHANGE:
	 //g_print("changing on chan %d to prog? %d\n", chan,  event->midi_buffer[1]);
         success = fluid_synth_program_change(synth, chan,  event->midi_buffer[1]);
	 //g_print("success = %d\n", success);
	 break;
 
	 //     case CHANNEL_PRESSURE:
	 //return fluid_synth_channel_pressure(synth, chan,  event->midi_buffer[1]);
 
       case PITCH_BEND:
         fluid_synth_pitch_bend(synth, chan, event->midi_buffer[1] + (event->midi_buffer[2]<<7)
				       /*I think! fluid_midi_event_get_pitch(event)*/);
	 break;
 
       case MIDI_SYSTEM_RESET:
         fluid_synth_system_reset(synth);
	 break;

      case SYS_EXCLUSIVE_MESSAGE1:
	if(FLUIDSYNTH_VERSION_MAJOR>=1 && FLUIDSYNTH_VERSION_MINOR>=1) {
	 gint ret = fluid_synth_sysex(synth, event->midi_buffer+1, 19, NULL, 0, NULL, FALSE);
	 //g_print("Got ret %d bytes sent are %x %x %x %x %x\n", ret, event->midi_buffer[1], event->midi_buffer[2], event->midi_buffer[3], event->midi_buffer[4], event->midi_buffer[5]);
	}
	else
	  g_warning("Not supported by this fluidsynth version use >=1.1");
	//char *response, int *response_len, int *handled, int dryrun)
	break;

      }
  }
  return TRUE;
}

//return the midi key of the passed event if note on, else 0
int noteon_key(smf_event_t *event) {
if((event->midi_buffer[0] & SYS_EXCLUSIVE_MESSAGE1)==NOTE_ON)
  return event->midi_buffer[1];
return 0;
}

void fluid_midi_play(gchar *callback)
{
  DenemoGUI *gui = Denemo.gui;
  if (!synth)
    return;
  if((gui->si->smf==NULL) || (gui->si->smfsync!=gui->si->changecount))
    generate_midi();
  if (Denemo.gui->si->smf == NULL) {
    g_critical("Loading SMF failed.");
    return;
  }
  if(gui->si->recorded_midi_track)
    safely_add_track(gui->si->smf, gui->si->recorded_midi_track);
 
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



  playing_piece = TRUE;
  toggle_playbutton();
  smf_rewind(Denemo.gui->si->smf);
  last_draw_time = 0.0;
  if(Denemo.gui->midi_destination & (MIDIPLAYALONG|MIDICONDUCT))
    initialize_clock();
  initialize_playhead();

  gint error = smf_seek_to_seconds(gui->si->smf, pause_time>0.0? pause_time:gui->si->start_time);

  if( (pause_time>0.0) && (pause_time<gui->si->end_time))
    gui->si->start_player = get_time() - pause_time;//FIXME ??
  else
    pause_time = -1.0, gui->si->start_player = get_time() - gui->si->start_time;//FIXME ??
  gui->si->tempo_change_time = gui->si->start_player;

  g_idle_add((GSourceFunc)fluidsynth_play_smf_event, callback_string->str);
  //this is never turned off????
  
  // g_timeout_add_full (G_PRIORITY_HIGH, 15,(GSourceFunc)fluidsynth_play_smf_event, callback_string->str, NULL);                                             
}

void
fluid_midi_stop(void)
{
  if(playing_piece)
    toggle_playbutton();
  Denemo.gui->si->playingnow = NULL;
  playing_piece = FALSE;
  pause_time = -1.0;
}

void
fluid_midi_panic(void)
{
  fluid_synth_system_reset(synth);
  Denemo.gui->si->end_time = pause_time = -1.0;
  Denemo.gui->si->start_time =  0.0;
  displayhelper(Denemo.gui);
}


/* give audible feedback for entering a rhythmic element */
static gint rhythm_sounds[] = {41,48,64,62,60,70, 81, 69, 79};
void
fluid_rhythm_feedback(gint duration, gboolean rest, gboolean dot) {
  if(dot)
    fluid_playpitch(67, 100, 9, 60*Denemo.gui->si->master_volume);
  else
    fluid_playpitch(rhythm_sounds[duration], rest?100:200, 9, 127*Denemo.gui->si->master_volume);
  //add extra sound effect for rests
  if(rest)
    fluid_playpitch(46, 300, 9, 127*Denemo.gui->si->master_volume);
 
  //g_print("playing %d %d\n", rhythm_sounds[duration], (60/(4*Denemo.gui->si->tempo*(1<<duration)))*1000);

}




static fluid_midi_driver_t* midi_in;
//#define ENTERING_MASK (GDK_MOD1_MASK) //Alt
#define EDITING_MASK (GDK_SHIFT_MASK)  
static void handle_midi_event(gchar *buf) {
  //g_print("%x : %x %x %x %x\n", Denemo.keyboard_state, GDK_CONTROL_MASK, GDK_SHIFT_MASK, GDK_MOD1_MASK, GDK_LOCK_MASK);

  if( (Denemo.gui->midi_destination & MIDIRECORD) ||
      (Denemo.gui->midi_destination & (MIDIPLAYALONG|MIDICONDUCT))) {
    if(Denemo.gui->midi_destination & MIDIRECORD)
      record_midi(buf,  get_time() - Denemo.gui->si->start_player);
    if(Denemo.gui->midi_destination & (MIDIPLAYALONG))
      advance_clock(buf);
    fluid_output_midi_event(buf);
  } else {
    if((Denemo.keyboard_state==(GDK_SHIFT_MASK|GDK_LOCK_MASK)) ||
       Denemo.keyboard_state==(GDK_CONTROL_MASK) ||
       Denemo.keyboard_state==(ADDING_MASK) ||
       Denemo.keyboard_state==((ADDING_MASK)|(CHORD_MASK)) ||
       Denemo.keyboard_state==(GDK_CONTROL_MASK|GDK_LOCK_MASK) ||
       (Denemo.keyboard_state==0))
      process_midi_event(buf);
    else
      if(Denemo.keyboard_state==(GDK_SHIFT_MASK) ||
	 Denemo.keyboard_state==(GDK_LOCK_MASK)) {
	  adjust_midi_velocity(buf, 100 - Denemo.prefs.dynamic_compression);
	  fluid_output_midi_event(buf);
	 }
  }
}


#define MAX_MIDI (10)
static gchar midi_in_buf[MAX_MIDI][3]; 
static volatile gint midi_in_count = 0;
static gint midi_in_feed = 0;
static midi_in_timer_id = 0;
static gboolean midi_in_timer_callback(void) {

  if(midi_in_count>midi_in_feed) {
    gint theone;
    midi_in_feed++;
    theone = midi_in_feed;
    if(midi_in_count==midi_in_feed)
      midi_in_count=midi_in_feed=0;
    handle_midi_event(midi_in_buf[theone]);
  }
  if(midi_in_count==midi_in_feed)
    midi_in_count=midi_in_feed=0;
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

//Under Interrupt
static handle_midi_event_func_t handle_midi_in(void* data, fluid_midi_event_t* event)
{
  int type = 
    fluid_midi_event_get_type(event);
  //g_print("event type: %x\n", type);
  switch(type) {



  case CONTROL_CHANGE:
  case NOTE_ON:
  case NOTE_OFF:
  case KEY_PRESSURE:
  
  case 0xF2:    
    {
      int key = fluid_midi_event_get_key(event);
      int velocity = fluid_midi_event_get_velocity(event);
      if(type==NOTE_ON && velocity==0) {//Zero velocity NOTEON is used as NOTEOFF by some MIDI controllers
	type=NOTE_OFF;
	velocity=127;
      }
#if 0
      if (type == CONTROL_CHANGE && (key == 0x40)){
	if (velocity == 0x7F)
	  //PEDAL DOWN
	  Denemo.keyboard_state |= ADDING_MASK;
	else
	  Denemo.keyboard_state &= ~(CHORD_MASK|ADDING_MASK);
      }
#endif  
      type  |= ((DenemoStaff *)Denemo.gui->si->currentstaff->data)->midi_channel;
      load_midi_buf(type, key, velocity);
    }
    break;
  case PITCH_BEND:
  
    {
      short key = fluid_midi_event_get_pitch(event);
      load_midi_buf(type,  key&0x7F, key>>7);
    }
    break;

  case PROGRAM_CHANGE:
  case CHANNEL_PRESSURE:
  case 0xF3: 
    {
      int key = fluid_midi_event_get_key(event);
      load_midi_buf(type, key, 0);
    }
    break;
  default:
    g_warning("not handled type %x\n", type);
  }
  return 0;
}


int
fluid_start_midi_in(void)
{
  fluid_settings_t* settings = new_fluid_settings();
  int success = Denemo.prefs.fluidsynth_midi_driver->len?fluid_settings_setstr(settings, "midi.driver", Denemo.prefs.fluidsynth_midi_driver->str):0;
#ifdef OSS_DRIVER
  success = fluid_settings_setstr(settings, "midi.driver", "oss");
  success = fluid_settings_setstr(settings, "midi.oss.device", "/dev/midi1"); 
#endif
  
#ifdef ALSA_DRIVER
  success = fluid_settings_setstr(settings, "midi.driver", "alsa_seq");
#endif
  //g_print("success %d\n", success);
  midi_in = new_fluid_midi_driver(settings, 
    (handle_midi_event_func_t) handle_midi_in, NULL);


  //g_print("midi in on %p\n", midi_in);
  if(midi_in) {
      midi_in_timer_id = g_timeout_add (20, (GSourceFunc) midi_in_timer_callback, NULL);
      //midi_in_timer_id = g_idle_add_full(G_PRIORITY_DEFAULT_IDLE-10, (GSourceFunc)midi_in_timer_callback, NULL, NULL);


    return 0;
  }
  else
    return -1;
}
int
fluid_stop_midi_in(void)
{
  if(midi_in) {
    g_source_remove(midi_in_timer_id);
    delete_fluid_midi_driver(midi_in);
  }
  else
    return -1;
  midi_in = NULL;
  return 0;
}

#else // _HAVE_FLUIDSYNTH_
void fluid_playpitch(int key, int duration, int channel, int vol){}
void fluid_output_midi_event(unsigned char *buffer){}
void fluid_midi_play(void){}
void fluid_midi_stop(void){}
void fluid_midi_panic(void){}
int fluid_kill_timer(void){}
int fluid_stop_midi_in(void){}
int fluid_start_midi_in(void){}
void advance_time(void) {}
int noteon_key(void) {return 0}
#endif 

