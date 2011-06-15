/* midi.c
 * functions for direct output to /dev/sequencer
 * and direct input from /dev/midi
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Brian Delaney
 */

#include "config.h"
#include <denemo/denemo.h>
#include "smf.h"
#include "exportmidi.h"

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define MIDI_NOTEOFF            0x80
#define MIDI_NOTEON             0x90
#define MIDI_KEY_PRESSURE       0xA0

#define MIDI_CTL_CHANGE         0xB0
#define MIDI_PGM_CHANGE         0xC0
#define MIDI_CHN_PRESSURE       0xD0
#define MIDI_PITCH_BEND         0xE0

#define MIDI_SYSTEM_PREFIX      0xF0



static gboolean playing = FALSE;


void start_playing() {
  smf_t *smf = Denemo.gui->si->smf;

  int ignore = smf_seek_to_seconds(smf, 0.0);

  playing = TRUE;
}

void stop_playing() {
  playing = FALSE;
}

gboolean is_playing() {
  return playing;
}


gboolean get_smf_event(unsigned char *event_buffer, size_t *event_length, double *event_time, double until_time) {
  if (!playing) {
    return FALSE;
  }

  smf_t *smf = Denemo.gui->si->smf;

  for (;;) {
    smf_event_t *event = smf_peek_next_event(smf);

    if (event == NULL || event->time_seconds >= until_time) {
      return FALSE;
    }

    if (smf_event_is_metadata(event)) {
      // consume metadata event and continue with the next one
      event = smf_get_next_event(smf);
      continue;
    }

    // consume the event
    event = smf_get_next_event(smf);

    assert(event->midi_buffer_length <= 3);

    memcpy(event_buffer, event->midi_buffer, event->midi_buffer_length);
    *event_length = event->midi_buffer_length;
    *event_time = event->time_seconds;

    return TRUE;
  }
}




gdouble get_time() {
  GTimeVal tv;
  double seconds;

  g_get_current_time(&tv);

  seconds = tv.tv_sec + tv.tv_usec / 1000000.0;
  return seconds;
}


void generate_midi() {
  if((Denemo.gui->si->smf==NULL) || (Denemo.gui->si->smfsync!=Denemo.gui->si->changecount)) {
    exportmidi(NULL, Denemo.gui->si, 0, 0);
  }

  if (Denemo.gui->si->smf == NULL) {
    g_critical("Loading SMF failed.");
  }
}


/* return the time of the last event on the list events */
gdouble get_midi_off_time(GList *events) {
  smf_event_t *event = g_list_last(events)->data;
return event->time_seconds;
}

/* return the time of the first event on the list events */
gdouble get_midi_on_time(GList *events) {
  smf_event_t *event = events->data;
  return event->time_seconds;
}



DenemoObject *get_obj_for_start_time(smf_t *smf, gdouble time) {
  if(time<0.0)
      time=0.0;
  static smf_event_t *event;
  static guint smfsync;
  static DenemoScore *last_si = NULL;
  static gdouble last_time=-1.0;
  if( fabs(time-last_time)>0.001 || (last_si!=Denemo.gui->si) || (smfsync!=Denemo.gui->si->smfsync)) {
    smf_event_t *initial = smf_peek_next_event(smf);

    gdouble total = smf_get_length_seconds(smf);
    time = (time>total?total:time);
    gint error = smf_seek_to_seconds(smf, time);
    do {
      event = smf_get_next_event(smf);
    } while(event && (!(event->midi_buffer[0] & MIDI_NOTEON) || !event->user_pointer));
    if(initial)
      error = smf_seek_to_event(smf, initial);
    last_si = Denemo.gui->si;
    smfsync = Denemo.gui->si->smfsync;
    last_time = time;
  }
  if(event)
    return (DenemoObject *)(event->user_pointer);
  return NULL;
}

DenemoObject *get_obj_for_end_time(smf_t *smf, gdouble time) {
  if(time<0.0)
      time=0.0;
  static smf_event_t *event = NULL;
  static guint smfsync;
  static DenemoScore * last_si = NULL;
  static gdouble last_time=-1.0;
  if( fabs(time-last_time)>0.001 || (last_si!=Denemo.gui->si) || (smfsync!=Denemo.gui->si->smfsync)) {
    smf_event_t *initial = smf_peek_next_event(smf);

    gdouble total = smf_get_length_seconds(smf);
    time = (time>total?total:time);
    gint error = smf_seek_to_seconds(smf, time);
    do {
      event = smf_get_next_event(smf);
    } while(event && (!(event->midi_buffer[0] & MIDI_NOTEOFF) || !event->user_pointer));
    if(initial)
      error = smf_seek_to_event(smf, initial);
    last_si = Denemo.gui->si;
    smfsync = Denemo.gui->si->smfsync;
    last_time = time;
  }
  if(event)
    return (DenemoObject *)(event->user_pointer);
  return NULL;
}


// FIXME: not quite sure what to do with these yet

gint midi_init () { return 0; }

gint init_midi_input() { return 0; }
void start_midi_input() { }
gint stop_midi_input() { return 0; }

gint get_midi_channel() { return 0; }
gint get_midi_prognum() { return 0; }

gboolean set_midi_capture(gboolean set) { return FALSE; }

void process_midi_event(gchar *buf) { }

gboolean intercept_midi_event(gint *midi) { return FALSE; }

void change_tuning(gdouble *cents) { }
