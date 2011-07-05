/*
 * midi.c
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include <denemo/denemo.h>
#include "midi.h"
#include "smf.h"
#include "exportmidi.h"
#include "draw.h"

#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>


static volatile gboolean playing = FALSE;

static double last_draw_time;


void update_position(smf_event_t *event) {
  DenemoScore *si = Denemo.gui->si;

  if (event) {
    if ((event->midi_buffer[0] & 0xf0) == NOTE_ON &&
        event->time_seconds - last_draw_time > Denemo.prefs.display_refresh) {
      last_draw_time = event->time_seconds;
      queue_redraw_playhead(event);
    }
  } else {
    si->playingnow = NULL;
    si->playhead = 0;
    queue_redraw_all();
  }
}



void start_playing() {
  smf_t *smf = Denemo.gui->si->smf;

  smf_rewind(smf);

  smf_seek_to_seconds(smf, Denemo.gui->si->start_time);

  initialize_playhead();

  playing = TRUE;
  last_draw_time = 0.0;
}


void stop_playing() {
  update_position(NULL);
  playing = FALSE;
}


gboolean is_playing() {
  return playing;
}


double get_start_time() {
  return Denemo.gui->si->start_time;
}


double get_end_time() {
  return Denemo.gui->si->end_time;
}


smf_event_t *get_smf_event(double until_time) {
  smf_t *smf = Denemo.gui->si->smf;

  if (until_time > Denemo.gui->si->end_time) {
    until_time = Denemo.gui->si->end_time;
  }

  for (;;) {
    smf_event_t *event = smf_peek_next_event(smf);

    if (event == NULL || event->time_seconds >= until_time) {
      return NULL;
    }

    if (smf_event_is_metadata(event)) {
      // consume metadata event and continue with the next one
      event = smf_get_next_event(smf);
      continue;
    }

    // consume the event
    event = smf_get_next_event(smf);

    assert(event->midi_buffer_length <= 3);

    return event;
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
    } while(event && (!(event->midi_buffer[0] & NOTE_ON) || !event->user_pointer));
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
    } while(event && (!(event->midi_buffer[0] & NOTE_OFF) || !event->user_pointer));
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
