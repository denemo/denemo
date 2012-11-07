//      displayanimation.c
//      
//      Copyright 2012 Richard Shann <rshann@debian-box>
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 3 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.
#include "displayanimation.h"

static gint transition_steps = 0;
static gint transition_amount;//number of bars being moved to left, negative means to the right.
static gint cursor_steps = 0;
static gint measure_transition_steps = 0;
static gint measure_transition_amount = 0;
static gboolean measure_all;
static cursor_transition(void) {
	gtk_widget_queue_draw (Denemo.scorearea);
	return --cursor_steps;
}

static gboolean transition(void) {
	//g_print("Transition %d current bar= %d\n", transition_steps, transition_amount);
	if(transition_steps==1 && cursor_steps==0) {
		cursor_steps = 10;
		g_timeout_add(20, (GSourceFunc)cursor_transition, NULL);
	}
	gtk_widget_queue_draw (Denemo.scorearea);
	return --transition_steps;
}
static gboolean measure_transition(void) {
	//g_print("Measure transition %d current bar= %d\n", measure_steps, measure_amount);
	if(measure_transition_steps==1 && cursor_steps==0) {
		cursor_steps = 10;
		g_timeout_add(20, (GSourceFunc)cursor_transition, NULL);
	}
	gtk_widget_queue_draw (Denemo.scorearea);
	return --measure_transition_steps;
}
gdouble transition_offset(void) {
	if(Denemo.gui->view==DENEMO_PAGE_VIEW)
		return 0.0;
	return (gdouble)transition_steps*transition_amount*Denemo.gui->si->measurewidth/10;
}
gdouble measure_transition_offset(gboolean current) {
	if(Denemo.gui->view==DENEMO_PAGE_VIEW)
		return 0.0;
	if(current || measure_all)
		return (gdouble)measure_transition_steps*measure_transition_amount;
	else
		return 0.0;
}
gdouble transition_cursor_scale(void) {
	return cursor_steps?
	(gdouble)cursor_steps:1.0;
}
void set_viewport_transition(gint amount) {
	 if(transition_steps) return;
	 if(amount) {
	 transition_amount = amount;
	 transition_steps = 10;
	 g_timeout_add(20, (GSourceFunc)transition, NULL);
 }
}

void set_measure_transition(gint amount, gboolean all) {
	 if(measure_transition_steps) return;
	 if(amount) {
	 measure_transition_amount = amount;
	 measure_transition_steps = 10;
	 measure_all = all;
	 g_timeout_add(20, (GSourceFunc)measure_transition, NULL);
 }
}
