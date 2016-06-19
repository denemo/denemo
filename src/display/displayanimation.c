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
#include "display/displayanimation.h"
#include "display/draw.h"

#define TRANSITION_MS (20)
static gint transition_steps = 0;
static gint transition_amount;  //number of bars being moved to left, negative means to the right.
static gint cursor_steps = 0;
static gint measure_transition_steps = 0;
static gint measure_transition_amount = 0;

static gboolean measure_all = FALSE;
static gint staff_transition_amount = 0;
static gint staff_transition_steps = 0;
static gint movement_transition_amount = 0;
static gint movement_transition_steps = 0;
static gint
cursor_transition (void)
{
  draw_score_area();
  return --cursor_steps;
}

static gboolean
transition (void)
{
  //g_debug("Transition %d current bar= %d\n", transition_steps, transition_amount);
  if (transition_steps == 1)
    {
      set_cursor_transition ();
    }
  draw_score_area();
  return --transition_steps;
}

static gboolean
staff_transition (void)
{
  //g_debug("Transition %d current bar= %d\n", transition_steps, transition_amount);
  if (staff_transition_steps == 1)
    {
      set_cursor_transition ();
    }
  draw_score_area();
  return --staff_transition_steps;
}

static gboolean
movement_transition (void)
{
  if (movement_transition_steps == 1)
    {
      set_cursor_transition ();
    }
  draw_score_area();
  return --movement_transition_steps;
}

static gboolean
measure_transition (void)
{
  //g_debug("Measure transition %d current bar= %d\n", measure_steps, measure_amount);
  if (measure_transition_steps == 1)
    {
      set_cursor_transition ();
    }
  draw_score_area();
  return --measure_transition_steps;
}

gdouble
transition_offset (void)
{
  if (Denemo.project->view == DENEMO_PAGE_VIEW)
    return 0.0;
  return (gdouble) transition_steps *transition_amount * Denemo.project->movement->measurewidth / 10;
}

gdouble
staff_transition_offset (void)
{
  if (Denemo.project->view == DENEMO_PAGE_VIEW)
    return 0.0;
  return (gdouble) staff_transition_steps *staff_transition_amount;
}

gdouble
movement_transition_offset (void)
{
  if (Denemo.project->view == DENEMO_PAGE_VIEW)
    return 0.0;
  return (gdouble) movement_transition_steps *movement_transition_amount;
}

gdouble
measure_transition_offset (gboolean current)
{
  if (Denemo.project->view == DENEMO_PAGE_VIEW)
    return 0.0;
  if (current || measure_all)
    return (gdouble) measure_transition_steps *measure_transition_amount;
  else
    return 0.0;
}

gdouble
transition_cursor_scale (void)
{
  return cursor_steps ? (gdouble) cursor_steps : 1.0;
}

void
set_viewport_transition (gint amount)
{
  if (transition_steps)
    return;
  if (movement_transition_steps)
    return;
  if (Denemo.prefs.cursor_highlight &&  amount)
    {
      transition_amount = amount;
      transition_steps = 10;
      g_timeout_add (TRANSITION_MS, (GSourceFunc) transition, NULL);
    }
}

void
set_measure_transition (gint amount, gboolean all)
{
  if (measure_transition_steps)
    return;
  if (Denemo.prefs.cursor_highlight && amount)
    {
      measure_transition_amount = amount;
      measure_transition_steps = 10;
      measure_all = all;
      g_timeout_add (TRANSITION_MS, (GSourceFunc) measure_transition, NULL);
    }
}

void
set_cursor_transition (void)
{
  if (Denemo.prefs.cursor_highlight && cursor_steps == 0)
    {
      cursor_steps = 10;
      g_timeout_add (TRANSITION_MS, (GSourceFunc) cursor_transition, NULL);
    }
}

void
set_staff_transition (gint amount)
{
  if (movement_transition_steps)
    return;
  if (Denemo.prefs.cursor_highlight && staff_transition_steps == 0)
    {
      staff_transition_steps = 10;
      staff_transition_amount = amount;
      g_timeout_add (TRANSITION_MS, (GSourceFunc) staff_transition, NULL);
    }
}

void
set_movement_transition (gint amount)
{
  if (Denemo.prefs.cursor_highlight && movement_transition_steps == 0)
    {
      movement_transition_steps = 20;
      movement_transition_amount = amount;
      g_timeout_add (TRANSITION_MS, (GSourceFunc) movement_transition, NULL);
    }
}
