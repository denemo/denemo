/* moveviewport.c
 *  functions that change leftmeasurenum, rightmeasurenum, top_measure,
 *  bottom_measure
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *  (c) 2000-2005 Matthew Hiller
 */

#include "command/commandfuncs.h"
#include "command/contexts.h"
#include "ui/moveviewport.h"
#include "command/staff.h"
#include "core/utils.h"
#include "display/displayanimation.h"
#include "display/draw.h"
#include "command/select.h"

/**
 * update_hscrollbar should be called as a cleanup whenever
 * si->leftmeasurenum or si->rightmeasurenum may have been altered,
 *  e.g., by preceding calls to set_rightmeasurenum; or when the
 *  number of measures may have changed.
 */
void
update_hscrollbar (DenemoProject * gui)
{
  if(Denemo.non_interactive)
    return;
  GtkAdjustment *adj = GTK_ADJUSTMENT (Denemo.hadjustment);
  gdouble upper = g_list_length (gui->movement->measurewidths) + 1.0, page_size = gui->movement->rightmeasurenum - gui->movement->leftmeasurenum + 1.0;
  gdouble left = gtk_adjustment_get_value (adj);
  gtk_adjustment_set_upper (adj, upper);
  gtk_adjustment_set_page_size (adj, page_size);
  gtk_adjustment_set_page_increment (adj, page_size);
  gtk_adjustment_set_value (adj, gui->movement->leftmeasurenum);
  //gtk_adjustment_changed (adj);
  //g_debug("steps %d Difference %d\n",transition_steps, (gint)(left-gui->movement->leftmeasurenum));
  set_viewport_transition ((gint) (gui->movement->leftmeasurenum) - left);
}

/**
 * update_vscrollbar should be called as a cleanup whenever
 * si->top_staff or si->bottom_staff may have been altered,
 * e.g., by preceding calls to set_bottom_staff; or when the number of
 * staffs may have changed.
 *
 * For simplicity, this function treats nonprimary voices as
 * full-fledged staffs, which'll be visually confusing. I'll fix it
 * soon.
 */

void
update_vscrollbar (DenemoProject * gui)
{
  if(Denemo.non_interactive)
    return;
  GtkAdjustment *adj = GTK_ADJUSTMENT (Denemo.vadjustment);
  gtk_adjustment_set_upper (adj, g_list_length (gui->movement->thescore) + 1.0);
  gtk_adjustment_set_page_size (adj, gui->movement->bottom_staff - gui->movement->top_staff + 1.0);
  gtk_adjustment_set_page_increment (adj, gui->movement->bottom_staff - gui->movement->top_staff + 1.0);
  gtk_adjustment_set_value (adj, gui->movement->top_staff);

  //gtk_adjustment_changed (adj);
  //gtk_range_slider_update (GTK_RANGE (Denemo.vscrollbar));
}

/**
 * Sets the si->rigthmeasurenum to use
 * all the space si->widthtoworkwith assuming si->leftmeasurenum, as determined by the si->measurewidths
 * returns TRUE if si->rightmeasurenum is changed
 */
gboolean
set_rightmeasurenum (DenemoMovement * si)
{
  gint initial = si->rightmeasurenum;
  gint spaceleft = si->widthtoworkwith;
  GList *mwidthiterator = g_list_nth (si->measurewidths, si->leftmeasurenum - 1);

  for (si->rightmeasurenum = si->leftmeasurenum; mwidthiterator && spaceleft >= GPOINTER_TO_INT (mwidthiterator->data); spaceleft -= (GPOINTER_TO_INT (mwidthiterator->data) + SPACE_FOR_BARLINE), mwidthiterator = mwidthiterator->next, si->rightmeasurenum++)
    ;
  si->rightmeasurenum = MAX (si->rightmeasurenum - 1, si->leftmeasurenum);
  return initial != si->rightmeasurenum;
}

/**
 * Utility function for advancing a staff number and staff iterator to
 * the next primary voice, or to one off the end and NULL if there are
 * none remaining.
 */
static void
to_next_primary_voice (gint * staff_number, staffnode ** staff_iterator)
{
  if(!staff_iterator)
    return;

  do
    {
      (*staff_number)++;
      *staff_iterator = (*staff_iterator)->next;
    }
  while (*staff_iterator && !((DenemoStaff *) (*staff_iterator)->data)->voicecontrol & DENEMO_PRIMARY);
}

/**
 * This function also has a side effect of bumping si->top_staff
 * up to the staff number of the next primary voice if si->top_staff
 * initially points to a nonprimary voice.
 */
void
set_bottom_staff (DenemoProject * gui)
{
  if(Denemo.non_interactive)
    return;

  gint space_left;
  staffnode *staff_iterator;
  gint staff_number;

  /* Bump up si->top_staff, if necessary.  */
  staff_iterator = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);
  if (!staff_iterator) {gui->movement->top_staff = 1;g_critical ("Bad top staff number %d",gui->movement->top_staff);  return;}//g_assert (staff_iterator);
  if (!((DenemoStaff *) staff_iterator->data)->voicecontrol & DENEMO_PRIMARY)
    to_next_primary_voice (&gui->movement->top_staff, &staff_iterator);

  /* With that settled, now determine how many additional (primary)
     staves will fit into the window.  */
  staff_number = gui->movement->top_staff;

  space_left = get_widget_height (Denemo.scorearea) * gui->movement->system_height / gui->movement->zoom;
  // space_left -= 2*LINE_SPACE;
  do
    {
      DenemoStaff *staff = staff_iterator->data;
      if(!staff->hidden)
        space_left -= (staff->space_above -staff->space_shorten + staff->space_below + gui->movement->staffspace);    //2*STAFF_HEIGHT);
      to_next_primary_voice (&staff_number, &staff_iterator);
    }
  while (staff_iterator && space_left >= 0);
  if (space_left < 0 && staff_number > (gui->movement->top_staff + 1))
    staff_number--;
  gui->movement->bottom_staff = staff_number - 1;
}

/**
 * inverse of the below
 *
 */
void
isoffleftside (DenemoProject * gui)
{
  if (gui->movement->currentmeasurenum == 0) gui->movement->currentmeasurenum = 1;
  if (gui->movement->currentmeasurenum >= gui->movement->leftmeasurenum)
    return;
  while (gui->movement->currentmeasurenum < gui->movement->leftmeasurenum)
    {
      gui->movement->leftmeasurenum -= MAX ((gui->movement->rightmeasurenum - gui->movement->leftmeasurenum + 1) / 2, 1);
      if (gui->movement->leftmeasurenum < 1)
        gui->movement->leftmeasurenum = 1;
      set_rightmeasurenum (gui->movement);
    }
  find_leftmost_allcontexts (gui->movement);
  if(!Denemo.non_interactive)
    update_hscrollbar (gui);
}

/**
 * Advance the leftmeasurenum until currentmeasurenum is before rightmeasurenum
 * then adjust rightmeasurenum to match.
 */
void
isoffrightside (DenemoProject * gui)
{
  if (gui->movement->currentmeasurenum <= gui->movement->rightmeasurenum)
    return;
  while (gui->movement->currentmeasurenum > gui->movement->rightmeasurenum)
    {
      gui->movement->leftmeasurenum += MAX ((gui->movement->rightmeasurenum - gui->movement->leftmeasurenum + 1) / 2, 1);
      set_rightmeasurenum (gui->movement);
    }
  find_leftmost_allcontexts (gui->movement);
  if(!Denemo.non_interactive)
    update_hscrollbar (gui);
}

/**
 * Move the viewable part of the score up
 *
 */
void
move_viewport_up (DenemoProject * gui)
{
  if(Denemo.non_interactive)
    return;
  staffnode *staff_iterator;

  staff_iterator = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);
  while (gui->movement->currentstaffnum < gui->movement->top_staff || !((DenemoStaff *) staff_iterator->data)->voicecontrol & DENEMO_PRIMARY)
    {
      gui->movement->top_staff--;
      staff_iterator = staff_iterator->prev;
    }
  set_bottom_staff (gui);
  update_vscrollbar (gui);
}



static void
center_viewport (void)
{
  Denemo.project->movement->leftmeasurenum = Denemo.project->movement->currentmeasurenum - (Denemo.project->movement->rightmeasurenum - Denemo.project->movement->leftmeasurenum) / 2;
  if (Denemo.project->movement->leftmeasurenum < 1)
    Denemo.project->movement->leftmeasurenum = 1;
}

void
page_viewport (void)
{
  gdouble value, upper;
  GtkAdjustment *adj = GTK_ADJUSTMENT (Denemo.hadjustment);
  //g_debug("%d %d\n", Denemo.project->movement->leftmeasurenum, Denemo.project->movement->rightmeasurenum);
  gint amount = (Denemo.project->movement->rightmeasurenum - Denemo.project->movement->leftmeasurenum + 1);
  value = gtk_adjustment_get_value (adj);
  upper = gtk_adjustment_get_upper (adj);
  if (value + amount < upper)
    {
      gtk_adjustment_set_value (adj, value + amount);
    }
  else
    gtk_adjustment_set_value (adj, upper - 1);
}


/**
 * Move viewable part of the score down
 *
 */
void
move_viewport_down (DenemoProject * gui)
{
  if(Denemo.non_interactive)
    return;
  staffnode *staff_iterator;

  staff_iterator = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);
  while (gui->movement->currentstaffnum > gui->movement->bottom_staff)
    {
      to_next_primary_voice (&gui->movement->top_staff, &staff_iterator);
      set_bottom_staff (gui);
    }
  update_vscrollbar (gui);
}


/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it the leftmost measure visible, not extending selection
 *
 */
static gboolean
goto_currentmeasurenum (gint dest, gint leftmeasurenum)
{
  DenemoProject *gui = Denemo.project;
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->movement->measurewidths))))
    {
      //gui->movement->leftmeasurenum = dest;
      gui->movement->currentmeasurenum = dest;
      if(leftmeasurenum)
        gui->movement->leftmeasurenum = leftmeasurenum;
      set_rightmeasurenum (gui->movement);
      if ((dest < gui->movement->leftmeasurenum) || (dest > gui->movement->rightmeasurenum))
        center_viewport ();
      setcurrents (gui->movement);
      set_rightmeasurenum (gui->movement);
      find_leftmost_allcontexts (gui->movement);
      if(!Denemo.non_interactive)
        update_hscrollbar (gui);
      draw_score_area();
      return TRUE;
    }
  return FALSE;
}

/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it centered in the viewport if not already in view.
 *
 */
gboolean
set_currentmeasurenum (DenemoProject * gui, gint dest)
{
  return goto_currentmeasurenum (dest, 0);
}

/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it the leftmeasure the leftmost measure visible if possible otherwise centering on the viewport
 *
 */
gboolean
moveto_currentmeasurenum (DenemoProject * gui, gint dest, gint leftmeasurenum)
{
  return goto_currentmeasurenum (dest, leftmeasurenum);
}




/**
 * Sets the si->currentstaffnum to the given value
 * if it exists, extending selection if extend_selection
 *
 */
gboolean
goto_currentstaffnum (DenemoProject * gui, gint dest, gboolean extend_selection)
{
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->movement->thescore))))
    {
      //hide_lyrics(); cannot do this here, the lyrics pane when clicked on adjusts the current staff, and so this unselects the lyrics pane.
      gui->movement->currentstaffnum = dest;
      gui->movement->currentstaff = g_list_nth (gui->movement->thescore, gui->movement->currentstaffnum - 1);
      staff_set_current_primary (gui->movement);
      setcurrents (gui->movement);
      if (extend_selection)
        calcmarkboundaries (gui->movement);
      find_leftmost_allcontexts (gui->movement);
      if(!Denemo.non_interactive)
        update_vscrollbar (gui);
      //show_lyrics();
      draw_score_area();
      return TRUE;
    }
  return FALSE;
}

/**
 * Sets the si->currentstaffnum to the given value
 * if it exists
 *
 */
gboolean
set_currentstaffnum (DenemoProject * gui, gint dest)
{
  return goto_currentstaffnum (gui, dest, TRUE);
}

gboolean
moveto_currentstaffnum (DenemoProject * gui, gint dest)
{
  return goto_currentstaffnum (gui, dest, FALSE);
}

/**
 * Scroll the score vertically
 *
 */
void
vertical_scroll (GtkAdjustment * adjust, gpointer dummy)
{
  DenemoProject *gui = Denemo.project;
  gint dest;
  gdouble value = gtk_adjustment_get_value (adjust);
  if ((dest = (gint) (value + 0.5)) != gui->movement->top_staff)
    {
      gui->movement->top_staff = dest;
      //  while(gui->movement->top_staff>g_list_length (gui->movement->thescore))
      //  gui->movement->top_staff--;
      set_bottom_staff (gui);
      if (gui->movement->currentstaffnum > gui->movement->bottom_staff)
        {
          gui->movement->currentstaffnum = gui->movement->bottom_staff;
          gui->movement->currentstaff = g_list_nth (gui->movement->thescore, gui->movement->bottom_staff - 1);
          staff_set_current_primary (gui->movement);
          setcurrents (gui->movement);
          if (gui->movement->markstaffnum)
            calcmarkboundaries (gui->movement);
        }
      else if (gui->movement->currentstaffnum < gui->movement->top_staff)
        {
          gui->movement->currentstaffnum = gui->movement->top_staff;
          gui->movement->currentstaff = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);
          staff_set_current_primary (gui->movement);
          setcurrents (gui->movement);
          if (gui->movement->markstaffnum)
            calcmarkboundaries (gui->movement);
        }
      draw_score_area();
    }
  update_vscrollbar (gui);
}

/**
 * Scroll score horizontally
 *
 */
static void
h_scroll (gdouble value, DenemoProject * gui)
{
  gint dest;
  if ((dest = (gint) (value + 0.5)) != gui->movement->leftmeasurenum)
    {
      set_viewport_transition (dest - gui->movement->leftmeasurenum);
      gui->movement->leftmeasurenum = dest;
      set_rightmeasurenum (gui->movement);
      if (gui->movement->currentmeasurenum > gui->movement->rightmeasurenum)
        {
          gui->movement->currentmeasurenum = gui->movement->rightmeasurenum;

        }
      else if (gui->movement->currentmeasurenum < gui->movement->leftmeasurenum)
        {
          gui->movement->currentmeasurenum = gui->movement->leftmeasurenum;

        }
      find_leftmost_allcontexts (gui->movement);
      setcurrents (gui->movement);
      draw_score_area();
    }
  update_hscrollbar (gui);
}

void
horizontal_scroll (GtkAdjustment * adjust, gpointer dummy)
{
  DenemoProject *gui = Denemo.project;
  gdouble value = gtk_adjustment_get_value (adjust);
  h_scroll (value, gui);
}

void
scroll_left (void)
{
  if (Denemo.project->movement->leftmeasurenum > 1)
    h_scroll (Denemo.project->movement->leftmeasurenum - 1.0, Denemo.project);
}

void
scroll_right (void)
{
  if (Denemo.project->movement->leftmeasurenum < g_list_length (Denemo.project->movement->measurewidths))
    h_scroll (Denemo.project->movement->leftmeasurenum + 1.0, Denemo.project);
}
