/* moveviewport.cpp
 *  functions that change leftmeasurenum, rightmeasurenum, top_measure,
 *  bottom_measure
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *  (c) 2000-2005 Matthew Hiller
 */

#include "commandfuncs.h"
#include "contexts.h"
#include "moveviewport.h"
#include "staffops.h"
#include "utils.h"

/**
 * update_hscrollbar should be called as a cleanup whenever
 * si->leftmeasurenum or si->rightmeasurenum may have been altered,
 *  e.g., by preceding calls to set_rightmeasurenum; or when the
 *  number of measures may have changed.  
 */
void
update_hscrollbar (DenemoGUI * gui)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (gui->hadjustment);

  adj->upper = g_list_length (gui->si->measurewidths) + 1.0;
  adj->page_size = adj->page_increment
    = gui->si->rightmeasurenum - gui->si->leftmeasurenum + 1.0;
  adj->value = gui->si->leftmeasurenum;
#if GTK_MAJOR_VERSION > 1
  gtk_adjustment_changed(adj);
  //gtk_adjustment_value_changed(adj);
#else
  gtk_range_slider_update (GTK_RANGE (gui->hscrollbar));
#endif
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
update_vscrollbar (DenemoGUI * gui)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (gui->vadjustment);
  adj->upper = g_list_length (gui->si->thescore) + 1.0;
  adj->page_size = adj->page_increment
    = gui->si->bottom_staff - gui->si->top_staff + 1.0;
  adj->value = gui->si->top_staff;
#if GTK_MAJOR_VERSION > 1
  gtk_adjustment_changed(adj);
  //gtk_adjustment_value_changed(adj);
#else
  gtk_range_slider_update (GTK_RANGE (gui->vscrollbar));
#endif
}

/**
 * Sets the si->rigthmeasurenum to the largest
 * value in the score
 */
void
set_rightmeasurenum (DenemoScore * si)
{
  gint spaceleft = si->widthtoworkwith;
  GList *mwidthiterator =
    g_list_nth (si->measurewidths, si->leftmeasurenum - 1);

  for (si->rightmeasurenum = si->leftmeasurenum;
       mwidthiterator && spaceleft >= GPOINTER_TO_INT (mwidthiterator->data);
       spaceleft -=
       (GPOINTER_TO_INT (mwidthiterator->data) + SPACE_FOR_BARLINE),
       mwidthiterator = mwidthiterator->next, si->rightmeasurenum++)
    ;
  si->rightmeasurenum = MAX (si->rightmeasurenum - 1, si->leftmeasurenum);
}

/**
 * Utility function for advancing a staff number and staff iterator to
 * the next primary voice, or to one off the end and NULL if there are
 * none remaining.  
 */
static void
to_next_primary_voice (gint * staff_number, staffnode ** staff_iterator)
{
  do
    {
      (*staff_number)++;
      *staff_iterator = (*staff_iterator)->next;
    }
  while (*staff_iterator
	 && ((DenemoStaff *) (*staff_iterator)->data)->voicenumber == 2);
}

/**
 * This function also has a side effect of bumping si->top_staff
 * up to the staff number of the next primary voice if si->top_staff
 * initially points to a nonprimary voice.  
 */
void
set_bottom_staff (DenemoGUI * gui)
{
  gint space_left;
  staffnode *staff_iterator;
  gint staff_number;

  /* Bump up si->top_staff, if necessary.  */
  staff_iterator = g_list_nth (gui->si->thescore, gui->si->top_staff - 1);
  if (((DenemoStaff *) staff_iterator->data)->voicenumber == 2)
    to_next_primary_voice (&gui->si->top_staff, &staff_iterator);

  /* With that settled, now determine how many additional (primary)
     staves will fit into the window.  */
  staff_number = gui->si->top_staff;
  space_left = gui->scorearea->allocation.height/gui->si->zoom;
  do
    {
      space_left -= gui->si->staffspace;
      to_next_primary_voice (&staff_number, &staff_iterator);
    }
  while (staff_iterator && space_left >= gui->si->staffspace);

  gui->si->bottom_staff = staff_number - 1;
}

/**
 * Find out if measure is past the current leftmeasurenum
 *
 */
void
isoffleftside (DenemoGUI * gui)
{
  while (gui->si->currentmeasurenum < gui->si->leftmeasurenum)
    {
      gui->si->leftmeasurenum
	-=
	MAX ((gui->si->rightmeasurenum - gui->si->leftmeasurenum + 1) / 2, 1);
      if (gui->si->leftmeasurenum < 1)
	gui->si->leftmeasurenum = 1;
      set_rightmeasurenum (gui->si);
    }
  find_leftmost_allcontexts (gui->si);
  update_hscrollbar (gui);
}

/**
 * Find out if measure is past the current rightmeasurenum
 *
 */
void
isoffrightside (DenemoGUI * gui)
{
  while (gui->si->currentmeasurenum > gui->si->rightmeasurenum)
    {
      gui->si->leftmeasurenum
	+=
	MAX ((gui->si->rightmeasurenum - gui->si->leftmeasurenum + 1) / 2, 1);
      set_rightmeasurenum (gui->si);
    }
  find_leftmost_allcontexts (gui->si);
  update_hscrollbar (gui);
}

/**
 * Move the viewable part of the score up 
 *
 */
void
move_viewport_up (DenemoGUI * gui)
{
  staffnode *staff_iterator;

  staff_iterator = g_list_nth (gui->si->thescore, gui->si->top_staff - 1);
  while (gui->si->currentstaffnum < gui->si->top_staff
	 || ((DenemoStaff *) staff_iterator->data)->voicenumber == 2)
    {
      gui->si->top_staff--;
      staff_iterator = staff_iterator->prev;
    }
  set_bottom_staff (gui);
  update_vscrollbar (gui);
}


/**
 * Move viewable part of the score down
 *
 */
void
move_viewport_down (DenemoGUI * gui)
{
  staffnode *staff_iterator;

  staff_iterator = g_list_nth (gui->si->thescore, gui->si->top_staff - 1);
  while (gui->si->currentstaffnum > gui->si->bottom_staff)
    {
      to_next_primary_voice (&gui->si->top_staff, &staff_iterator);
      set_bottom_staff (gui);
    }
  update_vscrollbar (gui);
}


/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it the leftmost measure visible
 *
 */
gboolean
set_currentmeasurenum (DenemoGUI * gui, gint dest)
{
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->si->measurewidths))))
    {
      gui->si->leftmeasurenum = dest;
      gui->si->currentmeasurenum = dest;
      setcurrents (gui->si);
      set_rightmeasurenum (gui->si);
      find_leftmost_allcontexts (gui->si);
      update_hscrollbar (gui);
      gtk_widget_queue_draw (gui->scorearea);
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
set_currentstaffnum (DenemoGUI * gui, gint dest)
{
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->si->thescore))))
    {
      gui->si->currentstaffnum = dest;
      gui->si->currentstaff =
	g_list_nth (gui->si->thescore, gui->si->currentstaffnum - 1);
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      find_leftmost_allcontexts (gui->si);
      update_vscrollbar (gui);
      gtk_widget_queue_draw (gui->scorearea);
      return TRUE;
    }
  return FALSE;
}

/**
 * Scroll the score vertically
 *
 */
void
vertical_scroll (GtkAdjustment * adjust, DenemoGUI * gui)
{
  gint dest;
  if ((dest = (gint) (adjust->value + 0.5)) != gui->si->top_staff)
    {
      gui->si->top_staff = dest;
      //  while(gui->si->top_staff>g_list_length (gui->si->thescore))
      //  gui->si->top_staff--;
      set_bottom_staff (gui);
      if (gui->si->currentstaffnum > gui->si->bottom_staff)
	{
	  gui->si->currentstaffnum = gui->si->bottom_staff;
	  gui->si->currentstaff =
	    g_list_nth (gui->si->thescore, gui->si->bottom_staff - 1);
	  setcurrentprimarystaff (gui->si);
	  setcurrents (gui->si);
	}
      else if (gui->si->currentstaffnum < gui->si->top_staff)
	{
	  gui->si->currentstaffnum = gui->si->top_staff;
	  gui->si->currentstaff =
	    g_list_nth (gui->si->thescore, gui->si->top_staff - 1);
	  setcurrentprimarystaff (gui->si);
	  setcurrents (gui->si);
	}
      gtk_widget_queue_draw (gui->scorearea);
    }
  update_vscrollbar (gui);
}

/**
 * Scroll score horizontally
 *
 */
void
horizontal_scroll (GtkAdjustment * adjust, DenemoGUI * gui)
{
  gint dest;

  if ((dest = (gint) (adjust->value + 0.5)) != gui->si->leftmeasurenum)
    {
      gui->si->leftmeasurenum = dest;
      set_rightmeasurenum (gui->si);
      if (gui->si->currentmeasurenum > gui->si->rightmeasurenum)
	{
	  gui->si->currentmeasurenum = gui->si->rightmeasurenum;

	}
      else if (gui->si->currentmeasurenum < gui->si->leftmeasurenum)
	{
	  gui->si->currentmeasurenum = gui->si->leftmeasurenum;

	}
      find_leftmost_allcontexts (gui->si);
      setcurrents (gui->si);
      gtk_widget_queue_draw (gui->scorearea);
    }
  update_hscrollbar (gui);
}
