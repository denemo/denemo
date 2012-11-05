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

static gint transition_steps = 0;
static gint transition_amount;//number of bars being moved to left, negative means to the right.
static gint cursor_steps = 0;
static cursor_transition(void) {
	gtk_widget_queue_draw (Denemo.scorearea);
	return --cursor_steps;
}

static gboolean transition(void) {
	//g_print("Transition %d current bar= %d\n", transition_steps, transition_amount);
	if(transition_steps==1) {
		cursor_steps = 10;
		g_timeout_add(20, (GSourceFunc)cursor_transition, NULL);
	}
	gtk_widget_queue_draw (Denemo.scorearea);
	return --transition_steps;
}

gdouble transition_offset(void) {
	return (gdouble)transition_steps*transition_amount*Denemo.gui->si->measurewidth/10;
}
gdouble transition_cursor_scale(void) {
	return cursor_steps?
	(gdouble)cursor_steps:1.0;
}
 static void set_transition(gint amount) {
	 if(transition_steps) return;
	 if(amount) {
	 transition_amount = amount;
	 transition_steps = 10;
	 g_timeout_add(20, (GSourceFunc)transition, NULL);
 }
}
/**
 * update_hscrollbar should be called as a cleanup whenever
 * si->leftmeasurenum or si->rightmeasurenum may have been altered,
 *  e.g., by preceding calls to set_rightmeasurenum; or when the
 *  number of measures may have changed.  
 */
void
update_hscrollbar (DenemoGUI * gui)
{
 GtkAdjustment *adj = GTK_ADJUSTMENT (Denemo.hadjustment);
 gdouble upper = g_list_length (gui->si->measurewidths) + 1.0, page_size = gui->si->rightmeasurenum - gui->si->leftmeasurenum + 1.0;
 gdouble left = gtk_adjustment_get_value(adj);
 gtk_adjustment_set_upper (adj, upper);   
 gtk_adjustment_set_page_size(adj,  page_size);
 gtk_adjustment_set_page_increment(adj, page_size);
 gtk_adjustment_set_value(adj, gui->si->leftmeasurenum);
 gtk_adjustment_changed(adj);
 //g_print("steps %d Difference %d\n",transition_steps, (gint)(left-gui->si->leftmeasurenum));
 set_transition((gint)(gui->si->leftmeasurenum) - left);
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
  GtkAdjustment *adj = GTK_ADJUSTMENT (Denemo.vadjustment);
  gtk_adjustment_set_upper(adj, g_list_length (gui->si->thescore) + 1.0);
  gtk_adjustment_set_page_size(adj, gui->si->bottom_staff - gui->si->top_staff + 1.0);
  gtk_adjustment_set_page_increment(adj, gui->si->bottom_staff - gui->si->top_staff + 1.0);
  gtk_adjustment_set_value(adj, gui->si->top_staff);
  
  gtk_adjustment_changed(adj);
  //gtk_range_slider_update (GTK_RANGE (Denemo.vscrollbar));
}

/**
 * Sets the si->rigthmeasurenum to use
 * all the space si->widthtoworkwith assuming si->leftmeasurenum, as determined by the si->measurewidths
 * returns TRUE if si->rightmeasurenum is changed
 */
gboolean
set_rightmeasurenum (DenemoScore * si)
{
	gint initial = si->rightmeasurenum;
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
  do
    {
      (*staff_number)++;
      *staff_iterator = (*staff_iterator)->next;
    }
  while (*staff_iterator
	 && !((DenemoStaff *) (*staff_iterator)->data)->voicecontrol & DENEMO_PRIMARY);
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
  if (!((DenemoStaff *) staff_iterator->data)->voicecontrol & DENEMO_PRIMARY)
    to_next_primary_voice (&gui->si->top_staff, &staff_iterator);

  /* With that settled, now determine how many additional (primary)
     staves will fit into the window.  */
  staff_number = gui->si->top_staff;

  space_left = get_widget_height(Denemo.scorearea)*gui->si->system_height/gui->si->zoom;
   space_left -= 2*LINE_SPACE;
  do
    {
     DenemoStaff *staff = staff_iterator->data;
     space_left -= (staff->space_above + staff->space_below + 2*STAFF_HEIGHT);
     to_next_primary_voice (&staff_number, &staff_iterator);
    }
  while (staff_iterator && space_left >= 0);
  if(space_left<0 && staff_number>(gui->si->top_staff + 1))
    staff_number--;
  gui->si->bottom_staff = staff_number - 1;
}

/**
 * inverse of the below
 *
 */
void
isoffleftside (DenemoGUI * gui)
{
	if(gui->si->currentmeasurenum >= gui->si->leftmeasurenum)
		return;
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
 * Advance the leftmeasurenum until currentmeasurenum is before rightmeasurenum
 * then adjust rightmeasurenum to match.
 */
void
isoffrightside (DenemoGUI * gui)
{
	if(gui->si->currentmeasurenum<=gui->si->rightmeasurenum)
		return;
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
	 || !((DenemoStaff *) staff_iterator->data)->voicecontrol & DENEMO_PRIMARY)
    {
      gui->si->top_staff--;
      staff_iterator = staff_iterator->prev;
    }
  set_bottom_staff (gui);
  update_vscrollbar (gui);
}



static void center_viewport(void) {
      Denemo.gui->si->leftmeasurenum = Denemo.gui->si->currentmeasurenum - (Denemo.gui->si->rightmeasurenum-Denemo.gui->si->leftmeasurenum)/2;
      if(Denemo.gui->si->leftmeasurenum<1) Denemo.gui->si->leftmeasurenum = 1;
}

void page_viewport(void) {
  gdouble value, upper;
  GtkAdjustment *adj = GTK_ADJUSTMENT(Denemo.hadjustment);
  // g_print("%d %d\n", Denemo.gui->si->leftmeasurenum, Denemo.gui->si->rightmeasurenum);
  gint amount = (Denemo.gui->si->rightmeasurenum-Denemo.gui->si->leftmeasurenum + 1);
  value = gtk_adjustment_get_value (adj);
  upper = gtk_adjustment_get_upper(adj);
  if( value + amount < upper ) {
    gtk_adjustment_set_value(adj, value + amount);
  } else
    gtk_adjustment_set_value(adj, upper -1);
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
 * if exists, making it the leftmost measure visible, extending selection or not
 *
 */
gboolean
goto_currentmeasurenum (DenemoGUI * gui, gint dest, gboolean extend_selection)
{
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->si->measurewidths))))
    {
      //gui->si->leftmeasurenum = dest;
      gui->si->currentmeasurenum = dest;
      if((dest < gui->si->leftmeasurenum) ||
	 (dest > gui->si->rightmeasurenum))
	 center_viewport();
      setcurrents (gui->si);
      if(extend_selection)
	calcmarkboundaries (gui->si);
      set_rightmeasurenum (gui->si);
      find_leftmost_allcontexts (gui->si);
      update_hscrollbar (gui);
      gtk_widget_queue_draw (Denemo.scorearea);
      return TRUE;
    }
  return FALSE;
}

/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it the leftmost measure visible
 *
 */
gboolean
set_currentmeasurenum (DenemoGUI * gui, gint dest)
{
  return goto_currentmeasurenum (gui, dest, FALSE);
}

/**
 * Sets the si->currentmeasurenum to the given value
 * if exists, making it the leftmost measure visible, leaving selection
 *
 */
gboolean
moveto_currentmeasurenum (DenemoGUI * gui, gint dest)
{
  return goto_currentmeasurenum (gui, dest, FALSE);
}




/**
 * Sets the si->currentstaffnum to the given value
 * if it exists, extending selection if extend_selection
 *
 */
gboolean
goto_currentstaffnum (DenemoGUI * gui, gint dest, gboolean extend_selection)
{
  if ((dest > 0) && (dest <= (gint) (g_list_length (gui->si->thescore))))
    {
      gui->si->currentstaffnum = dest;
      gui->si->currentstaff =
	g_list_nth (gui->si->thescore, gui->si->currentstaffnum - 1);
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      if(extend_selection)
	calcmarkboundaries (gui->si);
      find_leftmost_allcontexts (gui->si);
      update_vscrollbar (gui);
      gtk_widget_queue_draw (Denemo.scorearea);
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
  return goto_currentstaffnum(gui, dest, TRUE);
}

gboolean
moveto_currentstaffnum (DenemoGUI * gui, gint dest)
{
  return goto_currentstaffnum(gui, dest, FALSE);
}

/**
 * Scroll the score vertically
 *
 */
void
vertical_scroll (GtkAdjustment * adjust, gpointer dummy)
{
  DenemoGUI *gui = Denemo.gui;
  gint dest;
  gdouble value = gtk_adjustment_get_value(adjust);
  if ((dest = (gint) (value + 0.5)) != gui->si->top_staff)
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
	  if(gui->si->markstaffnum)
	    calcmarkboundaries (gui->si);
	}
      else if (gui->si->currentstaffnum < gui->si->top_staff)
	{
	  gui->si->currentstaffnum = gui->si->top_staff;
	  gui->si->currentstaff =
	    g_list_nth (gui->si->thescore, gui->si->top_staff - 1);
	  setcurrentprimarystaff (gui->si);
	  setcurrents (gui->si);
	  if(gui->si->markstaffnum)
	    calcmarkboundaries (gui->si);
	}
      gtk_widget_queue_draw (Denemo.scorearea);
    }
  update_vscrollbar (gui);
}

/**
 * Scroll score horizontally
 *
 */
static void
h_scroll (gdouble value, DenemoGUI * gui)
{
  gint dest;
  if ((dest = (gint) (value + 0.5)) != gui->si->leftmeasurenum)
    {
			set_transition(dest-gui->si->leftmeasurenum);
      gui->si->leftmeasurenum = dest;
      set_rightmeasurenum (gui->si);
      if (gui->si->currentmeasurenum > gui->si->rightmeasurenum)
				{
					gui->si->currentmeasurenum = gui->si->rightmeasurenum;

				} else if (gui->si->currentmeasurenum < gui->si->leftmeasurenum)
				{
					gui->si->currentmeasurenum = gui->si->leftmeasurenum;

				}
      find_leftmost_allcontexts (gui->si);
      setcurrents (gui->si);
      gtk_widget_queue_draw (Denemo.scorearea);
    }
  update_hscrollbar (gui);
}

void
horizontal_scroll (GtkAdjustment * adjust, gpointer dummy)
{
  DenemoGUI *gui = Denemo.gui;
  gdouble value = gtk_adjustment_get_value(adjust);
  h_scroll (value, gui);
}
void scroll_left(void) {
  if(Denemo.gui->si->leftmeasurenum>1)
    h_scroll(Denemo.gui->si->leftmeasurenum-1.0, Denemo.gui);
}

void scroll_right (void) {
  if(Denemo.gui->si->leftmeasurenum < g_list_length (Denemo.gui->si->measurewidths))
    h_scroll(Denemo.gui->si->leftmeasurenum+1.0, Denemo.gui);
}
