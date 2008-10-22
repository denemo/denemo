/* mousing.cpp
 * callback functions for handling mouse clicks, drags, etc.
 *
 *  for Denemo, a gtk+ frontend to GNU Lilypond
 *  (c) 2000-2005 Matthew Hiller
 */

#include "commandfuncs.h"
#include "kbd-custom.h"
#include "staffops.h"
#include "utils.h"
#include "selectops.h"
#include "mousing.h"
/**
 * Get the mid_c_offset of an object or click from its height relative
 * to the top of the staff.  
 */
gint
offset_from_height (gdouble height, enum clefs clef)
{
  /* Offset from the top of the staff, in half-tones.  */
  gint half_tone_offset = ((gint) (height / HALF_LINE_SPACE+((height>0)?0.5:-0.5)));

#define R(x) return x - half_tone_offset

  switch (clef)
    {
    case DENEMO_TREBLE_CLEF:
      R (10);
      break;
    case DENEMO_BASS_CLEF:
      R (-2);
      break;
    case DENEMO_ALTO_CLEF:
      R (4);
      break;
    case DENEMO_G_8_CLEF:
      R (3);
      break;
    case DENEMO_TENOR_CLEF:
      R (2);
      break;
    case DENEMO_SOPRANO_CLEF:
      R (8);
      break;
    }
#undef R
  return 0;
}

/**
 * Set the cursor's y position from a mouse click
 *
 */
void
set_cursor_y_from_click (DenemoGUI * gui, gdouble y)
{
  /* Click height relative to the top of the staff.  */
  gdouble click_height;
  gint staffs_from_top;
  staffs_from_top = 0;
  GList *curstaff;
  DenemoStaff *staff;
  gint extra_space = 0;
  for(  curstaff = g_list_nth(gui->si->thescore,gui->si->top_staff-1) ; curstaff;curstaff=curstaff->next) {
    //    g_print("extra space %d\n", extra_space);
    staff = (DenemoStaff *) curstaff->data;
    if(staff->voicenumber == 1)
      extra_space += (staff->space_above );
    if(curstaff == gui->si->currentstaff)
      break;
    if(staff->voicenumber == 1){
      // FIXME this cannot work, different amounts of space_xxx may be stored in different voices
      extra_space += ((staffs_from_top?staff->space_below:0) + (staff->haslyrics?LYRICS_HEIGHT:0));
      staffs_from_top++;
    }

  }

  click_height =
    y - (gui->si->staffspace * staffs_from_top + gui->si->staffspace / 4 + extra_space);
  //  g_print("top staff is %d total %d staffs from top is %d click %f\n", gui->si->top_staff, extra_space, staffs_from_top, click_height);

  gui->si->cursor_y =
    offset_from_height (click_height, (enum clefs) gui->si->cursorclef);
  gui->si->staffletter_y = offsettonumber (gui->si->cursor_y);
}

struct placement_info
{
  gint staff_number, measure_number, cursor_x;
  staffnode *the_staff;
  measurenode *the_measure;
  objnode *the_obj;
  gboolean nextmeasure;
};

/* find which staff in si the height y lies in, return the staff number (not counting non-primary staffs ie voices) */

static gint staff_at (gint y, DenemoScore *si) {
  GList *curstaff;
  gint space = 0;
  gint count;
  for(curstaff = g_list_nth(si->thescore, si->top_staff-1), count=0; curstaff && y>space;curstaff=curstaff->next) {
    DenemoStaff *staff = (DenemoStaff *) curstaff->data;

    count++;
    if(staff->voicenumber == 1)
      space += (staff)->space_above +
	(staff)->space_below + si->staffspace; 
    //g_print("y %d and space %d count = %d\n",y,space, count);
  } 
  if(y<=1)
    return 1;
  return count+si->top_staff-1;
}

/**
 * Gets the position from the clicked position
 *
 */
void
get_placement_from_coordinates (struct placement_info *pi,
				gdouble x, gdouble y, DenemoScore * si)
{
  GList *mwidthiterator = g_list_nth (si->measurewidths,
				      si->leftmeasurenum - 1);
  objnode *obj_iterator;
  gint x_to_explain = (gint) (x);

  pi->staff_number = staff_at((gint)y, si);
/*   g_print("get staff number %d\n",pi->staff_number); */
  pi->measure_number = si->leftmeasurenum;
  x_to_explain -= (KEY_MARGIN + si->maxkeywidth + SPACE_FOR_TIME);
  while (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data)
	 && pi->measure_number < si->rightmeasurenum)
    {
      x_to_explain -= (GPOINTER_TO_INT (mwidthiterator->data)
		       + SPACE_FOR_BARLINE);
      mwidthiterator = mwidthiterator->next;
      pi->measure_number++;
    }
  pi->nextmeasure = (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data)
		     && pi->measure_number >= si->rightmeasurenum);
    
  pi->the_staff = g_list_nth (si->thescore, pi->staff_number - 1);
  pi->the_measure
    = nth_measure_node_in_staff (pi->the_staff, pi->measure_number - 1);
  if (pi->the_measure != NULL){ /*check to make sure user did not click on empty space*/
	  obj_iterator = (objnode *) pi->the_measure->data;
	  pi->cursor_x = 0;
	  pi->the_obj = NULL;
	  if (obj_iterator)
	    {
	      DenemoObject *current, *next;

	      for (; obj_iterator->next;
		   obj_iterator = obj_iterator->next, pi->cursor_x++)
		{
		  current = (DenemoObject *) obj_iterator->data;
		  next = (DenemoObject *) obj_iterator->next->data;
		  /* This comparison neatly takes care of two possibilities:

		     1) That the click was to the left of current, or

		     2) That the click was between current and next, but
		     closer to current.

		     Do the math - it really does work out.  */
		  if (x_to_explain - (current->x + current->minpixelsalloted)
		      < next->x - x_to_explain)
		    {
		      pi->the_obj = obj_iterator;
		      break;
		    }
		}
	      if (!obj_iterator->next)
		/* That is, we exited the loop normally, not through a break.  */
		{
		  DenemoObject *current = (DenemoObject *) obj_iterator->data;
		  pi->the_obj = obj_iterator;
		  /* The below makes clicking to get the object at the end of
		     a measure (instead of appending after it) require
		     precision.  This may be bad; tweak me if necessary.  */
		  if (x_to_explain > current->x + current->minpixelsalloted)
		    pi->cursor_x++;
		}
	    }
  }
}


/**
 * Mouse motion callback 
 *
 */
gint
scorearea_motion_notify (GtkWidget * widget, GdkEventButton * event)
{
  DenemoGUI *gui = Denemo.gui;
    if (gui->si->markstaffnum, 1){
      struct placement_info pi; 
      if (event->y < 0)
	get_placement_from_coordinates (&pi, event->x, 0, gui->si);
      else
	get_placement_from_coordinates (&pi, event->x, event->y, gui->si);
      if (pi.the_measure != NULL){ /*don't place cursor in a place that is not there*/
	
	gui->si->currentstaffnum = pi.staff_number;
	gui->si->currentstaff = pi.the_staff;
	gui->si->currentmeasurenum = pi.measure_number;
	gui->si->currentmeasure = pi.the_measure;
	gui->si->currentobject = pi.the_obj;
	gui->si->cursor_x = pi.cursor_x;
	gui->si->cursor_appending
	  =
	  (gui->si->cursor_x ==
	   (gint) (g_list_length ((objnode *) gui->si->currentmeasure->data)));
	
	//if(pi.nextmeasure) FIXME, extending selection is tricky, needs at least a timer
	//measureright(gui);
	set_cursor_y_from_click (gui, event->y);
	calcmarkboundaries (gui->si);

	/* redraw to show new cursor position  */
	gtk_widget_queue_draw (gui->scorearea);
      }
    }

}

GString* modifier_name(gint mod, gboolean press, gboolean left) {
  gint i;
  GString *ret = g_string_new(press?(left?"PrsL":"PrsR"):(left?"RlsL":"RlsR"));
  static const gchar* names[]= {
 "Shift"   ,
  "CapsLock"	   ,
  "Control" ,
  "Alt"	   ,
  "NumLock"	 ,
  "MOD3"	   ,
  "Penguin"	   ,
  "AltGr"
  };
  for(i=0;i<DENEMO_NUMBER_MODIFIERS;i++)
    if((1<<i)&mod)
      g_string_append_printf(ret, "%s%s", "-",names[i]);
  g_string_append_printf(ret, "%s", mod?"":"(Plain)");
  //g_print("Returning %s for mod %d\n", ret->str, mod);
  return ret;

}


/* perform an action for mouse-click stored with shortcuts */
static void  
perform_command(gint modnum, gboolean press, gboolean left)
{
  GString *modname = modifier_name(modnum, press, left);
  gint command_idx = lookup_command_for_keybinding_name (Denemo.commands, modname->str);
  if(command_idx>=0) {
    execute_callback_from_idx (Denemo.commands, command_idx);
    displayhelper (Denemo.gui);
  }
  g_string_free(modname, TRUE);
}  

/**
 * Mouse button press callback 
 *
 */
gint
scorearea_button_press (GtkWidget * widget, GdkEventButton * event)
{
DenemoGUI *gui = Denemo.gui;
  struct placement_info pi;
  gboolean left = (event->button != 3);
  if (event->y < 0)
    get_placement_from_coordinates (&pi, event->x, 0, gui->si);
  else
    get_placement_from_coordinates (&pi, event->x, event->y, gui->si);
  if (pi.the_measure != NULL){ /*don't place cursor in a place that is not there*/
	 
    gui->si->currentstaffnum = pi.staff_number;
    gui->si->currentstaff = pi.the_staff;
    gui->si->currentmeasurenum = pi.measure_number;
    gui->si->currentmeasure = pi.the_measure;
    gui->si->currentobject = pi.the_obj;
    gui->si->cursor_x = pi.cursor_x;
    gui->si->cursor_appending
      =
      (gui->si->cursor_x ==
       (gint) (g_list_length ((objnode *) gui->si->currentmeasure->data)));
    set_cursor_y_from_click (gui, event->y);
      if(pi.nextmeasure)
	measureright(gui);
      if(gui->si->markstaffnum)
	unset_mark(gui);
      // else
	set_mark(gui);
      write_status(gui);
      /* Redraw to show new cursor position*/
      gtk_widget_queue_draw (gui->scorearea);
      g_signal_handlers_unblock_by_func(gui->scorearea, G_CALLBACK (scorearea_motion_notify), gui);   
  }

  perform_command(event->state&DENEMO_MODIFIER_MASK, TRUE, left);
  
  return TRUE;
}


/**
 * Mouse button release callback 
 *
 */
gint
scorearea_button_release (GtkWidget * widget, GdkEventButton * event)
{
DenemoGUI *gui = Denemo.gui;
 gboolean left = (event->button != 3);
 g_signal_handlers_block_by_func(gui->scorearea, G_CALLBACK (scorearea_motion_notify), gui); 
 perform_command(event->state&DENEMO_MODIFIER_MASK, FALSE, left);

  return TRUE;
}

