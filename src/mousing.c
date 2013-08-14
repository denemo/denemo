/* mousing.c
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
#include "lilydirectives.h"
#include "lyric.h"
#include "moveviewport.h"
#include "mousing.h"
#include "fluid.h"
#include "draw.h"
#include "view.h"
#include "audiointerface.h"

static gboolean lh_down;
static gdouble last_event_x;
/**
 * Get the mid_c_offset of an object or click from its height relative
 * to the top of the staff.  
 */
gint
offset_from_height (gdouble height, enum clefs clef)
{
  /* Offset from the top of the staff, in half-tones.  */
  gint half_tone_offset = ((gint) (height / HALF_LINE_SPACE + ((height > 0) ? 0.5 : -0.5)));

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
    case DENEMO_F_8_CLEF:
      R (-9);
      break;
    case DENEMO_TENOR_CLEF:
      R (2);
      break;
    case DENEMO_SOPRANO_CLEF:
      R (8);
      break;
    case DENEMO_FRENCH_CLEF:
      R (12);
      break;
      //when adding clefs get utils.c calculateheight() function correct first, then do this
    default:
      R (0);
      break;
    }
#undef R
  return 0;
}


static gdouble
get_click_height (DenemoGUI * gui, gdouble y)
{
  gdouble click_height;
  gint staffs_from_top;
  staffs_from_top = 0;
  GList *curstaff;
  DenemoStaff *staff;
  gint extra_space = 0;
  gint space_below = 0;
  curstaff = g_list_nth (gui->si->thescore, gui->si->top_staff - 1);

  if (!(((DenemoStaff *) (gui->si->currentstaff->data))->voicecontrol & DENEMO_PRIMARY))
    staffs_from_top--;

  for (curstaff = g_list_nth (gui->si->thescore, gui->si->top_staff - 1); curstaff; curstaff = curstaff->next)
    {
      //g_print("before extra space %d\n", extra_space);
      staff = (DenemoStaff *) curstaff->data;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        extra_space += (staff->space_above) + space_below;
      if (curstaff == gui->si->currentstaff)
        break;

      if (staff->voicecontrol & DENEMO_PRIMARY)
        {

          space_below = 0;
          staffs_from_top++;
        }
      space_below = MAX (space_below, ((staff->space_below) + (staff->verses ? LYRICS_HEIGHT : 0)));
      //g_print("after extra space %d space_below %d\n", extra_space, space_below);
    }

  click_height = y - (gui->si->staffspace * staffs_from_top + gui->si->staffspace / 4 + extra_space);
  //  g_print("top staff is %d total %d staffs from top is %d click %f\n", gui->si->top_staff, extra_space, staffs_from_top, click_height);

  return click_height;



}

/**
 * Set the cursor's y position from a mouse click
 *
 */
void
set_cursor_y_from_click (DenemoGUI * gui, gdouble y)
{
  /* Click height relative to the top of the staff.  */
  gdouble click_height = get_click_height (gui, y);
  gui->si->cursor_y = offset_from_height (click_height, (enum clefs) gui->si->cursorclef);
  gui->si->staffletter_y = offsettonumber (gui->si->cursor_y);
}

struct placement_info
{
  gint staff_number, measure_number, cursor_x;
  staffnode *the_staff;
  measurenode *the_measure;
  objnode *the_obj;
  gboolean nextmeasure;
  gboolean offend;              //TRUE when the user has clicked beyond the last note, taking you into appending
};

/* find the primary staff of the current staff, return its staffnum */
static gint
primary_staff (DenemoScore * si)
{
  GList *curstaff;
  for (curstaff = si->currentstaff; curstaff && !(((DenemoStaff *) curstaff->data)->voicecontrol & DENEMO_PRIMARY); curstaff = curstaff->prev)
    ;                           //do nothing
  //g_print("The position is %d\n", 1+g_list_position(si->thescore, curstaff));
  return 1 + g_list_position (si->thescore, curstaff);
}


/* find which staff in si the height y lies in, return the staff number (not counting non-primary staffs ie voices) */

static gint
staff_at (gint y, DenemoScore * si)
{
  GList *curstaff;
  gint space = 0;
  gint count;
  gint ret;
  for (curstaff = g_list_nth (si->thescore, si->top_staff - 1), count = 0; curstaff && y > space; curstaff = curstaff->next)
    {
      DenemoStaff *staff = (DenemoStaff *) curstaff->data;

      count++;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        space += (staff)->space_above + (staff)->space_below + si->staffspace;
      //g_print("y %d and space %d count = %d\n",y,space, count);
    }

  if (y <= 1)
    ret = 1;
  ret = count + si->top_staff - 1;
  if (ret == primary_staff (si))
    ret = si->currentstaffnum;
  return ret > 0 ? ret : 1;
}

/**
 * Gets the position from the clicked position
 *
 */
static void
get_placement_from_coordinates (struct placement_info *pi, gdouble x, gdouble y, gint leftmeasurenum, gint rightmeasurenum, gint scale)
{
  DenemoScore *si = Denemo.gui->si;
  GList *mwidthiterator = g_list_nth (si->measurewidths,
                                      leftmeasurenum - 1);
  objnode *obj_iterator;
  gint x_to_explain = (gint) (x);
  pi->offend = FALSE;
  pi->the_obj = NULL;
  if (mwidthiterator == NULL)
    {
      g_critical ("Array of measurewidths too small for leftmeasure %d\n", leftmeasurenum);
      return;
    }
  pi->staff_number = staff_at ((gint) y, si);
  //g_print("L/R %d %d got staff number %d\n", leftmeasurenum, rightmeasurenum, pi->staff_number); 
  pi->measure_number = leftmeasurenum;
  if (scale)
    x_to_explain = (x_to_explain * scale) / 100;
  x_to_explain -= (KEY_MARGIN + si->maxkeywidth + SPACE_FOR_TIME);

  //g_print("Explaining %d\n", x_to_explain);
  while (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data) && pi->measure_number < rightmeasurenum)
    {
      x_to_explain -= (GPOINTER_TO_INT (mwidthiterator->data) + SPACE_FOR_BARLINE);
      mwidthiterator = mwidthiterator->next;
      pi->measure_number++;
    }
  //g_print("got to measure %d\n", pi->measure_number);
  pi->nextmeasure = ((si->system_height > 0.5 || x_to_explain > GPOINTER_TO_INT (mwidthiterator->data)) && pi->measure_number >= rightmeasurenum);

  pi->the_staff = g_list_nth (si->thescore, pi->staff_number - 1);
  pi->the_measure = nth_measure_node_in_staff (pi->the_staff, pi->measure_number - 1);
  if (pi->the_measure != NULL)
    {                           /*check to make sure user did not click on empty space */
      obj_iterator = (objnode *) pi->the_measure->data;
      pi->cursor_x = 0;
      pi->the_obj = NULL;
      if (obj_iterator)
        {
          DenemoObject *current, *next;

          for (; obj_iterator->next; obj_iterator = obj_iterator->next, pi->cursor_x++)
            {
              current = (DenemoObject *) obj_iterator->data;
              next = (DenemoObject *) obj_iterator->next->data;
              /* This comparison neatly takes care of two possibilities:

                 1) That the click was to the left of current, or

                 2) That the click was between current and next, but
                 closer to current.

                 Do the math - it really does work out.  */

              //???modify current->x by gx where graphic_override is set????
              if (x_to_explain - (current->x + current->minpixelsalloted) < next->x - x_to_explain)
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
              if ((x_to_explain > current->x + current->minpixelsalloted) || (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data) - current->minpixelsalloted / 3))   //if closer to barline than object center
                pi->offend = TRUE, pi->cursor_x++;
            }
        }
      //g_print("got to cursor x %d\n", pi->cursor_x);
    }
}


void
assign_cursor (guint state, guint cursor_num)
{
  guint *cursor_state = g_new (guint, 1);
  *cursor_state = state;
  //g_print("Storing cursor %x for state %x in hash table %p\n", cursor_num, state, Denemo.map->cursors );  
  GdkCursor *cursor = gdk_cursor_new (cursor_num);
  g_assert (cursor);
  g_hash_table_insert (Denemo.map->cursors, cursor_state, cursor);
}

void
set_cursor_for (guint state)
{
  gint the_state = state;
  GdkCursor *cursor = g_hash_table_lookup (Denemo.map->cursors, &the_state);
  //g_print("looked up %x in %p got cursor %p which is number %d\n", state, Denemo.map->cursors,  cursor, cursor?cursor->type:-1);
  if (cursor)
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), cursor);
  else
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR));       //FIXME? does this take time/hog memory
}


/* appends the name(s) for modifier mod to ret->str */

void
append_modifier_name (GString * ret, gint mod)
{
  gint i;
  static const gchar *names[] = {
    "Shift",
    "CapsLock",
    "Control",
    "Alt",
    "NumLock",
    "MOD3",
    "Penguin",
    "AltGr"
  };
  for (i = 0; i < DENEMO_NUMBER_MODIFIERS; i++)
    if ((1 << i) & mod)
      g_string_append_printf (ret, "%s%s", "-", names[i]);
  g_string_append_printf (ret, "%s", mod ? "" : "");
}

/* returns a newly allocated GString containing a shortcut name */
GString *
mouse_shortcut_name (gint mod, mouse_gesture gesture, gboolean left)
{

  GString *ret = g_string_new ((gesture == GESTURE_PRESS) ? (left ? "PrsL" : "PrsR") : ((gesture == GESTURE_RELEASE) ? (left ? "RlsL" : "RlsR") : (left ? "MveL" : "MveR")));

  append_modifier_name (ret, mod);
  //g_print("Returning %s for mod %d\n", ret->str, mod);
  return ret;

}


/* perform an action for mouse-click stored with shortcuts */
static void
perform_command (gint modnum, mouse_gesture press, gboolean left)
{
  GString *modname = mouse_shortcut_name (modnum, press, left);
  gint command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
  if (press != GESTURE_MOVE)
    {
      if (!Denemo.prefs.strictshortcuts)
        {
          if (command_idx < 0)
            {
              g_string_free (modname, TRUE);
              modname = mouse_shortcut_name (modnum & (~GDK_LOCK_MASK /*CapsLock */ ), press, left);
              command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
            }
          if (command_idx < 0)
            {
              g_string_free (modname, TRUE);
              modname = mouse_shortcut_name (modnum & (~GDK_MOD2_MASK /*NumLock */ ), press, left);
              command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
            }
          if (command_idx < 0)
            {
              g_string_free (modname, TRUE);
              modname = mouse_shortcut_name (modnum & (~(GDK_LOCK_MASK | GDK_MOD2_MASK)), press, left);
              command_idx = lookup_command_for_keybinding_name (Denemo.map, modname->str);
            }
        }
    }


  if (command_idx >= 0)
    {

      if (Denemo.prefs.learning)
        KeyPlusMouseGestureShow(modname->str, command_idx);
      
      execute_callback_from_idx (Denemo.map, command_idx);
      displayhelper (Denemo.gui);
    }
  g_string_free (modname, TRUE);
}

static gboolean selecting = FALSE;
static gboolean dragging_separator = FALSE;
static gboolean dragging_audio = FALSE;
static gboolean dragging_tempo = FALSE;


static gboolean
change_staff (DenemoScore * si, gint num, GList * staff)
{
  if (si->currentstaffnum == num)
    return FALSE;
  hide_lyrics ();
  si->currentstaffnum = num;
  si->currentstaff = staff;
  show_lyrics ();
  return TRUE;
}

static void
transform_coords (double *x, double *y)
{
  DenemoGUI *gui = Denemo.gui;

  gint application_height = get_widget_height (Denemo.scorearea);
  gint line_height = application_height * gui->si->system_height;
  gint line_num = ((int) *y) / line_height;
  *y -= line_num * line_height;
  *x /= gui->si->zoom;
  *y /= gui->si->zoom;
  // *x += ((double)line_num * gui->si->widthtoworkwith / ((int)(1/gui->si->system_height))) - 1.0* (line_num?(double)LEFT_MARGIN:0.0);
}


gint
scorearea_leave_event (GtkWidget * widget, GdkEventCrossing * event)
{
  gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR)); //FIXME? does this take time/hog memory
  return FALSE;                 //allow other handlers (specifically the pitch entry one)
}

gint
scorearea_enter_event (GtkWidget * widget, GdkEventCrossing * event)
{
//g_print("start the enter with ks = %x and state %x\n", Denemo.keyboard_state, event->state);
  if (event->state & GDK_CONTROL_MASK)
    Denemo.keyboard_state |= GDK_CONTROL_MASK;
  else
    Denemo.keyboard_state &= ~GDK_CONTROL_MASK;

  if (event->state & GDK_SHIFT_MASK)
    Denemo.keyboard_state |= GDK_SHIFT_MASK;
  else
    Denemo.keyboard_state &= ~GDK_SHIFT_MASK;
#if 0
//perhaps it would be better to clear Denemo.keyboard_state on focus out event???
  if (event->state & GDK_MOD1_MASK)
    Denemo.keyboard_state |= GDK_MOD1_MASK;
  else
    Denemo.keyboard_state &= ~(CHORD_MASK | GDK_MOD1_MASK);
#endif
//      g_print("end the enter with ks %x (values  %x %x)\n", event->state, ~GDK_CONTROL_MASK, Denemo.keyboard_state & (~GDK_CONTROL_MASK) );
  set_midi_in_status ();
  return FALSE;                 //allow other handlers 
}

/**
 * Mouse motion callback 
 *
 */
gint
scorearea_motion_notify (GtkWidget * widget, GdkEventButton * event)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui == NULL || gui->si == NULL)
    return FALSE;
  if (Denemo.scorearea == NULL)
    return FALSE;
  gint allocated_height = get_widget_height (Denemo.scorearea);
  gint line_height = allocated_height * gui->si->system_height;
  
  if (event->y < 0)
    event->y = 0.0;
  gint line_num = ((int) event->y) / line_height;

  if(gui->si->audio && dragging_audio)
	{		
		gui->si->audio->leadin -= 500*(event->x_root - last_event_x)/gui->si->zoom;//g_print("%d %d => %d\n", (int)(10*last_event_x), (int)(10*event->x_root), (int)(10*last_event_x) - (int)(10*event->x_root));
		last_event_x = event->x_root;
		update_leadin_widget ( gui->si->audio->leadin/(double)gui->si->audio->samplerate);
		return TRUE; 
	}
  if(gui->si->audio && dragging_tempo)
	{		
		gdouble change = (event->x_root - last_event_x)/gui->si->zoom;
		last_event_x = event->x_root;
		struct placement_info pi;
		get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
		change /= pi.measure_number;
		update_tempo_widget ( change);
		return TRUE; 
	}
#define DENEMO_MINIMUM_SYSTEM_HEIGHT (0.01)


  if (dragging_separator)
    {
      gui->si->system_height = event->y / get_widget_height (Denemo.scorearea);
      if (gui->si->system_height < DENEMO_MINIMUM_SYSTEM_HEIGHT)
        gui->si->system_height = DENEMO_MINIMUM_SYSTEM_HEIGHT;
      if (gui->si->system_height > 1.0)
        gui->si->system_height = 1.0;
      scorearea_configure_event (Denemo.scorearea, NULL);
      gtk_widget_queue_draw (Denemo.scorearea);
      return TRUE;
    }

  if (line_height - ((int) event->y - 8) % line_height < 12)
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_SB_V_DOUBLE_ARROW));
  else
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR));       //FIXME? does this take time/hog memory

  transform_coords (&event->x, &event->y);
  //  g_print("Marked %d\n", gui->si->markstaffnum);
  if (gui->lefts[line_num] == 0)
    return TRUE;




  if (lh_down || (selecting && gui->si->markstaffnum))
    {
      struct placement_info pi;
      pi.the_staff = NULL;
      if (event->y < 0)
        get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
      else
        get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
      if (pi.the_staff == NULL)
        return TRUE;            //could not place the cursor
      if (pi.the_measure != NULL)
        {                       /*don't place cursor in a place that is not there */
          change_staff (gui->si, pi.staff_number, pi.the_staff);
          gui->si->currentmeasurenum = pi.measure_number;
          gui->si->currentmeasure = pi.the_measure;
          gui->si->currentobject = pi.the_obj;
          gui->si->cursor_x = pi.cursor_x;
          gui->si->cursor_appending = (gui->si->cursor_x == (gint) (g_list_length ((objnode *) gui->si->currentmeasure->data)));

          set_cursor_y_from_click (gui, event->y);
          if (lh_down & !selecting)
            {
              set_mark (gui);
              selecting = TRUE;
            }
          calcmarkboundaries (gui->si);
          if (event->state & (GDK_BUTTON1_MASK | GDK_BUTTON2_MASK | GDK_BUTTON3_MASK))
            perform_command (event->state, GESTURE_MOVE, event->state & GDK_BUTTON1_MASK);

          /* redraw to show new cursor position  */
          gtk_widget_queue_draw (Denemo.scorearea);
        }
    }

  if (Denemo.gui->midi_destination & MIDICONDUCT)
    {
      advance_time (0.01);
      return TRUE;
    }
  return TRUE;
}


/**
 * Mouse button press callback 
 *
 */
gint
scorearea_button_press (GtkWidget * widget, GdkEventButton * event)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui == NULL || gui->si == NULL)
    return FALSE;
  gboolean left = (event->button != 3);
  //if the cursor is at a system separator start dragging it
  gint allocated_height = get_widget_height (Denemo.scorearea);
  gint line_height = allocated_height * gui->si->system_height;
  gint line_num = ((int) event->y) / line_height;
  last_event_x = event->x_root;
  //g_print("diff %d\n", line_height - ((int)event->y)%line_height);
  if (dragging_separator == FALSE)
    if (line_height - ((int) event->y - 8) % line_height < 12)
      {
        if (Denemo.prefs.learning)
          MouseGestureShow(_("Dragging line separator."), _("This will allow the display to show more music, split into lines. The typeset score is not affected."),
            MouseGesture);
        dragging_separator = TRUE;
        return TRUE;
      }
  dragging_separator = FALSE;
  
  if(gui->si->audio)
	{
	 // g_print("audio %f %f\n", event->x, event->y);


	  if(event->y < 20*gui->si->zoom /* see draw.c for this value, the note onsets are drawn in the top 20 pixels */)
		{
			if (event->type==GDK_2BUTTON_PRESS) 
				{	
					gui->si->marked_onset_position = (gint)event->x/gui->si->zoom;//g_print("marked %d\n", gui->si->marked_onset_position);
					gtk_widget_queue_draw(Denemo.scorearea);
					return TRUE;
				} else 
				{
					gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (left?GDK_SB_H_DOUBLE_ARROW:GDK_X_CURSOR));
					left? (dragging_audio = TRUE) : (dragging_tempo = TRUE);
					return TRUE;
				}
		}
	  
	}
  
  
  //g_print("before %f %f\n", event->x, event->y);
  transform_coords (&event->x, &event->y);
  //g_print("after %f %f\n", event->x, event->y);

  
  gtk_widget_grab_focus (widget);
  gint key = gui->si->maxkeywidth;
  gint cmajor = key ? 0 : 5;    //allow some area for keysig in C-major

  if (gui->lefts[line_num] == 0)
    return TRUE;                //On an empty system at the bottom where there is not enough room to draw another staff.

  struct placement_info pi;
  pi.the_staff = NULL;
  if (event->y < 0)
    get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
  else
    get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
  if (pi.the_staff == NULL)
    return TRUE;                //could not place the cursor
  change_staff (gui->si, pi.staff_number, pi.the_staff);


  if (left && (gui->si->leftmeasurenum > 1) && (event->x < KEY_MARGIN + SPACE_FOR_TIME + key) && (event->x > LEFT_MARGIN))
    {
      if (Denemo.prefs.learning)
        MouseGestureShow(_("Press Left."), _("This moved the cursor to the measure offscreen left. The display is shifted to place that measure on screen."),
          MouseGesture);
      moveto_currentmeasurenum (gui, gui->si->leftmeasurenum - 1);
      write_status (gui);
      gtk_widget_queue_draw (Denemo.scorearea);
      return TRUE;
    }
  else if (pi.nextmeasure)
    {
      if ((pi.the_obj==NULL) || ((pi.the_obj->next == NULL) && (pi.offend)))//crashed here with the_obj 0x131 !!!
        {
          if ((gui->si->currentmeasurenum != gui->si->rightmeasurenum) &&
                (!moveto_currentmeasurenum (gui, gui->si->rightmeasurenum + 1)))
              moveto_currentmeasurenum (gui, gui->si->rightmeasurenum);
          else if ((gui->si->cursor_appending) &&
                (!moveto_currentmeasurenum (gui, gui->si->rightmeasurenum + 1)))
              moveto_currentmeasurenum (gui, gui->si->rightmeasurenum);


          

          if (gui->si->currentmeasurenum != gui->si->rightmeasurenum) {
            if (Denemo.prefs.learning)
              MouseGestureShow(_("Press Left."), _("This moved the cursor to the measure off-screen right. The display is shifted to move the cursor to the middle."),
                MouseGesture);
          write_status (gui);
          return TRUE;
        }
        }
    }


  if (pi.the_measure != NULL)
    {                           /*don't place cursor in a place that is not there */
      //gui->si->currentstaffnum = pi.staff_number;
      //gui->si->currentstaff = pi.the_staff;
      gui->si->currentmeasurenum = pi.measure_number;
      gui->si->currentmeasure = pi.the_measure;
      gui->si->currentobject = pi.the_obj;
      gui->si->cursor_x = pi.cursor_x;
      gui->si->cursor_appending = (gui->si->cursor_x == (gint) (g_list_length ((objnode *) gui->si->currentmeasure->data)));
      set_cursor_y_from_click (gui, event->y);
      if (event->type==GDK_2BUTTON_PRESS) 
				{
          if (Denemo.prefs.learning)
            MouseGestureShow(_("Double Click."), _("This gives information about the object at the cursor. Click on a notehead for information about a note in a chord."),
              MouseGesture);
					display_current_object();
					return TRUE;
				}
			else 
				{
          if (Denemo.prefs.learning)
            MouseGestureShow(_("Press Left."), _("This moved the cursor to the object position clicked. The cursor height becomes the clicked point."),
              MouseGesture);
					write_status (gui);
				}
    }


  gint offset = (gint) get_click_height (gui, event->y);
  if ((((DenemoStaff *) gui->si->currentstaff->data)->voicecontrol == DENEMO_PRIMARY) && (gui->si->leftmeasurenum == 1) && (event->x > LEFT_MARGIN))
    {
      if (event->x < KEY_MARGIN - cmajor)
        {
          if (Denemo.prefs.learning)
            MouseGestureShow(_("Left on initial Clef."), _("This pops up the initial clef menu."),
              MouseGesture);
          popup_menu ("/InitialClefEditPopup");
          return TRUE;
        }
      else if (event->x < KEY_MARGIN + key + cmajor)
        {
          if (left)
            {
              if (offset > 0 && (offset < STAFF_HEIGHT / 2))
                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Left Click on blue."), _("This adds one sharp."),
                      MouseGesture);
                  call_out_to_guile ("(d-SharpenInitialKeysigs)");
                }
              else if (offset > 0 && (offset < STAFF_HEIGHT))
                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Left Click on red."), _("This adds one flat."),
                      MouseGesture);                                  
                  call_out_to_guile ("(d-FlattenInitialKeysigs)");
                }
            }
          else
            {
              if (Denemo.prefs.learning)
                MouseGestureShow(_("Right Click on key."), _("This pops up the key signature menu."),
                    MouseGesture);  
              popup_menu ("/InitialKeyEditPopup");
            }
          return TRUE;
        }
      else if (event->x < KEY_MARGIN + SPACE_FOR_TIME + key)
        {
          if (Denemo.prefs.learning)
            MouseGestureShow(_("Click on Time."), _("This pops up the time signature menu."),
                    MouseGesture); 
          popup_menu ("/InitialTimeEditPopup");
          return TRUE;
        }
    }

  if (event->x < LEFT_MARGIN)
    {
      if (pi.staff_number == gui->si->currentstaffnum)
        {
          gint offset = (gint) get_click_height (gui, event->y);
          if (offset < STAFF_HEIGHT / 2)
            {
              if (((DenemoStaff *) gui->si->currentstaff->data)->staff_directives)
                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Click on Staff Directives."), _("This pops up the staff directives menu for editing"),
                      MouseGesture);                  
                  gtk_menu_popup (((DenemoStaff *) gui->si->currentstaff->data)->staffmenu, NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
                }
              return TRUE;
            }
          else if (((DenemoStaff *) gui->si->currentstaff->data)->voice_directives)
            {
              if (Denemo.prefs.learning)
                MouseGestureShow(_("Click on Voice Directives."), _("This pops up the voice directives menu for editing"),
                    MouseGesture);  
              gtk_menu_popup (((DenemoStaff *) gui->si->currentstaff->data)->voicemenu, NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
              return TRUE;
            }
        }
    }

  if (left)
    {
      if (!(GDK_SHIFT_MASK & event->state))
        gui->si->markstaffnum = 0;
      lh_down = TRUE;
    }
  else
    {
      if (gui->si->cursor_appending)
        {
          if (Denemo.prefs.learning)
            MouseGestureShow(_("Right Click Appending."), _("This pops up the append menu"),
                    MouseGesture);  
          
          popup_menu ("/NoteAppendPopup");
          return TRUE;
        }
    }
  set_cursor_for (event->state | (left ? GDK_BUTTON1_MASK : GDK_BUTTON3_MASK));



  
  //displayhelper(Denemo.gui);
  draw_score(NULL);//this is needed to refresh cached values such as the prevailing time signature, before the command is invoked

  perform_command (event->state | (left ? GDK_BUTTON1_MASK : GDK_BUTTON3_MASK), GESTURE_PRESS, left);

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
  if (gui == NULL || gui->si == NULL)
    return FALSE;
  gboolean left = (event->button != 3);
  if(gui->si->audio && (dragging_tempo || dragging_audio))
	{		
			dragging_tempo = dragging_audio = FALSE;
			gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), gdk_cursor_new (GDK_LEFT_PTR));       //FIXME? does this take time/hog memory
			return TRUE; 
	}
  if (dragging_separator)
    {
      if (Denemo.prefs.learning)
        MouseGestureShow(_("Dragged line separator."), _("This allows the display to show more music, split into lines. The typeset score is not affected."),
          MouseGesture);
      dragging_separator = FALSE;
      return TRUE;
    }

  //g_signal_handlers_block_by_func(Denemo.scorearea, G_CALLBACK (scorearea_motion_notify), gui); 
  if (left)
    lh_down = FALSE;
  selecting = FALSE;
  set_cursor_for (event->state & DENEMO_MODIFIER_MASK);
  transform_coords (&event->x, &event->y);
  set_cursor_y_from_click (gui, event->y);
  gtk_widget_queue_draw(Denemo.scorearea);
  perform_command (event->state, GESTURE_RELEASE, left);

  return TRUE;
}

gint
scorearea_scroll_event (GtkWidget * widget, GdkEventScroll * event)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui == NULL || gui->si == NULL)
    return FALSE;
  switch (event->direction)
    {
      DenemoScriptParam param;
    case GDK_SCROLL_UP:
      if (event->state & GDK_CONTROL_MASK)
        {
          if (Denemo.prefs.learning) {
            gint command_idx = lookup_command_from_name(Denemo.map, "ZoomIn");
            KeyStrokeShow (_("Ctrl + Mouse Wheel Up"), command_idx, TRUE);
          }
          Denemo.gui->si->zoom *= 1.1;
          scorearea_configure_event (Denemo.scorearea, NULL);
        }
      else if (event->state & GDK_SHIFT_MASK)
        {
          gint command_idx = lookup_command_from_name(Denemo.map, "MoveToMeasureLeft");
          if (Denemo.prefs.learning) {
            KeyStrokeShow (_("Shift + Mouse Wheel Up"), command_idx, TRUE);
          }
          execute_callback_from_idx(Denemo.map, command_idx);//scroll_left ();

        }
      else
        {
          if (Denemo.prefs.learning) {
            gint command_idx = lookup_command_from_name(Denemo.map, "MoveToStaffUp");
            KeyStrokeShow (_("Unshifted + Mouse Wheel Up"), command_idx, TRUE);
          }
          movetostaffup (&param);
          if (!param.status) {
            DenemoStaff *thestaff = (DenemoStaff*)(Denemo.gui->si->currentstaff->data);
            if(thestaff->space_above < MAXEXTRASPACE)
              {
                thestaff->space_above++;
                g_debug ("Increasing the height of the top staff");
              } 
          }
        }
      break;
    case GDK_SCROLL_DOWN:
      if (event->state & GDK_CONTROL_MASK)
        {
          if (Denemo.prefs.learning) {
            gint command_idx = lookup_command_from_name(Denemo.map, "ZoomOut");
            KeyStrokeShow (_("Ctrl + Mouse Wheel Down"), command_idx, TRUE);
          }
          Denemo.gui->si->zoom /= 1.1;
          if (Denemo.gui->si->zoom < 0.01)
            Denemo.gui->si->zoom = 0.01;
          scorearea_configure_event (Denemo.scorearea, NULL);
          //displayhelper(gui);
        }
      else if (event->state & GDK_SHIFT_MASK)
        {
          gint command_idx = lookup_command_from_name(Denemo.map, "MoveToMeasureRight");
          if (Denemo.prefs.learning) {
            KeyStrokeShow (_("Shift + Mouse Wheel Down"), command_idx, TRUE);
          }
          execute_callback_from_idx(Denemo.map, command_idx);//scroll_right ();
        }
      else
        {
          if (Denemo.prefs.learning) {
            gint command_idx = lookup_command_from_name(Denemo.map, "MoveToStaffDown");
            KeyStrokeShow (_("Unshifted + Mouse Wheel Down"), command_idx, TRUE);
          }
          movetostaffdown (&param);
          if (!param.status) {
            warningmessage ("This is the bottom staff");
           // DenemoStaff *thestaff = (DenemoStaff*)(Denemo.gui->si->currentstaff->data);
           // thestaff->space_below++; //This doesn't help, because the viewport does not change.
           // warningmessage ("Increasing the space below the bottom staff");
           //move_viewport_down(Denemo.gui);
          }
        }
      break;
    case GDK_SCROLL_LEFT:
      movetomeasureleft (&param);
      if (!param.status)
        warningmessage ("This is the first measure");
      break;
    case GDK_SCROLL_RIGHT:
      movetomeasureright (&param);
      if (!param.status)
        warningmessage ("This is the last measure");
      break;

    default:
      break;
    }
  displayhelper (Denemo.gui);
  return FALSE;
}
