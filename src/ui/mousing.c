/* mousing.c
 * callback functions for handling mouse clicks, drags, etc.
 *
 *  for Denemo, a gtk+ frontend to GNU Lilypond
 *  (c) 2000-2005 Matthew Hiller
 */
#include <math.h>
#include "command/commandfuncs.h"
#include "core/kbd-custom.h"
#include "command/staff.h"
#include "core/utils.h"
#include "command/object.h"
#include "command/select.h"
#include "command/lilydirectives.h"
#include "command/lyric.h"
#include "ui/moveviewport.h"
#include "ui/mousing.h"
#include "audio/fluid.h"
#include "audio/playback.h"
#include "display/draw.h"
#include "core/view.h"
#include "audio/audiointerface.h"
#include "audio/midirecord.h"
#include "export/exportmidi.h"

#define HeightOfRecordingTrack (161) //includes the entire click track at the top

static gboolean lh_down;
static gdouble last_event_x;
static gdouble last_event_y;
static gint drag_display_x;
static gint drag_display_y;
#define DRAG_SCALE (20) //to make the dragging less sensitive
static DenemoDirective *last_directive;
typedef enum DragDirection { DRAG_DIRECTION_NONE = 0, DRAG_DIRECTION_UP, DRAG_DIRECTION_DOWN, DRAG_DIRECTION_LEFT, DRAG_DIRECTION_RIGHT} DragDirection;
static enum DragDirection dragging_outside = DRAG_DIRECTION_NONE; //dragging to left or right outside window.

static gint playback_brightness_shift = 0;
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

//height of cursor relative the the current staff. 0 is the top line of the staff, 40 the bottom line
static gdouble
get_click_height (DenemoProject * gui, gdouble y)
{
  gdouble click_height;
  gint staffs_from_top;
  staffs_from_top = 0;
  GList *curstaff;
  DenemoStaff *staff;
  gint extra_space = 0;
  gint space_below = 0;
  gint i;
  curstaff = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);

  if (!(((DenemoStaff *) (gui->movement->currentstaff->data))->voicecontrol & DENEMO_PRIMARY))
    staffs_from_top--;

  for (i=0, curstaff = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1); curstaff; i++, curstaff = curstaff->next)
    {
      staff = (DenemoStaff *) curstaff->data;
     // g_print("%d from top, staff %d extra space %d (next is %d %d  %d) previous space_below = %d)\n", i, gui->movement->currentstaffnum, extra_space, staff->space_above, staff->space_shorten, staff->space_below, space_below);

      if ((curstaff != Denemo.project->movement->currentstaff) && staff->hidden) continue;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        extra_space += (staff->space_above - (i?staff->space_shorten:0)
          + space_below);
      if (curstaff == gui->movement->currentstaff)
        break;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        {

          space_below = 0;
          staffs_from_top++;
        }
      space_below = MAX (space_below, ((staff->space_below) + (staff->verse_views ? LYRICS_HEIGHT : 0)));
     // g_print("after extra space %d space_below %d\n", extra_space, space_below);
    }

  click_height = y - (gui->movement->staffspace * staffs_from_top + gui->movement->staffspace / 4 + extra_space);
  //g_print("top staff is %d total %d staffs from top is %d click %f\n", gui->movement->top_staff, extra_space, staffs_from_top, click_height);

  return click_height;
}
//height of cursor relative the staff hovered over. 0 is the top line of the staff, STAFF_HEIGHT (ie 40) the bottom line
static gdouble
get_click_height_on_closest_staff (DenemoProject * gui, gdouble y)
{
  gdouble click_height;
  gint staffs_from_top;
  staffs_from_top = 0;
  GList *curstaff;
  DenemoStaff *staff;
  gint extra_space = 0;
  gint space_below = 0;
  gint i;
  curstaff = g_list_nth (gui->movement->thescore, gui->movement->top_staff - 1);

  if (!(((DenemoStaff *) (gui->movement->currentstaff->data))->voicecontrol & DENEMO_PRIMARY))
    staffs_from_top--;

  for (i=0; curstaff; i++, curstaff = curstaff->next)
    {
      staff = (DenemoStaff *) curstaff->data;
      //g_print ("%d from top, staff %d extra space %d (next is %d %d  %d) previous space_below = %d)\n", i, gui->movement->currentstaffnum, extra_space, staff->space_above, staff->space_shorten, staff->space_below, space_below);

      if ((curstaff != Denemo.project->movement->currentstaff) && staff->hidden) continue;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        extra_space += (staff->space_above - (i?staff->space_shorten:0)
          + space_below);
      click_height = y - (gui->movement->staffspace * staffs_from_top + gui->movement->staffspace / 4 + extra_space);
      if (click_height>0 && click_height<STAFF_HEIGHT)
        break;
      if (staff->voicecontrol & DENEMO_PRIMARY)
        {
          space_below = 0;
          staffs_from_top++;
        }
      space_below = MAX (space_below, ((staff->space_below) + (staff->verse_views ? LYRICS_HEIGHT : 0)));
     // g_print("after extra space %d space_below %d\n", extra_space, space_below);
    }
  return click_height;
}
/**
 * Set the cursor's y position from a mouse click
 *
 */
void
set_cursor_y_from_click (DenemoProject * gui, gdouble y)
{
  DenemoStaff *staff = (DenemoStaff*)gui->movement->currentstaff->data;
  gint cursorclef;
  if (gui->movement->currentobject)
    cursorclef = ((DenemoObject *)gui->movement->currentobject->data)->clef->type;
  else
    cursorclef = ((DenemoMeasure *)gui->movement->currentmeasure->data)->clef->type;
  /* Click height relative to the top of the staff.  */
  gdouble click_height = get_click_height (gui, y);
  gint offset = offset_from_height (click_height, (enum clefs) cursorclef);
  //g_print ("Does %d come within range %d %d?\n", offset, staff->range_hi, staff->range_lo);
  if (staff->range)
  {
    if(offset < staff->range_lo)
        offset = staff->range_lo;
    else
      if(offset > staff->range_hi)
        offset = staff->range_hi;
  }
  gui->movement->cursor_y = offset;
  gui->movement->staffletter_y = offsettonumber (gui->movement->cursor_y);
}

struct placement_info
{
  gint staff_number, measure_number, cursor_x;
  staffnode *the_staff;
  measurenode *the_measure;
  objnode *the_obj;
  gboolean offend;              //TRUE when the user has clicked beyond the last note, taking you into appending
};

/* find the primary staff of the current staff, return its staffnum */
static gint
primary_staff (DenemoMovement * si)
{
  GList *curstaff;
  for (curstaff = si->currentstaff; curstaff && !(((DenemoStaff *) curstaff->data)->voicecontrol & DENEMO_PRIMARY); curstaff = curstaff->prev)
    ;                           //do nothing
  //g_debug("The position is %d\n", 1+g_list_position(si->thescore, curstaff));
  return 1 + g_list_position (si->thescore, curstaff);
}


/* find which staff in si the height y lies in, return the staff number (not counting non-primary staffs ie voices) */

static gint
staff_at (gint y, DenemoMovement * si)
{
  GList *curstaff;
  gint space = 0;
  gint count;
  gint ret;
  for (curstaff = g_list_nth (si->thescore, si->top_staff - 1), count = 0; curstaff && y > space; curstaff = curstaff->next)
    {
      DenemoStaff *staff = (DenemoStaff *) curstaff->data;

      count++;
      if ((!((curstaff != Denemo.project->movement->currentstaff) && staff->hidden)) &&
      (staff->voicecontrol & DENEMO_PRIMARY))
        space += staff->space_above + staff->space_below - staff->space_shorten + si->staffspace + (staff->verse_views ? LYRICS_HEIGHT : 0);
      //g_debug("y %d and space %d count = %d\n",y,space, count);
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
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
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
  //g_debug("L/R %d %d got staff number %d\n", leftmeasurenum, rightmeasurenum, pi->staff_number);
  pi->measure_number = leftmeasurenum;
  if (scale)
    x_to_explain = (x_to_explain * scale) / 100;
  x_to_explain -= ((gui->leftmargin+35) + si->maxkeywidth + SPACE_FOR_TIME);

  //g_debug("Explaining %d\n", x_to_explain);
  while (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data) && pi->measure_number < rightmeasurenum)
    {
      x_to_explain -= (GPOINTER_TO_INT (mwidthiterator->data) + SPACE_FOR_BARLINE);
      mwidthiterator = mwidthiterator->next;
      if (mwidthiterator==NULL)
        {
            g_critical ("Error trying to position mouse pointer x position %d, measure number %d", x_to_explain, pi->measure_number);
            return; 
        }
      pi->measure_number++;
    }
  //g_debug("got to measure %d\n", pi->measure_number);

  pi->the_staff = g_list_nth (si->thescore, pi->staff_number - 1);
  pi->the_measure = staff_nth_measure_node (pi->the_staff, pi->measure_number - 1);
  if (pi->the_measure != NULL)
    {                           /*check to make sure user did not click on empty space */
      obj_iterator = (objnode *) ((DenemoMeasure *)pi->the_measure->data)->objects;
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
              pi->the_obj = obj_iterator;//g_print("x_to_explain %d, compare current->x=%d and minpix %d\n",x_to_explain,current->x,current->minpixelsalloted);
              /* The below makes clicking to get the object at the end of
                 a measure (instead of appending after it) require
                 precision.  This may be bad; tweak me if necessary.  */
              if ((x_to_explain > current->x + current->minpixelsalloted) || (x_to_explain > GPOINTER_TO_INT (mwidthiterator->data) - current->minpixelsalloted / 3))   //if closer to barline than object center
                pi->offend = TRUE, pi->cursor_x++;
            }
        }
      //g_debug("got to cursor x %d\n", pi->cursor_x);
    }
}


void
assign_cursor (guint state, guint cursor_num)
{
  guint *cursor_state = g_new (guint, 1);
  *cursor_state = state;
  //g_print("Storing cursor %d for state 0x%x in hash table %p\n", cursor_num, state, Denemo.map->cursors );
  GdkCursor *cursor = gdk_cursor_new_for_display (gdk_display_get_default (), cursor_num);
  if (cursor)
    g_hash_table_insert (Denemo.map->cursors, cursor_state, cursor);
}

void
set_cursor_for (guint state)
{
  gint the_state = state;
  GdkCursor *cursor = g_hash_table_lookup (Denemo.map->cursors, &the_state);
  //g_print("looked up %x in %p got cursor %p\n", state, Denemo.map->cursors,  cursor);
  if (cursor)
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), cursor);
  else
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR);       //FIXME? does this take time/hog memory
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
  //g_debug("Returning %s for mod %d\n", ret->str, mod);
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
      displayhelper (Denemo.project);
    }
  g_string_free (modname, TRUE);
}

static gboolean selecting = FALSE;
static gboolean dragging_separator = FALSE;
static gboolean dragging_display = FALSE;
static gboolean dragging_recording_sync = FALSE;
static gboolean dragging_tempo = FALSE;
static gboolean motion_started = FALSE;//once dragging_xxx is true and a motion_notify has come in this becomes true. Becomes false when dragging_xxx becomes true

static gboolean
change_staff (DenemoMovement * si, gint num, GList * staff)
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
  DenemoProject *gui = Denemo.project;

  gint application_height = get_widget_height (Denemo.scorearea);
  gint line_height = application_height * gui->movement->system_height;
  gint line_num = ((int) *y) / line_height;
  *y -= line_num * line_height;
  *x /= gui->movement->zoom;
  *y /= gui->movement->zoom;
  // *x += ((double)line_num * gui->movement->widthtoworkwith / ((int)(1/gui->movement->system_height))) - 1.0* (line_num?(double)gui->leftmargin:0.0);
}

static void extend_selection (DragDirection direction)
{
   switch (direction) {
       case DRAG_DIRECTION_RIGHT:
        cursorright (NULL, NULL);
        break;
      case DRAG_DIRECTION_LEFT:
        cursorleft (NULL, NULL);
        break;
      case DRAG_DIRECTION_UP:
        staffup (NULL, NULL);
        move_viewport_up (Denemo.project);
        break;
      case DRAG_DIRECTION_DOWN:
        staffdown (NULL, NULL);
        move_viewport_down (Denemo.project);
        break;
    }

    gtk_widget_queue_draw(Denemo.scorearea);
}
gint
scorearea_leave_event (GtkWidget * widget, GdkEventCrossing * event)
{
    show_tooltip (NULL, NULL, NULL);
    if(Denemo.object_hovering_over)
      {
        Denemo.object_hovering_over = NULL;
        gtk_widget_queue_draw(Denemo.scorearea);
      }
    gint allocated_height = get_widget_height (Denemo.scorearea);
    gint allocated_width = get_widget_width (Denemo.scorearea);
  if (event->state & GDK_BUTTON1_MASK)
    {
       dragging_outside = (event->x>=allocated_width)?DRAG_DIRECTION_RIGHT:(event->x < 0)?DRAG_DIRECTION_LEFT:(event->y < 0)? DRAG_DIRECTION_UP : DRAG_DIRECTION_DOWN;
       last_event_x = event->x_root;
       last_event_y = event->y_root;
    }
  gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR); //FIXME? does this take time/hog memory
  return FALSE;                 //allow other handlers (specifically the pitch entry one)
}

gint
scorearea_enter_event (GtkWidget * widget, GdkEventCrossing * event)
{
  dragging_outside = DRAG_DIRECTION_NONE;
  if(Denemo.keyboard_state_locked) return FALSE;
//g_debug("start the enter with ks = %x and state %x\n", Denemo.keyboard_state, event->state);
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
//      g_debug("end the enter with ks %x (values  %x %x)\n", event->state, ~GDK_CONTROL_MASK, Denemo.keyboard_state & (~GDK_CONTROL_MASK) );
  set_midi_in_status ();
  return FALSE;                 //allow other handlers
}

static gint is_nearby (gint line_height, gint y) {
    return !(abs (line_height-y)/Denemo.project->movement->zoom <4);
    
}

static gint hidden_staff_line (gint line_height)
    {
        GList *found = g_list_find_custom (Denemo.hidden_staff_heights, GINT_TO_POINTER(line_height), (GCompareFunc)is_nearby);// if(found)g_print ("found %d for line height %d hidden line %d\t", found->data, line_height,1 + g_list_position (Denemo.hidden_staff_heights, found));
        if (found)
            return 1 + g_list_position (Denemo.hidden_staff_heights, found);
        else return 0;
    }
    
/**
 * Mouse motion callback
 *
 */
gint
scorearea_motion_notify (GtkWidget * widget, GdkEventMotion * event)
{
  DenemoProject *gui = Denemo.project;
  static gboolean hovering_over_hidden = FALSE;
  gint drag_y = event->y;
  if (gui == NULL || gui->movement == NULL)
    return FALSE;
  if (Denemo.scorearea == NULL)
    return FALSE;
    
   gui->movement->hovering_over_midi_track = (gui->movement->recording && (event->y < HeightOfRecordingTrack*gui->movement->zoom));

  // this would avoid an interesting "cursor follow the pointer" mode, which we don't use because you would not be able to move the mouse pointer away e.g. to choose a menu without the Denemo Cursor moving...
  //if (selecting && lh_down && !Denemo.project->movement->markstaffnum)
  // selecting = lh_down = 0; 
  // it should never happen, so we don't need to guard against it.
  if (dragging_recording_sync || dragging_tempo) 
	{
		if ((!motion_started) )
			{
				gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), dragging_recording_sync?Denemo.GDK_SB_H_DOUBLE_ARROW:Denemo.GDK_CIRCLE);
				if (is_playing ())
				 return TRUE;// avoid crashing
				motion_started = TRUE;
			}
	}
  gint allocated_height = get_widget_height (Denemo.scorearea);
  gint line_height = allocated_height * gui->movement->system_height;
  if(dragging_outside)
    {
          gint incrx, incry;
          incrx=incry=0;
          if(((gint)((last_event_x - event->x_root)/gui->movement->zoom)) != 0)
            {
                incrx = -(last_event_x - event->x_root)/gui->movement->zoom;
                last_event_x = event->x_root;
            }
          if( ((gint)((last_event_y - event->y_root)/gui->movement->zoom)) != 0)
            {
                incry = -(last_event_y - event->y_root)/gui->movement->zoom;
                last_event_y = event->y_root;
            }
        if((dragging_outside==DRAG_DIRECTION_RIGHT) && (incrx > 1)
            || ((dragging_outside==DRAG_DIRECTION_LEFT) && (incrx < -1))
            || ((dragging_outside==DRAG_DIRECTION_UP) && (incry < 0))
            || ((dragging_outside==DRAG_DIRECTION_DOWN) && (incry > 0)))
            extend_selection(dragging_outside);
    return TRUE;
    }
  if (event->y < 0)
    event->y = 0.0;
  gint line_num = ((int) event->y) / line_height;


   if (last_directive && (GDK_SHIFT_MASK & event->state) && (GDK_CONTROL_MASK & event->state))
      {
          gint incrx, incry;
          incrx=incry=0;
          if(((gint)((last_event_x - event->x_root)/gui->movement->zoom)) != 0)
            {
                incrx = (last_event_x - event->x_root)/gui->movement->zoom;
                last_event_x = event->x_root;
            }
          if( ((gint)((last_event_y - event->y_root)/gui->movement->zoom)) != 0)
            {
                incry = (last_event_y - event->y_root)/gui->movement->zoom;
                last_event_y = event->y_root;
            }

        if(last_directive->graphic)
            {
                last_directive->gx -= incrx;
                last_directive->gy -= incry;
            }
        else
            {
                last_directive->tx -= incrx;
                last_directive->ty -= incry;
            }
        draw_score_area();

        return TRUE;
      }

  
	



  if(gui->movement->recording && dragging_recording_sync)
    {
        if(gui->movement->recording->type == DENEMO_RECORDING_MIDI)
        { //g_print ("motion notify recording & dragging sync\n\n");
//FIXME repeated code:
		  struct placement_info pi;
		  pi.the_staff = NULL;
		  if (event->y < 0)
			get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
		  else
			get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
		  if (pi.the_staff == NULL)
			return TRUE;            //could not place the cursor
			
		  if ((pi.staff_number>1) && pi.the_measure != NULL)
			{                       /*don't place cursor in a place that is not there nor on the MIDI track*/
			  change_staff (gui->movement, pi.staff_number, pi.the_staff);
			  gui->movement->currentmeasurenum = pi.measure_number;
			  gui->movement->currentmeasure = pi.the_measure;
			  gui->movement->currentobject = pi.the_obj;
			  gui->movement->cursor_x = pi.cursor_x;
			  gui->movement->cursor_appending = (gui->movement->cursor_x == (gint) (g_list_length ((objnode *) ((DenemoMeasure*)gui->movement->currentmeasure->data)->objects)));
			  set_cursor_y_from_click (gui, event->y);
			 }
            //g_warning("Dragging for MIDI ... release on note");
            return TRUE;
        }
//NOT Recording MIDI, so recording AUDIO
        gui->movement->recording->leadin -= 500*(event->x_root - last_event_x)/gui->movement->zoom;//g_debug("%d %d => %d\n", (int)(10*last_event_x), (int)(10*event->x_root), (int)(10*last_event_x) - (int)(10*event->x_root));
        last_event_x = event->x_root;
        update_leadin_widget ( gui->movement->recording->leadin/(double)gui->movement->recording->samplerate);
        gtk_widget_queue_draw(Denemo.scorearea);
        return TRUE;
    }
  if (gui->movement->recording && (gui->movement->recording->type == DENEMO_RECORDING_AUDIO) && dragging_tempo)
    {
        //~ gdouble change = (event->x_root - last_event_x)/gui->movement->zoom;
        //~ last_event_x = event->x_root;
        //~ struct placement_info pi;
        //~ get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
        //~ change /= pi.measure_number;
        //~ update_tempo_widget ( change);
        //~ score_status (Denemo.project, TRUE);
        //~ exportmidi (NULL, gui->movement);
        //~ gtk_widget_queue_draw(Denemo.scorearea);
        return TRUE;
    }
    else if (gui->movement->recording && (gui->movement->recording->type == DENEMO_RECORDING_MIDI) && dragging_tempo)
		{
			gdouble change = (event->x_root - last_event_x)/gui->movement->zoom;
			if (change>0)
				scale_recording (1.005);
			else
				scale_recording (0.995);
			last_event_x = event->x_root;		
			return TRUE;
			
		}
#define DENEMO_MINIMUM_SYSTEM_HEIGHT (0.01)


  if (dragging_separator)
    {
      gui->movement->system_height = event->y / get_widget_height (Denemo.scorearea);
      if (gui->movement->system_height < DENEMO_MINIMUM_SYSTEM_HEIGHT)
        gui->movement->system_height = DENEMO_MINIMUM_SYSTEM_HEIGHT;
      if (gui->movement->system_height > 1.0)
        gui->movement->system_height = 1.0;
      scorearea_configure_event (Denemo.scorearea, NULL);
      draw_score_area();
      return TRUE;
    }

  if (dragging_display)
    {
      //g_print ("dragging display %d %d\n",(gint)event->x, (gint)event->y);
      if (drag_display_x < 0)
		{
			drag_display_x = (gint)event->x/DRAG_SCALE;
			drag_display_y = (gint)event->y/DRAG_SCALE;
			return TRUE;
		}
	  if (drag_display_x > (gint)event->x/DRAG_SCALE)
			call_out_to_guile ("(d-ShrinkMeasures)");
	  if (drag_display_y > (gint)event->y/DRAG_SCALE)
			call_out_to_guile ("(d-ShorterStaffs)");	
		  if (drag_display_x < (gint)event->x/DRAG_SCALE)
			call_out_to_guile ("(d-WidenMeasures)");
	  if (drag_display_y < (gint)event->y/DRAG_SCALE)
			call_out_to_guile ("(d-TallerStaffs)");		
			
	drag_display_x = (gint)event->x/DRAG_SCALE;
	drag_display_y = (gint)event->y/DRAG_SCALE;
				
      draw_score_area();
      return TRUE;
    }


  if (line_height - ((int) event->y - 8) % line_height < 12)
	   gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_SB_V_DOUBLE_ARROW);
  else
       gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR);       //FIXME? does this take time/hog memory

  transform_coords (&event->x, &event->y);
  //g_debug("Marked %d\n", gui->movement->markstaffnum);


  if (gui->lefts[line_num] == 0)
    return TRUE;




  if (lh_down || (selecting && gui->movement->markstaffnum))
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
          change_staff (gui->movement, pi.staff_number, pi.the_staff);
          gui->movement->currentmeasurenum = pi.measure_number;
          gui->movement->currentmeasure = pi.the_measure;
          gui->movement->currentobject = pi.the_obj;
          gui->movement->cursor_x = pi.cursor_x;
          gui->movement->cursor_appending = (gui->movement->cursor_x == (gint) (g_list_length ((objnode *) ((DenemoMeasure*)gui->movement->currentmeasure->data)->objects)));

          set_cursor_y_from_click (gui, event->y);
          if (lh_down & !selecting)
            {
              
              if (pi.the_obj && Denemo.project->movement->smf)
                {
                  gdouble end = Denemo.project->movement->end_time, start = Denemo.project->movement->start_time,
                  latest = ((DenemoObject*)pi.the_obj->data)->latest_time, earliest = ((DenemoObject*)pi.the_obj->data)->earliest_time;
					
                  
                   if ((event->state & (GDK_CONTROL_MASK|GDK_MOD1_MASK)) == (GDK_CONTROL_MASK|GDK_MOD1_MASK)) //ALT+CONTROL drag to shift playback markers
                     {//g_print ("\nend %f and earliest %f", end, earliest);
					  generate_midi();
                      if ((earliest>end) || (fabs(end-earliest) < fabs(latest-earliest) + 0.01))
                        {
							if (!Denemo.dragging_end_playback_marker)
								playback_brightness_shift = drag_y;
							Denemo.dragging_end_playback_marker = TRUE;
						 }
                      else
						{
                         if (!Denemo.dragging_start_playback_marker)
								playback_brightness_shift = drag_y;
						 Denemo.dragging_start_playback_marker = TRUE;
						}
                    }
                }
             if (!(Denemo.dragging_end_playback_marker || Denemo.dragging_start_playback_marker)) 
                  {
                    if (gui->movement->markstaffnum)
                      set_point (NULL, NULL);
                    else
                      set_mark (NULL, NULL);
                    selecting = TRUE;
                    
                  }
              }
            
          calcmarkboundaries (gui->movement);
          if (pi.the_obj && (Denemo.dragging_end_playback_marker || Denemo.dragging_start_playback_marker))
            {
              gdouble end = Denemo.project->movement->end_time, start = Denemo.project->movement->start_time,
                latest = ((DenemoObject*)pi.the_obj->data)->latest_time, earliest = ((DenemoObject*)pi.the_obj->data)->earliest_time;
                if ((Denemo.dragging_end_playback_marker) && (latest > Denemo.project->movement->start_time))
                    Denemo.project->movement->end_time = latest;
                else
                  if (earliest < Denemo.project->movement->end_time)
                    Denemo.project->movement->start_time = earliest;
               fix_start_end_ordering ();
                //g_print ("before event->y %d bright %d pbms %d\n", drag_y, Denemo.playback_marker_brightness, playback_brightness_shift);
               Denemo.playback_marker_brightness += (playback_brightness_shift - drag_y);
               if (Denemo.playback_marker_brightness>50) Denemo.playback_marker_brightness = 50;
               if (Denemo.playback_marker_brightness<-50) Denemo.playback_marker_brightness = -50;
               
               playback_brightness_shift = drag_y;
               //g_print ("after drag_y %d bright %d pbms %d\n", drag_y, Denemo.playback_marker_brightness, playback_brightness_shift);
            }
 
          if (event->state & (GDK_BUTTON1_MASK | GDK_BUTTON2_MASK | GDK_BUTTON3_MASK))
            perform_command (event->state, GESTURE_MOVE, event->state & GDK_BUTTON1_MASK);

          /* redraw to show new cursor position  */
          draw_score_area();
        }
     return TRUE;
    }

 {
 gboolean oldm = Denemo.hovering_over_movement;   
 gboolean oldla = Denemo.hovering_over_left_arrow;   
 gboolean oldra= Denemo.hovering_over_right_arrow;   
 gboolean oldmu = Denemo.hovering_over_margin_up;   
 gboolean oldmd = Denemo.hovering_over_margin_down;   
 gboolean oldb = Denemo.hovering_over_brace;
 gboolean oldp = Denemo.hovering_over_partname;
 gboolean oldc = Denemo.hovering_over_clef;
 gboolean oldks = Denemo.hovering_over_keysharpen;
 gboolean oldkf = Denemo.hovering_over_keyflatten;
 gboolean oldt = Denemo.hovering_over_timesig;
 Denemo.hovering_over_movement = FALSE;

 
 Denemo.hovering_over_movement = Denemo.hovering_over_left_arrow = Denemo.hovering_over_right_arrow = Denemo.hovering_over_brace = Denemo.hovering_over_margin_up = Denemo.hovering_over_margin_down = Denemo.hovering_over_partname = Denemo.hovering_over_clef = Denemo.hovering_over_timesig = Denemo.hovering_over_keysharpen = Denemo.hovering_over_keyflatten = FALSE;

 if (event->x<25 && event->y<25)
      {
            Denemo.hovering_over_movement = TRUE;
      }
 else {
      if (event->x < gui->leftmargin)
        {
           if (gui->braces && (gui->movement->leftmeasurenum == 1) && ((Denemo.hovering_over_brace =  ((gui->leftmargin - event->x) <  BRACEWIDTH * g_list_length (gui->braces)))))
            ; //do nothing more hovering over brace is set
            else
            {
                gint offset = (gint) get_click_height (gui, event->y);
                if (offset > 0 && (offset < STAFF_HEIGHT / 2))  
                    Denemo.hovering_over_margin_up = TRUE;
                else if (offset > 0 && (offset < STAFF_HEIGHT))
                    Denemo.hovering_over_margin_down = TRUE;
            }
        }
      else
         {
            gint key = gui->movement->maxkeywidth;
            gint cmajor = key ? 0 : 5;   
           
            if (((gint) get_click_height_on_closest_staff (gui, event->y)<-10) && (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key))
                 {   
                    struct placement_info pi;
                    if (event->y < 0)
                        get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
                    else
                        get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);

                    if (pi.the_staff == NULL)
                        return TRUE;  
                    if (gui->movement->currentstaffnum == pi.staff_number)    
                             Denemo.hovering_over_partname = TRUE;
                 }
            else  
                { 
           
                if (event->x < (gui->leftmargin+35) - cmajor)
                    {
                        Denemo.hovering_over_clef = TRUE;
                    }
                else if (event->x < (gui->leftmargin+35) + key + cmajor)   
                   {
                       gint offset = (gint) get_click_height_on_closest_staff (gui, event->y);
                       if (offset > 0 && (offset < STAFF_HEIGHT / 2))  
                            Denemo.hovering_over_keysharpen = TRUE;
                       else if (offset > 0 && (offset < STAFF_HEIGHT))
                            Denemo.hovering_over_keyflatten = TRUE;
                    }
                else if (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key)
                   Denemo.hovering_over_timesig = TRUE;       
                }
        }
    }
    
 //Detect arrow left/right
 {
    struct placement_info pi;
    if (event->y < 0)
        get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
    else
        get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
     if(pi.the_staff)
        {
          if ((gui->movement->leftmeasurenum > 1) && (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + gui->movement->maxkeywidth) && (event->x > gui->leftmargin))
             Denemo.hovering_over_left_arrow = TRUE;
          else 
          if (Denemo.right_arrow_x && ((gint)event->x >= Denemo.right_arrow_x))
             Denemo.hovering_over_right_arrow = TRUE;
          else
			  Denemo.hovering_over_left_arrow= Denemo.hovering_over_right_arrow = FALSE;
        }
 }   

  if ((oldm != Denemo.hovering_over_movement) 
  || (oldla != Denemo.hovering_over_left_arrow) 
  || (oldra != Denemo.hovering_over_right_arrow) 
  || (oldmu != Denemo.hovering_over_margin_up) 
  || (oldmd != Denemo.hovering_over_margin_down)
  || (oldb != Denemo.hovering_over_brace)
  || (oldp != Denemo.hovering_over_partname)
  || (oldc != Denemo.hovering_over_clef)
  || (oldt != Denemo.hovering_over_timesig)
  || (oldks != Denemo.hovering_over_keysharpen)
  || (oldkf != Denemo.hovering_over_keyflatten))
    gtk_widget_queue_draw(Denemo.scorearea);
 }


  struct placement_info pi;
   if (event->y < 0)
    get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
  else
    get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
    
    //showing what object would become current if the user clicks
  GList *old_obj = Denemo.object_hovering_over;
  
  if(gui->movement->recording && (event->y < HeightOfRecordingTrack*gui->movement->zoom))
	{
		return TRUE;
	}
  
  Denemo.object_hovering_over = pi.the_obj;
  if (old_obj != Denemo.object_hovering_over)
    gtk_widget_queue_draw (Denemo.scorearea);
    
    
   
    if( hidden_staff_line ((gint)(0.5 + event->y)))
        {
           //if (!hovering_over_hidden) something else keeps re-setting the cursor, so just force it
                gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_TARGET);
            hovering_over_hidden = TRUE;
            
        }
    else
        {
            if (hovering_over_hidden)
                gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR);       //FIXME? does this take time/hog memory
            hovering_over_hidden = FALSE;
        }
    

  if (Denemo.project->midi_destination & MIDICONDUCT)
    {
      advance_time (0.01);
      return TRUE;
    }
  return TRUE;
}

//action to take on right clicking
gboolean activate_right_click (gint state)
    {
      DenemoProject *gui = Denemo.project;
      if (gui->movement->cursor_appending && (state==0))
        {
          if (Denemo.prefs.learning)
           {

                    MouseGestureShow(_("Right Click Appending."), _("This pops up the append menu"),
                        MouseGesture);

           }
          popup_menu ("InsertDuration");
          return TRUE;
        }

        if ((GDK_CONTROL_MASK & state) == GDK_CONTROL_MASK) {
            if (Denemo.prefs.learning)
                    MouseGestureShow(_("Control-Right Click."), _("This pops up menu for inserting barlines and many other sorts of objects"),
                        MouseGesture);
            popup_menu ("Markings");
            return TRUE;
        }
      if ((GDK_SHIFT_MASK & state) == GDK_SHIFT_MASK) {
            if (Denemo.prefs.learning)
                    MouseGestureShow(_("Shift-Right Click."), _("This allows editing the directives/attributes of the object at the cursor"),
                        MouseGesture);
            call_out_to_guile ("(d-EditSimilar 'once)");
            return TRUE;
        }
     return FALSE;
    }
    

/**
 * Mouse button press callback
 *
 */
gint
scorearea_button_press (GtkWidget * widget, GdkEventButton * event)
{ 
	//g_print ("press with state %x\n", event->state);
  DenemoProject *gui = Denemo.project;
  if (gui == NULL || gui->movement == NULL)
    return FALSE;
  gboolean left = (event->button != 3);
  //if the cursor is at a system separator start dragging it
  gint allocated_height = get_widget_height (Denemo.scorearea);
  gint line_height = allocated_height * gui->movement->system_height;
  gint line_num = ((int) event->y) / line_height;
  last_event_x = event->x_root;
  last_event_y = event->y_root;
  //g_debug("diff %d\n", line_height - ((int)event->y)%line_height);

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
  
   if (dragging_display == FALSE)
    if (left && (event->state == GDK_CONTROL_MASK))
      {
        if (Denemo.prefs.learning)
          MouseGestureShow(_("Dragging measure width/staff spacing."), _("This will widen/narrow the measures/staffs in the display."),
            MouseGesture);
        dragging_display = TRUE;
        static gboolean done;
        if (!done)
			{
				done = TRUE;
				assign_cursor ( 99, GDK_CROSS);
			}
        set_cursor_for (99);
        return TRUE;
      }
  dragging_display = FALSE; 
  drag_display_x = -1;//to reset
  
  
  if (gui->movement->top_staff == 1)
	{
	  if(gui->movement->recording && gui->movement->recording->type==DENEMO_RECORDING_AUDIO)
		{
		 //g_debug("audio %f %f\n", event->x, event->y);
		  if(event->y < HeightOfRecordingTrack*gui->movement->zoom /* see draw.c for this value, the note onsets are drawn in the top 20 pixels */)
			{
				if (event->type==GDK_2BUTTON_PRESS)
					{
						gui->movement->recording->marked_onset_position = (gint)event->x/gui->movement->zoom;
						if (gui->movement->recording->marked_onset_position < (gui->leftmargin+35) + SPACE_FOR_TIME + gui->movement->maxkeywidth) {
							 if (Denemo.prefs.learning)
								MouseGestureShow(_("Double Click Note Onset"), _("This represents detected note onsets which occur\nbefore the start of the score.\nIf they are just noise,\nor if you are working on just a portion of the audio that is ok.\nOtherwise drag with left mouse button to synchronize\nwith the start of the score."),
													MouseGesture);
							}
						else if (Denemo.prefs.learning)
								MouseGestureShow(_("Double Click Recorded MIDI Note"), _("This marks/un-marks the current recorded MIDI note."),
			  MouseGesture);
						gtk_widget_queue_draw(Denemo.scorearea);//sets marked_onset to match marked_onset_position
						return TRUE;
					} 
			  else
					{
						if (is_playing ())
							{
								g_warning ("Stop playing first");
								return TRUE;
							}
						
						//gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), left?Denemo.GDK_SB_H_DOUBLE_ARROW:Denemo.GDK_X_CURSOR);
						left? (dragging_recording_sync = TRUE) : (dragging_tempo = TRUE);
						motion_started = FALSE;
						
						if (Denemo.prefs.learning) 
						 left? 	MouseGestureShow(_("Left on Note Onset"), _("preparing to drag..."),
												MouseGesture) :
								MouseGestureShow(_("Start Playing Recording"), _("This starts playing the recorded MIDI notes from the marked onset"),
												MouseGesture);
						
						gtk_widget_queue_draw(Denemo.scorearea);
						return TRUE;
					}
			}
		} //end of Audio
	   else  if(gui->movement->recording && (gui->movement->recording->type==DENEMO_RECORDING_MIDI) && (event->y < HeightOfRecordingTrack*gui->movement->zoom) && midi_track_present ())
		{
			if (left)
				{
					
					if (control_held_down () && shift_held_down ())
						{
							gui->movement->recording->marked_onset_position = (gint)event->x/gui->movement->zoom;
							play_recorded_midi ();//toggles
							if (Denemo.prefs.learning) 
								 MouseGestureShow(_("Control-Shift Left click on MIDI Recording Track"), _("Starts/Stops playback of recorded MIDI from marked note."),
														MouseGesture);
						} else
					if (control_held_down ())
						{
							dragging_tempo = TRUE;
							motion_started = FALSE;
							if (Denemo.prefs.learning) 
								 MouseGestureShow(_("Control Left click on MIDI Recording Track"), _("Start dragging to change tempo of MIDI recorded track."),
														MouseGesture);
						} else
					if (shift_held_down ())
						{

							dragging_recording_sync = TRUE;
							motion_started = FALSE;
							if (Denemo.prefs.learning) 
								 MouseGestureShow(_("Shift Left click on MIDI Recording Track"), _("Drag the marked MIDI note to synchronize to a place in the score."),
														MouseGesture);
						} else
						{
							gui->movement->recording->marked_onset_position = (gint)event->x/gui->movement->zoom;
							pause_recording_midi ();//ensures further recording does not leave a long gap if the user has recording long rests enabled
							if (Denemo.prefs.learning)
								MouseGestureShow(_("Left Click Recorded MIDI Note"), _("This marks the current recorded MIDI note."),
			  MouseGesture);
						}
					return TRUE;
				}
			else
				{
					if (Denemo.prefs.learning) 
						 MouseGestureShow(_("Right click on MIDI Recording Track"), _("Menu of options for MIDI recorded track."),
												MouseGesture);
					popup_menu ("Recording");
					return TRUE;
				}
		}
  }

  //g_debug("before %f %f\n", event->x, event->y);
  transform_coords (&event->x, &event->y);
  //g_debug("after %f %f\n", event->x, event->y);

    if (Denemo.hovering_over_movement)
        {
          call_out_to_guile ("(EditMovement)");  
          return TRUE;
        }


  gtk_widget_grab_focus (widget);
  gint key = gui->movement->maxkeywidth;
  gint cmajor = key ? 0 : 5;    //allow some area for keysig in C-major

  if (gui->lefts[line_num] == 0)
    return TRUE;                //On an empty system at the bottom where there is not enough room to draw another staff.

  struct placement_info pi;
  pi.the_staff = NULL;

//navigate to a hidden staff if on a hidden staff line  
  {
      gint theline = hidden_staff_line ((gint)(0.5 + event->y));
      if (theline)
        {
            GList *this = gui->movement->thescore;
            gint count = 0;
            gint staffnum = 0;
            while (this)
                {
                    DenemoStaff *thestaff = (DenemoStaff*)this->data;
                    if (thestaff->hidden && (this != gui->movement->currentstaff))
                        {
                            count++;
                            if (count == theline)
                                {
                                    staffnum = 1 + g_list_position (gui->movement->thescore, this); //g_print ("Looked for hidden line %d got staffnum %d\n", theline, staffnum);
                                    break;
                                }
                        }
                if (this)
                    this = this->next;
                }
        //staffnum is the first of the hidden staffs belonging to the line theline 
        
        goto_movement_staff_obj (NULL, -1, staffnum, gui->movement->currentmeasurenum, 0, gui->lefts[line_num]);    
        return TRUE;
        }   
  }
    
  
  
  if (event->y < 0)
    get_placement_from_coordinates (&pi, event->x, 0, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
  else
    get_placement_from_coordinates (&pi, event->x, event->y, gui->lefts[line_num], gui->rights[line_num], gui->scales[line_num]);
  if (pi.the_staff == NULL)
    return TRUE;                //could not place the cursor

  change_staff (gui->movement, pi.staff_number, pi.the_staff);

  if (left && (gui->movement->leftmeasurenum > 1) && (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key) && (event->x > gui->leftmargin))
    {
      if (Denemo.prefs.learning)
        MouseGestureShow(_("Press Left."), _("This moved the cursor to the measure offscreen left. The display is shifted to place that measure on screen."),
          MouseGesture);
      set_currentmeasurenum (gui, gui->movement->leftmeasurenum - 1);
      write_status (gui);
      draw_score_area();
      return TRUE;
    }
  else
    {
      if (Denemo.hovering_over_right_arrow)
        {
		if (gui->movement->currentmeasure && gui->movement->currentmeasure->next)
			set_currentmeasurenum (gui, gui->movement->rightmeasurenum + 1);
		if (Denemo.prefs.learning)
		  MouseGestureShow(_("Press Left."), _("This moved the cursor to the measure off-screen right. That measure becomes the second from the left."),
			MouseGesture);
		write_status (gui);
		return TRUE;

        }
    }

  if (pi.the_measure != NULL)
    {                           /*don't place cursor in a place that is not there */
      //gui->movement->currentstaffnum = pi.staff_number;
      //gui->movement->currentstaff = pi.the_staff;
      gui->movement->currentmeasurenum = pi.measure_number;
      gui->movement->currentmeasure = pi.the_measure;
      gui->movement->currentobject = pi.the_obj;
      gui->movement->cursor_x = pi.cursor_x;
      gui->movement->cursor_appending = (gui->movement->cursor_x == (gint) (g_list_length ((objnode *) ((DenemoMeasure*)gui->movement->currentmeasure->data)->objects)));
      set_cursor_y_from_click (gui, event->y);
      if (event->type==GDK_2BUTTON_PRESS)
                {
                    if(gui->movement->recording &&  !g_strcmp0 (((DenemoStaff *) gui->movement->currentstaff->data)->denemo_name->str, DENEMO_CLICK_TRACK_NAME))
                        {//FIXME this is never reached as we don't allow the mouse to access the click track 
                            gui->movement->recording->marked_onset_position = (gint)event->x/gui->movement->zoom;
                            if (Denemo.prefs.learning)
                                MouseGestureShow(_("Double Click on Click Track"), _("This will mark the MIDI note onset."), MouseGesture);
                            return TRUE;

                        }

                    else
                        {
                          if (Denemo.prefs.learning)
                            MouseGestureShow(_("Double Click."), _("This gives information about the object at the cursor. Click on a notehead for information about a note in a chord."),
                              MouseGesture);
                                    display_current_object();
                                    return TRUE;
                        }
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

   if ((((DenemoStaff *) gui->movement->currentstaff->data)->voicecontrol != DENEMO_PRIMARY)
        && (gui->movement->leftmeasurenum == 1) && (event->x > gui->leftmargin)
        && ((event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key)))
    {
          infodialog(_("The clef shown here affects the display only (as this voice is displayed on the staff above)."
          " You can change the display clef using the clef menu."
          "\nWarning! you will get confused if you set the key signature or time signature of a voice different "
          "to the staff it is typeset on. Run the Staff/Voice property editor to adjust any inconsistencies."));

    }

  if ((((DenemoStaff *) gui->movement->currentstaff->data)->voicecontrol == DENEMO_PRIMARY) && (gui->movement->leftmeasurenum == 1) && (event->x > gui->leftmargin))
    {
        
        if ((offset<-10) && (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key))
            {
                if (Denemo.prefs.learning)
                MouseGestureShow(_("Left on Part name."), _("This pops up the built-in staff properties. For other properties of the current staff see the staff menu or the tools icon before the clef."),
                  MouseGesture);
                staff_properties_change_cb (NULL, NULL);
                return TRUE;
            }
        else
        {
          if (event->x < (gui->leftmargin+35) - cmajor)
            {

                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Left on initial Clef."), _("This pops up the initial clef menu."),
                      MouseGesture);
                  popup_menu ("ClefMenu");
                }
              return TRUE;
            }
          else if (event->x < (gui->leftmargin+35) + key + cmajor)
            {
              if (left)
                {
                  if (offset > 0 && (offset < STAFF_HEIGHT / 2))
                    {
                      if (Denemo.prefs.learning)
                        MouseGestureShow(_("Left Click on blue."), _("This adds one sharp."),
                          MouseGesture);
                    if ((gui->movement->currentmeasure->next==NULL)  || confirm (_("Initial Key Signature Change"), _("Sharpen Keysignature?")))
                      call_out_to_guile ("(d-SharpenInitialKeysigs)");
                    }
                  else if (offset > 0 && (offset < STAFF_HEIGHT))
                    {
                      if (Denemo.prefs.learning)
                        MouseGestureShow(_("Left Click on red."), _("This adds one flat."),
                          MouseGesture);
                      if ((gui->movement->currentmeasure->next==NULL) || confirm (_("Initial Key Signature Change"), _("Flatten Keysignature?")))
                        call_out_to_guile ("(d-FlattenInitialKeysigs)");
                    }
                }
              else
                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Right Click on key."), _("This pops up the key signature menu."),
                        MouseGesture);
                  popup_menu ("Key");
                }
              return TRUE;
            }
          else if (event->x < (gui->leftmargin+35) + SPACE_FOR_TIME + key)
            {
              if (Denemo.prefs.learning)
                MouseGestureShow(_("Click on Time."), _("This pops up the time signature menu."),
                        MouseGesture);
              popup_menu ("TimeSig");
              return TRUE;
            }
        }
    }

  if (event->x < gui->leftmargin)
    {
       if (gui->braces && (gui->movement->leftmeasurenum == 1))
        {
                gint width = BRACEWIDTH * g_list_length (gui->braces);
                //gint count = (gui->leftmargin - event->x)/BRACEWIDTH;
                if ((gui->leftmargin - event->x) < width)
                {
                    gint count = 1 + (width - gui->leftmargin + event->x)/BRACEWIDTH;


                    if ((count>0) && (count <= g_list_length (gui->braces)))
                        {
                            DenemoBrace *brace = (DenemoBrace*)g_list_nth_data (gui->braces, count-1);
                            gint choice = choose_option_or_cancel (_("Editing Staff Groups (Braces)"), _("Edit Start Brace"), _("Edit End Brace"), TRUE);
                            if( choice>=0)
                                {
                                    gint staffnum = choice?brace->startstaff:brace->endstaff;
                                    //g_print ("Count is %d for start at %d\n", count, staffnum);
                                    //GtkWidget *menu = gtk_ui_manager_get_widget (Denemo.ui_manager, "StaffGroupings");
                                    goto_movement_staff_obj (NULL, -1, staffnum, 1, 0, 0);
                                    
                                    if (choice)
                                        popup_menu ("StaffGroupings");
                                    else
                                    {
                                        if (staff_directive_get_tag ("BraceEnd"))
                                            call_out_to_guile ("(d-BraceEnd)");
                                        else
                                            warningdialog (_( "This staff grouping has no End Brace so it finishes on the lowest staff. Use the Staffs/Voices->Staff Groupings menu to place an End Brace on the desired staff"));
                                    }
                                    //note the popup returns as soon as the menu is popped up, so we can't go back to the original position.

                                }
                        }

                    return TRUE;
                }

        }

      if (pi.staff_number == gui->movement->currentstaffnum)
        {
          gint offset = (gint) get_click_height (gui, event->y);
          if (offset < STAFF_HEIGHT / 2)
            {
              if (((DenemoStaff *) gui->movement->currentstaff->data)->staff_directives, 1)
                {
                  if (Denemo.prefs.learning)
                    MouseGestureShow(_("Click on Staff Directives."), _("This pops up the staff directives menu for editing"),
                      MouseGesture);
                  call_out_to_guile ("(EditStaff)"); //edit_staff_properties ();//gtk_menu_popup (((DenemoStaff *) gui->movement->currentstaff->data)->staffmenu, NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
                }
              return TRUE;
            }
          else if (((DenemoStaff *) gui->movement->currentstaff->data)->voice_directives, 1)
            {
              if (Denemo.prefs.learning)
                MouseGestureShow(_("Click on Voice Directives."), _("This pops up the voice directives menu for editing"),
                    MouseGesture);
              edit_voice_properties ();//gtk_menu_popup (((DenemoStaff *) gui->movement->currentstaff->data)->voicemenu, NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
              return TRUE;
            }
        }
    }

  if (left)
    {
      if (!(GDK_SHIFT_MASK & event->state))
        gui->movement->markstaffnum = 0;
      lh_down = TRUE;
    }
  else
    activate_right_click (event->state);

  if (left && (GDK_SHIFT_MASK & event->state) && (GDK_CONTROL_MASK & event->state))
  {
     // if current object is directive, start dragging its graphic, dragging_display=TRUE

      DenemoObject *obj;
      if (Denemo.prefs.learning)
                    MouseGestureShow(_("Control-Shift-Drag."), _("This allows dragging objects in the display.\nAll sorts of directives such as staccato dots, ornaments, repeat marks etc can be dragged if the display is too cluttered.\nThe typeset score is unaffected.\nClick on a notehead to drag things attached to the notehead,\nor off the noteheads for things attached to the whole chord."),
                        MouseGesture);

    last_directive = get_next_directive_at_cursor ();
    if(last_directive)
        {
            score_status (Denemo.project, TRUE);
            return TRUE;
        }
    infodialog (_("Control-Shift-Drag is used to tidy up the Denemo display. Useful if Denemo has created a clutter with your input music.\nIf you have several things attached to one object you can move them in turn by dragging them in turn.\nNotes, Slurs and Ties are fixed but most other things can be moved to make the input music clear. Does not affect the typeset score!\nNB! if you have dragged something to one side of a note you have to control-shift-click on the note itself to drag it back - it is where the cursor is that counts."));
    return TRUE;
 }






  set_cursor_for (event->state | (left ? GDK_BUTTON1_MASK : GDK_BUTTON3_MASK));




  //displayhelper(Denemo.project);
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
  DenemoProject *gui = Denemo.project;
  if (gui == NULL || gui->movement == NULL)
    return FALSE;
  last_directive = NULL;
  if(Denemo.dragging_end_playback_marker || Denemo.dragging_start_playback_marker)
    {
      Denemo.dragging_start_playback_marker = Denemo.dragging_end_playback_marker = FALSE;
      playback_brightness_shift = 0;
      gui->movement->markstaffnum = 0;
    }
  gboolean left = (event->button != 3);
  if(gui->movement->recording && (dragging_tempo || dragging_recording_sync))
    {
		//g_print ("button release -  current object = 0x%p dragging sync %d or tempo %d\n", gui->movement->currentobject, dragging_recording_sync, dragging_tempo);
        if (motion_started)
			{
				if (gui->movement->recording && gui->movement->recording->type == DENEMO_RECORDING_MIDI)
					{
						if(dragging_recording_sync)
							{
								Denemo.project->movement->recording->sync = NULL;
							    synchronize_recording ();//changes leadin only
							}
						else
							//g_print ("Scaling Done\n");//this should stretch or squash the timings in Denemo.project->recording->notes keeping any marked offset at the same time
							;//do nothing
						dragging_tempo = dragging_recording_sync = FALSE;
						return TRUE;
					}
				else
				gdk_window_set_cursor (gtk_widget_get_window (Denemo.window), Denemo.GDK_LEFT_PTR);       //FIXME? does this take time/hog memory
			}
            
            dragging_tempo = dragging_recording_sync = FALSE;
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
    
  if (dragging_display)
    {
      dragging_display = FALSE;
      return TRUE;
    }
     
    
    

	if(gui->movement->recording && (event->y < HeightOfRecordingTrack*gui->movement->zoom) && midi_track_present ())
	{
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
  DenemoProject *gui = Denemo.project;
  if (gui == NULL || gui->movement == NULL)
    return FALSE;
  switch (event->direction)
    {
    DenemoScriptParam param;//for testing move to staffup/down
    case GDK_SCROLL_UP:
      if (event->state & GDK_CONTROL_MASK)
        {
          if (Denemo.prefs.learning) {
            gint command_idx = lookup_command_from_name(Denemo.map, "ZoomIn");
            KeyStrokeShow (_("Ctrl + Mouse Wheel Up"), command_idx, TRUE);
          }
          Denemo.project->movement->zoom *= 1.1;
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
         if (!(midi_track_present () && Denemo.project->movement->currentstaff == Denemo.project->movement->thescore->next))
			{
				movetostaffup (NULL, &param);
				if (!param.status) { //i.e. movetostaffup failed
				DenemoStaff *thestaff = (DenemoStaff*)(Denemo.project->movement->currentstaff->data);
				if(thestaff->space_above < MAXEXTRASPACE)
				  {
					thestaff->space_above++;
					g_debug ("Increasing the height of the top staff");
				  }
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
          Denemo.project->movement->zoom /= 1.1;
          if (Denemo.project->movement->zoom < 0.01)
            Denemo.project->movement->zoom = 0.01;
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
          movetostaffdown (NULL, &param);
          if (!param.status) {
			  DenemoStaff *thestaff = (DenemoStaff*)(Denemo.project->movement->thescore->data);
				if((thestaff->space_above > 0))
				  {
					thestaff->space_above--;
					g_debug ("Decreasing the height of the top staff");
				  }
          }
        }
      break;
    case GDK_SCROLL_LEFT:
      movetomeasureleft (NULL, &param);
      if (!param.status)
        warningmessage ("This is the first measure");
      break;
    case GDK_SCROLL_RIGHT:
      movetomeasureright (NULL, &param);
      if (!param.status)
        warningmessage ("This is the last measure");
      break;

    default:
      break;
    }
  displayhelper (Denemo.project);
  return FALSE;
}
