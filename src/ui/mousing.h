/* mousing.h
   header for functions that handle mouse clicks, drags, etc.

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005, 2008  Matthew Hiller, Richard Shann
*/
#ifndef MOUSING_H
#define MOUSING_H
gint scorearea_button_release (GtkWidget * widget, GdkEventButton * event);
gint scorearea_button_press (GtkWidget * widget, GdkEventButton * event);
gint scorearea_motion_notify (GtkWidget * widget, GdkEventMotion * event);
gint scorearea_scroll_event (GtkWidget * widget, GdkEventScroll * event);
gint scorearea_leave_event (GtkWidget * widget, GdkEventCrossing * event);
gint scorearea_enter_event (GtkWidget * widget, GdkEventCrossing * event);
typedef enum mouse_gesture
{
  GESTURE_PRESS,
  GESTURE_RELEASE,
  GESTURE_MOVE
} mouse_gesture;

gboolean activate_right_click (gint state);

GString *mouse_shortcut_name (gint mod, mouse_gesture gesture, gboolean left);
void append_modifier_name (GString * ret, gint mod);
void assign_cursor (guint state, guint cursor_num);
void set_cursor_for (guint state);
#define DENEMO_NUMBER_MODIFIERS (8)     /* built in to GDK for GdkModifierType */
#define DENEMO_MODIFIER_MASK (255)

#endif
