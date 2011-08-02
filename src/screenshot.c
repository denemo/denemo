/* screenshot.c - Take a screenshot from user selected rectangle
 * Copyright (C) 2011 Richard Shann
 *
 * Copyright (C) 2001 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright (C) 2006 Emmanuele Bassi <ebassi@gnome.org>
 * Copyright (C) 2008 Cosimo Cecchi <cosimoc@gnome.org>
 * Copyright (C) 2011 Philippe Corbes <philippe.corbes@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#ifndef G_OS_WIN32
 
#include <gdk/gdkx.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>



typedef struct {
  GdkRectangle  rect;
  GdkRectangle  draw_rect;
  gboolean      button_pressed;
  /* only needed because we're not using cairo to draw the rectangle */
  GdkWindow    *root;
  GdkGC        *gc;
} select_area_filter_data;

static GdkWindow *
screenshot_find_active_window (void)
{
  GdkWindow *window;
  GdkScreen *default_screen;

  default_screen = gdk_screen_get_default ();
  window = gdk_screen_get_active_window (default_screen);

  return window;
}


static gboolean
screenshot_window_is_desktop (GdkWindow *window)
{
  GdkWindow *root_window = gdk_get_default_root_window ();
  GdkWindowTypeHint window_type_hint;

  if (window == root_window)
    return TRUE;

  window_type_hint = gdk_window_get_type_hint (window);
  if (window_type_hint == GDK_WINDOW_TYPE_HINT_DESKTOP)
    return TRUE;

  return FALSE;
      
}

GdkWindow *
screenshot_find_current_window ()
{
  GdkWindow *current_window;

  current_window = screenshot_find_active_window ();
  
  /* If there's no active window, we fall back to returning the
   * window that the cursor is in.
   */
  if (!current_window)
    current_window = gdk_window_at_pointer (NULL, NULL);

  if (current_window)
    {
      if (screenshot_window_is_desktop (current_window))
	/* if the current window is the desktop (e.g. nautilus), we
	 * return NULL, as getting the whole screen makes more sense.
         */
        return NULL;

      /* Once we have a window, we take the toplevel ancestor. */
      current_window = gdk_window_get_toplevel (current_window);
    }

  return current_window;
}

static void
select_area_button_press (XButtonEvent    *event,//BUG! should be XButtonEvent
                          GdkRectangle *rect,
                          GdkRectangle *draw_rect)
{
  rect->x = event->x_root;
  rect->y = event->y_root;

  draw_rect->x = rect->x;
  draw_rect->y = rect->y;
  draw_rect->width  = 0;
  draw_rect->height = 0;
}

static void
select_area_button_release (XButtonEvent    *event,//BUG! should be XButtonEvent
                            GdkRectangle *rect,
                            GdkRectangle *draw_rect,
                            GdkWindow    *root,
                            GdkGC        *gc)
{
  /* remove the old rectangle */
  if (draw_rect->width > 0 && draw_rect->height > 0)
    gdk_draw_rectangle (root, gc, FALSE, 
                        draw_rect->x, draw_rect->y,
                        draw_rect->width, draw_rect->height);

  rect->width  = ABS (rect->x - event->x_root);
  rect->height = ABS (rect->y - event->y_root);

  rect->x = MIN (rect->x, event->x_root);
  rect->y = MIN (rect->y, event->y_root);
}

static void
select_area_motion_notify (XButtonEvent    *event,//BUG! should be XMotionEvent
                           GdkRectangle *rect,
                           GdkRectangle *draw_rect,
                           GdkWindow    *root,
                           GdkGC        *gc)
{
  /* FIXME: draw some nice rubberband with cairo if composited */

  /* remove the old rectangle */
  if (draw_rect->width > 0 && draw_rect->height > 0)
    gdk_draw_rectangle (root, gc, FALSE, 
                        draw_rect->x, draw_rect->y,
                        draw_rect->width, draw_rect->height);
  draw_rect->width  = ABS (rect->x - event->x_root);
  draw_rect->height = ABS (rect->y - event->y_root);

  draw_rect->x = MIN (rect->x, event->x_root);
  draw_rect->y = MIN (rect->y, event->y_root);

  /* draw the new rectangle */
  if (draw_rect->width > 0 && draw_rect->height > 0)
    gdk_draw_rectangle (root, gc, FALSE, 
                        draw_rect->x, draw_rect->y,
                        draw_rect->width, draw_rect->height);
}



static GdkFilterReturn
select_area_filter (GdkXEvent *gdk_xevent,
                    GdkEvent  *event,
                    gpointer   user_data)
{
  select_area_filter_data *data = user_data;
  XEvent *xevent = (XEvent *) gdk_xevent;

  switch (xevent->type)
    {
    case ButtonPress:
      if (!data->button_pressed)
        {
          select_area_button_press (&xevent->xbutton,//BUG! should be xbutton
                                    &data->rect, &data->draw_rect);
          data->button_pressed = TRUE;
        }
      return GDK_FILTER_REMOVE;
    case ButtonRelease:
      if (data->button_pressed)
      {
        select_area_button_release (&xevent->xbutton,//BUG! should be xbutton
                                    &data->rect, &data->draw_rect,
                                    data->root, data->gc);
        gtk_main_quit ();
      }
      return GDK_FILTER_REMOVE;
    case MotionNotify:
      if (data->button_pressed)
        select_area_motion_notify (&xevent->xbutton,//BUG! should be xmotion
                                   &data->rect, &data->draw_rect,
                                   data->root, data->gc);
      return GDK_FILTER_REMOVE;
    case KeyPress:
      //if (xevent->xkey.keycode == XKeysymToKeycode (gdk_display, XK_Escape))
        {
          data->rect.x = 0;
          data->rect.y = 0;
          data->rect.width  = 0;
          data->rect.height = 0;
          gtk_main_quit ();
          return GDK_FILTER_REMOVE;
        }
      break;
    default:
      break;
    }
 
  return GDK_FILTER_CONTINUE;
}

gboolean
screenshot_select_area (int *px, int *py, int *pwidth, int *pheight){
  GdkWindow               *root;
  GdkCursor               *cursor;
  select_area_filter_data  data;
  GdkGCValues              values;
  GdkColor                 color;

  root = gdk_get_default_root_window ();
  cursor = gdk_cursor_new (GDK_CROSSHAIR);

  if (gdk_pointer_grab (root, FALSE,
                        GDK_POINTER_MOTION_MASK|GDK_BUTTON_PRESS_MASK|GDK_BUTTON_RELEASE_MASK,
                        NULL, cursor,
                        GDK_CURRENT_TIME) != GDK_GRAB_SUCCESS)
    {
      gdk_cursor_unref (cursor);
      return FALSE;
    }

  if (gdk_keyboard_grab (root, FALSE, GDK_CURRENT_TIME) != GDK_GRAB_SUCCESS)
    {
      gdk_pointer_ungrab (GDK_CURRENT_TIME);
      gdk_cursor_unref (cursor);
      return FALSE;
    }

  gdk_window_add_filter (root, (GdkFilterFunc) select_area_filter, &data);

  gdk_flush ();

  data.rect.x = 0;
  data.rect.y = 0;
  data.rect.width  = 0;
  data.rect.height = 0;
  data.button_pressed = FALSE;
  data.root = root;

  values.function = GDK_XOR;
  values.fill = GDK_SOLID;
  values.clip_mask = NULL;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  values.clip_x_origin = 0;
  values.clip_y_origin = 0;
  values.graphics_exposures = 0;
  values.line_width = 0;
  values.line_style = GDK_LINE_SOLID;
  values.cap_style = GDK_CAP_BUTT;
  values.join_style = GDK_JOIN_MITER;

  data.gc = gdk_gc_new_with_values (root, &values,
                                    GDK_GC_FUNCTION | GDK_GC_FILL |
                                    GDK_GC_CLIP_MASK | GDK_GC_SUBWINDOW |
                                    GDK_GC_CLIP_X_ORIGIN |
                                    GDK_GC_CLIP_Y_ORIGIN | GDK_GC_EXPOSURES |
                                    GDK_GC_LINE_WIDTH | GDK_GC_LINE_STYLE |
                                    GDK_GC_CAP_STYLE | GDK_GC_JOIN_STYLE);
  gdk_color_parse ("white", &color);
  gdk_gc_set_rgb_fg_color (data.gc, &color);
  gdk_color_parse ("black", &color);
  gdk_gc_set_rgb_bg_color (data.gc, &color);

  gtk_main ();

  g_object_unref (data.gc);

  gdk_window_remove_filter (root, (GdkFilterFunc) select_area_filter, &data);

  gdk_keyboard_ungrab (GDK_CURRENT_TIME);
  gdk_pointer_ungrab (GDK_CURRENT_TIME);
  gdk_cursor_unref (cursor);

  *px = data.rect.x;
  *py = data.rect.y;
  *pwidth  = data.rect.width;
  *pheight = data.rect.height;

  return TRUE;
}

GdkRectangle *
screenshot_find_rectangle (void)
{
  GdkRectangle *rectangle;
  rectangle = g_new0 (GdkRectangle, 1);
  if (screenshot_select_area (&rectangle->x, &rectangle->y,
                              &rectangle->width, &rectangle->height)) {
    if ((rectangle->width > 0) && (rectangle->height >= 0))
      return rectangle;
  }
  g_free (rectangle);
  return NULL;
}

GdkPixbuf *
screenshot_get_pixbuf (GdkWindow    *window,
                       GdkRectangle *rectangle)
{
  GdkWindow *root;
  GdkPixbuf *screenshot;
  gint x_real_orig, y_real_orig, x_orig, y_orig;
  gint width, real_width, height, real_height;


  root = gdk_get_default_root_window ();

  gdk_drawable_get_size (window, &real_width, &real_height);      
  gdk_window_get_origin (window, &x_real_orig, &y_real_orig);

  x_orig = x_real_orig;
  y_orig = y_real_orig;
  width  = real_width;
  height = real_height;

  if (x_orig < 0)
    {
      width = width + x_orig;
      x_orig = 0;
    }

  if (y_orig < 0)
    {
      height = height + y_orig;
      y_orig = 0;
    }

  if (x_orig + width > gdk_screen_width ())
    width = gdk_screen_width () - x_orig;

  if (y_orig + height > gdk_screen_height ())
    height = gdk_screen_height () - y_orig;

  if (rectangle)
    {
      x_orig = rectangle->x - x_orig;
      y_orig = rectangle->y - y_orig;
      width  = rectangle->width;
      height = rectangle->height;
    }
  
  screenshot = gdk_pixbuf_get_from_drawable (NULL, root, NULL,
                                             x_orig, y_orig, 0, 0,
                                             width, height);

  //FIXME mask_monitors (screenshot, root);
  
  return screenshot;
}

#endif /*Not windows, this would require GdkXEvent to be handled appropriately - something called MSG??? */

