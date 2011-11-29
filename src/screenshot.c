
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
#include <gtk/gtk.h>
#include <glib.h>
#include <gdk/gdk.h>
#if GTK_CHECK_VERSION(3,0,0)
#if 0 //remove this and the corresponding endif to compile and test on gkt3

#ifdef G_OS_WIN32
#include "windows.h"
#else
#include <gdk/gdkx.h>
#endif

typedef void (* SelectAreaCallback) (GdkRectangle *rectangle);

typedef struct {
  GdkRectangle  rect;
  GdkRectangle  draw_rect;
  gboolean      button_pressed;
  
  GtkWidget *window;
  gboolean aborted;
} select_area_filter_data;

static
select_area_filter_data the_data;
static gboolean
select_window_draw (GtkWidget *window, cairo_t *cr, gpointer unused)
{
  GtkAllocation allocation;
  GtkStyle *style;

  style = gtk_widget_get_style (window);

  if (gtk_widget_get_app_paintable (window))
    {
      cairo_set_line_width (cr, 1.0);

      gtk_widget_get_allocation (window, &allocation);

      cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
      cairo_set_source_rgba (cr, 0, 0, 0, 0);
      cairo_paint (cr);

      cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
      gdk_cairo_set_source_color (cr, &style->base[GTK_STATE_SELECTED]);
      cairo_paint_with_alpha (cr, 0.25);

      cairo_rectangle (cr,
                       allocation.x + 0.5, allocation.y + 0.5,
                       allocation.width - 1, allocation.height - 1);
      cairo_stroke (cr);
    }
  else
    {
      gdk_cairo_set_source_color (cr, &style->base[GTK_STATE_SELECTED]);
      cairo_paint (cr);
    }

  return TRUE;
}
static GtkWidget *
create_select_window (void)
{
  GtkWidget *window;
  GdkScreen *screen;
  GdkVisual *visual;

  screen = gdk_screen_get_default ();
  visual = gdk_screen_get_rgba_visual (screen);

  window = gtk_window_new (GTK_WINDOW_POPUP);
  if (gdk_screen_is_composited (screen) && visual)
    {
      gtk_widget_set_visual (window, visual);
      gtk_widget_set_app_paintable (window, TRUE);
    }

  g_signal_connect (window, "draw", G_CALLBACK (select_window_draw), NULL);

  gtk_window_move (GTK_WINDOW (window), -100, -100);
  gtk_window_resize (GTK_WINDOW (window), 10, 10);
  gtk_widget_show (window);

  return window;
}


static void
rectangle_found_cb (GdkRectangle *rectangle)
{
  if (rectangle != NULL) {

    the_data.rect = *rectangle;//structure copy
  }
 else
  the_data.rect.width = -1;
 gtk_main_quit ();
}

typedef struct {
  GdkRectangle rectangle;
  SelectAreaCallback callback;
  gboolean aborted;
} CallbackData;

static gboolean
emit_select_callback_in_idle (gpointer user_data)
{
  CallbackData *data = user_data;

  if (!data->aborted)
    data->callback (&data->rectangle);
  else
    data->callback (NULL);

  g_slice_free (CallbackData, data);

  return FALSE;
}


static void
empty_rectangle (gint x_root, gint y_root,
                          GdkRectangle *rect,
                          GdkRectangle *draw_rect)
{
  rect->x = x_root;
  rect->y = y_root;

  draw_rect->x = rect->x;
  draw_rect->y = rect->y;
  draw_rect->width  = 0;
  draw_rect->height = 0;
}

static void
fix_rectangle (gint x_root, gint y_root,
                            GdkRectangle *rect,
                            GdkRectangle *draw_rect,
                            GdkWindow    *root)
{
  rect->width  = ABS (rect->x - x_root);
  rect->height = ABS (rect->y - y_root);

  rect->x = MIN (rect->x, x_root);
  rect->y = MIN (rect->y, y_root);
}

static void
select_area_motion_action (GtkWidget *window,
                           GdkRectangle *rect,
                           GdkRectangle *pdraw_rect,
                           gint x_root, gint y_root)
{
  GdkRectangle draw_rect = (*pdraw_rect);

  gtk_window_move (GTK_WINDOW (window), draw_rect.x, draw_rect.y);
  gtk_window_resize (GTK_WINDOW (window), draw_rect.width, draw_rect.height);

  /* We (ab)use app-paintable to indicate if we have an RGBA window */
  if (!gtk_widget_get_app_paintable (window))
    {
      GdkWindow *gdkwindow = gtk_widget_get_window (window);

      /* Shape the window to make only the outline visible */
      if (draw_rect.width > 2 && draw_rect.height > 2)
        {
          cairo_region_t *region;
          GdkRectangle region_rect = {
            0, 0,
            draw_rect.width, draw_rect.height
          };

          region = cairo_region_create_rectangle (&region_rect);
          region_rect.x++;
          region_rect.y++;
          region_rect.width -= 2;
          region_rect.height -= 2;
          cairo_region_subtract_rectangle (region, &region_rect);

          gdk_window_shape_combine_region (gdkwindow, region, 0, 0);

          cairo_region_destroy (region);
        }
      else
        gdk_window_shape_combine_region (gdkwindow, NULL, 0, 0);
    }

   
  draw_rect.width  = ABS (rect->x - x_root);
  draw_rect.height = ABS (rect->y - y_root);

  draw_rect.x = MIN (rect->x, x_root);
  draw_rect.y = MIN (rect->y, y_root);
  
  g_print("... and Drew %d %d for %d, %d\n", draw_rect.x, draw_rect.y,
                        draw_rect.width, draw_rect.height);
  
}


select_area_button_press (GtkWidget               *window,
                          GdkEventButton          *event,
                          select_area_filter_data *data) {
  gdouble xroot, yroot;
  gint x_root, y_root;
  gdk_event_get_root_coords (event, &xroot, &yroot);
  x_root = (gint)xroot;
  y_root = (gint)yroot;
  gboolean left = (event->button!=3);
  if(left) {
    if (!data->button_pressed) {
        empty_rectangle (x_root, y_root,
                                    &data->rect, &data->draw_rect);//sets the origin, width, height 0
        data->button_pressed = TRUE;
    } else {
        fix_rectangle (x_root, y_root,
                                    &data->rect, &data->draw_rect,
                                    data->root);//sets the far corner                      
        gtk_main_quit ();
      }
  } else {
          gint x = x_root;
          gint y = y_root;
          GdkDisplay *disp = gdk_display_get_default();
          g_print("moving pointer to x %d y %d\n", data->rect.x, data->rect.y);
          gdk_display_warp_pointer (disp, gdk_display_get_default_screen (disp), data->rect.x, data->rect.y);
          data->rect.x = x;
          data->rect.y = y;
        }
  return TRUE;
}

static gboolean
select_area_motion_notify (GtkWidget               *window,
                           GdkEventMotion          *event,
                           select_area_filter_data *data) {     
  gdouble xroot, yroot;
  gint x_root, y_root;
  gdk_event_get_root_coords (event, &xroot, &yroot);
  x_root = (gint)xroot;
  y_root = (gint)yroot;
  select_area_motion_action (window,
                                   &data->rect, &data->draw_rect,
                                   x_root, y_root);//draws the rectangle
  return TRUE;

}
static gboolean
select_area_key_press (GtkWidget               *window,
                       GdkEventKey             *event,
                       select_area_filter_data *data){

 data->button_pressed = FALSE;    
 data->rect.x = 0;
 data->rect.y = 0;
 data->rect.width  = 0;
 data->rect.height = 0;
 gtk_main_quit ();
 return TRUE;
        
}

void
screenshot_select_area_async (SelectAreaCallback callback)
{
  GdkCursor *cursor;
  select_area_filter_data  data;
  CallbackData *cb_data;
  GdkDeviceManager *manager;
  GdkDevice *pointer, *keyboard;
  GdkGrabStatus res;

  data.rect.x = 0;
  data.rect.y = 0;
  data.rect.width  = 0;
  data.rect.height = 0;
  data.button_pressed = FALSE;
  data.aborted = FALSE;
  data.window = create_select_window();

  cb_data = g_slice_new0 (CallbackData);
  cb_data->callback = callback;

  g_signal_connect (data.window, "key-press-event", G_CALLBACK (select_area_key_press), &data);
  g_signal_connect (data.window, "button-press-event", G_CALLBACK (select_area_button_press), &data);
  g_signal_connect (data.window, "motion-notify-event", G_CALLBACK (select_area_motion_notify), &data);

  cursor = gdk_cursor_new (GDK_CROSSHAIR);
  manager = gdk_display_get_device_manager (gdk_display_get_default ());
  pointer = gdk_device_manager_get_client_pointer (manager);
  keyboard = gdk_device_get_associated_device (pointer);

  res = gdk_device_grab (pointer, gtk_widget_get_window (data.window),
                         GDK_OWNERSHIP_NONE, FALSE,
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK | 
                         GDK_BUTTON_RELEASE_MASK,
                         cursor, GDK_CURRENT_TIME);

  if (res != GDK_GRAB_SUCCESS)
    {
      g_object_unref (cursor);
      goto out;
    }

  res = gdk_device_grab (keyboard, gtk_widget_get_window (data.window),
                         GDK_OWNERSHIP_NONE, FALSE,
                         GDK_KEY_PRESS_MASK |
                         GDK_KEY_RELEASE_MASK,
                         NULL, GDK_CURRENT_TIME);
  if (res != GDK_GRAB_SUCCESS)
    {
      gdk_device_ungrab (pointer, GDK_CURRENT_TIME);
      g_object_unref (cursor);
      goto out;
    }

  gtk_main ();

  gdk_device_ungrab (pointer, GDK_CURRENT_TIME);
  gdk_device_ungrab (keyboard, GDK_CURRENT_TIME);

  gtk_widget_destroy (data.window);
  g_object_unref (cursor);

  gdk_flush ();

 out:
  cb_data->aborted = data.aborted;
  cb_data->rectangle = data.rect;

  /* FIXME: we should actually be emitting the callback When
   * the compositor has finished re-drawing, but there seems to be no easy
   * way to know that.
   */
  g_timeout_add (200, emit_select_callback_in_idle, cb_data);
}


gboolean
screenshot_select_area (int *px, int *py, int *pwidth, int *pheight){
  screenshot_select_area_async(rectangle_found_cb);
  gtk_main ();
if(the_data.rect.width>0) {
  *px = the_data.rect.x;
  *py = the_data.rect.y;
  *pwidth  = the_data.rect.width;
  *pheight = the_data.rect.height;
  the_data.rect.x += the_data.rect.width;
  the_data.rect.y += the_data.rect.height;
  return TRUE;
}
else
return FALSE;
}

GdkRectangle *
screenshot_find_rectangle (void)
{
  GdkRectangle *rectangle;
  rectangle = g_new0 (GdkRectangle, 1);
  if (screenshot_select_area (&rectangle->x, &rectangle->y,
                              &rectangle->width, &rectangle->height)) {
    if ((rectangle->width > 0) && (rectangle->height > 0))
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
  GdkPixbuf *screenshot = NULL;
  gint  x_orig, y_orig;
  gint width, real_width, height, real_height;
  root = gdk_get_default_root_window ();

  if (rectangle)
    {
      x_orig = rectangle->x;
      y_orig = rectangle->y;
      width  = rectangle->width;
      height = rectangle->height;
      if(width>0 && height>0)
        screenshot = gdk_pixbuf_get_from_window	(root, x_orig, y_orig, width, height);
   }
  return screenshot;
}
#else
GdkPixbuf *
screenshot_get_pixbuf (GdkWindow    *window,
                       GdkRectangle *rectangle)
{
g_warning("Not available on gtk3 yet\n");
return NULL;
}
#endif //remove to compile and test on gtk3


#else //gtk 2
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
#include <gtk/gtk.h>
#include <glib.h>
#ifdef G_OS_WIN32
#include "windows.h"
#else
#include <gdk/gdkx.h>
#endif




typedef struct {
  GdkRectangle  rect;
  GdkRectangle  draw_rect;
  gboolean      button_pressed;
  /* only needed because we're not using cairo to draw the rectangle */
  GdkWindow    *root;
  GdkGC        *gc;
} select_area_filter_data;


#ifdef G_OS_WIN32
#else
static void
empty_rectangle (XButtonEvent    *event,
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
fix_rectangle (XButtonEvent    *event,
                            GdkRectangle *rect,
                            GdkRectangle *draw_rect,
                            GdkWindow    *root,
                            GdkGC        *gc)
{
  /* do not remove the old rectangle as it shows you what you have captured so far */
  rect->width  = ABS (rect->x - event->x_root);
  rect->height = ABS (rect->y - event->y_root);

  rect->x = MIN (rect->x, event->x_root);
  rect->y = MIN (rect->y, event->y_root);
}

static void
select_area_motion_notify (XButtonEvent    *event,
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
#endif


static GdkFilterReturn
select_area_filter (GdkXEvent *gdk_xevent,
                    GdkEvent  *event,
                    gpointer   user_data)
{
  select_area_filter_data *data = user_data;
#ifdef G_OS_WIN32
  MSG *wevent = (MSG*) gdk_xevent;
g_print("Received event %x %x %x at %ld %ld\n", wevent->message, wevent->wParam, wevent->lParam, wevent->pt.x, wevent->pt.y);
return GDK_FILTER_REMOVE;
#else
  XEvent *xevent = (XEvent *) gdk_xevent;
  switch (xevent->type)
    {
    case ButtonPress:
      switch(xevent->xbutton.button) {
        case 1:
          if (!data->button_pressed) {
            empty_rectangle (&xevent->xbutton,
                                    &data->rect, &data->draw_rect);//sets the origin, width, height 0
            data->button_pressed = TRUE;
          } else {
            fix_rectangle (&xevent->xbutton,
                                    &data->rect, &data->draw_rect,
                                    data->root, data->gc);//sets the far corner                      
            gtk_main_quit ();
          }
          break;
        case 2:
        case 3:
        case 4: //scroll up
        case 5: //scroll down
         {
          gint x = xevent->xbutton.x_root;
          gint y = xevent->xbutton.y_root;
          GdkDisplay *disp = gdk_display_get_default();
          g_print("moving pointer to x %d y %d\n", data->rect.x, data->rect.y);
          gdk_display_warp_pointer (disp, gdk_display_get_default_screen (disp), data->rect.x, data->rect.y);
          data->rect.x = x;
          data->rect.y = y;
          break;
        }
        default:
        g_print("button %d\n", xevent->xbutton.button);
        //return GDK_FILTER_CONTINUE; no other application responds to the button press even with this return value.
        break;
      }
      return GDK_FILTER_REMOVE;
    case ButtonRelease:
      return GDK_FILTER_REMOVE;
    case MotionNotify:
      if (data->button_pressed)
        select_area_motion_notify (&xevent->xbutton,
                                   &data->rect, &data->draw_rect,
                                   data->root, data->gc);//draws the rectangle
      return GDK_FILTER_REMOVE;
    case KeyPress:
     // if (xevent->xkey.keycode == XKeysymToKeycode (gdk_display, XK_Escape)) let any key end - may need to re-instate this for Ctrl-press to join pixbufs
        {
          // this undraws in the wrong place, 
         // gdk_draw_rectangle (data->root, data->gc, FALSE, 
         //               data->rect.x - data->rect.width, data->rect.y - data->rect.height,
          //              data->rect.width, data->rect.height);
          data->button_pressed = FALSE;    
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
 #endif


  return GDK_FILTER_CONTINUE;
}

gboolean
screenshot_select_area (int *px, int *py, int *pwidth, int *pheight){
  GdkWindow               *root;
  GdkCursor               *cursor;
  static select_area_filter_data  data;
  GdkGCValues              values;
  GdkColor                 color;
#ifdef G_OS_WIN32
g_warning("Not available on windows, sorry");
return FALSE;
#endif
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

  if(data.button_pressed) {
    GdkDisplay *disp = gdk_display_get_default();
    //g_print("re-starting and moving pointer to x %d y %d\n", data.rect.x+data.rect.width, data.rect.y);
    gdk_display_warp_pointer (disp, gdk_display_get_default_screen (disp), data.rect.x+data.rect.width, data.rect.y-data.rect.height);
  }

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
  data.rect.x += data.rect.width;
  data.rect.y += data.rect.height;
  return TRUE;
}

GdkRectangle *
screenshot_find_rectangle (void)
{
  GdkRectangle *rectangle;
  rectangle = g_new0 (GdkRectangle, 1);
  if (screenshot_select_area (&rectangle->x, &rectangle->y,
                              &rectangle->width, &rectangle->height)) {
    if ((rectangle->width > 0) && (rectangle->height > 0))
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
  GdkPixbuf *screenshot = NULL;
  gint  x_orig, y_orig;
  gint width, real_width, height, real_height;
  root = gdk_get_default_root_window ();

  if (rectangle)
    {
      x_orig = rectangle->x;
      y_orig = rectangle->y;
      width  = rectangle->width;
      height = rectangle->height;
      if(width>0 && height>0)
        screenshot = gdk_pixbuf_get_from_drawable (NULL, root, NULL,
                                              x_orig, y_orig, 0, 0,
                                              width, height);
   }
  return screenshot;
}

#endif //gtk2 version
