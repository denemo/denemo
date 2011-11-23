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
#ifdef G_OS_WIN32
#include "windows.h"
#else
#include <gdk/gdkx.h>
#endif




typedef struct {
  cairo_rectangle_int_t  rect;
  cairo_rectangle_int_t  draw_rect;
  gboolean      button_pressed;
  /* only needed because we're not using cairo to draw the rectangle */
  GdkWindow    *root;
  cairo_t        *cr;
} select_area_filter_data;


#ifdef G_OS_WIN32
#else
static void
empty_rectangle (XButtonEvent    *event,
                          cairo_rectangle_int_t *rect,
                          cairo_rectangle_int_t *draw_rect)
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
                            cairo_rectangle_int_t *rect,
                            cairo_rectangle_int_t *draw_rect,
                            GdkWindow    *root,
                            cairo_t        *cr)
{
  /* do not remove the old rectangle as it shows you what you have captured so far */
  rect->width  = ABS (rect->x - event->x_root);
  rect->height = ABS (rect->y - event->y_root);

  rect->x = MIN (rect->x, event->x_root);
  rect->y = MIN (rect->y, event->y_root);
}

static void
select_area_motion_notify (XButtonEvent    *event,
                           cairo_rectangle_int_t *rect,
                           cairo_rectangle_int_t *draw_rect,
                           GdkWindow    *root,
                           cairo_t        *cr)
{
  /* FIXME: draw some nice rubberband with cairo if composited */

  /* remove the old rectangle */
  if (draw_rect->width > 0 && draw_rect->height > 0)
    //gdk_draw_rectangle (root, cr, FALSE, 
      //                  draw_rect->x, draw_rect->y,
        //FIXME                draw_rect->width, draw_rect->height);
  draw_rect->width  = ABS (rect->x - event->x_root);
  draw_rect->height = ABS (rect->y - event->y_root);

  draw_rect->x = MIN (rect->x, event->x_root);
  draw_rect->y = MIN (rect->y, event->y_root);

  /* draw the new rectangle */
  //if (draw_rect->width > 0 && draw_rect->height > 0)
  //  gdk_draw_rectangle (root, gc, FALSE, 
    //                    draw_rect->x, draw_rect->y,
      //                  draw_rect->width, draw_rect->height);
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
                                    data->root, data->cr);//sets the far corner                      
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
                                   data->root, data->cr);//draws the rectangle
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
//  GdkGCValues              values;
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
#if 0
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
#endif
//  data.gc = gdk_gc_new_with_values (root, &values,
//                                    GDK_GC_FUNCTION | GDK_GC_FILL |
//                                    GDK_GC_CLIP_MASK | GDK_GC_SUBWINDOW |
//                                    GDK_GC_CLIP_X_ORIGIN |
//                                    GDK_GC_CLIP_Y_ORIGIN | GDK_GC_EXPOSURES |
//                                    GDK_GC_LINE_WIDTH | GDK_GC_LINE_STYLE |
// FIXME                                    GDK_GC_CAP_STYLE | GDK_GC_JOIN_STYLE);
  gdk_color_parse ("white", &color);
//  gdk_gc_set_rgb_fg_color (data.gc, &color);
  gdk_color_parse ("black", &color);
//  gdk_gc_set_rgb_bg_color (data.gc, &color);

  if(data.button_pressed) {
    GdkDisplay *disp = gdk_display_get_default();
    //g_print("re-starting and moving pointer to x %d y %d\n", data.rect.x+data.rect.width, data.rect.y);
    gdk_display_warp_pointer (disp, gdk_display_get_default_screen (disp), data.rect.x+data.rect.width, data.rect.y-data.rect.height);
  }

  gtk_main ();

  g_object_unref (data.cr);

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

cairo_rectangle_int_t *
screenshot_find_rectangle (void)
{
  cairo_rectangle_int_t *rectangle;
  rectangle = g_new0 (cairo_rectangle_int_t, 1);
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
                       cairo_rectangle_int_t *rectangle)
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
        screenshot = 
#if GTK_CHECK_VERSION(3,0,0)
   gdk_pixbuf_get_from_window
	(root, x_orig, y_orig, width, height);
#else
   gdk_pixbuf_get_from_drawable
	(NULL, root, NULL, x_orig, y_orig, 0, 0, width, height);
#endif

   }
  return screenshot;
}

#if 0
typedef struct tagMSG {
  HWND   hwnd;
  UINT   message;//The message identifier. Applications can only use the low word
  //prefix = BCM, BCN, BM, and BN	Button control	Button Control Messages and Button Control Notifications
//#define WM_KEYDOWN                      0x0100
//#define WM_KEYUP                        0x0101
   WM_LBUTTONDOWN
        case WM_MOUSEMOVE: 

            // When moving the mouse, the user must hold down 
            // the left mouse button to draw lines. 
 
            if (wParam & MK_LBUTTON) 
   
  WPARAM wParam;Additional information about the message.
  LPARAM lParam;
  DWORD  time;
  POINT  pt;The cursor position, in screen coordinates, when the message was posted.
} MSG, *PMSG, *LPMSG;
#endif

