//      source.c
//      
//      Copyright 2012 Richard Shann 
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 3 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.
#include <string.h>
#include "source.h"
#include "view.h"
#include "utils.h"
#include <evince-view.h>

typedef struct fileview
{
  gchar *filename;
  EvView *view;
} fileview;

GList *theviews = NULL;
static void
set_window_position (EvView * view, gint x, gint y, gint page)
{
  EvDocumentModel *model = (EvDocumentModel *) g_object_get_data (G_OBJECT (view), "model");
  ev_document_model_set_page (model, page);
}


static void
get_window_position (EvView * view, gint * x, gint * y, gint * page, gdouble * scale)
{
  GtkAdjustment *adj = g_object_get_data (G_OBJECT (view), "hadj");
  *x = (gint) gtk_adjustment_get_value (adj);
  adj = g_object_get_data (G_OBJECT (view), "vadj");
  *y = gtk_adjustment_get_value (adj);
  EvDocumentModel *model = (EvDocumentModel *) g_object_get_data (G_OBJECT (view), "model");
  *scale = ev_document_model_get_scale (model);

  *page = ev_document_model_get_page (model);
}


#define MARKER (24)
static GdkRectangle Mark;
static GdkRectangle OldMark;
static gboolean
overdraw (cairo_t * cr, EvView * view)
{
  gint x, y, page;
  gdouble scale;
  get_window_position (view, &x, &y, &page, &scale);
  // cairo_scale( cr, Denemo.gui->si->preview_zoom, Denemo.gui->si->preview_zoom );
  cairo_translate (cr, -x, -y);
  if (Mark.width)
    {
      cairo_set_source_rgba (cr, 0.5, 0.5, 1.0, 0.5);
      cairo_rectangle (cr, Mark.x * scale, Mark.y * scale, MARKER, MARKER);     //this is not right once there is space outside the document visible inside the window.
      cairo_fill (cr);
    }
  if (OldMark.width)
    {
      cairo_set_source_rgba (cr, 1, 0.5, 0.5, 0.5);
      cairo_rectangle (cr, OldMark.x * scale, OldMark.y * scale, MARKER, MARKER);       //this is not right once there is space outside the document visible inside the window.
      cairo_fill (cr);
    }
  return TRUE;
}

#if GTK_MAJOR_VERSION==3
gint
draw_event (EvView * widget, cairo_t * cr)
{
  return overdraw (cr, widget);
}
#else
gint
draw_event (EvView * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw (cr, widget);
  cairo_destroy (cr);
  return TRUE;
}
#endif



static gint
button_press (EvView * view, GdkEventButton * event)
{
  if (event->button == 1)
    g_print ("Use right button to create link\n");
  else
    {
      gint x, y, page;
      gdouble scale;
      GdkRectangle candidate;
      candidate = Mark;
      get_window_position (view, &x, &y, &page, &scale);
      gchar *filename = g_object_get_data (G_OBJECT (view), "filename");
      x += event->x;
      y += event->y;
      gchar *escaped = g_strescape(filename, "");
      gchar *text = g_strdup_printf ("(InsertLink \"%s:%d:%d:%d\")", escaped, (gint) (x / scale), (gint) (y / scale), page);
      g_free(escaped);
      Mark.x = (x - MARKER / 2) / scale;
      Mark.y = (y - MARKER / 2) / scale;
      Mark.width = Mark.height = MARKER;
      if (!gdk_rectangle_intersect (&Mark, &candidate, NULL))
        OldMark = candidate;
      gtk_widget_queue_draw (GTK_WIDGET (view));
      call_out_to_guile (text);
      switch_back_to_main_window ();
    }
  return FALSE;
}

static void
next_page (GtkWidget * button, EvView * view)
{
  ev_view_next_page (view);
}

static void
prev_page (GtkWidget * button, EvView * view)
{
  ev_view_previous_page (view);
}


static gboolean
position_source_window (EvView * view)
{
  GtkWidget *top_vbox = gtk_widget_get_toplevel (GTK_WIDGET (view));
  if (Denemo.gui->source_scale)
    {
      //gtk_widget_set_size_request(GTK_WIDGET(top_vbox), Denemo.gui->source_width, Denemo.gui->source_height);
      //EvDocumentModel *model = (EvDocumentModel*)g_object_get_data(G_OBJECT(view), "model");
      //ev_document_model_set_scale(model, Denemo.gui->source_scale/1000.0);
      gtk_window_move (GTK_WINDOW (top_vbox), Denemo.gui->source_x, Denemo.gui->source_y);
    }
  else
    gtk_window_present (GTK_WINDOW (top_vbox));
  return FALSE;
}


static EvView *
get_view (gchar * filename)
{
  GFile *file;
  GError *err = NULL;
  EvView *view = NULL;
  GList *g;
  g_print("\nget_view(filename = %s)\n", filename);
  for (g = theviews; g; g = g->next)
    if (!strcmp (((fileview *) g->data)->filename, filename))
      return (((fileview *) g->data)->view);
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err) {
	  g_print("Error creating view from URI <%s> : message was %s\n", uri, err->message);
	  return NULL;
	}
  OldMark.width = Mark.width = 0;
  view = (EvView *) ev_view_new ();
  EvDocumentModel *model = ev_document_model_new_with_document (doc);
  //ev_document_model_set_continuous(model, FALSE);

  ev_view_set_model (view, model);
  GtkWidget *top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (top_vbox), g_strdup_printf ("Denemo - Source File: %s", filename));

#if 0
  if (Denemo.gui->source_scale)
    {
      ! ! ! !nooooo, get_window_position is getting the vadj, hadj settings, ie where it is scrolled to we need to get the position of the root window top vbox on the desk top, and its width and height, possibly the scale will follow ... ! ! ! ! ! ! ! !this is the gtk window widget not the gdk window ! gtk_window_move (GtkWindow * window, gint x, gint y);
      gtk_window_get_position (GtkWindow * window, gint * root_x, gint * root_y);
      nooo set_window_position (view, gui->source_x, gui->source_y, gui->source_page);
    scale}
  else
    gtk_widget_set_size_request (GTK_WIDGET (top_vbox), 600, 750);
#else

  g_idle_add ((GSourceFunc) position_source_window, view);
//gtk_window_move (GTK_WINDOW(top_vbox), Denemo.gui->source_x, Denemo.gui->source_y);
//gtk_widget_set_size_request(GTK_WIDGET(top_vbox), 600, 750);
  if (Denemo.gui->source_scale)
    gtk_window_set_default_size (GTK_WINDOW (top_vbox), Denemo.gui->source_width, Denemo.gui->source_height);
  else
    gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);
#endif

  g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (gtk_widget_hide), NULL);

#if GTK_MAJOR_VERSION==3
  g_signal_connect_after (G_OBJECT (view), "draw", G_CALLBACK (draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (view), "expose_event", G_CALLBACK (draw_event), NULL);
#endif
  g_signal_connect (G_OBJECT (view), "button_press_event", G_CALLBACK (button_press), NULL);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
//FIXM put next etc buttons in hbox
  GtkWidget *button = gtk_button_new_with_label ("Next");
  g_signal_connect (button, "clicked", G_CALLBACK (next_page), (gpointer) view);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label ("Previous");
  g_signal_connect (button, "clicked", G_CALLBACK (prev_page), (gpointer) view);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);



  GtkAdjustment *viewvadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_vscrollbar_new (GTK_ADJUSTMENT (viewvadjustment));

  GtkAdjustment *viewhadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_hscrollbar_new (GTK_ADJUSTMENT (viewhadjustment));

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (viewhadjustment, viewvadjustment);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (score_and_scroll_hbox), GTK_WIDGET (view));
  g_object_set_data (G_OBJECT (view), "vadj", viewvadjustment);
  g_object_set_data (G_OBJECT (view), "hadj", viewhadjustment);
  g_object_set_data (G_OBJECT (view), "filename", g_strdup (filename));
  g_object_set_data (G_OBJECT (view), "model", model);

  gtk_widget_show_all (top_vbox);

  fileview *theview = (fileview *) g_malloc (sizeof (fileview));
  theview->filename = g_strdup (filename);
  theview->view = view;
  theviews = g_list_append (theviews, theview);

  return view;
}

static gboolean
position_view (EvView * eview, gint x, gint y, gint page)
{
  if (eview == NULL)
    return FALSE;
  set_window_position (eview, x, y, page);
  Mark.width = Mark.height = MARKER;
  Mark.x = x - MARKER / 2;
  Mark.y = y - MARKER / 2;
  OldMark.width = 0;
  gtk_widget_show (gtk_widget_get_toplevel (GTK_WIDGET (eview)));
  gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (eview))));
  return TRUE;
}

gboolean
open_source (gchar * filename, gint x, gint y, gint page)
{
  EvView *eview = get_view (filename);
  gboolean ret = position_view (eview, x, y, page);
  switch_back_to_main_window ();
  return ret;
}

#if 0
//Finds the scale and position of the (first) source file. Returns FALSE if none
gboolean
source_position (gint * x, gint * y, gint * width, gint * height, gdouble * scale)
{
  if (theviews == NULL)
    return FALSE;

  eview = (fileview *) theviews->data)->view;
  gtk_window_get_position (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (eview))), &x, &y);
  gdk_window_get_root_origin (GTK_WIDGET (eview))->window, x, y);
  gtk_window_get_size (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (eview)), &width, &height); EvDocumentModel * model = (EvDocumentModel *) g_object_get_data (G_OBJECT (view), "model"); *scale = ev_document_model_get_scale (model);
                       //or ???? gdk_window_get_width(gtk_widget_get_toplevel(GTK_WIDGET(eview))->window, width);
                       //          gdk_window_get_height(gtk_widget_get_toplevel(GTK_WIDGET(eview))->window, height);
                       // get_window_position(eview, x, y, page, scale);nooo
                       return TRUE;}
#else
gboolean
source_position (gint * x, gint * y, gint * width, gint * height, gint * scale)
{
  if (theviews == NULL)
    return FALSE;
  EvView *view = ((fileview *) theviews->data)->view;
  GtkWindow *top = (GtkWindow *) gtk_widget_get_toplevel (GTK_WIDGET (view));

  gtk_window_get_position (top, x, y);
  gtk_window_get_size (top, width, height);
  EvDocumentModel *model = (EvDocumentModel *) g_object_get_data (G_OBJECT (view), "model");
  *scale = (int) 1000 *ev_document_model_get_scale (model);
  return TRUE;
}
#endif
