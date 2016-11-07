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
#include "source/source.h"
#include "core/view.h"
#include "core/utils.h"
#include <evince-view.h>

static gboolean Dragging;
static GdkPoint DragStart, DragEnd;

typedef struct fileview
{
  gchar *filename;
  EvView *view;
  GList *highlights;//data are GdkRectangle*
} fileview;

static GList *FileViews = NULL;
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

static void
get_window_size (EvView* view, gint * w, gint * h)
{
  GdkWindow *window;
  GtkWidget *top_vbox = gtk_widget_get_toplevel (GTK_WIDGET (view));
  if (!GTK_IS_LAYOUT (top_vbox))
    window = gtk_widget_get_window (GTK_WIDGET (top_vbox));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (top_vbox));
  if (window)
    {
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (view), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      
#if GTK_MAJOR_VERSION==2
      gdk_drawable_get_size (window, w, h);
#else
      *w = gdk_window_get_width (window);
      *h = gdk_window_get_height (window);
#endif
      *w *= scale;
      *h *= scale;
    }
}




static gboolean
overdraw (cairo_t * cr, GtkWidget* view)
{
  gint x, y, page;
  gdouble scale;
  get_window_position ((EvView*)view, &x, &y, &page, &scale);
  // cairo_scale( cr, Denemo.project->movement->preview_zoom, Denemo.project->movement->preview_zoom );
  cairo_translate (cr, -x, -y);
  fileview *theview = g_object_get_data (G_OBJECT(view), "fileview");
  GList *Highlights = theview->highlights;
  if (Highlights)
    {
        GList *g;
        for (g=Highlights;g;g = g->next)
            {
               GdkRectangle *r = (GdkRectangle*)g->data;
               cairo_set_source_rgba (cr, 0.5, 0.5, 0.5, 0.5);
               cairo_rectangle (cr, r->x * scale, r->y * scale, abs(r->width)* scale, abs(r->height)* scale);
               // cairo_rectangle (cr, r->x , r->y , abs(r->width), abs(r->height));
               cairo_fill (cr); // cairo_clip (cr);//
            }
    }
  if (Dragging)
    {
        gdouble xx = MIN (DragStart.x, DragEnd.x);
        gdouble yy = MIN (DragStart.y, DragEnd.y);
      cairo_set_source_rgba (cr, 0.5, 0.5, 0.5, 0.5);
      cairo_rectangle (cr, xx * scale, yy * scale, abs(DragStart.x-DragEnd.x) * scale, abs(DragStart.y-DragEnd.y) * scale);
      cairo_fill (cr);
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_rectangle (cr, xx * scale, yy * scale, abs(DragStart.x-DragEnd.x) * scale, abs(DragStart.y-DragEnd.y) * scale);   
      cairo_stroke (cr);
    }
  

  if (Mark.width)
    {
      cairo_set_source_rgba (cr, 0.5, 0.5, 1.0, 0.5);
      cairo_rectangle (cr, Mark.x * scale, Mark.y * scale, MARKER, MARKER);     //this is not right once there is space outside the document visible inside the window.
      cairo_fill (cr);
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_rectangle (cr, Mark.x * scale, Mark.y * scale, MARKER, MARKER); 
      cairo_stroke (cr);
    }
  if (OldMark.width)
    {
      cairo_set_source_rgba (cr, 1, 0.5, 0.5, 0.5);
      cairo_rectangle (cr, OldMark.x * scale, OldMark.y * scale, MARKER, MARKER);       //this is not right once there is space outside the document visible inside the window.
      cairo_fill (cr);
      cairo_set_source_rgb (cr, 0, 0, 0);
      cairo_rectangle (cr, OldMark.x * scale, OldMark.y * scale, MARKER, MARKER);      
      cairo_stroke (cr);
    }
    
  return TRUE;
}

#if GTK_MAJOR_VERSION==3
static gint
draw_event (GtkWidget * view, cairo_t * cr)
{
  return overdraw (cr, view);
}

#else
static gint
draw_event (GtkWidget * view, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw (cr, view);
  cairo_destroy (cr);
  return TRUE;
}

#endif

static GList *locate_highlight (GtkWidget *view, gint x, gint y)
{
    fileview *theview = g_object_get_data (G_OBJECT(view), "fileview");
    GList *g;
    for (g = theview->highlights; g; g = g->next)
        {
            GdkRectangle *rect = (GdkRectangle *)g->data;
            if (x > rect->x && (x< rect->x+rect->width) && (y> rect->y) && (y < rect->y + rect->height))
                return g;
        }
  return NULL;  
}

static void remove_highlight (GtkWidget *menu, GList *highlight)
{
    if (highlight)
        {
            fileview *theview = g_object_get_data (G_OBJECT(menu), "fileview");
            g_free (highlight->data);
            theview->highlights = g_list_delete_link (theview->highlights, highlight); 
        }         
}

static void remove_highlights (GtkWidget *view)
{
    fileview *theview = g_object_get_data (G_OBJECT(view), "fileview");
    g_list_free_full (theview->highlights, g_free);
    theview->highlights = NULL;
}

static void help (void)
 {
     infodialog (_("To insert a link at the Denemo cursor position to a point in this document\nright-click on the point.\nLater you will be able to re-open the document at that point by right clicking on the link in the Denemo display.\nTo shade in gray parts of the source that you don't want to see drag over the area.\nUse this for transcribing from a score with many parts to ease following the part from system to system.\nClick on a grayed-out patch to remove it."));
 }
        
static gboolean
motion_notify (EvView * view, GdkEventMotion * event)
{
 if (Dragging)
    {gint x, y, page;
      gdouble scale;
      get_window_position (view, &x, &y, &page, &scale);
      x += event->x;
      y += event->y;
      DragEnd.x = x/scale;
      //DragEnd.x = x;///scale;
      DragEnd.y = y/scale;
      //DragEnd.y = y;//y/scale;
      gtk_widget_queue_draw (GTK_WIDGET (view));
    }
  return TRUE;  
}
static gint
button_release (EvView * view, GdkEventButton * event)
{
    fileview *theview = g_object_get_data (G_OBJECT(view), "fileview");
    if (Dragging && ((abs(DragEnd.x-DragStart.x)>5) || (abs(DragEnd.y-DragStart.y)>5))) //do not allow very small patches, difficult to remove
        {
          GdkRectangle *r = g_malloc (sizeof (GdkRectangle));
          r->x = MIN(DragStart.x, DragEnd.x);
          r->y = MIN(DragStart.y, DragEnd.y);
          r->width = abs (DragStart.x-DragEnd.x);
          r->height = abs (DragStart.y-DragEnd.y);
          theview->highlights = g_list_append (theview->highlights, r);
        }
   else if (event->button==1)
        {
            static gboolean once;
            if (!once)
                help ();
            once = TRUE;
        }
    Dragging = FALSE;
    gtk_widget_queue_draw (GTK_WIDGET (view));
    return TRUE;  
}

    
static void popup_highlight_menu (GtkWidget *view, GList *highlight, GdkEventButton *event)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item = gtk_menu_item_new_with_label (_("Help"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (help), NULL);

  item = gtk_menu_item_new_with_label (_("Remove this Shading"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_object_set_data (G_OBJECT (item), "fileview", g_object_get_data (G_OBJECT (view), "fileview"));
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (remove_highlight), highlight);

  item = gtk_menu_item_new_with_label (_("Remove all Shading"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (remove_highlights), view);
  gtk_widget_show_all (menu);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, event->time);
}

static gint
button_press (EvView * view, GdkEventButton * event)
{
  if (event->button == 1)
    {
      gint x, y, page;
      gdouble scale;
      get_window_position (view, &x, &y, &page, &scale);

      x += event->x;
      y += event->y;
      
      GList *highlight = locate_highlight (GTK_WIDGET(view), (gint)(x/scale), (gint)(y/scale));
      
      if (highlight)
        {
            popup_highlight_menu (GTK_WIDGET(view), highlight, event); 
        }
    else
        {
      
          DragStart.x = x/scale;
          DragStart.y = y/scale;
          DragEnd.x = DragStart.x;
          DragEnd.y = DragStart.y;
          Dragging = TRUE; 
        }
    }
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
      
            GList *highlight = locate_highlight (GTK_WIDGET(view), (gint)(x/scale), (gint)(y/scale));
      
      if (highlight)
        {
            popup_highlight_menu (GTK_WIDGET(view), highlight, event); 
        }
    else
        {
      
      
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
  if (Denemo.project->source_scale)
    {
      //gtk_widget_set_size_request(GTK_WIDGET(top_vbox), Denemo.project->source_width, Denemo.project->source_height);
      //EvDocumentModel *model = (EvDocumentModel*)g_object_get_data(G_OBJECT(view), "model");
      //ev_document_model_set_scale(model, Denemo.project->source_scale/1000.0);
      gtk_window_move (GTK_WINDOW (top_vbox), Denemo.project->source_x, Denemo.project->source_y);
    }
  else
    gtk_window_present (GTK_WINDOW (top_vbox));
  return FALSE;
}

static gchar *locate_file (gchar *filename) {
    if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
     gchar *basename = g_path_get_basename(filename);
     gchar *pathdir = g_path_get_dirname (Denemo.project->filename->str);
     filename = g_build_filename (pathdir, basename, NULL);
     g_free(basename);
     g_free(pathdir);
    }
    return filename;
}
static EvView *
get_view (gchar * filename)
{
  GFile *file;
  GError *err = NULL;
  EvView *view = NULL;
  GList *g;
  filename = locate_file (filename);
  for (g = FileViews; g; g = g->next)
    if (!strcmp (((fileview *) g->data)->filename, filename))
      return (((fileview *) g->data)->view);
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err) {
      g_critical("Error creating view from URI <%s> : message was %s", uri, err->message);
      return NULL;
    }
  OldMark.width = Mark.width = 0;
  
  //g_list_free_full (Highlights, g_free);
  //Highlights = NULL;
  
  view = (EvView *) ev_view_new ();
  EvDocumentModel *model = ev_document_model_new_with_document (doc);
  //ev_document_model_set_continuous(model, FALSE);

  ev_view_set_model (view, model);
  GtkWidget *top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  // use a dialog when the user clicks instead  gtk_widget_set_tooltip_text (top_vbox, HELP_TEXT);

  gtk_window_set_title (GTK_WINDOW (top_vbox), g_strdup_printf ("Denemo - Source File: %s", filename));


  g_idle_add ((GSourceFunc) position_source_window, view);
  
  if (Denemo.project->source_scale)
    gtk_window_set_default_size (GTK_WINDOW (top_vbox), Denemo.project->source_width, Denemo.project->source_height);
  else
    gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);

  g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (gtk_widget_hide), NULL);


#if GTK_MAJOR_VERSION==3
  g_signal_connect_after (G_OBJECT (view), "draw", G_CALLBACK (draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (view), "expose_event", G_CALLBACK (draw_event), NULL);
#endif



  g_signal_connect (G_OBJECT (view), "button_press_event", G_CALLBACK (button_press), NULL);
  g_signal_connect (G_OBJECT (view), "button_release_event", G_CALLBACK (button_release), NULL);
  g_signal_connect (G_OBJECT (view), "motion_notify_event", G_CALLBACK (motion_notify), NULL);

  
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

  fileview *theview = (fileview *) g_malloc0 (sizeof (fileview));
  theview->filename = g_strdup (filename);
  theview->view = view;
  g_object_set_data (G_OBJECT (view), "fileview", theview);
  FileViews = g_list_append (FileViews, theview);

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


//Finds the scale and position of the window (first) source file. Returns the Denemo.project->scale_* values if the window is not visible. Returns FALSE if none
//FIXME, it would be better to set Denemo.project->scale_* values in a "configure" callback on the window.
gboolean
source_position (gint * x, gint * y, gint * width, gint * height, gint * scale)
{
  if (FileViews == NULL)
    return FALSE;
  EvView *view = ((fileview *) FileViews->data)->view;
  GtkWindow *top = (GtkWindow *) gtk_widget_get_toplevel (GTK_WIDGET (view));
  if(gtk_widget_get_visible(GTK_WIDGET(top)))
  {
          gtk_window_get_position (top, x, y);
          gtk_window_get_size (top, width, height);
          EvDocumentModel *model = (EvDocumentModel *) g_object_get_data (G_OBJECT (view), "model");
          *scale = (int) 1000 *ev_document_model_get_scale (model);
          if(!*scale) g_warning("Scale of document is zero!!!");
  } else {
        *x = Denemo.project->source_x;
        *y = Denemo.project->source_y;
        *scale = Denemo.project->source_scale;
  }

  return TRUE;
}

