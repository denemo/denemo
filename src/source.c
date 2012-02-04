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

#include "source.h"
#include <evince-view.h>

typedef struct fileview { gchar *filename; EvView *view;} fileview;

GList *theviews = NULL;
static void set_window_position(EvView *view, gint x, gint y) {
 GtkAdjustment * adj = g_object_get_data(G_OBJECT(view), "vadj");
 if(adj) {
	gtk_adjustment_set_value(adj, x);
	gtk_adjustment_changed(adj);
 }
 adj = g_object_get_data(G_OBJECT(view), "hadj");
 if(adj) {
	gtk_adjustment_set_value(adj, y);
	gtk_adjustment_changed(adj);
 }
}
static EvView *get_view(gchar *filename) {
	GFile *file;
	GError *err = NULL;
	EvView *view=NULL;
	GList *g;
	for(g=theviews;g;g=g->next)
		if(!strcmp(((fileview *)g->data)->filename, filename))
			return (((fileview*)g->data)->view);
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if(err)
		return NULL;
	view = (EvView*)ev_view_new();
	EvDocumentModel  *model = ev_document_model_new_with_document(doc);
  ev_view_set_model(view, model);
  	GtkWidget *top_vbox = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(top_vbox), filename);
  g_signal_connect (G_OBJECT (top_vbox), "delete-event",
		    G_CALLBACK (gtk_widget_hide), NULL);
		    
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox =  gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox,FALSE, TRUE, 0);
//FIXM put next etc buttons in hbox

  GtkAdjustment *viewvadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  GtkWidget *viewvscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (viewvadjustment));
		     
  GtkAdjustment *viewhadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  GtkWidget *viewhscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (viewhadjustment));

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (viewhadjustment, viewvadjustment);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);
	gtk_container_add (GTK_CONTAINER(score_and_scroll_hbox), GTK_WIDGET(view));
	g_object_set_data(G_OBJECT(view), "vadj", viewvadjustment);
	g_object_set_data(G_OBJECT(view), "hadj", viewhadjustment);

  
	gtk_widget_show_all(top_vbox);

  
  fileview *theview = (fileview*)g_malloc(sizeof(fileview));
  theview->filename = g_strdup(filename);
  theview->view = view;
  theviews = g_list_append(theviews, theview);
  return view;

}

static gboolean position_view(EvView* eview, gint x, gint y) {
	if(eview==NULL)
		return FALSE;
return TRUE;
return FALSE;
}
gboolean open_source(gchar *filename, gint x, gint y) {

g_print("opening %s at %d %d (not implemented yet)\n", filename, x, y);
EvView* eview = get_view(filename);
return position_view(eview, x, y);
}
