//      proof.c
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
#include "source/proof.h"
#include "core/view.h"
#include "core/utils.h"
#include <evince-view.h>
static gint
action_for_link (G_GNUC_UNUSED EvView * view, EvLinkAction * obj);

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
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err) {
      g_critical("Error creating view from URI <%s> : message was %s", uri, err->message);
      return NULL;
    }
  view = (EvView *) ev_view_new ();
  EvDocumentModel *model = ev_document_model_new_with_document (doc);
  ev_view_set_model (view, model);
  GtkWidget *top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_set_tooltip_text (top_vbox, _("To locate a note in the Denemo Display click on the note head."));

  gtk_window_set_title (GTK_WINDOW (top_vbox), g_strdup_printf ("Denemo - Proof-Read File: %s", filename));
  gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label ("Next");
  g_signal_connect (button, "clicked", G_CALLBACK (next_page), (gpointer) view);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label ("Previous");
  g_signal_connect (button, "clicked", G_CALLBACK (prev_page), (gpointer) view);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);

  g_signal_connect (G_OBJECT (view), "external-link", G_CALLBACK (action_for_link), NULL);

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (score_and_scroll_hbox), GTK_WIDGET (view));

  gtk_widget_show_all (top_vbox);

  return view;
}


gboolean
open_proofread_file (gchar * filename)
{
  EvView *eview = get_view (filename);
 return eview  != NULL;
}

//signal handler for link
static gint
action_for_link (G_GNUC_UNUSED EvView * view, EvLinkAction * obj)
{
 gchar *uri = (gchar *) ev_link_action_get_uri (obj);
 if(get_print_status()->updating_id &&  (get_print_status()->typeset_type != TYPESET_ALL_MOVEMENTS))
   {
    warningdialog (_("Cannot do location when only a range of music is typeset. Turn off continuous typesetting or set the range to All Movements"));
    return TRUE;
    }
    
  if (uri)
    {
      gchar **orig_vec = g_strsplit (uri, ":", 6);
      gchar **vec = orig_vec;
      if (vec[0] && vec[1] && vec[2] && vec[3] && vec[4] && vec[5] && *vec[5])
        vec++;//this will be the case where the file name has a colon in it, (windows drive name) we do not allow for more than one colon. vec[0] is used hereafter.
      if (g_str_has_prefix (uri, "textedit:") && vec[1] && vec[2] && vec[3])
        {
          DenemoTarget old_target = Denemo.project->movement->target;
          gboolean ObjectLocated = goto_lilypond_position (atoi (vec[2]), atoi (vec[3]));     //sets si->target
          if (!ObjectLocated)
            {
            g_debug ("Object not located\n");
            }
        }
    }
    return TRUE;
}   





