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
#include <evince-document.h>

static const gchar *nearest_annotation_text = NULL;
static GList *annotated_pages = NULL;
static GList *current_page;
static GtkWidget *top_window;
static gchar *help_text = NULL;

//signal handler for link
static gint
action_for_link (EvView * view, EvLinkAction * obj, EvDocumentModel *model)
{
    EvDocument *doc = ev_document_model_get_document (model);
 gchar *uri = (gchar *) ev_link_action_get_uri (obj);
 if(Denemo.printstatus->updating_id &&  (Denemo.printstatus->typeset_type != TYPESET_ALL_MOVEMENTS))
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
          if (ObjectLocated && nearest_annotation_text)
            { DenemoScriptParam param;
                param.string = g_string_new (nearest_annotation_text);
                paste_comment (NULL, &param);
                g_string_free (param.string, TRUE);
            }
           else {
                warningdialog (_("Object not located, no annotation on page, or empty annotation.\n"));
            }
        }
    }
    return TRUE;
}

static void
next_page (GtkWidget * button, EvDocumentModel *model)
{
    if (current_page->next)
    {
        current_page = current_page->next;
    }
    else
    {
        current_page = annotated_pages;
    }
    ev_document_model_set_page (model, GPOINTER_TO_INT(current_page->data));

}

static void
prev_page (GtkWidget * button, EvDocumentModel *model)
{
    if (current_page->prev)
    {
        current_page = current_page->prev;
    }
    else
    {
        current_page = g_list_last (annotated_pages);
    }
    ev_document_model_set_page (model, GPOINTER_TO_INT(current_page->data));
}
static gboolean
press (EvView * view,  GdkEventButton  *event, EvDocumentModel *model)
{
    EvDocument *doc = ev_document_model_get_document (model);

    gint i = ev_document_model_get_page (model);
    if (event->button != 1)
        infodialog (help_text);
    nearest_annotation_text = NULL;
            extern EvMappingList * ev_document_annotations_get_annotations();
            EvMappingList *mapping_list = ev_document_annotations_get_annotations (doc, ev_document_get_page(doc, i));
            if(mapping_list)
            {
            gdouble nearest = G_MAXDOUBLE;
            GList *g = ev_mapping_list_get_list (mapping_list);

            for (;g;g=g->next)
                {
                EvMapping *mapping = g->data;
                EvAnnotation *annot = mapping->data;
                gdouble annottx = ev_document_model_get_scale (model)*(mapping->area.x1 + mapping->area.x2)/2;
                gdouble annotty = ev_document_model_get_scale (model)*(mapping->area.y1 + mapping->area.y2)/2;
                gdouble dist = (annottx-event->x)*(annottx-event->x) + (annotty-event->y)*(annotty-event->y);

                if(dist < nearest) {
                    nearest = dist;
                    nearest_annotation_text = ev_annotation_get_contents (annot);
                    }
                }
            }
  //g_print("\n\npress signal (%f, %f) %s\n", event->x, event->y, nearest_annotation_text);
  return   FALSE;
}

static gboolean
find_annotated_pages (EvDocumentModel *model)
{
    EvDocument *doc = ev_document_model_get_document (model);

    gint i;
    for (i=0; i< ev_document_get_n_pages(doc);i++)
        {
            extern EvMappingList * ev_document_annotations_get_annotations();
            EvMappingList *mapping_list = ev_document_annotations_get_annotations (doc, ev_document_get_page(doc, i));
            if(mapping_list)
            {
             annotated_pages = g_list_append (annotated_pages, GINT_TO_POINTER(i));
            }
        }
  return  (annotated_pages != NULL);
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
  help_text = _("For each annotation on the page click on the (nearby) notehead or rest etc that the annotation refers to. This will insert a comment in the score. Transfer all the annotations in this way before editing the score, otherwise the locations will not match. You can use the EditSimilar (Ctrl-e,e and Ctrl-e,r) command to move from one comment to the next, stopping and editing the score as suggested by the comment.");
  filename = locate_file (filename);
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err) {
      g_critical("Error creating view from URI <%s> : message was %s", uri, err->message);
      return NULL;
    }
  if (annotated_pages)
    {
        gtk_widget_destroy (top_window);
        top_window = NULL;
        g_list_free (annotated_pages);
        current_page = annotated_pages = NULL;
    }
  view = (EvView *) ev_view_new ();
  EvDocumentModel *model = ev_document_model_new_with_document (doc);
#ifndef EV_SIZING_FIT_PAGE
#define EV_SIZING_FIT_PAGE EV_SIZING_BEST_FIT
#endif
  ev_document_model_set_sizing_mode (model, EV_SIZING_FIT_PAGE);
  ev_document_model_set_continuous (model, FALSE);
  ev_view_set_model (view, model);

  if (find_annotated_pages (model))
  {
  current_page = annotated_pages;
  ev_document_model_set_page (model, GPOINTER_TO_INT(current_page->data));
  top_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_set_tooltip_text (top_window, help_text);
  gtk_window_set_title (GTK_WINDOW (top_window), g_strdup_printf ("Denemo - Proof-Read File: %s", filename));
  gtk_window_set_default_size (GTK_WINDOW (top_window), 600, 750);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (top_window), main_vbox);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Next Annotated Page"));
  g_signal_connect (button, "clicked", G_CALLBACK (next_page), (gpointer) model);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Previous Annotated Page"));
  g_signal_connect (button, "clicked", G_CALLBACK (prev_page), (gpointer) model);
  gtk_box_pack_start (GTK_BOX (main_hbox), button, FALSE, TRUE, 0);

  g_signal_connect (G_OBJECT (view), "external-link", G_CALLBACK (action_for_link), (gpointer)model);
  g_signal_connect (G_OBJECT (view), "button-press-event", G_CALLBACK (press), (gpointer)model);

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (score_and_scroll_hbox), GTK_WIDGET (view));
  gtk_widget_show_all (top_window);
  gtk_window_present (GTK_WINDOW (top_window)); //this doesn't appear to work...

  return view;
  } else
  {
    warningdialog (_("This PDF file contains no annotations. It has to be a PDF file generated by Denemo for the current score to which annotations have been added."));
    return NULL;
  }
}


gboolean
open_proofread_file (gchar * filename)
{
  if (Denemo.non_interactive)
    return FALSE;
  EvView *eview = get_view (filename);
 return eview  != NULL;
}









