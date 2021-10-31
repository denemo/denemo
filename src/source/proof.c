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
#include <math.h>
#include "source/proof.h"
#include "core/view.h"
#include "core/utils.h"
#ifdef USE_ATRIL
#include <atril-view.h>
#include <atril-document.h>
#else
#include <evince-view.h>
#include <evince-document.h>
#endif

static const gchar *nearest_annotation_text = NULL;
static GList *annotated_pages = NULL;
static GList *current_page;
static GtkWidget *top_window;
static gchar *help_text = NULL;

//Warning - this structure has to be sync'd with the definition in the (independent) twopageturner program - see tools/twopageturner.c
//it is not compatible with the older pageturner.c program
typedef struct Annotation {
     gchar *annotation;
     gint page;
     gint x, y;
     double r,g,b,a;
     gchar *font;
} Annotation;

static  gint width=720;//width and
static  gint height=1030;//height of proof read screen
static GtkAdjustment *VAdj;//vertical scroll of the score
static GList *annotations = NULL;// a list of annotations to be displayed on the score
static GList *current_annotation = NULL;//last visited annotation
static gchar *markings_file = NULL;//full path to file for storing repeat marks and annotations for currently loaded pdf
static EvDocumentModel *model;
static void free_annotation (Annotation *a)
{
   g_free (a->annotation);
   g_free (a);
}
//load annotations from the file associated with the opened pdf score
static void load_markings (gchar *pdfname)
{
   g_list_free_full (annotations, (GDestroyNotify)free_annotation);
   annotations = NULL;
   if (markings_file) 
      g_free (markings_file);
   markings_file = g_strdup_printf ("%s%s", pdfname, ".marks");
   FILE *fp = fopen (markings_file, "r");
      if (fp)
      {
         if (2==fscanf (fp, "%d%d", &width, &height))
            {
               gint page, x, y;  
               gchar text1[100], text2[100];
               *text1 = *text2 = 0;
               double r,g,b,a;   
               while (7 == fscanf (fp, "%d%d%d%lf%lf%lf%lf\n", &page, &x, &y, &r, &g, &b, &a))
                                 {
                                    Annotation *ann = (Annotation*)g_malloc (sizeof (Annotation));
                                    ann->x = x;
                                    ann->y = y;
                              
                                    ann->page = page;
                                    ann->r = r;
                                    ann->g = g;
                                    ann->b = b;
                                    ann->a = a;
                                    if (fgets (text1, 100, fp))
                                       ann->font = g_strdup (text1);
                                    else
                                       ann->font = g_strdup ("Times 12\n");  
                                    
                                    if (fgets (text2, 100, fp))
                                       ann->annotation = g_strdup (text2);
                                    else
                                       ann->annotation = g_strdup ("???\n"); 
                                    *(ann->font + strlen (ann->font) - 1) = 0;   
                                    *(ann->annotation + strlen (ann->annotation) - 1) = 0;   
                                          
                                    annotations = g_list_append (annotations, ann);
                                 } 
            }
         else  g_warning ("Corrupt markings file");
        fclose (fp);
      }
}


static gboolean overdraw (GtkWidget* view, cairo_t * cr)
{
   GList *g;
   static gboolean phase = FALSE;
   cairo_set_source_rgba (cr,0, 0, 0, 0.5);
   general_draw_text (cr, "Sans 20", _("Right click for menu"), 10, 10);
    for (g = annotations; g; g=g->next)
      {
         Annotation *ann = (Annotation*)g->data;
         gdouble x = ann->x, y = ann->y; 
         y -= (int)gtk_adjustment_get_value (VAdj) - (ann->page * gtk_adjustment_get_page_size (VAdj));
          if (ann->page == ev_document_model_get_page (model))
            {
               g==current_annotation?  
                  (phase?cairo_set_source_rgba (cr, 1.0, 0.2, 0.2, 0.5):
                         cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 1)):
                  cairo_set_source_rgba (cr, 0.3, 0.4, 1.0, 0.5);

               general_draw_text (cr, ann->font, ann->annotation, x, y);
               if (phase && (g==current_annotation))
                {
                  cairo_set_source_rgba (cr, 0.0, 0.2, 1.0, 0.5);
                  cairo_arc (cr, x, y, 40.0, 0.0, 2 * M_PI);
                  cairo_fill (cr);
                }
            }
      }
  phase = !phase;
  return FALSE;
}

static gboolean refresh_draw (void) {

  if (GTK_IS_WIDGET (top_window))
    {
      gtk_widget_queue_draw(top_window);
      return TRUE;
    }
  return FALSE;
  
}
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
                if (annotations && !ObjectLocated)
                    warningdialog (_("Object not located, no annotation on page, or empty annotation.\n"));
            }
        }
    }
    return TRUE;
}

static void
next_page (EvDocumentModel *model)
{
  if (annotations)
    {
      current_annotation = current_annotation->next;
      if (current_annotation == NULL)
        current_annotation = annotations;
      ev_document_model_set_page (model, GPOINTER_TO_INT(((Annotation*)current_annotation->data)->page));
      gtk_widget_queue_draw (top_window);
    }
  else
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
}

static void
prev_page (EvDocumentModel *model)
{
    if (annotations)
      {
        current_annotation = current_annotation->prev;
        if (current_annotation == NULL)
          current_annotation = g_list_last (annotations);
        ev_document_model_set_page (model, GPOINTER_TO_INT(((Annotation*)current_annotation->data)->page));
        gtk_widget_queue_draw (top_window);
      }
  else
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
}
//set the page to the current annotation
static void set_page (void)
{
  if (current_annotation)
    {
    ev_document_model_set_page (model, GPOINTER_TO_INT(((Annotation*)current_annotation->data)->page));
    gtk_widget_queue_draw (top_window);
  }
}

static void delete_annotation(void)
  {
    GList *next = NULL;
    if (current_annotation->next == current_annotation->prev)
      {
        warningdialog (_("This is the last annotation. The Proof Reading Window will now be closed"));
      }
    else
      next = current_annotation->next;
    annotations = g_list_delete_link (annotations, current_annotation);
    if (next)
      current_annotation = next;
    else
      {
        current_annotation = annotations;
        set_page ();
      }
    if (annotations == NULL)
        gtk_widget_destroy (top_window);
    else
        gtk_widget_queue_draw (top_window);
    set_page ();
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

static gboolean button_release (GtkWidget * view, GdkEventButton * event)
{
 if (event->button != 1)
   { 
      GtkWidget *menu = gtk_menu_new ();
      GtkWidget *item;
      item = gtk_menu_item_new_with_label (_("Next Annotation"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (next_page), (gpointer) model);
      item = gtk_menu_item_new_with_label (_("Previous Annotation"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (prev_page), (gpointer) model);
      
      item = gtk_menu_item_new_with_label (_("Drop Current Annotation"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (delete_annotation), NULL);
      
      item = gtk_menu_item_new_with_label  (_("Help"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate",  G_CALLBACK (infodialog), help_text);
      
      item = gtk_menu_item_new_with_label (_("Drop proof read window"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate",  G_CALLBACK (gtk_widget_destroy), top_window);
 
      gtk_widget_show_all (menu);
#if ((GTK_MAJOR_VERSION==3) && (GTK_MINOR_VERSION>=22))
      gtk_menu_popup_at_pointer (GTK_MENU (menu), NULL);
#else
// FIXME something for gtk2
#endif   
      return TRUE;
   }
 return FALSE;
}


static EvView *
get_view (gchar * filename)
{
  GFile *file;
  GError *err = NULL;
  EvView *view = NULL;
  GList *g;
  filename = locate_file (filename);
  load_markings (filename);
  help_text = annotations?
        _("For each annotation on the page click on the (nearby) notehead or rest etc that the annotation refers to (look for the hand-shaped cursor). This will position the Denemo cursor at that point and you can then make any necessary edits. You can drop the current annotation and the page will move to show the next one."): 
        _("For each annotation on the page click on the (nearby) notehead or rest etc that the annotation refers to. This will insert a comment in the score. Transfer all the annotations in this way before editing the score, otherwise the locations will not match. You can use the EditSimilar (Ctrl-e,e and Ctrl-e,r) command to move from one comment to the next, stopping and editing the score as suggested by the comment.");
  
  
  
  file = g_file_new_for_commandline_arg (filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err) {
      g_critical("Error creating view from URI <%s> : message was %s", uri, err->message);
      return NULL;
    }
  if (top_window)
    {
        gtk_widget_destroy (top_window);
        top_window = NULL;     
    }
  if (annotated_pages)
    {

        g_list_free (annotated_pages);
        current_page = annotated_pages = NULL;
    }
  view = (EvView *) ev_view_new ();
  model = ev_document_model_new_with_document (doc);
#ifndef EV_SIZING_FIT_PAGE
#define EV_SIZING_FIT_PAGE EV_SIZING_BEST_FIT
#endif
  if (!annotations)
    {
      ev_document_model_set_sizing_mode (model, EV_SIZING_FIT_PAGE);
      ev_document_model_set_continuous (model, FALSE);
    }
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
  button = gtk_button_new_with_label (_("Help"));
  g_signal_connect_swapped (button, "clicked", G_CALLBACK (infodialog), help_text);
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
    if (annotations)
      {
         current_annotation = annotations;
         ev_document_model_set_page (model, ((Annotation*)annotations->data)->page);
         top_window=gtk_window_new(GTK_WINDOW_TOPLEVEL);
         gtk_widget_set_tooltip_text (top_window, help_text);
         gtk_window_set_decorated (GTK_WINDOW (top_window), FALSE);
         gtk_window_set_default_size (GTK_WINDOW (top_window), width, height);//height-5 fudge to get annotations correctly placed
         
         GtkWidget *box =  gtk_vbox_new (FALSE, 0);//gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
         gtk_container_add (GTK_CONTAINER(top_window), box);
                    
         GtkWidget *eventbox = gtk_event_box_new ();
         gtk_widget_add_events (eventbox, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK ));
         gtk_box_pack_start (GTK_BOX(box), eventbox, TRUE, TRUE, 0);
         GtkWidget *scroll = gtk_scrolled_window_new (NULL, NULL);
         gtk_container_add (GTK_CONTAINER(eventbox), scroll);
         gtk_container_add (GTK_CONTAINER (scroll), GTK_WIDGET (view));
         VAdj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(scroll));

         g_signal_connect (G_OBJECT (view), "external-link", G_CALLBACK (action_for_link), (gpointer)model);
         
         g_signal_connect_after (G_OBJECT (view), "draw", G_CALLBACK (overdraw), NULL);
         g_signal_connect (G_OBJECT(view), "button-release-event", G_CALLBACK (button_release), NULL);
         
         g_timeout_add (400, (GSourceFunc) refresh_draw, top_window);

         gtk_widget_show_all (top_window); 
         gtk_widget_hide (gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(scroll)));        
      }
    else
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
 //g_print ("returning with %p so value returned %d\n", eview, eview  != NULL);
 return eview  != NULL;
}







