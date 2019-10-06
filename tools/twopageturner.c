//gcc -Wall twopageturner.c -o twopageturner `pkg-config --cflags --libs gtk+-3.0` `pkg-config --cflags --libs evince-view-3.0`
//Tested on debian 3.16.0 and GTK3.14.5
//      pageturner.c
//
//      Copyright 2019 Richard Shann
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
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include<gtk/gtk.h>
#include <evince-view.h>

#define SPOT_SIZE (20) //size of spot indicating the position of a repeat to be marked
typedef struct Location {
     gint  adj;
     gint x, y;
} Location;
typedef struct Annotation {
     gchar *annotation;
     gint page;
     gint  adj;
     gint x, y;
} Annotation;

typedef struct Page {
     EvDocumentModel *model;
     GtkWidget *eventbox;
     gint pnum; //page num starting from 0
     gint  ovi; //overlay index starting from 0 - highest number is on top
     gint x; // horizontal position of page
} Page;

static const gchar *music_home;//the directory where your scores are stored
static GtkWidget *score_window;//the window containing the two halves of the score
static GtkAdjustment *VAdj1, *VAdj2, *VAdj3;//control over the parts of the score visible in each half
static GList *repeat_locations = NULL;// a list of locations for use when a repeat starts in the middle/lower half of a page
static GList *annotations = NULL;// a list of annotations to be displayed on the score
static GtkWidget *view1, *view2, *view3;//the two halves of the score - the top moves on to the next page while you are still reading the bottom 
static GtkWidget *overlay;
static GtkWidget *eventbox1, *eventbox2, *eventbox3;
static Page page1, page2, page3;//the three Page objects
static Page *lh_page, *rh_page, *os_page;//pointers to the three Page objects. During transitions they point to destination
static gint num_pages;
gboolean tr1_running = FALSE;//transition1 is happening, both pages are sliding to left and the off-screen page is entering from right
gboolean tr2_running = FALSE;//transition2 is happening, lh page is sliding under rh page and off-screen page is entering from left
gboolean tr3_running = FALSE;//transition3 is happening, rh page is being updated to match lh
gboolean tr4_running = FALSE;//transition4 is happening, right hand page is sliding under lh_page and off-screen page is entering from right
static guint timeout=5; //number of milliseconds between transition steps
gint default_transition_step = 1, transition_step = 1;//how far to shift horizontally in the timeout milliseconds while comfortably reading the shifting music page
gint default_quick_transition_step = 20, quick_transition_step = 20;//how far to shift when *not* reading that page - the transition is just to aid the user understand what is happening
static gboolean went_back;//lh and rh pages are out of sync because of going back (for repeat) else because of going forward (for couplet)
static  gint x=0;//Window size and position on screen: 0 0 is top left corner of screen
static  gint y=0;
static  gint width=0;
static  gint height=0;
static gdouble aspect_ratio=1.414; //A4 page size default
static  EvDocumentModel *model1, *model2, *model3;
static gboolean cpl;//current page is on left

static void next_page (void);//turn to the next page

static void previous_page (void);//turn back one page
static gchar *markings_file = NULL;//full patht to file for storing repeat marks and annotations for currently loaded score
static gboolean markings_unsaved = FALSE; //TRUE when user has created or deleted markings in the current score 
static gchar page_on = 'a', page_back = 'c', skip_page = 'b';
static gchar *help_text =
"The music page-turner allows hands-free turning of pages as you play from a musical score."
"\nIt also allows for annotations to be added to the score - anything from reminder accidentals"
" to the spectacles O^O that warn of a tricky passage to take special care with."
"\nControl of page turning is via foot pedals which send key press signals just like those of a normal keyboard."
" The whole screen height is devoted to a single page so when you go to the following page the"
" transition is made in two stages: first the top of the page changes to the next page, and then"
" after a delay the lower part of the page"
" is rendered. This means you do not have to suddenly switch at just the right point in the music.\n"
"\nOne pedal moves to the next page, one goes back to the previous page and the third pedal forces"
" completion of the page. This last is not normally needed, except in the case of repeats that start low on a page."
" The location of the start of a repeat in the score can be marked with the effect that the repeat appears at the top of"
" the screen when you go back to it. In this case you go on to the next page when you reach the bottom of the page,"
" rather than when you reach the bottom of the screen."
"\nTo set a repeat location you scroll the music (using the mouse-wheel or arrow keys) until it is at the top of the screen"
" and then right-click with the mouse and choose \"Mark a repeat here\" from the menu."
"\nThe menu also lets you enter annotations, navigate the score, set the delay for page completion etc."
"\n Currently Keypresses '%c' to go one page on, '%c' to go one page back and '%c' to complete the page."
"\nTo change the defaults you can pass values on the command line - type pageturner --help to see the command line usage."
;
static void show_help (void)
{
   GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW (gtk_window_new(GTK_WINDOW_TOPLEVEL)), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_OK, 
   help_text,
   page_on, page_back, skip_page);
   gtk_widget_show_all (dialog);
   g_signal_connect (dialog, "response", G_CALLBACK (gtk_widget_destroy), NULL);
}


static void free_annotation (Annotation *a)
{
   g_free (a->annotation);
   g_free (a);
}
//load repeat marks and annotations from the file associated with the opened pdf score
static void load_markings (gchar *pdfname)
{
   if (markings_file) 
      g_free (markings_file);
   markings_file = g_strdup_printf ("%s%s", pdfname, ".marks");
   FILE *fp = fopen (markings_file, "r");
   if (fp)
      {
         char type[20];
         gint window_width, window_height;
         if (2==fscanf (fp, "%d%d", &window_width, &window_height))
            {
               if ((width != window_width) || (height != window_height))
                  g_warning ("Height and Width of markings file does not match current window - expect misplaced marks %d %d %d %d\n", width, height, window_width, window_height);
               while (1 == fscanf (fp, "%20s", type))
                  {
                     if (!strcmp(type, "Repeat:"))
                        {
                           Location *loc = g_malloc (sizeof (Location));
                           if (3 == fscanf (fp, "%d %d %d", &loc->adj, &loc->x, &loc->y))
                              repeat_locations = g_list_append (repeat_locations, loc);
                        }
                     else
                       if (!strcmp(type, "Annotate:"))
                           {
                              Annotation *ann = g_malloc (sizeof (Annotation));
                              if (4 == fscanf (fp, "%d %d  %d %d", &ann->page, &ann->adj, &ann->x, &ann->y))
                                 {
                                    gchar text[100];
                                    if (fgets (text, 100, fp))
                                       ann->annotation = g_strdup (text);
                                    else
                                       ann->annotation = g_strdup ("???");
                                    annotations = g_list_append (annotations, ann);
                                 } 
                           }
                        else g_warning ("Corrupt markings file");
                  }
            }
            else  g_warning ("Corrupt markings file");
        fclose (fp);
      }
   markings_unsaved = FALSE;
}
static void
load_score (gchar *pdfname, GError ** err)
{
  GFile *file;
  file = g_file_new_for_commandline_arg (pdfname);
  gchar *uri = g_file_get_uri (file);
  EvDocument *doc = ev_document_factory_get_document (uri, err);
  if (*err)
    {
      g_warning ("Trying to read the pdf file %s gave an error: %s", uri, (*err)->message);
      gtk_widget_queue_draw (view1);
      gtk_widget_queue_draw (view2);
    }
  else
   {

     //model1 = g_object_get_data (G_OBJECT (view1), "model");  
     //model2 = g_object_get_data (G_OBJECT (view2), "model");  
     //model3 = g_object_get_data (G_OBJECT (view3), "model");  
     if (model1 == NULL)
       {
         model1 = ev_document_model_new_with_document (doc);
         ev_view_set_model ((EvView *) view1, model1);
         //g_object_set_data (G_OBJECT (view1), "model", model1); 
      }
     else
      {
         ev_document_model_set_document (model1, doc);
       }
     if (model2 == NULL)
       {          
         model2 = ev_document_model_new_with_document (doc);
         ev_view_set_model ((EvView *) view2, model2);
         //g_object_set_data (G_OBJECT (view2), "model", model2);        
       }
     else
       {
         ev_document_model_set_document (model2, doc);
       }
     if (model3 == NULL)
       {          
         model3 = ev_document_model_new_with_document (doc);
         ev_view_set_model ((EvView *) view3, model3);
         //g_object_set_data (G_OBJECT (view3), "model", model3);        
       }
     else
       {
         ev_document_model_set_document (model3, doc);
       }
       
       for (num_pages=1;;num_pages++)
         {
            if (!ev_view_next_page ((EvView*)view1))
               break;
         }
       g_print ("Number of pages %d\n", num_pages);
      page1.model = model1;
      page1.eventbox = eventbox1;
      page1.pnum = 0;
      page1.ovi = 0;
      page1.x = 0;
      lh_page = &page1;
      ev_document_model_set_page (model1, 0); 
      
      page2.model = model2;
      page2.eventbox = eventbox2;
      page2.pnum = 1;
      page1.ovi = 1;
      rh_page = &page2;
      page2.x = width;
      ev_document_model_set_page (model2, 1);
      
      page3.model = model3;
      page3.eventbox = eventbox3;
      page3.pnum = 0;
      page3.ovi = 3;
      os_page = &page3;

      cpl = TRUE;
      g_list_free_full (repeat_locations, g_free);
      repeat_locations = NULL;
      g_list_free_full (annotations, (GDestroyNotify)free_annotation);
      annotations = NULL;
      load_markings (pdfname);
   }

  return;
}

//This triggers re-positioning the pages, surely there must be a way to do this more simply?
static void force_page_recalc (void)
{
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), os_page->eventbox, -1);
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), rh_page->eventbox, -1);
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), lh_page->eventbox, -1);   
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), os_page->eventbox, os_page->ovi);
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), rh_page->eventbox, rh_page->ovi);
         gtk_overlay_reorder_overlay (GTK_OVERLAY(overlay), lh_page->eventbox, lh_page->ovi);   
}

static gboolean slide_left (void)
{
   if ((lh_page->x - transition_step) <0)
      {
       tr1_running = FALSE;
       transition_step = default_transition_step;
      }
   else
      {
         lh_page->x -= transition_step;
         rh_page->x -= transition_step;
         os_page->x -= transition_step;
         force_page_recalc ();

      }
   return tr1_running;
}

//quickly slide lh page in from left to be ready to go back to a repeat
static gboolean lh_slide_right (void)
{
   if ((lh_page->x + transition_step) >= 0)
      {
       tr2_running = FALSE;
       quick_transition_step = default_quick_transition_step;
      }
   else
      {
         lh_page->x += quick_transition_step;
         os_page->x += quick_transition_step;
         force_page_recalc ();
      }
   return tr2_running;
}
//quickly slide a page into the rh from the right to be ready to go on to a couplet or verse in a rondeau
static gboolean rh_slide_left (void)
{
   if ((rh_page->x - quick_transition_step) <= width)
      {
       tr4_running = FALSE;
       quick_transition_step = default_quick_transition_step;
      }
   else
      {
         rh_page->x -= quick_transition_step;
         os_page->x -= quick_transition_step;
         force_page_recalc ();
      }
   return tr4_running;
}

//quickly slide a page into the rh from the center to be ready to go on after starting a repeat on lh page
static gboolean rh_slide_right (void)
{
   if ((rh_page->x + quick_transition_step) >= width)
      {
       tr3_running = FALSE;
       quick_transition_step = default_quick_transition_step;
      }
   else
      {
         rh_page->x += quick_transition_step;
         os_page->x += quick_transition_step;
         force_page_recalc ();
      }
   return tr3_running;
}


static void transition1 (gint to_page)
{
 if (to_page >= num_pages)
      return;
 if (tr1_running)
   {
      transition_step = 2 * transition_step;
      return;
   }
  ev_document_model_set_page (os_page->model, to_page);
  os_page->x = 2 * width;
  Page *temp = lh_page;
  lh_page = rh_page;
  rh_page = os_page;
  os_page = temp;
  rh_page->pnum = to_page;
  lh_page->pnum = to_page - 1;
  tr1_running = g_timeout_add (timeout, (GSourceFunc)slide_left, NULL);
}

//quickly slide lh page in from left to go backwards for a repeat
static void transition2 (gint to_page)
{
   if (lh_page->pnum == 0)
      return;
   if (tr1_running)
      return;
   went_back = TRUE;
   ev_document_model_set_page (os_page->model, to_page);
   os_page->x = -width;
   Page *temp = lh_page;
   lh_page = os_page;
   os_page = temp;
   lh_page->pnum = to_page; 
   os_page->ovi = 0; //underneath
   lh_page->ovi = 1;
   rh_page->ovi = 2;
   tr2_running = g_timeout_add (timeout, (GSourceFunc)lh_slide_right, NULL);
}
//quickly slide rh page in from center to match lh after starting a repeat on left hand page
static void transition3 (gint to_page)
{
   if (tr1_running)
      return;

   ev_document_model_set_page (os_page->model, to_page);
   os_page->x = 0;
   Page *temp = rh_page;
   rh_page = os_page;
   os_page = temp;
   rh_page->pnum = to_page; 
   os_page->ovi = 0; //underneath
   rh_page->ovi = 1;
   lh_page->ovi = 2;
   tr3_running = g_timeout_add (timeout, (GSourceFunc)rh_slide_right, NULL);      
}
//move rh page on to another verse for a rondeau
static void transition4 (gint to_page)
{
   if (rh_page->pnum == num_pages)
      return;
   if (tr1_running)
      return;
   went_back = FALSE;
   ev_document_model_set_page (os_page->model, to_page);
   os_page->x = 2*width;
   Page *temp = rh_page;
   rh_page = os_page;
   os_page = temp;
   rh_page->pnum = to_page; 
   os_page->ovi = 0; //underneath
   lh_page->ovi = 1;
   rh_page->ovi = 2;
   tr4_running = g_timeout_add (timeout, (GSourceFunc)rh_slide_left, NULL);
}

static void next_page (void)
{
      (rh_page->pnum == (1 + lh_page->pnum) || !went_back)? 
      transition1(rh_page->pnum + 1):
      transition3(lh_page->pnum + 1);
}


static void previous_page (void)
{
 transition2(lh_page->pnum - 1);  
}

static void advance_right_page (void)
{
   transition4 (rh_page->pnum + 1);
   return;

}


static gboolean keypress (GtkWidget *eventbox, GdkEventKey * event)
{
  guint keyval = event->keyval; 

   if (keyval==page_on)
    {
   
      next_page();
    }

   if (keyval==skip_page)
    {
       advance_right_page();
    }
   if (keyval==page_back)
    {
     previous_page();
    }
 
  return FALSE;
}

static gboolean choose (gchar *filename)
{
   GtkWidget *top_window=gtk_window_new(GTK_WINDOW_TOPLEVEL);

   GtkWidget *chooser = gtk_file_chooser_dialog_new ("Choose Score",
                        GTK_WINDOW (top_window),
                        GTK_FILE_CHOOSER_ACTION_OPEN,
                        "Ok", GTK_RESPONSE_ACCEPT,
                         NULL);

   GtkFileFilter *filter = gtk_file_filter_new ();
   gtk_file_filter_set_name (filter, "PDF files");
   gtk_file_filter_add_pattern (filter, "*.pdf");
   gtk_file_filter_add_pattern (filter, "*.PDF");
   gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (chooser), filter);

   if (filename && g_file_test(filename, G_FILE_TEST_IS_DIR))
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), filename);
   if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT)
       {
         gchar *filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
         GError *err = NULL;
         load_score (filename, &err);
      }
   gtk_widget_destroy (chooser);
  return FALSE; //remove this callback
}

static void change_delay (GtkSpinButton * widget)
{
   //g_print ("Value is %f\n", gtk_spin_button_get_value (widget));
  timeout = (guint)(1000*gtk_spin_button_get_value (widget));
}
static void set_delay (void)
{
  GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  GtkWidget *label = gtk_label_new ("Set delay (secs) before bottom half of page synchronizes: ");
  GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_container_add (GTK_CONTAINER (window), box);
  gtk_box_pack_start (GTK_BOX (box), label, FALSE, TRUE, 0);
  GtkWidget *spinner_adj = (GtkWidget *) gtk_adjustment_new (timeout/1000.0, 0.001, 1000.0, 0.1, 1.0, 0.0);
  GtkWidget *spinner = (GtkWidget *) gtk_spin_button_new (GTK_ADJUSTMENT(spinner_adj), 1.0, 3);
  gtk_box_pack_start (GTK_BOX (box), spinner, FALSE, TRUE, 0);
  g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_delay), NULL);
  gtk_widget_show_all (window); 
}

static void change_page (GtkSpinButton * widget, EvDocumentModel *model)
{
  gint page = (gint)gtk_spin_button_get_value (widget);
  ev_document_model_set_page (model, page - 1);
}

static void navigate (void)
{
  static GtkWidget *window = NULL;
  if (window == NULL)
   {
     window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
     g_signal_connect (window, "delete-event", G_CALLBACK(gtk_widget_hide), NULL);
     GtkWidget *label = gtk_label_new ("Set Page Number:");
     GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
     gtk_container_add (GTK_CONTAINER (window), box);
     gtk_box_pack_start (GTK_BOX (box), label, FALSE, TRUE, 0);
     GtkWidget *spinner_adj = (GtkWidget *) gtk_adjustment_new (1.0, 1.0, 999.0, 1.0, 1.0, 1.0);
     GtkWidget *spinner = (GtkWidget *) gtk_spin_button_new (GTK_ADJUSTMENT(spinner_adj), 100.0, 0);
     gtk_box_pack_start (GTK_BOX (box), spinner, FALSE, TRUE, 0);
     g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_page), model1);
  }
     gtk_widget_show_all (window);
     gtk_window_present (GTK_WINDOW (window)); 
}

static Annotation *create_annotation (gchar *text, gint page, gdouble adjust, gdouble x, gdouble y)
{
    Annotation *ann;
    markings_unsaved = TRUE;
    ann = g_malloc (sizeof (Annotation));
    ann->annotation = text;
    ann->page = page;
    ann->adj = adjust;
    ann->x = x;
    ann->y = y;
    return ann;
}

static void delete_annotations (void)
{
    GList *g;
    for (g=annotations;g;g=g->next)
      {
       Annotation *ann = (Annotation *)g->data;
       if (((ann->adj + ann->y) > gtk_adjustment_get_value (VAdj1)) && ((ann->adj + ann->y) < (gtk_adjustment_get_value (VAdj1) + gtk_adjustment_get_page_size (VAdj1))))
            {
             markings_unsaved = TRUE;
             annotations = g_list_delete_link (annotations, g);
             break;
            }
      }
}


gchar *
get_annotation_from_user (void)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;
  gchar *entry_string = NULL;
  entry = gtk_entry_new ();

  dialog = gtk_dialog_new_with_buttons ("Annotation", GTK_WINDOW (gtk_window_new(GTK_WINDOW_TOPLEVEL)), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), "OK", GTK_RESPONSE_ACCEPT, "Cancel", GTK_RESPONSE_REJECT, NULL);
  label = gtk_label_new ("Give annotation");
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), label);
  GtkWidget *widget = NULL;//a widget to insert "#" etc...
  if (widget)
    gtk_container_add (GTK_CONTAINER (content_area), widget);
  gtk_entry_set_text (GTK_ENTRY (entry), "O^O");//FIXME add options for inserting ♯ ♮ ♭ 𝄫 𝄪  etc
  gtk_container_add (GTK_CONTAINER (content_area), entry);

  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
        {
          gchar *string = NULL;
          if (GTK_DIALOG (dialog))
            {
              entry_string = (gchar *) gtk_entry_get_text (GTK_ENTRY (entry));
              string = g_strdup (entry_string);
              gtk_widget_destroy (dialog);
            }
          return string;
       }
  else
   {
     gtk_widget_destroy (dialog);
   }
   return NULL;
}


  
static void mark_annotation (Annotation *p)
{
   gchar *text = get_annotation_from_user ();
   if (text)
      {
      Annotation *ann = create_annotation (text, p->page, 0, p->x, p->y);
      if (ann)
         annotations = g_list_append (annotations, ann);
      }
}

static void save_markings (void)
{
  if (markings_file)
      {
         FILE *fp = fopen (markings_file, "w");
         if (fp)
            {
               GList *g;
               fprintf (fp, "%d %d\n", width, height);
               for (g=repeat_locations;g;g=g->next)
                  {
                   Location *loc = (Location *)g->data;
                   fprintf (fp, "Repeat: %d %d %d\n", (int)loc->adj, (int)loc->x, (int)loc->y);
                  }
               for (g=annotations;g;g=g->next)
                  {
                   Annotation *ann = (Annotation *)g->data;
                   fprintf (fp, "Annotate: %d %d %d %d %s\n", ann->page, (int)ann->adj, (int)ann->x, (int)ann->y, ann->annotation);
                  }  
               fclose (fp);
            }
         else
            g_warning ("Could not write %s for markings\n", markings_file);
      }
}

static gint get_page_num_for_view (GtkWidget *view)
   {
    return view == view1?page1.pnum:(view==view2?page2.pnum:page3.pnum); 
   }
   
static gboolean clicked (GtkWidget * view, GdkEventButton * event)
{
  if (event->button != 1)
   { 
      static Annotation current_position;
      current_position.page = get_page_num_for_view (view);
      current_position.x = (int)event->x;
      current_position.y = (int)event->y;
      GtkWidget *menu = gtk_menu_new ();
      GtkWidget *item;

      item = gtk_menu_item_new_with_label ("Choose score");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (choose), NULL);

      item = gtk_menu_item_new_with_label ("Annotate here");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (mark_annotation), &current_position);
      item = gtk_menu_item_new_with_label ("Delete annotations from this page");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK ((view==view1)?delete_annotations:delete_annotations), NULL);
      
      item = gtk_menu_item_new_with_label ("Save Annotations");
      gtk_widget_set_sensitive (item,markings_unsaved);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (save_markings), NULL);
        
      item = gtk_menu_item_new_with_label ("Navigate");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (navigate), NULL);
      item = gtk_menu_item_new_with_label ("Set delay for page completion");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (set_delay), NULL);  
      item = gtk_menu_item_new_with_label ("Help");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (show_help), NULL); 
      item = gtk_menu_item_new_with_label ("Quit");
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (gtk_main_quit), NULL);     

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

static void draw_page_break (cairo_t *cr)
{
   double x0, y0, x1, y1;
   x0 = width-50;
   x1 = width;
   y0 = 0;
   y1 = height;
   
   cairo_pattern_t *pat = cairo_pattern_create_linear (x0, y0, x1, y0);
   cairo_pattern_add_color_stop_rgba (pat, 1, 0, 0, 0, 0.8);
   cairo_pattern_add_color_stop_rgba (pat, 0, 1, 1, 1, 0.1);
   cairo_rectangle (cr, x0, y0, x1 - x0, y1 - y0);//width and start of line is a hack - work from page width
   cairo_set_source (cr, pat);
   cairo_fill (cr);
   cairo_pattern_destroy (pat);
   cairo_fill (cr);
}



static gboolean overdraw (GtkWidget* view, cairo_t * cr)
{
   GList *g;
   if (lh_page==NULL) return FALSE;
   gint pnum = get_page_num_for_view (view);
   for (g = annotations; g; g=g->next)
      {
         Annotation *ann = (Annotation*)g->data;
         gdouble x = ann->x, y = ann->y; //g_print ("annotation %s page %d for page %d?\n", ann->annotation, ann->page, pnum);
         if (ann->page == pnum)
            {
               cairo_set_source_rgba (cr, 0.2, 0.4, 1, 1);
               cairo_set_font_size (cr, 16);
               cairo_move_to (cr, x, y);
               cairo_show_text (cr, ann->annotation);  
            }
      }
    if ((lh_page->pnum != (rh_page->pnum - 1)) &&
            pnum == lh_page->pnum)
      {
         draw_page_break (cr);
         
      }      
         
  return FALSE;
}


static gboolean adjust_page_positions (GtkOverlay   *overlay,
               GtkWidget    *widget,
               GdkRectangle *allocation,
               gpointer      user_data)
   {
      allocation->x = (widget==page1.eventbox)?
                                    page1.x:
                                    ((widget==page2.eventbox)?
                                       page2.x: page3.x);
      allocation->y = 0;
      allocation->width = width;
      allocation->height = height;

      return TRUE;
   }               
        


int main(int argc, char **argv)
 {
   gtk_init(&argc, &argv);
   GdkRectangle r;
#if ((GTK_MAJOR_VERSION==3) && (GTK_MINOR_VERSION>=22))
   gdk_monitor_get_workarea (gdk_display_get_primary_monitor (gdk_display_get_default ()), &r);
#else
// FIXME something for gtk2
#endif   
   width = r.width;
   height = 0;
   if (argc>1)
      timeout = atoi (argv[1]);
   if (timeout<0.001)
      goto error;
      
   if (argc>2 && (1 != sscanf (argv[2], "%c", &page_on)))
    goto error;      
   if (argc>3 && (1 != sscanf (argv[3], "%c", &page_back)))
    goto error;      
   if (argc>4 && (1 != sscanf (argv[4], "%c", &skip_page)))
    goto error;      

      
   if (argc>5 && (1 != sscanf (argv[5], "%lf", &aspect_ratio)))
    goto error;
   if (argc>6 && (1 != sscanf (argv[6], "%d", &width)))
    goto error;
   if (argc>7 && (1 != sscanf (argv[7], "%d", &height)))
    goto error;
   if (height == 0)
         {
            if (aspect_ratio<0.001)
               aspect_ratio = 1.414;
            height = width * aspect_ratio;
            if (height > r.height)
               height = r.height;
            width = height/aspect_ratio;   
            x = (r.width-width)/2;
         }
   g_print ("Using height %d width %d, aspect ratio %f at %d horizontally\n", height, width, aspect_ratio, x);
   music_home = g_get_home_dir();
   ev_init ();
   score_window=gtk_window_new(GTK_WINDOW_TOPLEVEL);
   gtk_window_set_decorated (GTK_WINDOW (score_window), FALSE);
   g_signal_connect (score_window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

   gtk_window_move (GTK_WINDOW (score_window), x, y);
   gtk_window_set_default_size (GTK_WINDOW (score_window), 2*width, height - 20);//Last page cannot be scrolled to the bottom in the lower pane without -20
   
   overlay = gtk_overlay_new ();
   g_signal_connect (G_OBJECT(overlay), "get-child-position", G_CALLBACK (adjust_page_positions), NULL);   

   gtk_container_add (GTK_CONTAINER(score_window), overlay);
   
   eventbox1 = gtk_event_box_new ();
   gtk_widget_add_events (eventbox1, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK ));
   gtk_overlay_add_overlay (GTK_OVERLAY (overlay), eventbox1);
   GtkWidget *scroll1 = gtk_scrolled_window_new (NULL, NULL);
   gtk_container_add (GTK_CONTAINER(eventbox1), scroll1); 
   VAdj1 = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(scroll1));
   g_signal_connect (G_OBJECT(eventbox1), "key-press-event", G_CALLBACK (keypress), NULL);
   view1 = (GtkWidget *) ev_view_new ();
   g_signal_connect (G_OBJECT(view1), "button-release-event", G_CALLBACK (clicked), NULL);
   g_signal_connect_after (G_OBJECT (view1), "draw", G_CALLBACK (overdraw), NULL);
   gtk_container_add (GTK_CONTAINER (scroll1), view1);


   eventbox2 = gtk_event_box_new ();
   gtk_widget_add_events (eventbox2, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK ));
   gtk_overlay_add_overlay (GTK_OVERLAY (overlay), eventbox2);
   GtkWidget *scroll2 = gtk_scrolled_window_new (NULL, NULL);
   gtk_container_add (GTK_CONTAINER(eventbox2), scroll2); 
   VAdj2 = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(scroll2));
   g_signal_connect (G_OBJECT(eventbox2), "key-press-event", G_CALLBACK (keypress), NULL);
   view2 = (GtkWidget *) ev_view_new ();
   g_signal_connect (G_OBJECT(view2), "button-release-event", G_CALLBACK (clicked), NULL);
   g_signal_connect_after (G_OBJECT (view2), "draw", G_CALLBACK (overdraw), NULL);
   gtk_container_add (GTK_CONTAINER (scroll2), view2); 
   
   eventbox3 = gtk_event_box_new ();
   gtk_widget_add_events (eventbox3, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK ));
   gtk_overlay_add_overlay (GTK_OVERLAY (overlay), eventbox3);   
   GtkWidget *scroll3 = gtk_scrolled_window_new (NULL, NULL);
   gtk_container_add (GTK_CONTAINER(eventbox3), scroll3); 
   VAdj3 = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(scroll3));
   g_signal_connect (G_OBJECT(eventbox3), "key-press-event", G_CALLBACK (keypress), NULL);
   view3 = (GtkWidget *) ev_view_new ();
   g_signal_connect (G_OBJECT(view3), "button-release-event", G_CALLBACK (clicked), NULL);
   g_signal_connect_after (G_OBJECT (view3), "draw", G_CALLBACK (overdraw), NULL);
   gtk_container_add (GTK_CONTAINER (scroll3), view3); 

   gtk_widget_show_all (score_window);
   gtk_widget_hide (gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(scroll1)));
   gtk_widget_hide (gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(scroll2)));
   gtk_widget_hide (gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(scroll3)));
  
   if (argc==1)
      show_help();
   g_idle_add ((GSourceFunc) choose, (gpointer) music_home);
   gtk_main();

   return 0;  
   error:
        g_print ("Usage: pageturner [delay ms] [page down, default '%c'] [page up, default '%c']  [complete page, default '%c']  [aspect ratio, default 1.414, ie A4] [width in pixels, default maximum][height in pixels - overrides monitor size and aspect ratio]\n",
               page_on, page_back, skip_page);
         return -1;
 }
