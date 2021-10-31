#ifdef USE_ATRIL
#include <atril-view.h>
#else
#include <evince-view.h>
#endif
#include <errno.h>
#include <math.h>
#include <glib/gstdio.h>
#include "export/print.h"
#include "ui/markup.h"
#include "printview/printview.h"


gboolean finished = FALSE;
gint changes = 0;


static GtkWidget *DenemoMarkupArea;
static gboolean
overdraw_print (cairo_t * cr)
{
  gint x, y;
  gint message_height = 50;
 // get_window_position (&x, &y);

  //cairo_translate (cr, -x, -y);

  //cairo_save (cr);

  
  if (Denemo.printstatus->invalid)
  {
      cairo_set_source_rgba (cr, 0.5, 0.0, 0.0, 0.4);
      cairo_set_font_size (cr, 48.0);
      cairo_move_to (cr, 50, message_height);
      cairo_show_text (cr, _( "Cannot Typeset!"));
      cairo_move_to (cr, 50, message_height + 50);
      cairo_set_font_size (cr, 24.0);
      cairo_show_text (cr, _( "Edit the LilyPond syntax in the pane below or cancel"));

  }
 return TRUE;
}
   
        
static void
set_printarea_doc (EvDocument * doc)
{
  EvDocumentModel *model;
  if (DenemoMarkupArea == NULL)
    return;
  model = g_object_get_data (G_OBJECT (DenemoMarkupArea), "model");     //there is no ev_view_get_model(), when there is use it
  if (model == NULL)
    {
      model = ev_document_model_new_with_document (doc);
      ev_view_set_model ((EvView *) DenemoMarkupArea, model);
      g_object_set_data (G_OBJECT (DenemoMarkupArea), "model", model);  //there is no ev_view_get_model(), when there is use it
    }
  else
    {
      g_object_unref (ev_document_model_get_document (model));  //FIXME check if this releases the file lock on windows.s
      ev_document_model_set_document (model, doc);
    }
}

static void
set_printarea (GError ** err)
{
  GFile *file;
  gchar *filename = Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle];
  //g_debug("using %s\n", filename);
  if (Denemo.printstatus->invalid == 0)
    Denemo.printstatus->invalid = (g_file_test (filename, G_FILE_TEST_EXISTS)) ? 0 : 3;
  file = g_file_new_for_commandline_arg (filename);
  //g_free(filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, err);
  //gint x = 0, y = 0, hupper, hlower, vupper, vlower;//store current position for reloading
  //get_window_position(&x, &y, &hupper, &hlower, &vupper, &vlower);
  if (*err)
    {
      g_warning ("Trying to read the pdf file %s gave an error: %s", uri, (*err)->message);
      Denemo.printstatus->invalid = 3;
      gtk_widget_queue_draw (DenemoMarkupArea);
    }
  else
    set_printarea_doc (doc);
  return;
}

void
markupview_finished (G_GNUC_UNUSED GPid pid, gint status, gboolean print)
{
    changes = 0;
    finished = TRUE;
#if GLIB_CHECK_VERSION(2,34,0)
  {
    GError *err = NULL;
    if (!g_spawn_check_exit_status (status, &err))
      g_warning ("Lilypond did not end successfully: %s", err->message);
  }
#endif
  g_spawn_close_pid (Denemo.printstatus->printpid);
  //g_debug("background %d\n", Denemo.printstatus->background);
  if (Denemo.printstatus->background == STATE_NONE)
    {
      process_lilypond_errors ((gchar *) get_printfile_pathbasename ());
    }
  else
    {
      if (LilyPond_stderr != -1)
        close (LilyPond_stderr);
      LilyPond_stderr = -1;
    }
  Denemo.printstatus->printpid = GPID_NONE;
  unpause_continuous_typesetting ();
  GError *err = NULL;
  set_printarea (&err);
  if (Denemo.printstatus->invalid)
    changes++;
}

#if GTK_MAJOR_VERSION==3
static gint
markuparea_draw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return overdraw_print (cr);
}
#else
static gint
markuparea_draw_event (GtkWidget * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw_print (cr);
  cairo_destroy (cr);
  return TRUE;
}
#endif



void
install_markup_preview (GtkWidget * top_vbox, gchar *tooltip)
{
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  Denemo.printstatus->background = STATE_OFF;
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);
  ev_init ();
  DenemoMarkupArea = (GtkWidget *) ev_view_new ();
  GtkWidget *score_and_scroll_box = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_set_size_request (GTK_WIDGET (score_and_scroll_box), 600, 200);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_box, TRUE, TRUE, 0);  
  gtk_container_add (GTK_CONTAINER (score_and_scroll_box), DenemoMarkupArea);
  gtk_widget_show_all (main_vbox);
  
  
#if GTK_MAJOR_VERSION != 2
  g_signal_connect_after (G_OBJECT (DenemoMarkupArea), "draw", G_CALLBACK (markuparea_draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (DenemoMarkupArea), "expose_event", G_CALLBACK (markuparea_draw_event), NULL);
#endif

}

void drop_markup_area (void)
{
    DenemoMarkupArea = NULL;
}
gchar *Prior, *Post;
static void preview_text (gchar *text)
{
   
    gchar *syntax = g_strconcat (Prior, text, Post, NULL);
    create_pdf_for_lilypond (syntax);
    g_free (syntax);

}
gboolean run_preview (GtkWidget *textbuffer)
{
    
    if (finished && changes)
        {
        GtkTextIter startiter, enditer;
        gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER(textbuffer), &startiter);
        gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER(textbuffer), &enditer);
        gchar *text = gtk_text_buffer_get_text (GTK_TEXT_BUFFER(textbuffer), &startiter, &enditer, FALSE);
        pause_continuous_typesetting ();
        finished = FALSE;
        preview_text (text);
        }
    return TRUE; //continuous timer
}

static gboolean keypress_callback (GtkWidget * w, GdkEventKey * event, GtkWidget *textbuffer)
{
  changes++;
  return FALSE; //pass it on to the standard handler.
 }

//This is a modal get, however the routine in markup.c to get markup can be run non-modally, so if the user then ran this routine while the other was running things would go badly wrong FIXM£
//The two routines need to be designed together, the other allows a collection of buttons to be present injecting text into the text editor.
gchar *get_lilypond_syntax_from_user (gchar* title, gchar *instruction, gchar *prior_context, gchar *post_context, gchar *initial_markup)
{
    Prior = prior_context;
    Post = post_context;
    gchar *ret = NULL;
    Denemo.printstatus->background = STATE_OFF;
    GtkWidget *textview = gtk_text_view_new ();
    GtkWidget *dialog = gtk_dialog_new_with_buttons (title,
                                                   GTK_WINDOW (Denemo.window),
                                                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                                   GTK_STOCK_OK,
                                                   GTK_RESPONSE_ACCEPT,
                                                   GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                                   NULL);
    GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
    gtk_box_pack_start (GTK_BOX (content_area), main_vbox, TRUE, TRUE, 10);
    install_markup_preview (main_vbox, "type markup");

    GtkWidget *label = gtk_label_new(instruction);
    gtk_box_pack_start (GTK_BOX (main_vbox), label, FALSE, TRUE, 0);
    GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_box_pack_start (GTK_BOX (main_vbox), sw, TRUE, TRUE, 10);
    gtk_container_add (GTK_CONTAINER (sw), textview);

    GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
    gtk_text_buffer_set_text (textbuffer, initial_markup, -1);

    g_object_set_data (G_OBJECT (textview), "textbuffer", textbuffer);
    g_signal_connect_after (G_OBJECT (textview), "key-release-event", G_CALLBACK (keypress_callback), textbuffer);

    gtk_widget_show_all (dialog);
    //run_preview (textbuffer);
    finished = TRUE;
    changes = 1;
    gint timer_id =  g_timeout_add ( 100, (GSourceFunc)run_preview, textbuffer);
    gint result = gtk_dialog_run (GTK_DIALOG (dialog));
    g_source_remove (timer_id);
    if (result==GTK_RESPONSE_ACCEPT)
      {
          GtkTextIter startiter, enditer;
          gtk_text_buffer_get_start_iter (textbuffer, &startiter);
          gtk_text_buffer_get_end_iter (textbuffer, &enditer);
          gchar *text = gtk_text_buffer_get_text (textbuffer, &startiter, &enditer, FALSE);
          ret = text;
      }
    gtk_widget_destroy (dialog);
    return ret;
}
