#include <evince-view.h>
#include <errno.h>
#include <math.h>
#include <glib/gstdio.h>

#include "printview/printview.h"
#include "export/print.h"
#include "core/view.h"
#include "scripting/scheme-callbacks.h"
#include "command/scorelayout.h"
#include "command/lilydirectives.h"
#include "export/exportlilypond.h"
#include "source/sourceaudio.h"

static gint changecount = -1;   //changecount when the printfile was last created FIXME multiple tabs are muddled
static gchar *thumbnailsdirN = NULL;
static gchar *thumbnailsdirL = NULL;

static gboolean retypeset (void);
static gdouble get_center_staff_offset (void);
static gboolean LeftButtonPressed;
static unsigned
file_get_mtime (gchar * filename)
{
  struct stat thebuf;
  g_stat (filename, &thebuf);
  unsigned mtime = thebuf.st_mtime;
  // g_debug("the mt is %u %u\n", mtime, thebuf.st_mtim.tv_nsec);
  return mtime;
}

// Displaying Print Preview

static void
start_busy_cursor (void)
{
  busy_cursor (Denemo.printarea);
}

static void
start_normal_cursor (void)
{
  normal_cursor (Denemo.printarea);
}
static gboolean ContinuousTypesettingPaused;
void unpause_continuous_typesetting (void)
{
 if (ContinuousTypesettingPaused)
     if (Denemo.printstatus->background & STATE_PAUSED)
        {
          if (Denemo.prefs.typesetrefresh)
            Denemo.printstatus->updating_id = g_timeout_add (Denemo.prefs.typesetrefresh, (GSourceFunc) retypeset, NULL);
          else
            Denemo.printstatus->updating_id = g_idle_add ((GSourceFunc) retypeset, NULL);
        }
 ContinuousTypesettingPaused = FALSE;
 Denemo.printstatus->background &= ~STATE_PAUSED;  
}
void pause_continuous_typesetting (void)
{
      Denemo.printstatus->background |= STATE_PAUSED;
      
      if (Denemo.printstatus->updating_id)
        {
          ContinuousTypesettingPaused = TRUE;
          g_source_remove (Denemo.printstatus->updating_id);    //if this is not turned off the print preview thread hangs until it is.
          Denemo.printstatus->updating_id = 0;
        }
}
 
/*void                user_function                      (EvPrintOperation       *evprintoperation,
                                                        GtkPrintOperationResult arg1,
                                                        gpointer                user_data)             : Run Last */
static void
printop_done (EvPrintOperation * printop, G_GNUC_UNUSED GtkPrintOperationResult arg1, GtkPrintSettings ** psettings)
{
  if (*psettings)
    g_object_unref (*psettings);
  *psettings = ev_print_operation_get_print_settings (printop);
  g_object_ref (*psettings);
  //g_debug("Came away with uri %s\n", gtk_print_settings_get(*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI));
  gchar *uri = g_strdup (gtk_print_settings_get (*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI));
  gchar *unesc = g_uri_unescape_string (uri, NULL);
  g_free (uri);
  set_current_scoreblock_uri (unesc);
  unpause_continuous_typesetting ();
 
  call_out_to_guile ("(FinalizePrint)");
}

static gboolean
libevince_print (void)
{
  GError *err = NULL;
  gchar *filename = Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle];

  if (filename == NULL)
    {
      g_warning ("Typesetting not done? No output filename set.");
      return -1;
    }
    
  EvDocumentModel *model = (EvDocumentModel*)g_object_get_data (G_OBJECT (Denemo.printarea), "model"); 

#ifdef G_OS_WIN32
  infodialog (_("Direct Printing not available under Windows. Create PDF and print from that"));
  return -1;
#endif

  gchar *uri = g_filename_to_uri (filename, NULL, &err);

  if (err)
    {
      g_warning ("Malformed filename %s", filename);
      return -1;
    }

  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err)
    {
      g_warning ("Trying to print the pdf file %s gave an error: %s", uri, err->message);
      g_error_free (err);
      err = NULL;
      return -1;
    }
  else
    {
      static GtkPrintSettings *settings;
      if (settings == NULL)
        settings = gtk_print_settings_new ();
      EvPrintOperation *printop = ev_print_operation_new (doc);
      g_signal_connect (printop, "done", G_CALLBACK (printop_done), &settings);
      gtk_print_settings_set (settings, GTK_PRINT_SETTINGS_OUTPUT_URI, get_output_uri_from_scoreblock ());
      ev_print_operation_set_print_settings (printop, settings);

      pause_continuous_typesetting ();


      ev_print_operation_run (printop, NULL);
    }
  return 0;
}

gboolean
print_typeset_pdf (void)
{
  return libevince_print ();
}

static void
set_printarea_doc (EvDocument * doc)
{
  EvDocumentModel *model;
  changecount = Denemo.project->changecount;
  model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");     //there is no ev_view_get_model(), when there is use it
  if (model == NULL)
    {
      model = ev_document_model_new_with_document (doc);
      ev_view_set_model ((EvView *) Denemo.printarea, model);
      g_object_set_data (G_OBJECT (Denemo.printarea), "model", model);  //there is no ev_view_get_model(), when there is use it
    }
  else
    {
      g_object_unref (ev_document_model_get_document (model));  //FIXME check if this releases the file lock on windows.s
      ev_document_model_set_document (model, doc);
    }
   gint dual = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "Duplex"));
   gint invert = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "Invert"));
  
   ev_document_model_set_inverted_colors (model, invert);
#if 0 // it is Evince version we need to test ((GTK_MAJOR_VERSION == 3) && (GTK_MINOR_VERSION >= 8))
  if (dual)
    {
      ev_document_model_set_page_layout (model, EV_PAGE_LAYOUT_DUAL);
    }
  else
    {
      ev_document_model_set_page_layout (model, EV_PAGE_LAYOUT_SINGLE);
    }
#else       
     ev_document_model_set_dual_page (model, dual);
#endif
  get_wysiwyg_info ()->Mark.width = 0;  //indicate that there should no longer be any Mark placed on the score
}

static void
get_window_position (gint * x, gint * y)
{
  GtkAdjustment *adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printhscrollbar));
  *x = (gint) gtk_adjustment_get_value (adjust);
  adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printvscrollbar));
  *y = gtk_adjustment_get_value (adjust);
}

//setting up Denemo.pixbuf so that parts of the pdf can be dragged etc.
static void
get_window_size (gint * w, gint * h)
{
  GdkWindow *window;
  if (!GTK_IS_LAYOUT (Denemo.printarea))
    window = gtk_widget_get_window (GTK_WIDGET (Denemo.printarea));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
  if (window)
    {
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      //      gdouble staffsize = atof(Denemo.project->lilycontrol.staffsize->str);
      //      if(staffsize<1) staffsize = 20.0;
      //      scale *= (staffsize/4);//Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
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

//setting up Denemo.pixbuf so that parts of the pdf can be dragged etc.
static void
set_denemo_pixbuf (gint x, gint y)
{
  GdkWindow *window;
  GdkPixbuf *pixbuf;
  if (!GTK_IS_LAYOUT (Denemo.printarea))
    window = gtk_widget_get_window (GTK_WIDGET (Denemo.printarea));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
  if (window)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      x -= xx;
      y -= yy;
#define GROB_SIZE 20            // a rough amount to drag grobs around recognizably
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      gint grob_size = GROB_SIZE * (staffsize / 20.0);
      x -= scale * grob_size / 2;
      y -= scale * grob_size / 2;
      if (x < 0)
        x = 0;
      if (y < 0)
        y = 0;
#if GTK_MAJOR_VERSION==2
      gint width, height;
      gdk_drawable_get_size (window, &width, &height);
      pixbuf = gdk_pixbuf_get_from_drawable (NULL, window, NULL /*gdk_colormap_get_system () */ ,
                                             (gint) (x), (gint) (y), 0, 0, scale * grob_size, scale * grob_size);
#else
      pixbuf = gdk_pixbuf_get_from_window (window, (gint) (x), (gint) (y), scale * grob_size, scale * grob_size);
#endif
      if (Denemo.pixbuf)
        g_object_unref (Denemo.pixbuf);
      Denemo.pixbuf = gdk_pixbuf_add_alpha (pixbuf, TRUE, 255, 255, 255);
      g_object_unref (pixbuf);
    }
}

//draw a circle to mark a dragging point
static void
place_spot (cairo_t * cr, gint x, gint y)
{
  cairo_move_to (cr, x, y);
  cairo_arc (cr, x, y, PRINTMARKER / 4, 0.0, 2 * M_PI);
  cairo_fill (cr);
}

//over-draw the evince widget with padding etc ...
static gboolean
overdraw_print (cairo_t * cr)
{
  gint x, y;
  gint message_height = 50;
  get_window_position (&x, &y);

  // gint width, height;
//  width = gdk_pixbuf_get_width( GDK_PIXBUF(Denemo.pixbuf));
  // height = gdk_pixbuf_get_height( GDK_PIXBUF(Denemo.pixbuf));

  // cairo_scale( cr, Denemo.project->movement->preview_zoom, Denemo.project->movement->preview_zoom );
  cairo_translate (cr, -x, -y);
//  gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(Denemo.pixbuf), -x, -y);
  cairo_save (cr);

  if ((get_wysiwyg_info ()->Mark.width > 0.0) && (get_wysiwyg_info ()->stage != WaitingForDrag) && (get_wysiwyg_info ()->stage != DraggingNearEnd) && (get_wysiwyg_info ()->stage != DraggingFarEnd))
    {
      cairo_set_source_rgba (cr, 0.5, 0.5, 1.0, 0.5);
      cairo_rectangle (cr, get_wysiwyg_info ()->Mark.x - PRINTMARKER / 2, get_wysiwyg_info ()->Mark.y - PRINTMARKER / 2, PRINTMARKER, PRINTMARKER);
      cairo_fill (cr);
    }
  if (Denemo.printstatus->invalid /*!print_is_valid */ )
    {
      gchar *headline, *explanation, *error_file = NULL;
      switch (Denemo.printstatus->invalid)
        {
        case 1:
          headline = _("Possibly Invalid");
          explanation = _("Cursor not moved.");
          break;
        case 2:
          headline = _("Check Score.");
          explanation = _("Cursor may have moved to error point in the score.");
          break;
        case 3:
          headline = _("INVALID! try Score->Check Score command.");
          explanation = _("LilyPond could not typeset this score.");
          break;
        }
      if (Denemo.printstatus->invalid)
        error_file = Denemo.printstatus->error_file;
      cairo_set_source_rgba (cr, 0.5, 0.0, 0.0, 0.4);
      cairo_set_font_size (cr, 48.0);
      cairo_move_to (cr, 50, message_height);
      cairo_show_text (cr, headline);
      cairo_set_font_size (cr, 18.0);
      message_height += 30;
      cairo_move_to (cr, 50, message_height);
      cairo_show_text (cr, explanation);

      if (error_file)
        {
          message_height += 20;
          cairo_move_to (cr, 50, message_height);
          cairo_show_text (cr, _("File causing error:"));
          message_height += 20;
          cairo_move_to (cr, 50, message_height);
          cairo_set_source_rgba (cr, 0.0, 0.0, 0.5, 0.4);
          cairo_show_text (cr, error_file);
        }
    }
  {
    DenemoScoreblock *sb = selected_scoreblock ();
    if (sb)
      {
        if (g_list_find (Denemo.project->standard_scoreblocks, sb) == NULL)
          {
            cairo_set_source_rgba (cr, 0.5, 0.7, 0.25, 0.3);
            cairo_set_font_size (cr, 20.0);
            message_height += 30;
            cairo_move_to (cr, 50, message_height);
            cairo_show_text (cr, _("(Custom Score Layout)"));
            cairo_set_font_size (cr, 18.0);
            message_height += 20;
            cairo_move_to (cr, 50, message_height);
            cairo_show_text (cr, _("See View->Score Layout to delete."));
          }
      }
  }
  if (Denemo.printstatus->updating_id && (Denemo.printstatus->background != STATE_NONE))
    {
      cairo_set_source_rgba (cr, 0.5, 0.0, 0.5, 0.3);
      cairo_set_font_size (cr, 64.0);
      cairo_move_to (cr, 0, 0);
      cairo_rotate (cr, M_PI / 4);
      cairo_move_to (cr, 200, 80);
      if (Denemo.printstatus->typeset_type == TYPESET_MOVEMENT)
        cairo_show_text (cr, _("Current Movement"));
      else if (Denemo.printstatus->typeset_type == TYPESET_EXCERPT)
        cairo_show_text (cr, _("Excerpt Only"));
    }




  cairo_restore (cr);

  if (get_wysiwyg_info ()->stage == SelectingFarEnd)
    {
      cairo_set_source_rgba (cr, 0.3, 0.3, 0.7, 0.9);
      //cairo_rectangle (cr, get_wysiwyg_info()->near.x-PRINTMARKER/2, get_wysiwyg_info()->near.y-PRINTMARKER/2, PRINTMARKER, PRINTMARKER );
      cairo_move_to (cr, get_wysiwyg_info ()->nearpoint.x, get_wysiwyg_info ()->nearpoint.y);
      cairo_arc (cr, get_wysiwyg_info ()->nearpoint.x, get_wysiwyg_info ()->nearpoint.y, 1.5, 0.0, 2 * M_PI);
      cairo_fill (cr);
    }
  if (get_wysiwyg_info ()->stage == WaitingForDrag)
    {
      cairo_set_source_rgba (cr, 0.3, 0.3, 0.7, 0.9);
      place_spot (cr, get_wysiwyg_info ()->farpoint.x, get_wysiwyg_info ()->farpoint.y);

      place_spot (cr, get_wysiwyg_info ()->nearpoint.x, get_wysiwyg_info ()->nearpoint.y);

    }
  if ((get_wysiwyg_info ()->stage == WaitingForDrag) || (get_wysiwyg_info ()->stage == DraggingNearEnd) || (get_wysiwyg_info ()->stage == DraggingFarEnd))
    {
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.7);
      cairo_move_to (cr, get_wysiwyg_info ()->nearpoint.x, get_wysiwyg_info ()->nearpoint.y);
      cairo_line_to (cr, get_wysiwyg_info ()->farpoint.x, get_wysiwyg_info ()->farpoint.y);
      cairo_stroke (cr);
      return TRUE;
    }

  if ((get_wysiwyg_info ()->stage == SelectingPoint) || (get_wysiwyg_info ()->stage == WaitingForCurveDrag) || (get_wysiwyg_info ()->stage == Dragging1) || (get_wysiwyg_info ()->stage == Dragging2) || (get_wysiwyg_info ()->stage == Dragging3) || (get_wysiwyg_info ()->stage == Dragging4))
    {
      //place_spot for all non-null points Curve.p1...
      if (get_wysiwyg_info ()->Curve.p1.x)
        {
          place_spot (cr, get_wysiwyg_info ()->Curve.p1.x, get_wysiwyg_info ()->Curve.p1.y);
        }
      if (get_wysiwyg_info ()->Curve.p2.x)
        {
          place_spot (cr, get_wysiwyg_info ()->Curve.p2.x, get_wysiwyg_info ()->Curve.p2.y);
        }
      if (get_wysiwyg_info ()->Curve.p1.x)
        {
          place_spot (cr, get_wysiwyg_info ()->Curve.p3.x, get_wysiwyg_info ()->Curve.p3.y);
        }

      if (get_wysiwyg_info ()->Curve.p4.x)
        {                       //all control points initialized
          place_spot (cr, get_wysiwyg_info ()->Curve.p4.x, get_wysiwyg_info ()->Curve.p4.y);

          cairo_set_source_rgba (cr, 0.5, 0.8, 0.0, 0.7);
          cairo_move_to (cr, get_wysiwyg_info ()->Curve.p1.x, get_wysiwyg_info ()->Curve.p1.y);
          cairo_curve_to (cr, get_wysiwyg_info ()->Curve.p2.x, get_wysiwyg_info ()->Curve.p2.y, get_wysiwyg_info ()->Curve.p3.x, get_wysiwyg_info ()->Curve.p3.y, get_wysiwyg_info ()->Curve.p4.x, get_wysiwyg_info ()->Curve.p4.y);
          cairo_stroke (cr);
        }
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == SelectingReference)
    {
      gint w, h;
      get_window_size (&w, &h);
      cairo_set_source_rgba (cr, 0.0, 0.0, 1.0, 0.7);
      cairo_move_to (cr, get_wysiwyg_info ()->curx, 0);
      cairo_line_to (cr, get_wysiwyg_info ()->curx, h);
      cairo_move_to (cr, 0, get_wysiwyg_info ()->cury);
      cairo_line_to (cr, w, get_wysiwyg_info ()->cury);
      cairo_stroke (cr);
    }
  if (get_wysiwyg_info ()->stage == Offsetting)
    {
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.7);
      cairo_move_to (cr, get_wysiwyg_info ()->Mark.x, get_wysiwyg_info ()->Mark.y);
      cairo_line_to (cr, get_wysiwyg_info ()->curx, get_wysiwyg_info ()->cury);
      cairo_stroke (cr);
      //g_debug("grob is %d %d\n\n\n\n", get_wysiwyg_info()->grob, OBJ_NONE);
      if (Denemo.pixbuf)
        {
          if (get_wysiwyg_info ()->grob == OBJ_NONE)
            {
              guint width = gdk_pixbuf_get_width (GDK_PIXBUF (Denemo.pixbuf));
              guint height = gdk_pixbuf_get_height (GDK_PIXBUF (Denemo.pixbuf));
              cairo_save (cr);
              gdk_cairo_set_source_pixbuf (cr, GDK_PIXBUF (Denemo.pixbuf), get_wysiwyg_info ()->curx - width / 2, get_wysiwyg_info ()->cury - height / 2);
              cairo_rectangle (cr, get_wysiwyg_info ()->curx - width / 2, get_wysiwyg_info ()->cury - height / 2, width, height);

              cairo_fill (cr);
              cairo_restore (cr);
            }
        }
      else
        g_warning ("No pixbuf");
    }
  if (get_wysiwyg_info ()->stage == (unsigned int) Padding)
    {
      gint pad = ABS (get_wysiwyg_info ()->Mark.x - get_wysiwyg_info ()->curx);
      gint w = get_wysiwyg_info ()->nearpoint.x - get_wysiwyg_info ()->Mark.x;
      gint h = get_wysiwyg_info ()->nearpoint.y - get_wysiwyg_info ()->Mark.y;
      cairo_set_source_rgb (cr, 0.5, 0.5, 0.5);
      cairo_rectangle (cr, get_wysiwyg_info ()->Mark.x - pad / 2, get_wysiwyg_info ()->Mark.y - pad / 2, w + pad, h + pad);

      /*GdkWindow *window = */ gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
      // gdk_draw_pixbuf(window, NULL, GDK_PIXBUF(Denemo.pixbuf),
      //    get_wysiwyg_info()->Mark.x+x, get_wysiwyg_info()->Mark.y+y, get_wysiwyg_info()->Mark.x, get_wysiwyg_info()->Mark.y,/* x, y in pixbuf, x,y in window */
      //    w,  h, GDK_RGB_DITHER_NONE,0,0);
    }
  return TRUE;
}

#if GTK_MAJOR_VERSION==3
static gint
printarea_draw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return overdraw_print (cr);
}
#else
static gint
printarea_draw_event (GtkWidget * widget, GdkEventExpose * event)
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
      gtk_widget_queue_draw (Denemo.printarea);
    }
  else
    set_printarea_doc (doc);
  static gboolean shown_once = FALSE;   //Make sure the user knows that the printarea is on screen
  if (!shown_once)
    {
      shown_once = TRUE;
      gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.printarea)));
    }
  return;
}

void
printview_finished (G_GNUC_UNUSED GPid pid, gint status, gboolean print)
{
  progressbar_stop ();
  console_output (_("Done"));
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
      call_out_to_guile ("(FinalizeTypesetting)");
      process_lilypond_errors ((gchar *) get_printfile_pathbasename ());
    }
  else
    {
      if (LilyPond_stderr != -1)
        close (LilyPond_stderr);
      LilyPond_stderr = -1;
    }
  Denemo.printstatus->printpid = GPID_NONE;
  GError *err = NULL;
  set_printarea (&err);
  if (!err && print)
    libevince_print ();
  start_normal_cursor ();

  if (Denemo.printarea)
    {
      GtkWidget *printarea = gtk_widget_get_toplevel (Denemo.printarea);
      //set_toggle (TogglePrintView_STRING, TRUE);
      if (gtk_window_is_active (GTK_WINDOW (printarea)))
        gtk_window_present (GTK_WINDOW (printarea));
    }
}

void
present_print_view_window (void)
{
    
    
  set_toggle (TogglePrintView_STRING, TRUE);
    
  GtkWidget *w = gtk_widget_get_toplevel (Denemo.printarea);
  if (gtk_widget_get_visible (w))
    gtk_window_present (GTK_WINDOW (w));
  else
    gtk_widget_show (w);
}

static gboolean
initialize_typesetting (void)
{
  return call_out_to_guile ("(InitializeTypesetting)");
}

static gboolean
typeset (gboolean force)
{

  if ((force) || (changecount != Denemo.project->changecount))
    {
      if (initialize_typesetting ())
        {
          g_warning ("InitializeTypesetting failed");
          return FALSE;
        }
      DenemoProject *gui = Denemo.project;
      gui->movement->markstaffnum = 0;  //FIXME save and restore selection?
      gui->lilycontrol.excerpt = FALSE;
      create_pdf (FALSE, TRUE);
      changecount = Denemo.project->changecount;
      return TRUE;
    }
  return FALSE;
}

static gboolean
typeset_movement (gboolean force)
{

  if ((force) || (changecount != Denemo.project->changecount))
    {
      if (initialize_typesetting ())
        {
          g_warning ("InitializeTypesetting failed");
          return FALSE;
        }
      DenemoProject *gui = Denemo.project;
      gui->movement->markstaffnum = 0;  //FIXME save and restore selection?
      gui->lilycontrol.excerpt = FALSE;
      create_pdf (FALSE, FALSE);
      return TRUE;
    }
  return FALSE;
}


void
refresh_print_view (G_GNUC_UNUSED gboolean interactive)
{
  start_busy_cursor ();
  if (typeset (FALSE))
    g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
  else
    start_normal_cursor ();
}

void
print_from_print_view (gboolean all_movements)
{
  if (!all_movements)
    changecount = -1;
  start_busy_cursor ();
  if (all_movements ? typeset (FALSE) : typeset_movement (FALSE))
    {
      g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
    }
  else
    {
      start_normal_cursor ();
      libevince_print ();       //printview_finished (Denemo.printstatus->printpid, 0, TRUE);
    }
  if (!all_movements)
    changecount = Denemo.project->changecount;
}

static gchar *
get_thumb_directory (void)
{
  return g_build_filename (g_get_home_dir (), ".thumbnails", "large", NULL);
}

static gchar *
get_thumb_printname (void)
{
  return g_build_filename (locateprintdir (), "denemothumb", NULL);
}

static gchar *
get_thumbname (gchar * uri)
{
  gchar *basethumbname = g_compute_checksum_for_string (G_CHECKSUM_MD5, uri, -1);
  gchar *thumbname = g_strconcat (basethumbname, ".png", NULL);
  g_free (basethumbname);
  return thumbname;
}

/*call back to finish thumbnail processing. */
static void
thumb_finished (gchar * thumbname)
{
  GError *err = NULL;
  g_spawn_close_pid (Denemo.printstatus->printpid);
  Denemo.printstatus->printpid = GPID_NONE;
  gchar *printname = get_thumb_printname ();
  gchar *printpng = g_strconcat (printname, ".png", NULL);

  GdkPixbuf *pbN = gdk_pixbuf_new_from_file_at_scale (printpng, 128, -1, TRUE, &err);
  if (err)
    {
      g_critical ("Thumbnail 128x128 file %s gave an error: %s", printpng, err->message);
      g_error_free (err);
      err = NULL;
    }

  GdkPixbuf *pbL = gdk_pixbuf_new_from_file_at_scale (printpng, 256, -1, TRUE, &err);
  if (err)
    {
      g_critical ("Thumbnail 256x256 file %s gave an error: %s", printpng, err->message);
      g_error_free (err);
      err = NULL;
    }

  //FIXME if pb->height>128 or 256 scale it down...
  if (pbN && pbL)
    {
      gchar *uri = g_strdup_printf ("file://%s", Denemo.project->filename->str);
      unsigned mtime = file_get_mtime (Denemo.project->filename->str);

      gchar *thumbpathN = g_build_filename (thumbnailsdirN, thumbname, NULL);
      gchar *thumbpathL = g_build_filename (thumbnailsdirL, thumbname, NULL);
      gchar *mt = g_strdup_printf ("%u", mtime);

      if (gdk_pixbuf_save (pbN, thumbpathN, "png", &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt, NULL))
        g_info ("Thumbnail generated at %s", thumbpathN);
      else
        g_critical ("Could not save normal thumbnail: %s", err->message);

      err = NULL;
      if (gdk_pixbuf_save (pbL, thumbpathL, "png", &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt, NULL))
        g_info ("Large thumbnail generated at %s", thumbpathL);
      else
        g_critical ("Could not save large thumbnail: %s", err->message);

      //FIXME do the pbN L need freeing???
      g_free (uri);
      g_free (mt);
      g_free (thumbname);
      g_free (thumbpathN);
      g_free (thumbpathL);
    }
  g_free (printname);
  Denemo.printstatus->printpid = GPID_NONE;
  progressbar_stop ();
}

// large_thumbnail_name takes a full path name to a .denemo file and returns the full path to the large thumbnail of that .denemo file. Caller must g_free the returned string
gchar *
large_thumbnail_name (gchar * filepath)
{
  gchar *temp = g_strdup_printf ("file://%s", filepath);
  gchar *ret = get_thumbname (temp);
  g_free (temp);
  return g_build_filename (get_thumb_directory (), ret, NULL);
}

static void
thumbnail_finished (GPid pid, gint status, gpointer data)
{
  if (status)
    g_warning ("Thumbnailer: Lilyond did not end successfully");
}

/***
 *  Create a thumbnail for Denemo.project if needed
 */
gboolean
create_thumbnail (gboolean async, gchar * thumbnail_path)
{
#ifdef G_OS_WIN32
  return FALSE;
#endif

  GError *err = NULL;
  gchar *thumbpathN = NULL;
  gchar *thumbname = NULL;

  if (Denemo.printstatus->printpid != GPID_NONE)
    return FALSE;

  if (!Denemo.project->filename->len)
    return TRUE;

  if (thumbnail_path)
    {
      thumbpathN = thumbnail_path;

      if (!g_path_is_absolute (thumbnail_path))
        thumbpathN = g_build_path (g_get_current_dir (), thumbnail_path, NULL);

      if (!thumbnailsdirN)
        thumbnailsdirN = g_path_get_dirname (thumbpathN);

      if (!thumbnailsdirL)
        thumbnailsdirL = g_path_get_dirname (thumbpathN);

      thumbname = g_path_get_basename (thumbpathN);
    }

  else
    {
      if (!thumbnailsdirN)
        {
          thumbnailsdirN = g_build_filename (g_get_home_dir (), ".thumbnails", "normal", NULL);
          g_mkdir_with_parents (thumbnailsdirN, 0700);
        }
      if (!thumbnailsdirL)
        {
          thumbnailsdirL = g_build_filename (g_get_home_dir (), ".thumbnails", "large", NULL);
          g_mkdir_with_parents (thumbnailsdirL, 0700);
        }

      gchar *uri = g_strdup_printf ("file://%s", Denemo.project->filename->str);
      thumbname = get_thumbname (uri);
      thumbpathN = g_build_filename (thumbnailsdirN, thumbname, NULL);
    }

  //check if thumbnail is newer than file
  struct stat thebuf;
  g_stat (Denemo.project->filename->str, &thebuf);
  unsigned mtime = thebuf.st_mtime;

  thebuf.st_mtime = 0;
  g_stat (thumbpathN, &thebuf);
  unsigned mtime_thumb = thebuf.st_mtime;

  if (mtime_thumb >= mtime)
    {
      g_debug ("Do not update thumbnail %s", thumbpathN);
      return FALSE;
    }

  g_info ("Attempt to create thumbnail %s", thumbpathN);

  gint saved = g_list_index (Denemo.project->movements, Denemo.project->movement);
  Denemo.project->movement = Denemo.project->movements->data;   //Thumbnail is from first movement
  //set selection to thumbnailselection, if not set, to the selection, if not set to first three measures of staff 1
  if (Denemo.project->thumbnail.firststaffmarked)
    memcpy (&Denemo.project->movement->selection, &Denemo.project->thumbnail, sizeof (DenemoSelection));
  else if (Denemo.project->movement->selection.firststaffmarked)
    memcpy (&Denemo.project->thumbnail, &Denemo.project->movement->selection, sizeof (DenemoSelection));
  else
    {
      Denemo.project->thumbnail.firststaffmarked = 1;
      Denemo.project->thumbnail.laststaffmarked = 3;
      Denemo.project->thumbnail.firstmeasuremarked = 1;
      Denemo.project->thumbnail.lastmeasuremarked = 3;
      Denemo.project->thumbnail.firstobjmarked = 0;
      Denemo.project->thumbnail.lastobjmarked = 100;    //or find out how many there are
      memcpy (&Denemo.project->movement->selection, &Denemo.project->thumbnail, sizeof (DenemoSelection));
    }
  Denemo.project->movement->markstaffnum = Denemo.project->movement->selection.firststaffmarked;
  gchar *printname = get_thumb_printname ();
  Denemo.project->lilycontrol.excerpt = TRUE;

  if (async)
    {
      gchar *arguments[] = {
        g_build_filename (get_system_bin_dir (), "denemo", NULL),
        "-n", "-a", "(d-CreateThumbnail #f)(d-Exit)",
        Denemo.project->filename->str,
        NULL
      };
      GPid pid;
      gboolean success = g_spawn_async_with_pipes (NULL,        /* any dir */
                                                   arguments, NULL,     /* env */
                                                   G_SPAWN_SEARCH_PATH, NULL,   /* child setup func */
                                                   NULL,        /* user data */
                                                   &pid,        /* pid */
                                                   NULL,        /* stdin */
                                                   NULL,        /* stdout */
                                                   NULL,        /* stderr */
                                                   &err);
      if (success)
        g_info ("Launched thumbnail subprocess");
      else
        g_critical ("An error happened during thumbnail generation: %s", err->message);
      g_child_watch_add (pid, thumbnail_finished, NULL);
    }
  else
    {
      export_png (printname, NULL, Denemo.project);
      thumb_finished (thumbname);
    }

  g_free (printname);
  Denemo.project->movement = g_list_nth_data (Denemo.project->movements, saved);
  if (Denemo.project->movement == NULL)
    Denemo.project->movement = Denemo.project->movements->data;

  return TRUE;
}

//This gets an offset relative to get_wysiwyg_info()->Mark which must be already setup on entry.
//A patch of the score around the target is dragged over the image with white showing as transparent and a line connects the original and new positions.
gboolean
get_offset (gdouble * offsetx, gdouble * offsety)
{
  get_wysiwyg_info ()->stage = Offsetting;
  gtk_main ();
  if (get_wysiwyg_info ()->stage == Offsetting)
    {
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      *offsetx = (get_wysiwyg_info ()->curx - get_wysiwyg_info ()->Mark.x) / scale;     //Could/Should this better be get_wysiwyg_info()->Reference????
      *offsety = -(get_wysiwyg_info ()->cury - get_wysiwyg_info ()->Mark.y) / scale;


#if 0

//here if figured bass adjust for center
//get_center_staff_offset. Instead in wysiwyg.scm I have used do-center-relative-offset
      gdouble nearadjust = get_center_staff_offset ();
      g_info ("Adjusting %f by %f\n", *offsety, (nearadjust / scale));
      *offsety -= (nearadjust / scale);

#endif

      get_wysiwyg_info ()->stage = STAGE_NONE;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    return FALSE;
}


// start_seeking_end
//if repeatable and grob is slur or beam and request matches gives prompt for slur or beam and goes to Waiting for drag
//else sets up near point to last button press and goes to selecting far end.
static void
start_seeking_end (WwGrob grob)
{
  gchar *msg = (grob == Slur) ? _("Now select the notehead of the note where the slur ends") : (grob == Tie) ? _("Now select the notehead of the note where the tie ends") : _("Now select the notehead of the note where the beam ends");

  if (get_wysiwyg_info ()->repeatable && get_wysiwyg_info ()->grob == grob)
    {
      get_wysiwyg_info ()->stage = WaitingForDrag;
      msg = (get_wysiwyg_info ()->grob == Slur) ? _("Now drag the begin/end markers to suggest slur position/angle\nRight click when done.") : (get_wysiwyg_info ()->grob == Tie) ? _("Now drag the begin/end markers to suggest tie position\nRight click when done.") : _("Now drag the begin/end markers to set position/angle of beam\nRight click when done.");    //FIXME repeated text
    }
  else
    {
      get_wysiwyg_info ()->nearpoint = get_wysiwyg_info ()->near_i = get_wysiwyg_info ()->last_button_press;
      get_wysiwyg_info ()->stage = SelectingFarEnd;
    }
  if (get_wysiwyg_info ()->grob != grob)
    get_wysiwyg_info ()->repeatable = FALSE;
  get_wysiwyg_info ()->grob = grob;
  gtk_widget_show (get_wysiwyg_info ()->dialog);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwyg_info ()->dialog), msg);
  gtk_widget_queue_draw (Denemo.printarea);
}

static gdouble
get_center_staff_offset (void)
{
  gdouble yadjust = 0.0;
  if (Denemo.project->movement->currentobject)
    {
      DenemoObject *obj = (DenemoObject *) Denemo.project->movement->currentobject->data;
      if (obj->type == CHORD)
        {
          chord *thechord = (chord *) obj->object;
          beamandstemdirhelper (Denemo.project->movement);
          if (thechord->notes)
            {
              note *thenote = (note *) (thechord->notes->data);
              gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
              if (staffsize < 1)
                staffsize = 20.0;
              yadjust = -(4 - thenote->y / 5) * staffsize / 8;
              EvDocumentModel *model;
              model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
              gdouble scale = ev_document_model_get_scale (model);
              yadjust *= scale;
            }
        }
    }
  return yadjust;
}

// get_postions gets two y-heights interactively, giving prompts either for slur or beam
//

gboolean
get_positions (gdouble * neary, gdouble * fary, WwGrob grob)
{
  get_wysiwyg_info ()->task = Positions;
  start_seeking_end (grob);     //goes to WaitingForDrag
  gtk_main ();
  if (get_wysiwyg_info ()->stage == WaitingForDrag)
    {
      EvDocumentModel *model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");        //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwyg_info ()->pos.staff, get_wysiwyg_info ()->pos.measure, get_wysiwyg_info ()->pos.object, get_wysiwyg_info ()->pos.leftmeasurenum);   //the cursor to the slur-begin note.
      gdouble nearadjust = get_center_staff_offset ();

      *neary = -(get_wysiwyg_info ()->nearpoint.y - get_wysiwyg_info ()->near_i.y + nearadjust) / scale;
      *fary = -(get_wysiwyg_info ()->farpoint.y - get_wysiwyg_info ()->near_i.y + nearadjust) / scale;  //sic! the value of far_i.y is irrelevant
      get_wysiwyg_info ()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwyg_info ()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

gboolean
get_curve (gdouble * x1, gdouble * y1, gdouble * x2, gdouble * y2, gdouble * x3, gdouble * y3, gdouble * x4, gdouble * y4)
{
  //FIXME check for stage, to avoid re-entering
  get_wysiwyg_info ()->task = Shape;
  get_wysiwyg_info ()->stage = WaitingForCurveDrag;
  gtk_main ();
  if (get_wysiwyg_info ()->stage == WaitingForCurveDrag)
    {
      EvDocumentModel *model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");        //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwyg_info ()->pos.staff, get_wysiwyg_info ()->pos.measure, get_wysiwyg_info ()->pos.object, get_wysiwyg_info ()->pos.leftmeasurenum);   //the cursor to the slur-begin note.
      //!!! is pos set up?
      g_debug ("Reference is %f %f %d %d\n", get_wysiwyg_info ()->Reference.x, get_wysiwyg_info ()->Reference.y, get_wysiwyg_info ()->Curve.p4.x, get_wysiwyg_info ()->Curve.p4.y);
      *x1 = (get_wysiwyg_info ()->Curve.p1.x - get_wysiwyg_info ()->Reference.x) / scale;
      *y1 = -(get_wysiwyg_info ()->Curve.p1.y - get_wysiwyg_info ()->Reference.y) / scale;

      *x2 = (get_wysiwyg_info ()->Curve.p2.x - get_wysiwyg_info ()->Reference.x) / scale;
      *y2 = -(get_wysiwyg_info ()->Curve.p2.y - get_wysiwyg_info ()->Reference.y) / scale;
      *x3 = (get_wysiwyg_info ()->Curve.p3.x - get_wysiwyg_info ()->Reference.x) / scale;
      *y3 = -(get_wysiwyg_info ()->Curve.p3.y - get_wysiwyg_info ()->Reference.y) / scale;
      *x4 = (get_wysiwyg_info ()->Curve.p4.x - get_wysiwyg_info ()->Reference.x) / scale;
      *y4 = -(get_wysiwyg_info ()->Curve.p4.y - get_wysiwyg_info ()->Reference.y) / scale;


      get_wysiwyg_info ()->repeatable = TRUE;

      get_wysiwyg_info ()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwyg_info ()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}


//Gets a new value into get_wysiwyg_info()->Mark.x,y and changes to SelectingFarEnd
gboolean
get_new_target (void)
{
  get_wysiwyg_info ()->stage = SelectingNearEnd;
  g_debug ("Starting main");
  gtk_main ();
  if (get_wysiwyg_info ()->stage == SelectingNearEnd)   //should have changed, but user cancelled
    return FALSE;
  else
    return TRUE;
}

//Gets a new value into get_wysiwyg_info()->Mark.x,y and changes to STAGE_NONE
gboolean
get_new_point (void)
{
  get_wysiwyg_info ()->stage = SelectingPoint;
  g_debug ("Starting main");
  gtk_main ();
  if (get_wysiwyg_info ()->stage == SelectingPoint)     //should have changed, but user cancelled
    return FALSE;
  else
    return TRUE;
}


gboolean
get_reference_point (void)
{
  get_wysiwyg_info ()->stage = SelectingReference;
  memset (&get_wysiwyg_info ()->Curve, 0, sizeof (Curve));
  gtk_main ();
  if (get_wysiwyg_info ()->stage == SelectingReference)
    {                           //should have changed, but the user cancelled
      return FALSE;
    }
  else
    {
      get_wysiwyg_info ()->Reference = get_wysiwyg_info ()->Mark;
      return TRUE;
    }
}

gboolean
get_control_point (gint which)
{
  gboolean ret = TRUE;
  if (get_new_point ())
    {                           //FIXME ... instead make purpose of get_new_target() the argument to it, and use that in the call
      switch (which)
        {
        case 1:
          get_wysiwyg_info ()->Curve.p1.x = get_wysiwyg_info ()->Mark.x;
          get_wysiwyg_info ()->Curve.p1.y = get_wysiwyg_info ()->Mark.y;
          break;
        case 2:
          get_wysiwyg_info ()->Curve.p2.x = get_wysiwyg_info ()->Mark.x;
          get_wysiwyg_info ()->Curve.p2.y = get_wysiwyg_info ()->Mark.y;
          break;
        case 3:
          get_wysiwyg_info ()->Curve.p3.x = get_wysiwyg_info ()->Mark.x;
          get_wysiwyg_info ()->Curve.p3.y = get_wysiwyg_info ()->Mark.y;
          break;
        case 4:
          get_wysiwyg_info ()->Curve.p4.x = get_wysiwyg_info ()->Mark.x;
          get_wysiwyg_info ()->Curve.p4.y = get_wysiwyg_info ()->Mark.y;
          break;
        default:
          g_warning ("Wrong call to get_control_point, no point %d possible", which);
          ret = FALSE;
          break;
        }

    }
  else
    ret = FALSE;
  gtk_widget_queue_draw (Denemo.printarea);
  get_wysiwyg_info ()->stage = (ret ? WaitingForCurveDrag : STAGE_NONE);
  return ret;
}

/*UNUSED
static gint
start_stage (GtkWidget * widget, WwStage stage)
{
  get_wysiwyg_info()->stage = stage;
  return TRUE;
}*/

static void
create_all_pdf (void)
{
  start_busy_cursor ();
  create_pdf (FALSE, TRUE);
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
create_full_score_pdf (void)
{
  start_busy_cursor ();
  create_default_scoreblock ();
  create_pdf (FALSE, TRUE);
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
copy_pdf (void)
{
  //copy file Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle] to user pdf name
  //use get_output_uri_from_scoreblock() as default name.
  //use a gtk_file_chooser like this:
  gchar *filename;
  gchar *outuri = get_output_uri_from_scoreblock ();
  gchar *outpath;
  gchar *outname;
  outuri += strlen ("file://"); //skip the uri bit of it
  outpath = g_path_get_dirname (outuri);
  outname = g_path_get_basename (outuri);
  GtkWidget *chooser = gtk_file_chooser_dialog_new (_("PDF creation"),
                                                    GTK_WINDOW (Denemo.window),
                                                    GTK_FILE_CHOOSER_ACTION_SAVE,
                                                    _("_Cancel"),
                                                    GTK_RESPONSE_REJECT,
                                                    _("_Save"),
                                                    GTK_RESPONSE_ACCEPT, NULL);
  GtkFileFilter *filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("PDF files"));
  gtk_file_filter_add_pattern (filter, "*.pdf");
  gtk_file_filter_add_pattern (filter, "*.PDF");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (chooser), filter);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), outpath);
  gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (chooser), outname);
  gtk_widget_show_all (chooser);
  if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT)
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
  else
    filename = NULL;
  gtk_widget_destroy (chooser);

  if (filename)
    {
      gchar *contents;
      gsize length;


      if (g_file_get_contents (Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle], &contents, &length, NULL))
        {

          if ((!g_file_test (filename, G_FILE_TEST_EXISTS)) || confirm (_("PDF creation"), _("File Exists, overwrite?")))
            {
              if (!g_file_set_contents (filename, contents, length, NULL))
                {
                  gchar *msg = g_strdup_printf (_("Errno %d:\nCould not copy %s to %s. Perhaps because some other process is using the destination file. Try again with a new location\n"),
                                                errno,
                                                Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle],
                                                filename);
                  warningdialog (msg);
                  g_free (msg);
                }
              else
                {
                  gchar *uri = g_strconcat ("file://", filename, NULL);
                  if (strcmp (uri, get_output_uri_from_scoreblock ()))
                    score_status (Denemo.project, TRUE);
                  set_current_scoreblock_uri (uri);

                  //g_print ("I have copied %s to %s (default was %s) uri %s\n", Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle], filename, outname, uri);
                }
              g_free (contents);
            }
        }
      g_free (outpath);
      g_free (outname);
      g_free (filename);
    }
}

static void
create_movement_pdf (void)
{
  return_on_windows_if_printing;
  start_busy_cursor ();
  create_pdf (FALSE, FALSE);
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
create_part_pdf (void)
{
  return_on_windows_if_printing;
  start_busy_cursor ();
  create_pdf (TRUE, TRUE);
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static gint
popup_object_edit_menu (void)
{
  call_out_to_guile ("(EditTarget)");
  return TRUE;
}

/*UNUSED
static gboolean
same_position (DenemoPosition * pos1, DenemoPosition * pos2)
{
  return pos1->movement == pos2->movement && pos1->staff == pos2->staff && pos1->measure == pos2->measure && pos1->object == pos2->object;
}*/

static gboolean
same_target (DenemoTarget * pos1, DenemoTarget * pos2)
{
  return pos1->type == pos2->type && pos1->objnum == pos2->objnum && pos1->measurenum == pos2->measurenum && pos1->staffnum == pos2->staffnum && pos1->mid_c_offset == pos2->mid_c_offset && pos1->directivenum == pos2->directivenum;
}

static gint
action_for_link (G_GNUC_UNUSED EvView * view, EvLinkAction * obj)
{
  if (get_wysiwyg_info ()->stage == TypesetForPlaybackView)
    {
      warningdialog (_("Use the Playback View or re-typeset"));
      return TRUE;
    }

  //g_debug("Link action Mark at %f, %f\n", get_wysiwyg_info()->Mark.x, get_wysiwyg_info()->Mark.y);
  gchar *uri = (gchar *) ev_link_action_get_uri (obj);
  //g_debug("Stage %d\n", get_wysiwyg_info()->stage);
  if ((get_wysiwyg_info ()->stage == SelectingPoint) || (get_wysiwyg_info ()->stage == Dragging1) || (get_wysiwyg_info ()->stage == Dragging2) || (get_wysiwyg_info ()->stage == Dragging3) || (get_wysiwyg_info ()->stage == Dragging4))
    return TRUE;
  if ((get_wysiwyg_info ()->stage == WaitingForDrag) || (get_wysiwyg_info ()->grob == Slur && (get_wysiwyg_info ()->stage == SelectingFarEnd)))
    {
      return TRUE;
    }
  if (get_wysiwyg_info ()->stage == WaitingForCurveDrag || (get_wysiwyg_info ()->stage == SelectingReference))
    return TRUE;

  if (get_wysiwyg_info ()->stage == Offsetting)
    {
      return TRUE;              //?Better take over motion notify so as not to get this while working ...
    }

  //g_debug("acting on external signal %s type=%d directivenum=%d\n", uri, Denemo.project->movement->target.type, Denemo.project->movement->target.directivenum);
  if (uri)
    {
      gchar **orig_vec = g_strsplit (uri, ":", 6);
      gchar **vec = orig_vec;
      if (vec[0] && vec[1] && vec[2] && vec[3] && vec[4] && vec[5] && *vec[5])
        vec++;                  //this will be the case where the file name has a colon in it, (windows drive name) we do not allow for more than one colon. vec[0] is used hereafter.
      if (g_str_has_prefix (uri, "textedit:") && vec[1] && vec[2] && vec[3])
        {
          DenemoTarget old_target = Denemo.project->movement->target;
          get_wysiwyg_info ()->ObjectLocated = goto_lilypond_position (atoi (vec[2]), atoi (vec[3]));   //sets si->target

          if ( (get_wysiwyg_info ()->stage == STAGE_NONE) && LeftButtonPressed && (!shift_held_down ()) && (get_wysiwyg_info ()->ObjectLocated))
            {
              call_out_to_guile ("(d-DenemoPlayCursorToEnd)");
              return TRUE;
            }

          if (get_wysiwyg_info ()->ObjectLocated)
            {
              if (!(get_wysiwyg_info ()->grob == Beam && (get_wysiwyg_info ()->stage == SelectingFarEnd)))
                {
                  get_position (Denemo.project->movement, &get_wysiwyg_info ()->pos);
                  get_wysiwyg_info ()->repeatable = same_target (&old_target, &Denemo.project->movement->target);
                }
              else
                Denemo.project->movement->target = old_target;  //undo the change of target when getting the end of beam note
            }
          else
            get_wysiwyg_info ()->repeatable = FALSE;
          //g_debug("Target type %d\n", Denemo.project->movement->target.type);

          if ((get_wysiwyg_info ()->stage == SelectingNearEnd))
            return TRUE;

          if (get_wysiwyg_info ()->ObjectLocated && Denemo.project->movement->currentobject)
            {
              DenemoDirective *directive = NULL;        //this information is collected but not used FIXME
              DenemoObject *obj = (DenemoObject *) Denemo.project->movement->currentobject->data;
              get_wysiwyg_info ()->grob = OBJ_NONE;
              if (obj->type == LILYDIRECTIVE)
                {
                  directive = ((lilydirective *) obj->object);
                }
              else
                switch (Denemo.project->movement->target.type)
                  {
                  case TARGET_NONE:
                    break;
                  case TARGET_NOTE:
                    if (Denemo.project->movement->target.directivenum)
                      {
                        if (Denemo.project->movement->target.type == TARGET_NOTE)
                          {
                            directive = get_note_directive_number (Denemo.project->movement->target.directivenum);
                          }
                      }
                    {
                      chord *thechord = (chord *) obj->object;
                      if (thechord->figure)
                        get_wysiwyg_info ()->grob = BassFigure;
                    }
                    break;
                  case TARGET_CHORD:
                    g_debug ("Chord directives may be not done");
                    if (Denemo.project->movement->target.directivenum)
                      {
                        //directive = get_chord_directive_number(Denemo.project->movement->target.directivenum);
                        if (obj->type == CHORD)
                          {
                            chord *thechord = (chord *) obj->object;
                            directive = (DenemoDirective *) g_list_nth_data (thechord->directives, Denemo.project->movement->target.directivenum - 1);
                            if (directive && directive->tag)
                              {
                                g_debug ("Found %s", directive->tag->str);
                                //This is things like ToggleTrill ToggleCoda which require different offsets to their center
                                get_wysiwyg_info ()->grob = Articulation;
                              }

                          }
                      }

                    break;
                  case TARGET_SLUR:
                    //g_debug("taking action on slur...");
                    if (get_wysiwyg_info ()->repeatable && get_wysiwyg_info ()->task == Positions)
                      {
                        if (confirm (_("Slur Angle/Position"), _("Repeat Slur Positioning Hint?")))
                          {
                            get_wysiwyg_info ()->stage = WaitingForDrag;
                            gtk_widget_queue_draw (Denemo.printarea);
                            call_out_to_guile ("(GetSlurPositions)");
                          }
                        else
                          get_wysiwyg_info ()->task = TASK_NONE;
                      }
                    else if (get_wysiwyg_info ()->stage == STAGE_NONE && get_wysiwyg_info ()->repeatable && get_wysiwyg_info ()->task == Shape)
                      {
                        if (confirm (_("Slur Shape"), _("Repeat Shaping Slur?")))
                          {
                            get_wysiwyg_info ()->stage = WaitingForCurveDrag;
                            gtk_widget_queue_draw (Denemo.printarea);
                            call_out_to_guile ("(ReshapeSlur)");
                          }
                        else
                          get_wysiwyg_info ()->task = TASK_NONE;
                      }
                    else
                      {
                        get_wysiwyg_info ()->stage = TargetEstablished;
                        get_wysiwyg_info ()->repeatable = FALSE;
                      }
                    break;
                  case TARGET_TIE:
                    if (get_wysiwyg_info ()->stage == STAGE_NONE && get_wysiwyg_info ()->repeatable && get_wysiwyg_info ()->task == Shape)
                      {
                        if (confirm (_("Tie Shape"), _("Repeat Shaping Tie?")))
                          {
                            get_wysiwyg_info ()->stage = WaitingForCurveDrag;
                            gtk_widget_queue_draw (Denemo.printarea);
                            call_out_to_guile ("(ReshapeTie)");
                          }
                        else
                          get_wysiwyg_info ()->task = TASK_NONE;
                      }
                    else
                      {
                        get_wysiwyg_info ()->stage = TargetEstablished;
                        get_wysiwyg_info ()->repeatable = FALSE;
                      }
                    break;

                  default:
                    g_warning ("Target type %d not yet done!!", Denemo.project->movement->target.type);
                    break;
                  }
            }



        }
      else if (g_str_has_prefix (uri, "http:"))
        {
          gchar *text = g_strdup_printf ("(d-Help \"%s\")", uri);
          call_out_to_guile (text);
          g_free (text);
        }
      else if (g_str_has_prefix (uri, "scheme:"))
        {
          gchar *text = uri + strlen ("scheme:");
          if (*text)
            call_out_to_guile (text);
          else
            g_warning ("No script given after scheme:");
        }
      else
        {
          g_warning ("Cannot follow link type %s", orig_vec[0]);
        }
      g_strfreev (orig_vec);
    }
  //!!!! do we want to set_denemo_pixbuf() here if the object is located ???? that is what we are going to drag ....
  g_debug ("Have get_wysiwyg_info()->ObjectLocated (%.2f, %.2f) (%.2f, %.2f)\n", get_wysiwyg_info ()->Mark.x, get_wysiwyg_info ()->Mark.y, get_wysiwyg_info ()->curx, get_wysiwyg_info ()->cury);
  set_denemo_pixbuf ((gint) get_wysiwyg_info ()->curx, (gint) get_wysiwyg_info ()->cury);
  return TRUE;                  //we do not want the evince widget to handle this.
}

static gboolean
in_selected_object (gint x, gint y)
{
  gint xx, yy;
  //g_debug("reading position of mark");
  get_window_position (&xx, &yy);
  x += (xx + PRINTMARKER / 2);
  y += (yy + PRINTMARKER / 2);
  return (x > get_wysiwyg_info ()->Mark.x && y > get_wysiwyg_info ()->Mark.y && x < (get_wysiwyg_info ()->Mark.x + get_wysiwyg_info ()->Mark.width) && y < (get_wysiwyg_info ()->Mark.y + get_wysiwyg_info ()->Mark.height));
}


static gboolean
is_near (gint x, gint y, WwPoint p)
{
  gint xx, yy;
  get_window_position (&xx, &yy);
  x += (xx + PRINTMARKER / 2);
  y += (yy + PRINTMARKER / 2);
  return (ABS (x - p.x) < PRINTMARKER) && (ABS (y - p.y) < PRINTMARKER);
}

static gboolean
printarea_motion_notify (G_GNUC_UNUSED GtkWidget * widget, GdkEventMotion * event)
{
  get_wysiwyg_info ()->ObjectLocated = FALSE;

  if (get_wysiwyg_info ()->stage == WaitingForDrag)
    {
      if ((is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->farpoint)) || (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->nearpoint)))
        {
          gtk_widget_queue_draw (Denemo.printarea);
        }
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == DraggingNearEnd)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      // get_wysiwyg_info()->near.x = xx + (gint)event->x;
      get_wysiwyg_info ()->nearpoint.y = yy + (gint) event->y;  //g_debug("near y becomes %d\n", get_wysiwyg_info()->near.y);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == DraggingFarEnd)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      // get_wysiwyg_info()->far.x = xx + (gint)event->x;
      get_wysiwyg_info ()->farpoint.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == Dragging1)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwyg_info ()->Curve.p1.x = xx + (gint) event->x;
      get_wysiwyg_info ()->Curve.p1.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == Dragging2)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwyg_info ()->Curve.p2.x = xx + (gint) event->x;
      get_wysiwyg_info ()->Curve.p2.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == Dragging3)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwyg_info ()->Curve.p3.x = xx + (gint) event->x;
      get_wysiwyg_info ()->Curve.p3.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == Dragging4)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwyg_info ()->Curve.p4.x = xx + (gint) event->x;
      get_wysiwyg_info ()->Curve.p4.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }





  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwyg_info ()->curx = xx + (gint) event->x;
  get_wysiwyg_info ()->cury = yy + (gint) event->y;


  if ((get_wysiwyg_info ()->stage == Offsetting) || (get_wysiwyg_info ()->stage == SelectingReference))
    {
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (in_selected_object ((int) event->x, (int) event->x))
    {
      return TRUE;              //we have handled this.
    }
  return FALSE;                 //propagate further
}



/* UNUSED
static void
normalize (void)
{
  if (get_wysiwyg_info()->near.x < get_wysiwyg_info()->Mark.x)
    {
      gdouble temp = get_wysiwyg_info()->near.x;
      get_wysiwyg_info()->near.x = get_wysiwyg_info()->Mark.x;
      get_wysiwyg_info()->Mark.x = temp;
    }
  if (get_wysiwyg_info()->near.y < get_wysiwyg_info()->Mark.y)
    {
      gdouble temp = get_wysiwyg_info()->near.y;
      get_wysiwyg_info()->near.y = get_wysiwyg_info()->Mark.y;
      get_wysiwyg_info()->Mark.y = temp;
    }
  if (get_wysiwyg_info()->Mark.x == get_wysiwyg_info()->near.x)
    get_wysiwyg_info()->near.x++;
  if (get_wysiwyg_info()->Mark.y == get_wysiwyg_info()->near.y)
    get_wysiwyg_info()->near.y++;

}
*/

static void
apply_tweak (void)
{
  //g_debug("Apply tweak Quitting with %d %d", get_wysiwyg_info()->stage, get_wysiwyg_info()->grob);
  gtk_main_quit ();
  return;
  if (get_wysiwyg_info ()->stage == Offsetting)
    {
      gtk_main_quit ();
    }
  else
    {
      start_normal_cursor ();
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.project->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwyg_info ()->pos.staff, get_wysiwyg_info ()->pos.measure, get_wysiwyg_info ()->pos.object, get_wysiwyg_info ()->pos.leftmeasurenum);   //the cursor to the slur-begin note.
      gdouble nearadjust = get_center_staff_offset ();

      gdouble neary = -(get_wysiwyg_info ()->nearpoint.y - get_wysiwyg_info ()->near_i.y + nearadjust) / scale;
      gdouble fary = -(get_wysiwyg_info ()->farpoint.y - get_wysiwyg_info ()->near_i.y + nearadjust) / scale;   //sic! the value of far_i.y is irrelevant
      //g_debug("near %d %d far %d %d\n", get_wysiwyg_info()->near.y, get_wysiwyg_info()->near_i.y, get_wysiwyg_info()->far.y, get_wysiwyg_info()->far_i.y);
      gchar *script = (get_wysiwyg_info ()->grob == Slur) ? g_strdup_printf ("(SetSlurPositions \"%.1f\" \"%.1f\")", neary, fary) : g_strdup_printf ("(SetBeamPositions \"%.1f\" \"%.1f\")", neary, fary);
      //Move back to the correct place in the score
      goto_movement_staff_obj (NULL, -1, get_wysiwyg_info ()->pos.staff, get_wysiwyg_info ()->pos.measure, get_wysiwyg_info ()->pos.object, get_wysiwyg_info ()->pos.leftmeasurenum);
      call_out_to_guile (script);
      g_free (script);
      get_wysiwyg_info ()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwyg_info ()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
    }

}

static void
cancel_tweak (void)
{
  //gtk_widget_set_tooltip_markup(gtk_widget_get_parent(Denemo.printarea), standard_tooltip);
  gtk_widget_set_tooltip_markup (gtk_widget_get_parent (Denemo.printarea), NULL);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwyg_info ()->dialog), _("Operation Cancelled"));
  gtk_widget_show (get_wysiwyg_info ()->dialog);
  get_wysiwyg_info ()->stage = STAGE_NONE;
  gtk_widget_queue_draw (Denemo.printarea);
  gtk_main_quit ();
}

static void
repeat_tweak (void)
{
  if (get_wysiwyg_info ()->grob == Slur)
    call_out_to_guile ("(EditSlur)");
  else if (get_wysiwyg_info ()->grob == Tie)
    call_out_to_guile ("(EditTie)");
  else if (get_wysiwyg_info ()->grob == Beam)   //if(get_wysiwyg_info()->repeatable && get_wysiwyg_info()->grob==(slur?Slur:Beam))
    call_out_to_guile ("(GetBeamPositions)");
  else
    warningdialog (_("Do not know what to repeat"));
}

static void
set_score_size (void)
{
  call_out_to_guile ("(d-SetFontSize)");
}

static void
help_tweak (void)
{
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwyg_info ()->dialog), _("To tweak the positions of objects (and more) move the mouse until the hand pointer appears\nClick on the object and follow the prompts.\nFor beams, click on the notehead of the note where the beam starts."));
  gtk_widget_show (get_wysiwyg_info ()->dialog);
}

static void
toggle_lilypond_structure_markers (void)
{
  call_out_to_guile ("(d-ToggleWysiwygMarks)");
  call_out_to_guile ("(d-ToggleCurveControl)");
}

static gint
popup_tweak_menu (void)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  if (get_wysiwyg_info ()->stage == WaitingForDrag || get_wysiwyg_info ()->stage == WaitingForCurveDrag || get_wysiwyg_info ()->stage == Offsetting)
    {
      item = gtk_menu_item_new_with_label (_("Apply"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (apply_tweak), NULL);
      item = gtk_menu_item_new_with_label (_("Cancel"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (cancel_tweak), NULL);
    }


  if (get_wysiwyg_info ()->stage == STAGE_NONE)
    {
      item = gtk_menu_item_new_with_label (_("Help for Tweaks"));
      gtk_widget_set_tooltip_markup (item, _("This window can be used to tweak the typesetting that LilyPond does in the case that it is not optimal"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (help_tweak), NULL);

      if (!current_scoreblock_is_custom ())
        {
          item = gtk_menu_item_new_with_label (_("Red dots and crosses (Off/On)"));
          gtk_widget_set_tooltip_markup (item, _("The exact positions of the graphical components of the score will be labelled with red dots\n" "and the control points for curves with red crosses for accurate tweaks\nTurn these off before printing!"));
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (toggle_lilypond_structure_markers), NULL);
        }
      item = gtk_menu_item_new_with_label (_("Score Size"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (set_score_size), NULL);

      if (get_wysiwyg_info ()->repeatable)
        {                       //never true
          item = gtk_menu_item_new_with_label (_("Repeat"));
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (repeat_tweak), NULL);
        }
    }



  gtk_widget_show_all (menu);

  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
  return TRUE;
}



static gint
printarea_button_press (G_GNUC_UNUSED GtkWidget * widget, GdkEventButton * event)
{
  //DenemoTargetType type = Denemo.project->movement->target.type;
  gboolean left = (event->button == 1);
  gboolean right = !left;
  LeftButtonPressed = left;
  //g_debug("Button press %d, %d %d\n",(int)event->x , (int)event->y, left);

  if (audio_is_playing ())
    {
      call_out_to_guile ("(DenemoStop)");
      switch_back_to_main_window ();
    }

  get_wysiwyg_info ()->button = event->button;
  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwyg_info ()->last_button_press.x = xx + event->x;
  get_wysiwyg_info ()->last_button_press.y = yy + event->y;
  gboolean hotspot = is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->nearpoint) || (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->farpoint));
  //g_debug("stage %d hotspot %d", get_wysiwyg_info()->stage, hotspot);
  if (left && (get_wysiwyg_info ()->stage == WaitingForDrag) && !hotspot)
    {
      popup_tweak_menu ();      //other stages STAGE_NONE for example. And make the offer of Repeat if appropriate...
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == WaitingForCurveDrag)
    {
      if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->Curve.p1))
        {
          get_wysiwyg_info ()->stage = Dragging1;       //gtk_widget_queue_draw (Denemo.printarea);
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->Curve.p2))
        {
          get_wysiwyg_info ()->stage = Dragging2;
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->Curve.p3))
        {
          get_wysiwyg_info ()->stage = Dragging3;
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->Curve.p4))
        {
          get_wysiwyg_info ()->stage = Dragging4;
          return TRUE;
        }
      popup_tweak_menu ();
      return TRUE;
    }
  if (right && get_wysiwyg_info ()->stage == WaitingForDrag && !hotspot)
    {
      apply_tweak ();
    }
  if ((get_wysiwyg_info ()->stage == SelectingNearEnd) || (get_wysiwyg_info ()->stage == SelectingReference))
    {
      get_wysiwyg_info ()->near_i = get_wysiwyg_info ()->nearpoint = get_wysiwyg_info ()->last_button_press;    //struct copy
      return TRUE;
    }
  if (get_wysiwyg_info ()->stage == SelectingPoint)
    {                           //handle on release as user may move before releasing
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == SelectingFarEnd)
    {                           //handle on release, after cursor has moved to note
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == WaitingForDrag)
    {
      if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->nearpoint))
        {
          get_wysiwyg_info ()->stage = DraggingNearEnd;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwyg_info ()->farpoint))
        {
          get_wysiwyg_info ()->stage = DraggingFarEnd;
        }
      //???text dialog
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }




  if (in_selected_object ((gint) event->x, (gint) event->y))
    {
      //g_debug("Popping up menu");
      popup_object_edit_menu ();
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage != Offsetting)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwyg_info ()->curx = xx + event->x;
      get_wysiwyg_info ()->cury = yy + event->y;
    }
  return TRUE;
}

static gint
printarea_button_release (G_GNUC_UNUSED GtkWidget * widget, GdkEventButton * event)
{
//g_debug("stage %d\n", get_wysiwyg_info()->stage);
  gboolean left = (event->button == 1);
  gboolean right = !left;
  gboolean object_located_on_entry = get_wysiwyg_info ()->ObjectLocated;
  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwyg_info ()->last_button_release.x = xx + event->x;
  get_wysiwyg_info ()->last_button_release.y = yy + event->y;
  if (left && get_wysiwyg_info ()->ObjectLocated)
    gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.scorearea)));
  //g_debug("Button release %d, %d\n",(int)event->x , (int)event->y);

  if (get_wysiwyg_info ()->stage == Dragging1)
    {
      get_wysiwyg_info ()->Curve.p1.x = get_wysiwyg_info ()->last_button_release.x;
      get_wysiwyg_info ()->Curve.p1.y = get_wysiwyg_info ()->last_button_release.y;
      get_wysiwyg_info ()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwyg_info ()->stage == Dragging2)
    {
      get_wysiwyg_info ()->Curve.p2.x = get_wysiwyg_info ()->last_button_release.x;
      get_wysiwyg_info ()->Curve.p2.y = get_wysiwyg_info ()->last_button_release.y;
      get_wysiwyg_info ()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwyg_info ()->stage == Dragging3)
    {
      get_wysiwyg_info ()->Curve.p3.x = get_wysiwyg_info ()->last_button_release.x;
      get_wysiwyg_info ()->Curve.p3.y = get_wysiwyg_info ()->last_button_release.y;
      get_wysiwyg_info ()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwyg_info ()->stage == Dragging4)
    {
      get_wysiwyg_info ()->Curve.p4.x = get_wysiwyg_info ()->last_button_release.x;
      get_wysiwyg_info ()->Curve.p4.y = get_wysiwyg_info ()->last_button_release.y;
      get_wysiwyg_info ()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == WaitingForCurveDrag)
    {
      g_debug ("End of curve drag - should give menu if right click");
      g_debug ("Check level > 1  %d", gtk_main_level ());
      gtk_main_quit ();
      return TRUE;
    }


  if (get_wysiwyg_info ()->ObjectLocated || (get_wysiwyg_info ()->stage == SelectingNearEnd) || (get_wysiwyg_info ()->stage == SelectingReference))
    {
      get_wysiwyg_info ()->Mark.width = get_wysiwyg_info ()->Mark.height = PRINTMARKER;
      gtk_widget_queue_draw (Denemo.printarea);
      get_wysiwyg_info ()->Mark.x = event->x + xx;
      get_wysiwyg_info ()->Mark.y = event->y + yy;
      // switch_back_to_main_window();
      get_wysiwyg_info ()->ObjectLocated = FALSE;
    }

  if ( /* left && */ get_wysiwyg_info ()->stage == TargetEstablished)
    {
      if (Denemo.project->movement->target.type == TARGET_SLUR)
        {
          get_wysiwyg_info ()->grob = Slur;
          call_out_to_guile ("(EditSlur)");
          get_wysiwyg_info ()->stage = STAGE_NONE;
          return TRUE;
        }
      else if (Denemo.project->movement->target.type == TARGET_TIE)
        {
          get_wysiwyg_info ()->grob = Tie;
          call_out_to_guile ("(EditTie)");
          get_wysiwyg_info ()->stage = STAGE_NONE;
          return TRUE;
        }

    }
  if (get_wysiwyg_info ()->stage == SelectingNearEnd)
    {
      get_wysiwyg_info ()->stage = SelectingFarEnd;
      gtk_main_quit ();
      return TRUE;
    }

  if (get_wysiwyg_info ()->stage == SelectingReference)
    {
      get_wysiwyg_info ()->stage = STAGE_NONE;
      gtk_main_quit ();
      return TRUE;
    }
  if (get_wysiwyg_info ()->stage == SelectingPoint)
    {
      get_wysiwyg_info ()->stage = STAGE_NONE;
      get_wysiwyg_info ()->Mark.width = get_wysiwyg_info ()->Mark.height = PRINTMARKER; //width=0 means no mark
      get_wysiwyg_info ()->Mark.x = event->x + xx;
      get_wysiwyg_info ()->Mark.y = event->y + yy;
      g_debug ("Selected point, %f %f \n", get_wysiwyg_info ()->Mark.x, get_wysiwyg_info ()->Mark.y);
      gtk_main_quit ();
      return TRUE;
    }
  if (get_wysiwyg_info ()->stage == SelectingFarEnd)
    {
      get_wysiwyg_info ()->far_i = get_wysiwyg_info ()->farpoint = get_wysiwyg_info ()->last_button_release;
      get_wysiwyg_info ()->stage = WaitingForDrag;
      //first post-insert a \stemNeutral if beaming
      if (get_wysiwyg_info ()->grob == Beam)
        {
          call_out_to_guile ("(d-MoveCursorRight)(if (not (StemDirective?)) (begin   (d-InfoDialog (_ \"Note that a Directive to revert to automatic stems is now placed after the beamed notes. Edit this as needed for the voice you are using.\")) (d-InsertStem)))");
        }
      //g_debug("yadjust %f %f\n", nearadjust, faradjust);
      //here we move the cursor back to the beam/slur start
      goto_movement_staff_obj (NULL, -1, get_wysiwyg_info ()->pos.staff, get_wysiwyg_info ()->pos.measure, get_wysiwyg_info ()->pos.object, get_wysiwyg_info ()->pos.leftmeasurenum);
      gtk_widget_queue_draw (Denemo.printarea);
      gchar *msg = (get_wysiwyg_info ()->grob == Slur) ? _("Now drag the begin/end markers to suggest slur position/angle\nRight click when done.") : _("Now drag the begin/end markers to set position/angle of beam\nRight click when done.");

      gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwyg_info ()->dialog), msg);
      gtk_widget_show (get_wysiwyg_info ()->dialog);
      return TRUE;
    }
  if ((get_wysiwyg_info ()->stage == DraggingNearEnd) || (get_wysiwyg_info ()->stage == DraggingFarEnd))
    {
      get_wysiwyg_info ()->stage = WaitingForDrag;
      return TRUE;
    }



  if (get_wysiwyg_info ()->stage == Offsetting)
    {
      if (right)
        popup_tweak_menu ();
      else
        {
          g_debug ("Offsetting quitting with %d %d", get_wysiwyg_info ()->stage, get_wysiwyg_info ()->grob);
          //The offset depends on the object being dragged. ToogleTrill sign uses bottom right, ToggleCoda uses center left.
          //      get_wysiwyg_info()->curx +=18;//for trill
          //      get_wysiwyg_info()->cury +=18;//for coda, mordent ...
          //      ???


          gtk_main_quit ();
        }
      return TRUE;
    }

  // \once \override DynamicLineSpanner #'padding = #10 setting padding for cresc and dimin
  // \once \override DynamicLineSpanner #'Y-offset = #-10 to move a cresc or dimin vertically downwards.
  // \once \override DynamicLineSpanner #'direction = #1 to place above/below (-1)
  //g_debug("Stage %d object loc %d left %d", get_wysiwyg_info()->stage, object_located_on_entry, left);
  if (right && (get_wysiwyg_info ()->stage == STAGE_NONE))
    {
      if (object_located_on_entry)      //set by action_for_link
        popup_object_edit_menu ();
      else
        popup_tweak_menu ();
      return TRUE;
    }


  return TRUE;

  return TRUE;
}

// Denemo.printstatus->mtime = file_get_mtime(filename); use in get_printfile_pathbasename

static void
typeset_control (gpointer data)
{
  static gpointer last_data = NULL;
  static GString *last_script = NULL;
  gint markstaff = Denemo.project->movement->markstaffnum;
  Denemo.project->movement->markstaffnum = 0;

  //g_debug("typeset control with %d : print view is %d\n",  Denemo.project->textwindow && gtk_widget_get_visible(Denemo.project->textwindow), Denemo.printstatus->background==STATE_ON);
//  if(Denemo.project->textwindow && gtk_widget_get_visible(Denemo.project->textwindow) && (Denemo.printstatus->background==STATE_ON) && Denemo.printstatus->typeset_type!=TYPESET_ALL_MOVEMENTS)
//                      return;
  if (Denemo.printstatus->background != STATE_ON)
    Denemo.printstatus->background = 0; //STATE_NONE
  if (last_script == NULL)
    last_script = g_string_new ("(d-PrintView)");

  if (data == create_all_pdf)
    create_all_pdf ();
  else if (data == create_full_score_pdf)
    create_full_score_pdf ();
  else if (data == create_movement_pdf)
    create_movement_pdf ();
  else if (data == create_part_pdf)
    create_part_pdf ();
  else if (data != NULL)
    {
      if (Denemo.printstatus->background == STATE_ON)
        {
          save_selection (Denemo.project->movement);
          if (Denemo.printstatus->typeset_type == TYPESET_ALL_MOVEMENTS)
            {
              Denemo.project->movement->markstaffnum = 0;
              create_pdf (FALSE, TRUE);
            }
          else if (Denemo.printstatus->typeset_type == TYPESET_MOVEMENT)
            {
              Denemo.project->movement->markstaffnum = 0;
              create_pdf (FALSE, FALSE);
            }
          else
            {
              gint value = Denemo.project->movement->currentstaffnum - Denemo.printstatus->first_staff;
              if (value < 1)
                value = 1;
              Denemo.project->movement->markstaffnum = Denemo.project->movement->selection.firststaffmarked = value;

              value = Denemo.project->movement->currentstaffnum + Denemo.printstatus->last_staff;
              if (value < 1)
                value = 1;
              Denemo.project->movement->selection.laststaffmarked = value;

              value = Denemo.project->movement->currentmeasurenum - Denemo.printstatus->first_measure;
              if (value < 1)
                value = 1;
              Denemo.project->movement->selection.firstmeasuremarked = value;

              value = Denemo.project->movement->currentmeasurenum + Denemo.printstatus->last_measure;
              if (value < 1)
                value = 1;
              Denemo.project->movement->selection.lastmeasuremarked = value;

              Denemo.project->movement->selection.firstobjmarked = 0;
              Denemo.project->movement->selection.lastobjmarked = G_MAXINT - 1; //counts from 0, +1 must be valid
              create_pdf (FALSE, FALSE);        //this movement only cursor-relative selection of measures
            }
        }
      else
        {
          start_busy_cursor ();
          create_pdf (FALSE, TRUE);
        }
      g_string_assign (last_script, data);
      last_data = NULL;
      g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
      if (Denemo.printstatus->background == STATE_ON)
        {
          restore_selection (Denemo.project->movement);
        }
      Denemo.project->movement->markstaffnum = markstaff;
      goto END;
    }
  else
    {                           //data is NULL, repeat last typeset
      if (last_data)
        {
          ((void (*)()) last_data) ();
          Denemo.project->movement->markstaffnum = markstaff;
          goto END;
        }
      else if (last_script->len)
        {

          start_busy_cursor ();
          call_out_to_guile (last_script->str);
          g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));

          Denemo.project->movement->markstaffnum = markstaff;
          goto END;
        }
      Denemo.project->movement->markstaffnum = markstaff;

      goto END;
    }
  last_data = data;
  Denemo.project->movement->markstaffnum = markstaff;

END:
  //bring view back to show cursor
  if (Denemo.textview)
    gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (Denemo.textview), gtk_text_buffer_get_insert (Denemo.textbuffer), 0.0, TRUE, 0.5, 0.5);

}

//Callback for the command PrintView
//Ensures the print view window is visible.
//if refresh_if_needed it calls create_all_pdf() provided the score has changed
void
implement_show_print_view (gboolean refresh_if_needed)
{
  present_print_view_window ();
#ifndef G_OS_WIN32   //intended to avoid killing already running typeset on windows
  if (refresh_if_needed && (changecount != Denemo.project->changecount || Denemo.project->lilysync != Denemo.project->changecount))
    {
      if (Denemo.prefs.manualtypeset && (!initialize_typesetting ()))
        {
          typeset_control (create_all_pdf);
          changecount = Denemo.project->changecount;
        }
    }
#endif
}

gboolean
printview_is_stale (void)
{
  return ((changecount != Denemo.project->changecount) || (Denemo.project->lilysync != Denemo.project->changecount));
}

void
typeset_current_layout (void)
{
  return_on_windows_if_printing;
  typeset_control (create_all_pdf);
}

/* typeset the score, and store the passed script for refresh purposes*/
gboolean
typeset_for_script (gchar * script)
{
  return1_on_windows_if_printing;
  typeset_control (script);
  start_busy_cursor ();
  show_print_view (NULL, NULL);
  return TRUE;
}

static void
page_display (G_GNUC_UNUSED GtkWidget * button, gint page_increment)
{
  gint i;
  for (i = 0; i < page_increment; i++)
    ev_view_next_page ((EvView *) Denemo.printarea);
  for (i = 0; i > page_increment; i--)
    ev_view_previous_page ((EvView *) Denemo.printarea);
}

static void
dual_page (GtkWidget * button)
{
  GError *err = NULL;
  gboolean duplex = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "Duplex"));
  gtk_button_set_label (GTK_BUTTON (button), duplex ? _("Duplex") : _("Single"));
  g_object_set_data (G_OBJECT (Denemo.printarea), "Duplex", GINT_TO_POINTER (!g_object_get_data (G_OBJECT (Denemo.printarea), "Duplex")));
  set_printarea (&err);
}
static void
invert_page (GtkWidget * button)
{
  GError *err = NULL; 
  g_object_set_data (G_OBJECT (Denemo.printarea), "Invert", GINT_TO_POINTER (!g_object_get_data (G_OBJECT (Denemo.printarea), "Invert")));
  set_printarea (&err);
}
#if 0
gint
printarea_scroll_event (GtkWidget * widget, GdkEventScroll * event)
{
  switch (event->direction)
    {
    case GDK_SCROLL_UP:
      //g_debug("scroll up event\n");
      break;
    case GDK_SCROLL_DOWN:
      //g_debug("scroll down event\n");
      break;
    }
  return FALSE;
}
#endif
static void
typeset_action (G_GNUC_UNUSED GtkWidget * button, gpointer data)
{
  if (initialize_typesetting ())
    {
      g_warning ("InitializeTypesetting failed");
    }
  else
    typeset_control (data);
}

void
typeset_part (void)
{
  typeset_control (create_part_pdf);
}

static gboolean
retypeset (void)
{
  static gint firstmeasure, lastmeasure, firststaff, laststaff, movementnum;
  DenemoMovement *si = Denemo.project->movement;
  if ((Denemo.printstatus->printpid == GPID_NONE) && (gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.printarea))))
    {
      if (Denemo.printstatus->typeset_type == TYPESET_ALL_MOVEMENTS)
        {
          if ((changecount != Denemo.project->changecount) || (Denemo.project->lilysync != Denemo.project->changecount))
            {
              Denemo.printstatus->background = STATE_ON;
              typeset_control ("(d-Info \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
              Denemo.printstatus->background = STATE_OFF;
              changecount = Denemo.project->changecount;
            }
        }
      else if ((changecount != Denemo.project->changecount) || (Denemo.project->lilysync != Denemo.project->changecount) || (si->currentmovementnum != movementnum) || ((Denemo.printstatus->typeset_type == TYPESET_EXCERPT) && (si->currentmeasurenum < firstmeasure || si->currentmeasurenum > lastmeasure || si->currentstaffnum < firststaff || si->currentstaffnum > laststaff)))
        {
          firstmeasure = si->currentmeasurenum - Denemo.printstatus->first_measure;
          if (firstmeasure < 0)
            firstmeasure = 0;
          lastmeasure = si->currentmeasurenum + Denemo.printstatus->last_measure;
          firststaff = si->currentstaffnum - Denemo.printstatus->first_staff;
          if (firststaff < 0)
            firststaff = 0;
          laststaff = si->currentstaffnum + Denemo.printstatus->last_staff;
          movementnum = si->currentmovementnum;
          Denemo.printstatus->background = STATE_ON;
          typeset_control ("(disp \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
          Denemo.printstatus->background = STATE_OFF;
          changecount = Denemo.project->changecount;
        }
    }
  return TRUE;                  //continue
}

GtkWidget *ContinuousUpdateButton = NULL;

//turn the continuous update off and on
static void
toggle_updates (void)
{
  if (Denemo.printstatus->updating_id)
    {
      g_source_remove (Denemo.printstatus->updating_id);
      Denemo.printstatus->updating_id = 0;
      gtk_button_set_label (GTK_BUTTON (ContinuousUpdateButton), MANUAL);
      if (Denemo.prefs.persistence)
        Denemo.prefs.manualtypeset = TRUE;
      gtk_window_set_transient_for (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.printarea)), NULL);
    }
  else
    {
      if (Denemo.prefs.typesetrefresh)
        Denemo.printstatus->updating_id = g_timeout_add (Denemo.prefs.typesetrefresh, (GSourceFunc) retypeset, NULL);
      else
        Denemo.printstatus->updating_id = g_idle_add ((GSourceFunc) retypeset, NULL);
      gtk_button_set_label (GTK_BUTTON (ContinuousUpdateButton), CONTINUOUS);
      if (Denemo.prefs.persistence)
        Denemo.prefs.manualtypeset = FALSE;
    }
}

void
set_continuous_typesetting (gboolean on)
{
  gboolean current = Denemo.printstatus->updating_id;
  if ((current && !on) || ((!current) && on))
    toggle_updates ();
}

static void
set_typeset_type (GtkWidget * radiobutton, GtkWidget * rangebox)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radiobutton)))
    {
      changecount = 0;          //reset so that a retype occurs
      gint index = g_slist_index (gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton)), radiobutton);
      //g_debug("Get %s at %d\n", gtk_button_get_label(GTK_BUTTON(radiobutton)), index);
      switch (index)
        {
        case 0:
          Denemo.printstatus->typeset_type = TYPESET_EXCERPT;
          gtk_widget_set_sensitive (rangebox, TRUE);
          break;
        case 1:
          Denemo.printstatus->typeset_type = TYPESET_MOVEMENT;
          gtk_widget_set_sensitive (rangebox, FALSE);
          break;
        case 2:
          Denemo.printstatus->typeset_type = TYPESET_ALL_MOVEMENTS;
          gtk_widget_set_sensitive (rangebox, FALSE);
        }
      if (Denemo.prefs.persistence)
        Denemo.prefs.typesettype = Denemo.printstatus->typeset_type;
    }
}

static void
value_change (GtkWidget * spinner, gint * value)
{
  *value = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (spinner));
  if (Denemo.prefs.persistence)
    {
      Denemo.prefs.firstmeasure = Denemo.printstatus->first_measure;
      Denemo.prefs.lastmeasure = Denemo.printstatus->last_measure;
      Denemo.prefs.firststaff = Denemo.printstatus->first_staff;
      Denemo.prefs.laststaff = Denemo.printstatus->last_staff;
    }
}

static void
range_dialog (void)
{
  static GtkWidget *dialog;
  if (dialog == NULL)
    {
      dialog = gtk_dialog_new ();
      GtkWidget *area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
      GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (area), vbox);
      GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
      GtkWidget *rangeBox = gtk_vbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), rangeBox, TRUE, TRUE, 0);

      gtk_box_pack_start (GTK_BOX (rangeBox), hbox, TRUE, TRUE, 0);

      GtkWidget *label = gtk_label_new (_("Measures before cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      GtkWidget *spinner = gtk_spin_button_new_with_range (0, 1000, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &Denemo.printstatus->first_measure);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), Denemo.printstatus->first_measure);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);


      label = gtk_label_new (_("Measures after cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 1000, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &Denemo.printstatus->last_measure);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), Denemo.printstatus->last_measure);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (rangeBox), hbox, TRUE, TRUE, 0);

      label = gtk_label_new (_("Staffs before cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 100, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &Denemo.printstatus->first_staff);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), Denemo.printstatus->first_staff);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      label = gtk_label_new (_("Staffs after cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 100, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &Denemo.printstatus->last_staff);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), Denemo.printstatus->last_staff);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);

      //   hbox = gtk_hbox_new (FALSE, 1);
      //  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);


      GtkWidget *button0 = gtk_radio_button_new_with_label_from_widget (NULL, _("All Movements"));
      g_signal_connect (G_OBJECT (button0), "toggled", G_CALLBACK (set_typeset_type), rangeBox);
      gtk_widget_set_tooltip_text (button0, _("If checked the current layout is re-typeset at every change"));
      gtk_box_pack_start (GTK_BOX (hbox), button0, TRUE, TRUE, 0);

      GtkWidget *button1 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (button0), _("Current Movement"));
      g_signal_connect (G_OBJECT (button1), "toggled", G_CALLBACK (set_typeset_type), rangeBox);
      gtk_widget_set_tooltip_text (button1, _("If checked the current movement is re-typeset at every change"));
      gtk_box_pack_start (GTK_BOX (hbox), button1, TRUE, TRUE, 0);


      GtkWidget *button2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (button0), _("Cursor Context"));
      g_signal_connect (G_OBJECT (button2), "toggled", G_CALLBACK (set_typeset_type), rangeBox);
      gtk_widget_set_tooltip_text (button2, _("If checked the range around the current cursor position is re-typeset at every change or when the cursor moves out of range."));
      gtk_box_pack_start (GTK_BOX (hbox), button2, TRUE, TRUE, 0);
      gtk_widget_set_sensitive (rangeBox, FALSE);
      if (Denemo.prefs.typesettype == TYPESET_MOVEMENT)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button1), TRUE);
      if (Denemo.prefs.typesettype == TYPESET_EXCERPT)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button2), TRUE);

      g_signal_connect (dialog, "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
      gtk_widget_show_all (dialog);
    }
  else
    gtk_widget_show (dialog);

}

static GtkWidget *
get_updates_menu (GtkWidget * button)
{
  static GtkWidget *menu;
  if (menu == NULL)
    {
      GtkWidget *item;
      menu = gtk_menu_new ();
      item = gtk_check_menu_item_new_with_label (CONTINUOUS);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      gtk_widget_set_tooltip_text (item, _("Set background updates on/off."));
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (toggle_updates), NULL);
      ContinuousUpdateButton = button;
      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (item), !Denemo.prefs.manualtypeset);
      item = gtk_menu_item_new_with_label (_("Range"));
      gtk_widget_set_tooltip_text (item, _("Set how much of the score to re-draw."));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (range_dialog), NULL);
      gtk_widget_show_all (menu);
    }
  return menu;
}

static void
updates_menu (GtkWidget * button)
{
  gtk_menu_popup (GTK_MENU (get_updates_menu (button)), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
}

static GtkWidget *
get_updates_button (void)
{
  GtkWidget *button = gtk_button_new_with_label (MANUAL);
  gtk_widget_set_tooltip_text (button, _("Set background updater on/off. This controls if typesetting is re-done after each change to the music. The amount of the score to be re-typeset can be set via this button."));
  g_signal_connect (button, "clicked", G_CALLBACK (updates_menu), NULL);
  return button;
}

//pops up a menu of layouts with the action being to typeset that layout. If only one, typeset that.
static void
popup_layouts_menu (void)
{
  GtkWidget *menu = GetLayoutMenu ();
  if (Denemo.project->custom_scoreblocks || (g_list_length (Denemo.project->standard_scoreblocks) > 1))
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
  else
    typeset_current_layout ();
}

void
install_printpreview (GtkWidget * top_vbox)
{
  if (Denemo.printarea)
    return;
  Denemo.printstatus->typeset_type = Denemo.prefs.typesettype;
  Denemo.printstatus->first_measure = Denemo.prefs.firstmeasure;
  Denemo.printstatus->last_measure = Denemo.prefs.lastmeasure;
  Denemo.printstatus->first_staff = Denemo.prefs.firststaff;
  Denemo.printstatus->last_staff = Denemo.prefs.laststaff;

  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_hbox), hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Print"));
  gtk_widget_set_tooltip_text (button, _("Pops up a Print dialog. From this you can send your typeset score to a printer or to a PDF file."));
  g_signal_connect (button, "clicked", G_CALLBACK (libevince_print), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("PDF"));
  gtk_widget_set_tooltip_text (button, _("Exports a pdf file for this layout"));
  g_signal_connect (button, "clicked", G_CALLBACK (copy_pdf), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Typeset"));

  gtk_widget_set_tooltip_text (button, _("Typesets the music using the one of the created layouts. See View → Score Layouts to see the layouts you have created."));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_layouts_menu), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


  button = gtk_button_new_with_label (_("Movement"));
  gtk_widget_set_tooltip_text (button, _("Typesets the music from the current movement. This creates a score layout comprising one movement."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), create_movement_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Part"));
  gtk_widget_set_tooltip_text (button, _("Typesets the music from the current part for all movements. A part is all the music with the same staff-name. This creates a score layout with one part, all movements."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), create_part_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Refresh"));
  gtk_widget_set_tooltip_text (button, _("Re-issues the last print command. Use this after modifying the file to repeat the typesetting."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = get_updates_button ();
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  (void) get_updates_menu (button);     //this is to initialize the continuous/manual state
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_end (GTK_BOX (main_hbox), hbox, FALSE, TRUE, 0);


  button = gtk_button_new_with_label (_("Duplex"));
  gtk_widget_set_tooltip_text (button, _("Shows pages side by side, so you can see page turns for back-to-back printing\n"));
  g_signal_connect (button, "clicked", G_CALLBACK (dual_page), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Invert"));
  gtk_widget_set_tooltip_text (button, _("Inverts colors in page\n"));
  g_signal_connect (button, "clicked", G_CALLBACK (invert_page), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Next"));
  gtk_widget_set_tooltip_text (button, _("Move to the next page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect (button, "clicked", G_CALLBACK (page_display), (gpointer) 1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Previous"));
  gtk_widget_set_tooltip_text (button, _("Move to the previous page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect (button, "clicked", G_CALLBACK (page_display), (gpointer) - 1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  if (top_vbox == NULL)
    top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  // if(!Denemo.prefs.manualtypeset)
  //      gtk_window_set_urgency_hint (GTK_WINDOW(Denemo.window), TRUE);//gtk_window_set_transient_for (GTK_WINDOW(top_vbox), GTK_WINDOW(Denemo.window));
  gtk_window_set_title (GTK_WINDOW (top_vbox), _("Denemo Print View"));
  gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);
  g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (hide_printarea_on_delete), NULL);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);

  GtkAdjustment *printvadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printvscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (printvadjustment));

  GtkAdjustment *printhadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printhscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (printhadjustment));

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (printhadjustment, printvadjustment);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);

  ev_init ();

  Denemo.printarea = (GtkWidget *) ev_view_new ();

  gtk_container_add (GTK_CONTAINER (score_and_scroll_hbox), Denemo.printarea);
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_markup (score_and_scroll_hbox,
                                   _
                                   ("This window shows the final typeset score from which you can print or (via print to file) create a PDF document.\nThis will be continuously updated while you edit the music in the main window.\nIn this Print View window you can click on a note to move to that place in the main Denemo display window. The right-click to get a menu of \"tweaks\" which you can apply to drag slurs, beams etc if they are not quite right.\n<b>Note</b>: It can take some time to generate a beautifully typeset score, especially for a large score on a slow machine so choose just a range to be continually updated in that case, or turn off continuous update."));

  g_signal_connect (G_OBJECT (Denemo.printarea), "external-link", G_CALLBACK (action_for_link), NULL);


#if GTK_MAJOR_VERSION != 2
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "draw", G_CALLBACK (printarea_draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "expose_event", G_CALLBACK (printarea_draw_event), NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.printarea), "motion_notify_event", G_CALLBACK (printarea_motion_notify), NULL);


  //g_signal_connect (G_OBJECT (Denemo.printarea), "focus_in_event",
  //            G_CALLBACK (printarea_focus_in_event), NULL);


//g_debug("Attaching signal...");
// !!!not available in early versions of libevince
//g_signal_connect (G_OBJECT (Denemo.printarea), "sync-source",
//                    G_CALLBACK (denemoprintf_sync), NULL);
//g_debug("...Attached signal?\n");

//what would this one fire on???? g_signal_connect (G_OBJECT (Denemo.printarea), "binding-activated",
//                    G_CALLBACK (denemoprintf_sync), NULL);

// Re-connect this signal to work on the pop up menu for dragging Denemo objects...
  g_signal_connect (G_OBJECT (Denemo.printarea), "button_press_event", G_CALLBACK (printarea_button_press), NULL);

// We may not need this signal
//  g_signal_connect (G_OBJECT (score_and_scroll_hbox), "scroll_event", G_CALLBACK(printarea_scroll_event), NULL);

  g_signal_connect_after (G_OBJECT (Denemo.printarea), "button_release_event", G_CALLBACK (printarea_button_release), NULL);

  gtk_widget_show_all (main_vbox);
  gtk_widget_hide (top_vbox);

  get_wysiwyg_info ()->dialog = infodialog ("");
  g_signal_connect (get_wysiwyg_info ()->dialog, "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
  g_signal_handlers_block_by_func (get_wysiwyg_info ()->dialog, G_CALLBACK (gtk_widget_destroy), get_wysiwyg_info ()->dialog);
  gtk_widget_hide (get_wysiwyg_info ()->dialog);
}

gboolean
continuous_typesetting (void)
{
  return (Denemo.printstatus->updating_id) && gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.printarea));
}
