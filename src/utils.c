/* utils.c
 * Functions useful across the different modules of
 * drawing and non-drawing code.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <stdio.h>
#include <string.h>             /*for SIGTERM */
#include <math.h>
#include <fontconfig/fontconfig.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <stdlib.h>
#include "accwidths.h"
#include <denemo/denemo.h>
#include "notewidths.h"
#include "utils.h"
#include "smf.h"
#include "print.h"
#include "kbd-custom.h"
#include "view.h"
#include <signal.h>             /*for SIGTERM */

#include "config.h"
#ifdef G_OS_WIN32
#include "windows.h"
#else
#include "binreloc.h"
#endif
#include "pitchentry.h"

#ifdef _MACH_O_
#include <mach-o/dyld.h>
#endif

/**
 * This checks to see if there's a .denemo/ directory in the user's
 * home directory, 
 * if create tries to create one if there isn't, and returns the
 * path to it
 * else returns NULL
 *
 * .denemo/ is used for holding configuration files, templates, and so on.
 *
 * On windows the home directory is the one containing the My Documents folder.
 */

const gchar *
get_user_data_dir (gboolean create)
{
  static gchar *dotdenemo = NULL;

  gboolean err;
  if (!dotdenemo)
    {
      dotdenemo = g_build_filename (g_get_home_dir (), ".denemo-" PACKAGE_VERSION, NULL);
    }
  if ((!create) && !g_file_test (dotdenemo, G_FILE_TEST_IS_DIR))
	return NULL;
  err = g_mkdir_with_parents (dotdenemo, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo for you personal settings"));
      g_free (dotdenemo);
      dotdenemo = NULL;
    }
  return dotdenemo;
}

/* return a path to a temporary directory to be used for print intermediate files */
const gchar *
locateprintdir (void)
{
  static gchar *printdir = NULL;
  if (!printdir)
    printdir = make_temp_dir ();
  return printdir;
}


void
add_font_directory (gchar * fontpath)
{
#ifdef G_OS_WIN32
  AddFontResource (fontpath);
#endif
  if(FcConfigAppFontAddDir (NULL, (FcChar8*) fontpath) == FcFalse)
    g_warning("Failed to add font dir %s.", fontpath);
}

void
add_font_file (gchar * fontname)
{
#ifdef G_OS_WIN32
  AddFontResource (fontname);
#endif
  if(FcConfigAppFontAddFile (NULL, (FcChar8*) fontname) == FcFalse)
    g_warning("Failed to add font file %s.", fontname);
}

#ifdef G_OS_WIN32
gboolean CoInitializeExCalled = FALSE;
#endif

// Create a unique temporary directory starting
gchar *
make_temp_dir (void)
{
  gchar *ret = NULL;
#ifdef G_OS_WIN32
  gchar buf[1024] = "C:\\TMP\\\0";
  gint length = 1024;
  (void) GetTempPath (length, buf);
  gint integer = 0;             //Windows does not delete the temp directory, use a constant one. g_rand_int(g_rand_new());
  ret = g_strdup_printf ("%sDenemo%d", buf, integer);

  gint fail = g_mkdir_with_parents (ret, 0700);
  if (fail)
    g_warning ("Could not create temp dir %s", ret);
  else
    g_info ("Created temp dir %s\n", ret);
#else
  ret = g_strdup ("/tmp/DenemoXXXXXX");
  mkdtemp ((char *) ret);
#endif
  return ret;
}

gboolean
run_file_association (gchar * filename)
{
#ifdef G_OS_WIN32
  gint value = 0;
  if (!CoInitializeExCalled)
    {
      value = CoInitializeExCalled = TRUE;
      CoInitializeEx (NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
      g_debug ("coinit returned %d\n", value);
    }
  g_info ("Running ShellExecute %s \n", filename);
  return ShellExecute (NULL, NULL, filename, NULL, NULL, 0) > 32 /* value above 32 indicating success */ ;
#else
  g_warning ("No file assoc code - set pref in externals tab of prefs dialog");
  return 0;
#endif

}

/**
   Popups up the menu named.
 */
void
popup_menu (gchar * name)
{
  GtkWidget *menu = gtk_ui_manager_get_widget (Denemo.ui_manager, name);
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
}

/**
 * outputs a warning and sounds bell
 * @return none
 */
void
warningmessage (gchar * msg)
{
  gdk_beep ();
  g_message ("%s", msg);
}

/**
 * Pops up a warning dialog and blocks until it is dismissed
 *  @param msg warning message to display
 * @return none
 */
void
warningdialog (gchar * msg)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_WARNING, GTK_BUTTONS_CLOSE, "%s", msg);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_destroy (dialog);
}

/**
 * Displays information message to screen, not blocking.
 * User can destroy window when no longer needed.
 * @param msg message to display
 * @return none
 */
GtkWidget *
infodialog (gchar * msg)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s", msg);
#ifdef G_OS_WIN32
  gtk_window_set_resizable (GTK_WINDOW (dialog), TRUE); //needed on windows because of a bug, not all text can be seen. 
#endif
  g_signal_connect_swapped (dialog, "response", G_CALLBACK (gtk_widget_hide), dialog);
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  return dialog;
}

/* data stucture to contain Progressbar 
 * data
 */
typedef struct _ProgressData
{
  GtkWidget *window;
  GtkWidget *pbar;
  int timer;
} ProgressData;


static volatile gboolean progressing = TRUE;
/* Update the value of the progress bar so that we get
 * some movement */
static gboolean
progress_timeout (ProgressData * pdata)
{
  if (progressing)
    gtk_progress_bar_pulse (GTK_PROGRESS_BAR (pdata->pbar));
  else
    {
      gtk_widget_hide (pdata->window);
      return FALSE;
    }
  return TRUE;
}

static ProgressData progress_data;



/**
 * Displays progress bar
 * optionally pass a callback to be run on destruction
 * @param msg message to display, callback (can be NULL)
 * @return the GtkWindow of the progress bar
 */
GtkWindow *
progressbar (gchar * msg, gpointer callback)
{

  GtkWidget *vbox;
  static ProgressData *pdata = NULL;

  progressing = TRUE;           /* If this is false the progress bar will stop */
  if (pdata == NULL)
    {
      /* Allocate memory for the data that is passed to the callbacks */
      pdata = &progress_data;

      /* Replace GTK_WINDOW_TOPLEVEL with GTK_WINDOW_POPUP
       * to have it without window decoration. 
       */
      if (callback && Denemo.prefs.progressbardecorations)
        pdata->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      else
        pdata->window = gtk_window_new (GTK_WINDOW_POPUP);
      gtk_window_set_accept_focus (GTK_WINDOW (pdata->window), FALSE);  //FIXME this is only a hint; perhaps we should embed the progress bar in the status line...
      gtk_window_set_title (GTK_WINDOW (pdata->window), _("Progress"));
      gtk_widget_set_tooltip_text (pdata->window, _("This indicates the the LilyPond typsetter is still working on setting the Denemo score. This can take a long time, particularly for polyphony where voices must not collide. You can continue editing while the typsetting is taking place.\nKill this window if you want to re-start the typesetting e.g. after fixing a mistake you just spotted."));
      vbox = gtk_vbox_new (FALSE, 5);
      gtk_container_add (GTK_CONTAINER (pdata->window), vbox);
      gtk_widget_show (vbox);

      pdata->pbar = gtk_progress_bar_new ();

      gtk_container_add (GTK_CONTAINER (vbox), pdata->pbar);
      gtk_widget_show (pdata->pbar);
    }
  /* set text inside progress bar */
  gtk_progress_bar_set_text (GTK_PROGRESS_BAR (pdata->pbar), msg);

  pdata->timer = g_timeout_add (100, (GSourceFunc) progress_timeout, pdata);

  gtk_window_set_keep_above (GTK_WINDOW (pdata->window), TRUE);
  gtk_widget_show (pdata->window);
  /* If widget is destroyed stop the printing */
  if(callback)
	g_signal_connect (G_OBJECT (pdata->window), "delete-event", G_CALLBACK (callback /*call_stop_lilypond*/), (gpointer)&progressing);
  else
	g_signal_connect (G_OBJECT (pdata->window), "delete-event", G_CALLBACK (progressbar_stop), NULL);
  return pdata->window;
}

void
progressbar_stop ()
{
  progressing = FALSE;
}


void
busy_cursor (GtkWidget *area)
{
  static GdkCursor *busycursor = NULL;
  if(!busycursor)
    busycursor = gdk_cursor_new (GDK_WATCH);
  if (gtk_widget_get_window (Denemo.printarea))
    gdk_window_set_cursor (gtk_widget_get_window (area), busycursor);
}
void
normal_cursor (GtkWidget *area)
{
  static GdkCursor *arrowcursor = NULL;
  if(!arrowcursor)
    arrowcursor = gdk_cursor_new (GDK_RIGHT_PTR);
  if (gtk_widget_get_window (area))
    gdk_window_set_cursor (gtk_widget_get_window (area), arrowcursor);
}


/**
 *  Draws the given bitmap mask on to the pixmap using the given 
 *  grahpics context.
 * 
 * @param pixmap pixmap be drawn on.
 * @param gc graphics context to use
 * @param mask  bitmap to be drawn
 * @param x x position on the pixmap
 * @param y y position on the pixmap
 * @param width width of the bitmap mask
 * @param height height of the bitmap mask
 * 
 * @return none
 */


void
drawbitmapinverse_cr (cairo_t * cr, DenemoGraphic * mask, gint x, gint y, gboolean invert)
{
  cairo_save (cr);
  switch (mask->type)
    {
    case DENEMO_BITMAP:
      {
#if GTK_MAJOR_VERSION==3
        gdk_cairo_set_source_window (cr, mask->graphic, x, y);  //??? bitmap???? asks torbenh
#else
        cairo_rectangle (cr, x, y, mask->width, mask->height);
#endif
        cairo_fill (cr);
        break;
      }
    case DENEMO_PATTERN:
      {
        cairo_pattern_t *pattern = (cairo_pattern_t *) mask->graphic;
        cairo_translate (cr, x, y);
        cairo_mask (cr, pattern);
        break;
      }
    case DENEMO_FONT:
      {
        DenemoGlyph *glyph = mask->graphic;
        cairo_select_font_face (cr, glyph->fontname, glyph->slant, glyph->weight);
        cairo_set_font_size (cr, glyph->size);
        cairo_move_to (cr, x, y);

        if (invert)
          cairo_scale (cr, 1, -1);
        cairo_show_text (cr, glyph->utf);
        break;
      }
    }
  cairo_restore (cr);
}

void
drawfetachar_cr (cairo_t * cr, gunichar uc, double x, double y)
{
  int len;
  char utf_string[8];
  len = g_unichar_to_utf8 (uc, utf_string);
  utf_string[len] = '\0';
  cairo_select_font_face (cr, "feta26", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size (cr, 35.0);
  cairo_move_to (cr, x, y);
  cairo_show_text (cr, utf_string);
}

void
drawtext_cr (cairo_t * cr, const char *text, double x, double y, double size)
{
  if (*text)
    {
      //use the FreeSerif font as it has music symbols - there is no font substitution done by cairo here
      cairo_select_font_face (cr, "Denemo", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
      cairo_set_font_size (cr, size);
      cairo_move_to (cr, x, y);
      cairo_show_text (cr, text);
    }
}

void
drawnormaltext_cr (cairo_t * cr, const char *text, double x, double y)
{
  drawtext_cr (cr, text, x, y, 14.0);
}

void
drawlargetext_cr (cairo_t * cr, const char *text, double x, double y)
{
  drawtext_cr (cr, text, x, y, 24.0);
}

/* draw display text and or graphics for directives
 return the widest graphic width*/
gint
draw_for_directives (cairo_t * cr, GList * directives, gint x, gint y, gboolean at_cursor)
{
  gint count = 10;
  gint maxwidth = 0;
  for (; directives; directives = directives->next, count += 10)
    {
      DenemoDirective *directive = (DenemoDirective *) directives->data;
      if (directive->graphic)
        {
          gint gwidth, gheight;
          gwidth = directive->graphic->width;
          gheight = directive->graphic->height;

          maxwidth = MAX (gwidth, maxwidth);
          //g_debug("drawing a graphic at %d %d\n", xx+directive->gx+count-gwidth/2,  y+height+directive->gy-gheight/2);
          drawbitmapinverse_cr (cr, directive->graphic, x + directive->gx + count - gwidth / 2, y + directive->gy - gheight / 2, FALSE);

        }
      if (directive->display)
        {
#define MAXLEN (8)
          gchar c = 0;          //if it is a long string only show it all when cursor is on it also only display from first line
          gchar *p;
          for (p = directive->display->str; *p; p++)
            {
              if (*p == '\n' || (!at_cursor && (p - directive->display->str) > MAXLEN))
                {
                  c = *p;
                  *p = 0;
                  break;
                }
            }
          drawnormaltext_cr (cr, directive->display->str, x + directive->tx + count, y + directive->ty);
          if (c)
            {
              *p = c;
            }
        }
    }

  return maxwidth;
}

/**
 * Utility function to set the number of ticks used by the given object
 * if it is within a given tuplet
 * 
 * @param theobj DenemoObject to set the number of ticks
 * @param numerator numerator of the current tuplet
 * @param denominator denominator of the current tuplet
 * @return none
 */
void
set_tuplefied_numticks (DenemoObject * theobj, gint numerator, gint denominator)
{
  theobj->durinticks = theobj->basic_durinticks * numerator / denominator;
  /* Though WHOLENUMTICKS is chosen strategically so that in common
   * cases, this division works out evenly, things should also work
   * out properly if it doesn't; i.e., durinticks_untupletized is
   * rounded down */
}

/**
 * Utility function to set the number of ticks of a mudela object
 * in a grace note
 * @param theobj the DenemoObject to set the number of ticks
 * @param multiplier the grace notes multiplier
 * @return none
 */
void
set_grace_numticks (DenemoObject * theobj, gint multiplier)
{

  theobj->durinticks = theobj->basic_durinticks / multiplier;


}


/**
 * Sets the number of ticks taken by the given DenemoObject.
 * 
 * @param theobj the mudela object to set the number of ticks on
 * @return none
 */
void
set_basic_numticks (DenemoObject * theobj)
{
  gint power;
  gint withoutdots;
  gint addperdot, i;

  switch (theobj->type)
    {
    case CHORD:
      if (((chord *) theobj->object)->baseduration < 0)
        {
          withoutdots = -((chord *) theobj->object)->baseduration;
        }
      else
        {
          power = 1 << ((chord *) theobj->object)->baseduration;
          withoutdots = WHOLE_NUMTICKS / power;
        }
      addperdot = withoutdots / 2;
      theobj->basic_durinticks = withoutdots;
      for (i = 0; i < ((chord *) theobj->object)->numdots; addperdot /= 2, i++)
        theobj->basic_durinticks += addperdot;

      break;
    default:
      theobj->basic_durinticks = 0;
      theobj->durinticks = 0;
      /* There's no reason not to set that as well */
      break;
    }
}

/** 
 * Returns the amount of space to be left after a note or rest, only
 * taking the width of the measure into consideration 
 *
 * @param numticks the number of ticks taken so far
 * @param wholenotewidth the number of ticks taken be a whole note
 * @return the amount of space to be left after a note or rest
 */

gint
space_after (gint numticks, gint wholenotewidth)
{
  return MAX (numticks * wholenotewidth / WHOLE_NUMTICKS, 0);
}

#define EXTRAWIDTH 5

/**
 * Sets the minimum space that needs to be allocated for drawing a mudela
 * object based on the type
 * also sets space_before
 * @param theobj the DenemoObject to set the minimum space on
 * @return none
 */

void
setpixelmin (DenemoObject * theobj)
{
  gint i, baseduration, headtype;
  chord chordval;
  GList *tnode;
  note *thetone;
  /* And these static declaration are copied right out of drawnotes.c
   * and drawaccidentals.c */

  switch (theobj->type)
    {
    case CHORD:
      chordval = *(chord *) theobj->object;
      baseduration = chordval.baseduration;
      baseduration = MAX (baseduration, 0);
      headtype = MIN (baseduration, 2);
      if (headtype < 0)
        headtype = 0;           //-ve values of baseduration are for specials
      gint directive_pixels = 0;        // the largest amount of extra space asked for by any directive
      GList *g = chordval.directives;
      for (; g; g = g->next)
        directive_pixels = MAX (directive_pixels, ((DenemoDirective *) g->data)->minpixels);
      if (chordval.notes)
        {
          theobj->minpixelsalloted = headwidths[headtype];
          //search through notes and their attached directives, find max display space requested
          //use this below

          g = chordval.notes;
          for (; g; g = g->next)
            {
              GList *h = ((note *) g->data)->directives;
              for (; h; h = h->next)
                directive_pixels = MAX (directive_pixels, ((DenemoDirective *) h->data)->minpixels);
            }
        }
      else                      /* a rest */
        theobj->minpixelsalloted = restwidths[baseduration];

      // Allow extra space specified by attached LilyPond directives - example:     
      theobj->minpixelsalloted += directive_pixels;



      /* 12 pixels for the first dot, 6 for each dot thereafter */
      if (chordval.numdots)
        theobj->minpixelsalloted += 6;
      for (i = 0; i < chordval.numdots; i++)
        theobj->minpixelsalloted += 6;

      theobj->space_before = 0;
      if (chordval.hasanacc)
        for (tnode = chordval.notes; tnode; tnode = tnode->next)
          {
            thetone = (note *) tnode->data;
            if (thetone->showaccidental)
              theobj->space_before = MAX (theobj->space_before, thetone->position_of_accidental);
          }
      if (chordval.is_reversealigned){
        if (chordval.is_stemup)
          theobj->minpixelsalloted += headwidths[headtype];
        else if (!chordval.hasanacc)
          /* Accidental positioning already accounts for the extra leading
             space that we need for reverse-aligned noteheads, provided
             the chord has an accidental in it somewhere. We only have to
             remark upon noteheads to the left of the stem if there weren't
             any accidentals to position.  */
          theobj->space_before += headwidths[headtype];
      }
      theobj->minpixelsalloted += EXTRAWIDTH;
      break;
    case TUPOPEN:
    case TUPCLOSE:
      /* The real way do this will be with a gdk_string_width. Until
       * then, though: */
      theobj->minpixelsalloted =
#if 0
        40;
#else
        16;
#endif
      theobj->space_before = theobj->minpixelsalloted / 2;
      break;
    case LILYDIRECTIVE:
      {
        DenemoDirective *directive = (DenemoDirective *) theobj->object;
        theobj->minpixelsalloted = directive->minpixels ? directive->minpixels : 16;
        theobj->space_before = theobj->minpixelsalloted / 2;
      }
      break;
    case CLEF:
      theobj->minpixelsalloted = 35;
      theobj->space_before = 0;
      break;
    case KEYSIG:
      theobj->space_before = 0;
      break;
    case TIMESIG:
      theobj->minpixelsalloted = 40;
      theobj->space_before = 0;
      break;
    case STEMDIRECTIVE:
      /* The real way do this will be with a gdk_string_width. Until
       * then, though: */
      theobj->minpixelsalloted = 40;
      theobj->space_before = 0;
      break;
    case DYNAMIC:
      theobj->minpixelsalloted = 40;
      theobj->space_before = 0;
      break;
    case GRACE_START:
    case GRACE_END:
      theobj->minpixelsalloted = 16;
      theobj->space_before = theobj->minpixelsalloted / 2;
      break;
    default:
      theobj->minpixelsalloted = 0;
      theobj->space_before = 0;
      break;
    }
}

/**
 * 
 * @param mid_c_offset the mid_c_offset of the the tone
 * @param dclef the clef of the current tone
 * 
 * @return the height of a tone based on its mid_c_offset and the clef that it's in 
 */
gint
calculateheight (gint mid_c_offset, gint dclef)
{
  switch (dclef)
    {
    case DENEMO_TREBLE_CLEF:
      return 5 * LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;                    /* Probably gratuitous */
    case DENEMO_ALTO_CLEF:
      return 2 * LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_G_8_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset - 1);
      break;
    case DENEMO_BASS_CLEF:
      return -LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_F_8_CLEF:
      return -5 * LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset - 1);
      break;
    case DENEMO_TENOR_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_SOPRANO_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset - 6);
      break;
    case DENEMO_FRENCH_CLEF:
      return 6 * LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset);
      break;
    }

  return (0);
}

/**
 * Converts the given offset to a number
 * 
 * @param n the offset to convert
 * @return the result of the offset conversion
 */
gint
offsettonumber (gint n)
{
  if (n >= 0)
    return n % 7;
  else
    return (7 - (-n % 7)) % 7;
  /* Not all C implementations conform to the more recent standard on how %
     should operate on negative operands.  */
}

/**
 * converts the int mid_c_offset to the lilypond name 
 * returns a gchar * so it will have to be freed
 * 0 returns "c", 1 returns "cis"
 * The octave ",,, or '''" is also appended" 
 */

gchar *
mid_c_offsettolily (int mid_c_offset, int enshift)
{
  gint octave, k;
  GString *lilynote = g_string_new ("");

  g_string_append_printf (lilynote, "%c", mid_c_offsettoname (mid_c_offset));
  if (enshift < 0)
    for (k = enshift; k; k++)
      g_string_append_printf (lilynote, "es");
  else
    for (k = enshift; k; k--)
      g_string_append_printf (lilynote, "is");
  octave = mid_c_offsettooctave (mid_c_offset);
  if (octave < 0)
    for (; octave; octave++)
      g_string_append_printf (lilynote, ",");
  else
    for (; octave; octave--)
      g_string_append_printf (lilynote, "\'");

  return g_string_free (lilynote, FALSE);
}

/**
 * converts the mid_c_offset to the correct letter name
 * @param mid_c_offset the mid_c_offset to convert
 * @return the character name of the mid_c_offset
 */
gchar
mid_c_offsettoname (gint mid_c_offset)
{
  gint otn = offsettonumber (mid_c_offset);

  return ((otn + 2) % 7) + 'a';
}

void
note2lilynotename (struct note *noteobject, GString * ret)
{
  gint mid_c_offset = noteobject->mid_c_offset;

  g_string_append_printf (ret, "%c", mid_c_offsettoname (mid_c_offset));
}

void
note2lilyaccidental (struct note *noteobject, GString * ret)
{
  gint enshift = noteobject->enshift;
  gint k;
  if (enshift < 0)
    for (k = enshift; k; k++)
      g_string_append_printf (ret, "es");
  else
    for (k = enshift; k; k--)
      g_string_append_printf (ret, "is");
}

void
note2lilyoctave (struct note *noteobject, GString * ret)
{
  gint mid_c_offset = noteobject->mid_c_offset;
  gint octave = mid_c_offsettooctave (mid_c_offset);
  if (octave < 0)
    for (; octave; octave++)
      g_string_append_printf (ret, ",");
  else
    for (; octave; octave--)
      g_string_append_printf (ret, "\'");
}

void
chord2lilyduration (struct chord *chordobject, GString * ret)
{
  chord2lilybaseduration (chordobject, ret);
  chord2lilynumdots (chordobject, ret);
}

void
chord2lilybaseduration (struct chord *chordobject, GString * ret)
{
  int baseduration = chordobject->baseduration;
  g_string_append_printf (ret, "%d", baseduration);
}

void
chord2lilynumdots (struct chord *chordobject, GString * ret)
{
  int numdots = chordobject->numdots;
  g_string_append_printf (ret, "%d", numdots);
}

/**
 * Calculate a pitches octave from the mid_c_offset
 * @param mid_c_offset the mid_c_offset to use
 * @return the octave of the given mid_c_offset
 */
gint
mid_c_offsettooctave (gint mid_c_offset)
{
  if (mid_c_offset < 0)
    return -((-mid_c_offset + 6) / 7) + 1;
  else
    return (mid_c_offset / 7) + 1;
}

/**
 * g_list_foreach helper function to free the given data
 * @param data the list elements data
 * @param user_data any user supplied data (not used in this case)
 */
void
freeit (gpointer data, gpointer user_data)
{
  g_free (data);
}







/************* routines for calling from debug code ***************/
#include "staffops.h"

G_GNUC_UNUSED void
printobj (objnode * obj)
{
  DenemoObject *curObj;

  curObj = (DenemoObject *) (obj->data);
  switch (curObj->type)
    {
    case CHORD:
      fprintf (stderr, "\t\t%s type\n", "CHORD");
      break;
    case TUPOPEN:
      fprintf (stderr, "\t\t%s type\n", "TUPOPEN");
      break;
    case TUPCLOSE:
      fprintf (stderr, "\t\t%s type\n", "TUPCLOSE");
      break;
    case CLEF:
      fprintf (stderr, "\t\t%s type\n", "CLEF");
      break;
    case TIMESIG:
      fprintf (stderr, "\t\t%s type\n", "TIMESIG");
      break;
    case KEYSIG:
      fprintf (stderr, "\t\t%s type\n", "KEYSIG");
      break;
    case BARLINE:
      fprintf (stderr, "\t\t%s type\n", "BARLINE");
      break;
    case STEMDIRECTIVE:
      fprintf (stderr, "\t\t%s type\n", "STEMDIRECTIVE");
      break;
    case MEASUREBREAK:
      fprintf (stderr, "\t\t%s type\n", "MEASUREBREAK");
      break;
    case DYNAMIC:
      fprintf (stderr, "\t\t%s type\n", "DYNAMIC");
      break;
    case GRACE_START:
      fprintf (stderr, "\t\t%s type\n", "GRACE_START");
      break;
    case GRACE_END:
      fprintf (stderr, "\t\t%s type\n", "GRACE_END");
      break;
    case LYRIC:
      fprintf (stderr, "\t\t%s type\n", "LYRIC");
      break;
    case FIGURE:
      fprintf (stderr, "\t\t%s type\n", "FIGURE");
      break;
    default:                   /* needs to be up to date with enum in include/denemo/denemo.h */
      fprintf (stderr, "!!!!!unknown object type %x - see enum in denemo.h\n", curObj->type);
      break;
    }
}

G_GNUC_UNUSED void
printobjs (objnode * obj)
{
  objnode *curobj;
  if (obj == NULL)
    {
      fprintf (stderr, "NULL object\n");
      return;
    }
  printobj (obj);
  fprintf (stderr, "previous objects\n");
  curobj = obj;
  while (curobj->prev)
    {
      printobj (curobj->prev);
      curobj = curobj->prev;
    }
  fprintf (stderr, "next objects\n");
  curobj = obj;
  while (curobj->next)
    {
      printobj (curobj->next);
      curobj = curobj->next;
    }
}

G_GNUC_UNUSED void
printmeasure (measurenode * mnode)
{
  if (mnode == NULL)
    {
      fprintf (stderr, "Empty measure\n");
      return;
    }
  printobjs (firstobjnode (mnode));
}

G_GNUC_UNUSED void
printmeasures (staffnode * thestaff)
{
  GList *measure = firstmeasurenode (thestaff);
  gint measurenum = 1;
  for (measure = firstmeasurenode (thestaff); measure; measure = measure->next)
    {
      fprintf (stderr, "*************Measure %d *************\n", measurenum++);
      printmeasure (measure);
    }
}

G_GNUC_UNUSED void
printscoreinfo (DenemoMovement * si)
{
  if (si->thescore == NULL)
    {
      fprintf (stderr, "Staff with NULL thescore field\n");
      return;
    }
  printmeasures (si->thescore);
}

/**
 * Function that initializes the code needed for the directory relocation.
 * @return none
 */
void
initdir ()
{
#ifndef G_OS_WIN32
  GError *error = NULL;
  if (!gbr_init (&error) && (error != (GError *) GBR_INIT_ERROR_DISABLED))
    {
      g_debug ("BinReloc failed to initialize:\n");
      g_debug ("Domain: %d (%s)\n", (int) error->domain, g_quark_to_string (error->domain));
      g_debug ("Code: %d\n", error->code);
      g_debug ("Message: %s\n", error->message);
      g_error_free (error);
      g_debug ("----------------\n");
    }
#endif /* not G_OS_WIN32 */
}

extern gchar *gbr_find_pkg_data_dir (const gchar * default_pkg_data_dir, const gchar * pkg_name);
  
static gchar *DENEMO_datadir = NULL;
const gchar *
get_system_data_dir ()
{
  //DENEMO_datadir?g_debug("datadir is %s at %p", DENEMO_datadir, DENEMO_datadir):g_debug("datadir not yet set");
  if (DENEMO_datadir == NULL)
    {
#ifdef G_OS_WIN32
      gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
      DENEMO_datadir = g_build_filename (rootdir, "share", "denemo", NULL);
      g_message ("rootdir=%s", rootdir);
      g_message ("datadir=%s", DENEMO_datadir);
      g_free (rootdir);
#else /* not G_OS_WIN32 */

#ifdef _MACH_O_

      {
        char path[1024];
        guint size = sizeof (path);
        _NSGetExecutablePath (path, &size);
        gchar *bindir = (gchar *) g_malloc (size);
        if (_NSGetExecutablePath (bindir, &size) == 0)
          g_message ("Using bin path %s", bindir);
        else
          g_critical ("Cannot get bin dir");
        DENEMO_datadir = g_build_filename (g_path_get_dirname (bindir), "..", "share", "denemo", NULL);
        g_message ("OSX set data dir to %s", DENEMO_datadir);
      }
#else
#ifndef ENABLE_BINRELOC
      DENEMO_datadir = g_strdup (PKGDATADIR);
#else
      DENEMO_datadir = gbr_find_pkg_data_dir (PKGDATADIR, PKGNAME);
#endif //ENABLE_BINRELOC

#endif //_MACH_O_
#endif /* not G_OS_WIN32 */
    }
  return DENEMO_datadir;
}

const gchar *
get_prefix_dir (void)
{
  gchar *prefix;
#ifdef G_OS_WIN32
  prefix = g_win32_get_package_installation_directory (NULL, NULL);
#else /* not G_OS_WIN32 */
#ifdef _MACH_O_
  {
    char path[1024];
    guint size = sizeof (path);
    _NSGetExecutablePath (path, &size);
    gchar *bindir = (gchar *) g_malloc (size);
    if (_NSGetExecutablePath (bindir, &size) == 0)
      {
        prefix = g_build_filename (bindir, "..", "..", NULL);
        g_message ("OSX set data prefix to %s", prefix);
      }
    else
      g_critical ("Cannot get bin dir");
  }
#else

#ifndef ENABLE_BINRELOC
  prefix = g_strdup (PREFIX);
#else
  prefix = gbr_find_prefix (PREFIX);
#endif //ENABLE_BINRELOC

#endif //_MACH_O_
#endif //G_OS_WIN32
  return prefix;
}
static gchar *DENEMO_bindir = NULL;
const gchar *
get_system_bin_dir (void)
{
  
  if (DENEMO_bindir == NULL)
    {
#ifdef G_OS_WIN32
      gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
      DENEMO_bindir = g_build_filename (rootdir, "bin", NULL);
      g_message ("rootdir=%s", rootdir);
      g_message ("bindir=%s", DENEMO_bindir);
      g_free (rootdir);
#else /* not G_OS_WIN32 */

#ifdef _MACH_O_

      {
        char path[1024];
        guint size = sizeof (path);
        _NSGetExecutablePath (path, &size);
        gchar *bin = (gchar *) g_malloc (size);
        if (_NSGetExecutablePath (bin, &size) == 0)
          {
            DENEMO_bindir = g_build_filename (bin, "..", NULL);
            g_message ("Using bin path %s", DENEMO_bindir);
          }
        else
          g_critical ("Cannot get bin dir");

        g_message ("OSX set bin dir to %s", DENEMO_bindir);
      }
#else

#ifndef ENABLE_BINRELOC
      DENEMO_bindir = g_strdup (BINDIR);
#else
      DENEMO_bindir = gbr_find_bin_dir (BINDIR);
#endif //ENABLE_BINRELOC

#endif //_MACH_O_
#endif /* not G_OS_WIN32 */
    }
  return DENEMO_bindir;
}

/** UNUSED
const gchar *
get_system_conf_dir ()
{
  static gchar *confdir = NULL;
  if (confdir == NULL)
    {
#ifdef G_OS_WIN32
      gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
      confdir = g_build_filename (rootdir, "etc", "denemo", NULL);
      g_free (rootdir);
#else // not G_OS_WIN32 
#ifdef _MACH_O_

      {
        char path[1024];
        guint size = sizeof (path);
        _NSGetExecutablePath (path, &size);
        gchar *bindir = (gchar *) g_malloc (size);
        if (_NSGetExecutablePath (bindir, &size) == 0)
          g_debug ("using bin path %s\n", bindir);
        else
          g_critical ("Cannot get bin dir\n");
        confdir = g_build_filename (g_path_get_dirname (bindir), "..", "etc", "denemo", NULL);
        g_debug ("OSX set conf dir to %s\n", confdir);
      }
#else

#ifndef ENABLE_BINRELOC
      confdir = g_build_filename (SYSCONFDIR, NULL);
#else
      confdir = g_build_filename (gbr_find_etc_dir (SYSCONFDIR), "denemo", NULL);
#endif //ENABLE_BINRELOC

#endif //_MACH_O_
#endif // not G_OS_WIN32
    }
  return confdir;
}
*/

const gchar *
get_system_locale_dir ()
{
  static gchar *localedir = NULL;
  if (localedir == NULL)
    {
#ifdef G_OS_WIN32
      gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
      localedir = g_build_filename (rootdir, "share", "locale", NULL);
      g_free (rootdir);
#else /* not G_OS_WIN32 */
#ifdef _MACH_O_

      {
        char path[1024];
        guint size = sizeof (path);
        _NSGetExecutablePath (path, &size);
        gchar *bindir = (gchar *) g_malloc (size);
        if (_NSGetExecutablePath (bindir, &size) == 0)
          g_message ("Using bin path %s", bindir);
        else
          g_critical ("Cannot get bin dir");
        localedir = g_build_filename (g_path_get_dirname (bindir), "..", "share", "locale", NULL);
        g_message ("OSX set locale dir to %s", localedir);
      }
#else
#ifndef ENABLE_BINRELOC
      /* it seems to be the standard way (no binreloc)
       * to set the path of translations this way:
       * messages are in $LOCALEDIR/$LANG/denemo
       */
      localedir = g_strdup (LOCALEDIR);
#else /* ENABLE_BINRELOC */
      /* binreloc says it is disabled even with built thanks to
       * --enable-binreloc... So, searhing falls back to
       *  $LOCALEDIR/denemo/$LANG which is not a valid path
       */
      localedir = gbr_find_locale_dir (LOCALEDIR);
#endif /* ENABLE_BINRELOC */
#endif
#endif /* not G_OS_WIN32 */
    }
  return localedir;
}

const gchar*
get_system_font_dir(){
  static gchar* fontdir = NULL;
  if(fontdir == NULL)
  {
#ifdef G_OS_WIN32
    gchar *prefix = g_win32_get_package_installation_directory (NULL, NULL);
#else
    gchar *prefix = g_build_filename (get_prefix_dir (), NULL);
#endif
    fontdir = g_build_filename (prefix, "share", "fonts", "truetype", "denemo", NULL);
  }
  return fontdir;
}

void
kill_process (GPid pid)
{
#ifdef G_OS_WIN32
  TerminateProcess (pid, 0);
#else /* not G_OS_WIN32 */
  kill (pid, SIGTERM);
#endif /* not G_OS_WIN32 */
  g_spawn_close_pid (pid);
}


void
init_denemo_notenames (void)
{

  define_scheme_literal_variable ("Denemo-Note0", NOTE0, NULL);
  define_scheme_literal_variable ("Denemo-Rest0", REST0, NULL);
  define_scheme_literal_variable ("Denemo-Note1", NOTE1, NULL);
  define_scheme_literal_variable ("Denemo-Rest1", REST1, NULL);
  define_scheme_literal_variable ("Denemo-Note2", NOTE2, NULL);
  define_scheme_literal_variable ("Denemo-Rest2", REST2, NULL);
  define_scheme_literal_variable ("Denemo-Note3", NOTE3, NULL);
  define_scheme_literal_variable ("Denemo-Rest3", REST3, NULL);
  define_scheme_literal_variable ("Denemo-Note4", NOTE4, NULL);
  define_scheme_literal_variable ("Denemo-Rest4", REST4, NULL);
  define_scheme_literal_variable ("Denemo-Note5", NOTE5, NULL);
  define_scheme_literal_variable ("Denemo-Rest5", REST5, NULL);
  define_scheme_literal_variable ("Denemo-Note6", NOTE6, NULL);
  define_scheme_literal_variable ("Denemo-Rest6", REST6, NULL);
  define_scheme_literal_variable ("Denemo-Note7", NOTE7, NULL);
  define_scheme_literal_variable ("Denemo-Rest7", REST7, NULL);
  define_scheme_literal_variable ("Denemo-Note8", NOTE8, NULL);
  define_scheme_literal_variable ("Denemo-Rest8", REST8, NULL);

}

#define HIGHLIGHT "<span background=\"lightblue\"> "

/* markup the passed string to be in the denemo music font
* caller must free the returned string
*/
gchar *
music_font (gchar * str)
{
  GString *s = g_string_new ("");
  gint c = *str;
  for (c = *str; c; c = *++str)
    switch (c)
      {
      case '0':
        g_string_append (s, " " NOTE0 " ");
        break;
      case HIGHLIGHT_OFFSET + '0':
        g_string_append (s, HIGHLIGHT NOTE0 " </span>");
        break;

      case '1':
        g_string_append (s, " " NOTE1 " ");
        break;
      case HIGHLIGHT_OFFSET + '1':
        g_string_append (s, HIGHLIGHT NOTE1 " </span>");
        break;
      case '2':
        g_string_append (s, " " NOTE2 " ");
        break;
      case HIGHLIGHT_OFFSET + '2':
        g_string_append (s, HIGHLIGHT NOTE2 " </span>");
        break;
      case '3':
        g_string_append (s, " " NOTE3 " ");
        break;
      case HIGHLIGHT_OFFSET + '3':
        g_string_append (s, HIGHLIGHT NOTE3 " </span>");
        break;
      case '4':
        g_string_append (s, " " NOTE4 " ");
        break;
      case HIGHLIGHT_OFFSET + '4':
        g_string_append (s, HIGHLIGHT NOTE4 " </span>");
        break;
      case '5':
        g_string_append (s, " " NOTE5 " ");
        break;
      case HIGHLIGHT_OFFSET + '5':
        g_string_append (s, HIGHLIGHT NOTE5 " </span>");
        break;
      case '6':
        g_string_append (s, " " NOTE6 " ");
        break;
      case HIGHLIGHT_OFFSET + '6':
        g_string_append (s, HIGHLIGHT NOTE6 " </span>");
        break;
      case '7':
        g_string_append (s, " " NOTE7 " ");
        break;
      case HIGHLIGHT_OFFSET + '7':
        g_string_append (s, HIGHLIGHT NOTE7 " </span>");
        break;
      case '8':
        g_string_append (s, " " NOTE8 " ");
        break;
      case HIGHLIGHT_OFFSET + '8':
        g_string_append (s, HIGHLIGHT NOTE8 " </span>");
        break;

      case 'r':
        g_string_append (s, " " REST0 " ");
        break;
      case HIGHLIGHT_OFFSET + 'r':
        g_string_append (s, HIGHLIGHT REST0 " </span>");
        break;

      case 's':
        g_string_append (s, " " REST1 " ");
        break;
      case HIGHLIGHT_OFFSET + 's':
        g_string_append (s, HIGHLIGHT REST1 " </span>");
        break;
      case 't':
        g_string_append (s, " " REST2 " ");
        break;
      case HIGHLIGHT_OFFSET + 't':
        g_string_append (s, HIGHLIGHT REST2 " </span>");
        break;
      case 'u':
        g_string_append (s, " " REST3 " ");
        break;
      case HIGHLIGHT_OFFSET + 'u':
        g_string_append (s, HIGHLIGHT REST3 " </span>");
        break;
      case 'v':
        g_string_append (s, " " REST4 " ");
        break;
      case HIGHLIGHT_OFFSET + 'v':
        g_string_append (s, HIGHLIGHT REST4 " </span>");
        break;
      case 'w':
        g_string_append (s, " " REST5 " ");
        break;
      case HIGHLIGHT_OFFSET + 'w':
        g_string_append (s, HIGHLIGHT REST5 " </span>");
        break;
      case 'x':
        g_string_append (s, " " REST6 " ");
        break;
      case HIGHLIGHT_OFFSET + 'x':
        g_string_append (s, HIGHLIGHT REST6 " </span>");
        break;
      case 'y':
        g_string_append (s, " " REST7 " ");
        break;
      case HIGHLIGHT_OFFSET + 'y':
        g_string_append (s, HIGHLIGHT REST7 " </span>");
        break;
      case 'z':
        g_string_append (s, " " REST8 " ");
        break;
      case HIGHLIGHT_OFFSET + 'z':
        g_string_append (s, HIGHLIGHT REST8 " </span>");
        break;
      default:
        g_string_append_c (s, c);
      }
  return g_string_free (s, FALSE);

}

void
set_title_bar (DenemoProject * gui)
{
  gchar *title;
  if (gui->tabname && gui->tabname->len)
    title = gui->tabname->str;
  else
    title = _("(Untitled)");
  title = g_strdup_printf ("%s%c", title, gui->notsaved ? '*' : ' ');
  gtk_window_set_title (GTK_WINDOW (Denemo.window), title);
  gchar *base = g_path_get_basename (title);
  gint index = g_list_index (Denemo.projects, gui);
  GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (Denemo.notebook), index);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (Denemo.notebook), page, base);

  gtk_notebook_set_menu_label_text (GTK_NOTEBOOK (Denemo.notebook), page, base);


  g_free (title);
  g_free (base);
}

static const gchar *
enshift_string (gint enshift)
{
  switch (enshift)
    {
    case -2:
      return "𝄫";
    case -1:
      return "♭";
    case 0:
      return " ";
    case 1:
      return "♯";
    case 2:
      return "𝄪";
    default:
      return _("Error");
    }
}

/* set the status of the current musical score - its change count and
   title bar and status bars.
   DenemoProject *gui the musical score.
   gboolean change TRUE = a change has just been made
                   FALSE = the score is to be assumed saved
*/
void
score_status (DenemoProject * gui, gboolean change)
{
  if (change)
    {
      gboolean just_changed = !gui->notsaved;
      gui->notsaved = TRUE;
      gui->changecount++;
      gui->si->changecount++;
      if (just_changed)
        set_title_bar (gui);
    }
  else
    {
      gui->notsaved = FALSE;
      set_title_bar (gui);
    }
  write_status (gui);
}

/**
 * If the curObj is a chord with a note(s)
 * return the first note at or below cursory, or the last note 
 * else return NULL
 */
note *
findnote (DenemoObject * curObj, gint cursory)
{
  note *curnote = NULL;
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->notes)
    {
      GList *notes = ((chord *) curObj->object)->notes;
      for (; notes; notes = notes->next)
        {
          curnote = (note *) notes->data;
          //g_debug("comparing %d and %d\n", cursory, curnote->y);
          if (cursory <= curnote->mid_c_offset)
            break;
        }

    }
  return curnote;
}

/* display full information about the object at the cursor */
GtkWidget *ObjectInfo = NULL;
static gboolean
drop_object_info (void)
{
  if (ObjectInfo)
    gtk_widget_hide (ObjectInfo);
  return TRUE;
}

static void
append_directives_information (GString * selection, GList * directives)
{
  do
    {
      DenemoDirective *directive = directives->data;
      const gchar *label = get_label_for_command (directive->tag->str);
      const gchar *menupath = get_menu_path_for_command (directive->tag->str);
      const gchar *tooltip = get_tooltip_for_command (directive->tag->str);
      if(label)
				g_string_append_printf (selection, _("Directive for command: \"%s\"\n"), label);
      else
				g_string_append_printf (selection, _("Directive tagged: \"%s\"\n"), directive->tag->str);
			if(menupath)
				g_string_append_printf (selection, _("Menu location for this command: \"%s\"\n"), menupath);
      if(directive->prefix)
				 g_string_append_printf (selection, _("LilyPond inserted in prefix to this object is \"%s\"\n"), directive->prefix->str);
			if(directive->postfix)
				 g_string_append_printf (selection, _("LilyPond inserted in postfix to this object is \"%s\"\n"), directive->postfix->str);
      if(tooltip)
        g_string_append_printf (selection, _("The help for the command that created this directive is:\n\"%s\"\n"), tooltip);
    }
  while (directives->next && (directives = directives->next));
}

void
display_current_object (void)
{
  DenemoProject *gui = Denemo.project;
  if (ObjectInfo == NULL)
    {
      ObjectInfo = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      g_signal_connect (G_OBJECT (ObjectInfo), "delete-event", G_CALLBACK (drop_object_info), NULL);
      gtk_window_set_keep_above (GTK_WINDOW (ObjectInfo), TRUE);
      gtk_window_set_accept_focus (GTK_WINDOW (ObjectInfo), FALSE);
    }
  else
    {
      gtk_widget_destroy (gtk_bin_get_child (GTK_BIN (ObjectInfo)));
    }

  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_add (GTK_CONTAINER (ObjectInfo), vbox);

  if (gui->si->currentobject == NULL)
    {
      GtkWidget *label = gtk_label_new ("The cursor is in an empty measure.\n" "As a special case this will be typeset as a non-printing whole measure rest.\n" "Note that if you put anything at all in this measure\n" "you must insert a real whole measure rest if that is what you want.");
      gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
    }
  else
    {
      DenemoObject *curObj = gui->si->currentobject->data;
      GString *selection = g_string_new (gui->si->cursor_appending ? _("The cursor is in the appending position after ") : _("The cursor is on "));
      GString *warning = g_string_new ("");
      switch (curObj->type)
        {
        case CHORD:
          {
            chord *thechord = ((chord *) curObj->object);
            if (thechord->notes)
              {
                if (thechord->notes->next)
                  {
                    selection = g_string_append (selection, _("a chord.\n"));
                  }
                else
                  {
                    selection = g_string_append (selection, _("a one-note chord.\n"));
                  }
                if (thechord->slur_begin_p)
                  selection = g_string_append (selection, _("A slur starts from here.\n" "There should be a matching end slur later.\n"));
                if (thechord->slur_end_p)
                  selection = g_string_append (selection, _("A slur ends here\n" "There should be a matching start slur earlier.\n"));
                if (thechord->is_tied)
                  selection = g_string_append (selection, _("This is tied to the following note or chord.\n" "The following note or chord should have the same pitch"));
                if (thechord->is_grace && !(thechord->is_grace & GRACED_NOTE))
                  selection = g_string_append (selection, _("This is an acciaccatura note\n"));
                if (thechord->is_grace & GRACED_NOTE)
                  selection = g_string_append (selection, _("This is an appoggiatura note\n"));
                if (curObj->isinvisible)
                  selection = g_string_append (selection, _("This note denotes a rhythm - use a MIDI keyboard to add pitches by playing.\n"));




                note *thenote = findnote (curObj, gui->si->cursor_y);
                if (thenote && gui->si->cursor_y==thenote->mid_c_offset)
                  {
  
										g_string_append_printf (selection, _("Within the chord the cursor is on the note %s \n"),
											mid_c_offsettolily (thenote->mid_c_offset, thenote->enshift));
                    if (thenote->directives)
                      {
                        selection = g_string_append (selection, _("Attached to this note:"));
                        append_directives_information (selection, thenote->directives);
                      }
                  }
                if ((thechord->notes->next) && curObj->isinvisible)
                        warning = g_string_append (warning, _("This rhythm has extra notes added to it, delete them and use the foot-pedal or Alt key to enter chords with the MIDI controller.\n"));

              }
            else
              {
                selection = g_string_append (selection, _("a rest.\n"));
                if (thechord->slur_begin_p)
                  warning = g_string_append (warning, _("This rest has a slur start on it, use the Notes/Rests → Slurs menu to remove it"));
                if (thechord->slur_end_p)
                  warning = g_string_append (warning, _("This rest has a slur end on it, use the Notes/Rests → Slurs menu to remove it"));
                if (thechord->is_tied)
                  warning = g_string_append (warning, _("This rest has a tie starting on it, use the Notes/Rests → Ties menu to remove it"));

                  
                if (curObj->isinvisible)
                  selection = g_string_append (selection, _("This rest will not print, just act as a spacer.\n"));
                if (thechord->is_grace && curObj->isinvisible)
                  warning = g_string_append (warning, _("This rest has the grace attribute set: this can be used to avoid a bug in the print view.\n"));

              }
            if (thechord->directives) {
							selection = g_string_append (selection, _("Attached to the chord:"));
              append_directives_information (selection, thechord->directives);
						}
          g_string_append_printf (selection, _("This starts %d/384 quarter notes into the measure and lasts %d/384 quarter notes.\n"), curObj->starttick, curObj->durinticks);
			if(curObj->lilypond)
				          g_string_append_printf (selection, _("The LilyPond syntax generated is: %s\n"), curObj->lilypond);

          }
          break;
        case TUPOPEN:
          { tuplet *thetup = ((tuplet *) curObj->object);
            g_string_append_printf (selection, _(" a Start Tuplet object\n""Meaning %d notes will take the time of %d notes\n" "until an End Tuplet object.\n"), thetup->denominator, thetup->numerator);
						if (thetup->directives) 
							{
								selection = g_string_append (selection, _("Attached to the Start Tuplet:"));
								append_directives_information (selection, thetup->directives);
							}
					}
          break;
        case TUPCLOSE:
          { tuplet *thetup = ((tuplet *) curObj->object);
            g_string_append_printf (selection, _("an End Tuplet object\n" "Note: the Start Tuplet must be in the same measure.\n"));
            if (thetup->directives) 
							{
								selection = g_string_append (selection, _("Attached to the End Tuplet:"));
								append_directives_information (selection, thetup->directives);
							}
          }
          break;
        case CLEF:
          {
			clef *theclef = ((clef *) curObj->object);
            g_string_append_printf (selection, _("a Clef Change object.\n"));   
			if (theclef->directives) 
							{
								selection = g_string_append (selection, _("Attached to the Clef Change:"));
								append_directives_information (selection, theclef->directives);
							}
			if (curObj->isinvisible)
                           selection = g_string_append (selection, _("This clef change is non-printing, it just affects the display.\n"));

          }
          break;
        case TIMESIG:
          {
						timesig *thetime = ((timesig *) curObj->object);
            g_string_append_printf (selection, _("a Time Signature Change object.\n"));
						if (thetime->directives) 
							{
								selection = g_string_append (selection, _("Attached to the Time Signature Change:"));
								append_directives_information (selection, thetime->directives);
							}           
            if (gui->si->currentobject->prev)
              g_string_append_printf (warning, _("A Time Signature Change should be the first object in a measure\n" "unless you are trying to do something unusual"));
          }
          break;
        case KEYSIG:
          {
						keysig *thekey = ((keysig *) curObj->object);
            g_string_append_printf (selection, _("a Key Signature Change object.\n"));
						if (thekey->directives) 
							{
								selection = g_string_append (selection, _("Attached to the Key Signature Change:"));
								append_directives_information (selection, thekey->directives);
							}
          }
          break;
        case STEMDIRECTIVE:
          {
						stemdirective *thestem = ((stemdirective *) curObj->object);
            g_string_append_printf (selection, _("a Stem Directive, the notes after the cursor %s"), ((stemdirective *) curObj->object)->type == DENEMO_STEMDOWN ? _("will have stems downwards") : ((stemdirective *) curObj->object)->type == DENEMO_STEMUP ? _("will have stems upwards") : _("will have stems up or down as needed"));
						if (thestem->directives) 
							{
								selection = g_string_append (selection, _("Attached to the Stemming Change:"));
								append_directives_information (selection, thestem->directives);
							}
          }
          break;
        case LILYDIRECTIVE:
          {
            DenemoDirective *directive = (DenemoDirective *) curObj->object;
            if(directive->tag==NULL)
							directive->tag = g_string_new("<Unknown Tag>");//shouldn't happen
            const gchar *label = get_label_for_command(directive->tag->str);
            const gchar *menupath = get_menu_path_for_command(directive->tag->str);
            const gchar *tooltip = get_tooltip_for_command (directive->tag->str);

            if(label)
               g_string_append_printf (selection, _("a Denemo Directive: (%s)"), label);
            else
							g_string_append_printf (selection, _("a Denemo Directive: (%s)"), directive->tag->str);
            if(tooltip)
                g_string_append_printf (selection, _("\nThe help for the command that created this directive is \"%s\""), tooltip);

           g_string_append_printf (selection, _("%s"), directive->x ? _("\nNot all layouts\n") : directive->y ? _("\nOnly for one Layout\n"): "\n");
           if(menupath)
						g_string_append_printf (selection, _("Menu location for this command: \"%s\"\n"), menupath);
						
						g_string_append_printf (selection, _("The LilyPond text inserted is %s%s\n"),  
							directive->prefix?directive->prefix->str:"",
							directive->postfix?directive->postfix->str:"");
						
           if (gui->si->currentobject->next == NULL && (gui->si->currentmeasure->next == NULL))
              g_string_assign (warning, _("This Directive is at the end of the music" "\nYou may need a closing double bar line - see Directives → barlines"));
          }
          break;
        default:
          {
            g_string_append (selection, _("The cursor is on an unknown object type. Please report how this happened!"));
          }
          break;
        }

      if (warning->len)
        {
          GtkWidget *label = gtk_label_new ("");
          warning = g_string_prepend (warning, _("<span font-desc=\"30\">Warning</span> "));
          gtk_label_set_markup (GTK_LABEL (label), warning->str);
          gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
        }
      if (selection->len)
        {
          GtkWidget *label = gtk_label_new (selection->str);
          gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, TRUE, 0);
        }
      g_string_free (warning, TRUE);
      g_string_free (selection, TRUE);
    }
  gtk_widget_show_all (ObjectInfo);
}

/****************
 * write the status bar

********************/

void
write_status (DenemoProject * gui)
{
  gint minutes = 0;
  gdouble seconds = 0.0;
  gdouble early = 0.0, late = 0.0;
  gchar *selection;
  if (gui->si == NULL)
    return;
  if (gui->si->currentobject && gui->si->currentobject->data)
    {
      DenemoObject *curObj = gui->si->currentobject->data;
      if ((gui->si->smfsync == gui->si->changecount))
        {
          if (curObj->midi_events)
            {
              smf_event_t *event = (smf_event_t *) curObj->midi_events->data;
              gdouble time = event->time_seconds;
              minutes = time / 60.0;
              seconds = time - 60 * minutes;
            }
          early = curObj->earliest_time, late = curObj->latest_time;
        }

      switch (curObj->type)
        {
        case CHORD:
          {
            chord *thechord = ((chord *) curObj->object);
            selection = g_strdup_printf ("%s%s%s%s%s%s%s%s%s",
                                         thechord->notes ? (g_list_length (thechord->notes) > 1 ? _("Chord ") : _("Note ")) : _("Rest "), thechord->slur_begin_p ? _(", begin slur") : "", thechord->slur_end_p ? _(", end slur") : "", thechord->is_tied ? _(", tied") : "", thechord->crescendo_begin_p ? _(", begin cresc.") : "", thechord->crescendo_end_p ? _(", end cresc.") : "", thechord->diminuendo_begin_p ? _(", begin dim.") : "", thechord->diminuendo_end_p ? _(", end dim.") : "", thechord->is_grace ? _(", grace note") : "");
            if (thechord->notes)
              {
                GList *g;
                for (g = thechord->notes; g; g = g->next)
                  {
                    note *thenote = (note *) g->data;
                    GList *h;
                    for (h = thenote->directives; h; h = h->next)
                      {
                        DenemoDirective *directive = (DenemoDirective *) h->data;
                        if (directive->postfix || directive->prefix)
                          {
                            gchar *old = selection;
                            selection = g_strdup_printf ("%.50s (%s) %.50s", directive->prefix ? directive->prefix->str : "", selection, directive->postfix ? directive->postfix->str : "");
                            g_free (old);
                          }
                      }
                  }
              }
          }
          break;

        case TUPOPEN:
          selection = g_strdup_printf (_("Tuplet %d/%d"), ((tupopen *) curObj->object)->numerator, ((tupopen *) curObj->object)->denominator);
          break;
        case TUPCLOSE:
          selection = g_strdup_printf (_("End tuplet"));
          break;
        case CLEF:
          selection = g_strdup_printf (_("Clef change"));
          break;
        case TIMESIG:
          selection = g_strdup_printf (_("Time signature change"));
          break;
        case KEYSIG:
          selection = g_strdup_printf (_("Key signature change"));
          break;
        case STEMDIRECTIVE:
          selection = g_strdup_printf (_("Stem directive: %s"), ((stemdirective *) curObj->object)->type == DENEMO_STEMDOWN ? _("stem down") : ((stemdirective *) curObj->object)->type == DENEMO_STEMUP ? _("stem up") : _("normal stemming"));
          break;
        case DYNAMIC:
          selection = g_strdup_printf (_("Dynamic: %s"), ((dynamic *) curObj->object)->type->str);
          break;

        case LILYDIRECTIVE:
          {
            DenemoDirective *directive = (DenemoDirective *) curObj->object;
            selection = g_strdup_printf (_("Directive:(%.20s) %.20s%.50s"), directive->tag ? directive->tag->str : _("Unknown Tag"), directive->x ? _("Not all layouts") : directive->y ? _("Only for one Layout") : "", directive->postfix ? directive->postfix->str : directive->prefix ? directive->prefix->str : directive->graphic_name ? directive->graphic_name->str : directive->display ? directive->display->str : "empty");
          }
          break;
        default:
          selection = g_strdup_printf (_("Cursor on an unknown object"));
        }
    }
  else
    selection = g_strdup_printf (_("Cursor not on any object"));

  GString *status = g_string_new (_("Movement"));
  gint index = g_list_index (gui->movements, gui->si);
  const gchar *dur ="";
  switch (get_prevailing_duration())
  {
	  case 0: dur = NOTE0;
	  break;
	  case 1: dur = NOTE1;
	  break;
	  case 2: dur = NOTE2;
	  break;
	  case 3: dur = NOTE3;
	  break;
	  case 4: dur = NOTE4;
	  break;
	  case 5: dur = NOTE5;
	  break;
	  case 6: dur = NOTE6;
	  break;
	  case 7: dur = NOTE7;
	  break;
	  case 8: dur = NOTE8;
	  break;
	 
	  
  }
  g_string_printf (status, "%s%s %d: %s: ", enshift_string (gui->si->pending_enshift), dur, index + 1, selection);
  

  
  if (gui->si->smf && (gui->si->smfsync == gui->si->changecount) && Denemo.prefs.playback_controls)
    g_string_append_printf (status, _("%d min %.2f sec %.2f %.2f"), minutes, seconds, early, late);
  else
    g_string_append_printf (status, _(" Staff %d Measure %d Position %d %s"), gui->si->currentstaffnum, gui->si->currentmeasurenum, gui->si->cursor_x + 1, gui->si->cursor_appending ? _("Appending") : _("Not Appending") /*not understood this one... , gui->si->cursoroffend?"Off End":"Not Off End" */ );

  if (Denemo.prefs.midi_in_controls)
    {
      gchar *thesharp = sharpest ();
      gchar *theflat = flattest ();
      g_string_append_printf (status, " |%s - %s|", theflat, thesharp);
      g_free (theflat);
      g_free (thesharp);
    }

  g_free (selection);
  
  gtk_label_set_text (GTK_LABEL (Denemo.statuslabel), status->str);
  g_string_free (status, TRUE);
  drop_object_info ();
}

void
write_input_status (void)
{
  gtk_label_set_text (GTK_LABEL (Denemo.input_source), Denemo.input_filters->str);
}

/**
 * Display a message box asking primary & secondary messages
 * @return TRUE if the OK button was clicked or Enter pressed
 */
gboolean
confirm (gchar * primary, gchar * secondary)
{
  GtkWidget *dialog;
  gboolean r = 0;

  dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "%s", primary);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s", secondary);
  gtk_widget_show_all (dialog);
  r = (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_YES);
  gtk_widget_destroy (dialog);
  return r;
}

/* free a GString and the string it holds, and set the pointer to it to NULL */
void
nullify_gstring (GString ** s)
{
  if (*s)
    g_string_free (*s, TRUE);
  *s = NULL;
}

/**
 * Pops up a dialog box that has a text entry box and ok/cancel buttons
 * title is a title for the box.
 * initial_value is for the text entry box, or NULL if none.
 * instruction is a prompt for the user.
 * Returns a new value on Ok and NULL if cancelled.
 * The returned value should be freed by the caller.
 *
 */

gchar *
string_dialog_entry (DenemoProject * gui, gchar * title, gchar * instruction, gchar * initial_value)
{
  return string_dialog_entry_with_widget (gui, title, instruction, initial_value, NULL);
}

/* as string_dialog_entry() but with extra widget */
gchar *
string_dialog_entry_with_widget_opt (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget, gboolean modal)
{

  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;
  gchar *entry_string;
  GString *string;
  entry = gtk_entry_new ();

  dialog = modal ? gtk_dialog_new_with_buttons (wlabel, GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL) : gtk_dialog_new_with_buttons (wlabel, GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, NULL);
  label = gtk_label_new (direction);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), label);

  if (widget)
    gtk_container_add (GTK_CONTAINER (content_area), widget);

  if (PreValue != NULL)
    {
      gtk_entry_set_text (GTK_ENTRY (entry), (gchar *) PreValue);
    }
  gtk_container_add (GTK_CONTAINER (content_area), entry);

  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  //gtk_widget_grab_focus (entry);
  //gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_CENTER);
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (modal)
    {
      gtk_widget_grab_focus (entry);
      if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
        {
          entry_string = (gchar *) gtk_entry_get_text (GTK_ENTRY (entry));
          string = g_string_new (entry_string);
          gtk_widget_destroy (dialog);
          return g_string_free (string, FALSE);
        }
      else
        {
          gtk_widget_destroy (dialog);
          return NULL;
        }
      return NULL;
    }
  else
    {
      g_signal_connect_swapped (dialog, "response", G_CALLBACK (gtk_main_quit), entry);
      gtk_main ();
      if (GTK_IS_WIDGET (entry))
        {
          entry_string = GTK_IS_WIDGET (entry) ? g_strdup ((gchar *) gtk_entry_get_text (GTK_ENTRY (entry))) : NULL;
          gtk_widget_destroy (dialog);
          return entry_string;
        }
      return NULL;
    }
}

gchar *
string_dialog_entry_with_widget (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget)
{
  return string_dialog_entry_with_widget_opt (gui, wlabel, direction, PreValue, widget, TRUE);
}

/* as string_dialog_entry_with_widget() but gives a text editor instead of a single line editor */
gchar *
string_dialog_editor_with_widget_opt (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget, gboolean modal)
{
  GtkWidget *dialog;
  GtkWidget *textview;
  GtkWidget *label;
  textview = gtk_text_view_new ();
  GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
  gtk_text_buffer_set_text (textbuffer, PreValue ? PreValue : "", -1);
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  dialog = modal ? gtk_dialog_new_with_buttons (wlabel, GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL) : gtk_dialog_new_with_buttons (wlabel, GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, NULL);

  label = gtk_label_new (direction);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_box_pack_start (GTK_BOX (content_area), label, FALSE, TRUE, 0);
  if (widget)
    {
      gtk_box_pack_start (GTK_BOX (content_area), widget, FALSE, TRUE, 0);
      g_object_set_data (G_OBJECT (widget), "textbuffer", textbuffer);
      g_object_set_data (G_OBJECT (widget), "textview", textview);
    }
  gtk_container_add (GTK_CONTAINER (sw), textview);
  gtk_box_pack_start (GTK_BOX (content_area), sw, TRUE, TRUE, 0);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);


  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_CENTER);
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  gtk_widget_grab_focus (textview);
  if (modal)
    {
      if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
        {
          GtkTextIter startiter, enditer;
          gtk_text_buffer_get_start_iter (textbuffer, &startiter);
          gtk_text_buffer_get_end_iter (textbuffer, &enditer);
          gchar *text = gtk_text_buffer_get_text (textbuffer, &startiter, &enditer, FALSE);
          gtk_widget_destroy (dialog);
          return text;
        }
      else
        {
          gtk_widget_destroy (dialog);
          return NULL;
        }
      return NULL;
    }
  else
    {
      g_signal_connect_swapped (dialog, "response", G_CALLBACK (gtk_main_quit), NULL);
      gtk_main ();
      if (GTK_IS_TEXT_BUFFER (textbuffer))
        {
          GtkTextIter startiter, enditer;
          gtk_text_buffer_get_start_iter (textbuffer, &startiter);
          gtk_text_buffer_get_end_iter (textbuffer, &enditer);
          gchar *text = gtk_text_buffer_get_text (textbuffer, &startiter, &enditer, FALSE);
          gtk_widget_destroy (dialog);
          return text;
        }
      else
        {
          return NULL;
        }
    }
}

gchar *
string_dialog_editor_with_widget (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget)
{
  return string_dialog_editor_with_widget_opt (gui, wlabel, direction, PreValue, widget, TRUE);
}

static gboolean
option_choice (GtkWidget * widget, gchar ** response)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
    *response = g_object_get_data (G_OBJECT (widget), "choice");
  //do not respond to the toggling off of other radio buttons
  return TRUE;
}

/* run a dialog for the user to select a string from the NULL separated strings, str
 return NULL if user cancels.*/
gchar *
get_option (gchar * str, gint length)
{
  gchar *response = NULL;
  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Select an Option (or Cancel)",
                                                   GTK_WINDOW (Denemo.window),
                                                   (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
                                                   GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                                   GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                                   NULL);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), vbox);

  gchar *opt;
  GtkWidget *widget1, *widget;
  for (opt = str; (opt - str) < length; opt += strlen (opt) + 1)
    {
      if (opt == str)
        {
          widget = widget1 = gtk_radio_button_new_with_label (NULL, opt);
          response = str;       //this is the first radio button, we set response to be the input string ie first option, in case the user makes no choice, in which case the callback option_choice is not called.
        }
      else
        {
          widget = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (widget1), opt);
        }
      g_object_set_data (G_OBJECT (widget), "choice", (gpointer) opt);
      g_signal_connect (G_OBJECT (widget), "toggled", G_CALLBACK (option_choice), &response);
      gtk_container_add (GTK_CONTAINER (vbox), widget);
    }
  gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_REJECT)
    {
      response = NULL;
    }
  g_debug ("Returning contents of response is %s\n", response);
  gtk_widget_destroy (dialog);
  return response;
}

/* output text to the console window */
void
console_output (gchar * text)
{
  GtkTextIter enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer ((GtkTextView *) (Denemo.console));
  gtk_text_buffer_get_end_iter (buffer, &enditer);
  gtk_text_buffer_insert (buffer, &enditer, text, -1);
}

/* returns an override flag ORd from all those in the list of directives, 
   excluding ones with DENEMO_OVERRIDE_HIDDEN set */
gint
get_override (GList * g)
{
  gint ret = 0;
  for (; g; g = g->next)
    {
      DenemoDirective *d = g->data;
      if (!(d->override & DENEMO_OVERRIDE_HIDDEN))
        ret |= d->override;
    }
  return ret;
}

//modfies name to truncate at the extension (a dot at end before any directory separators), returning a pointer to the extension chopped off
// returns NULL if no extension, with name unchanged

gchar *
remove_extension (gchar * name)
{
  gchar *c;
  if (name)
    for (c = name + strlen (name); c != name; c--)
      {
        if (*c == '.')
          {
            *c = '\0';
            return c + 1;
          }
        if (*c == G_DIR_SEPARATOR)
          break;
      }
  return NULL;
}

//modifies name, removing the extension and returns a newly allocated string
//with the passed extension
gchar *
substitute_extension (gchar * name, gchar * extension)
{
  (void) remove_extension (name);
  return g_strdup_printf ("%s.%s", name, extension);
}

enum clefs
cleftypefromname (gchar * str)
{
  enum clefs ret = DENEMO_TREBLE_CLEF;

  if (g_strcasecmp (str, "treble") == 0)
    ret = DENEMO_TREBLE_CLEF;
  else if (g_strcasecmp (str, "bass") == 0)
    ret = DENEMO_BASS_CLEF;
  else if (g_strcasecmp (str, "alto") == 0)
    ret = DENEMO_ALTO_CLEF;
  else if (g_strcasecmp (str, "\"g_8\"") == 0)
    ret = DENEMO_G_8_CLEF;
  else if (g_strcasecmp (str, "tenor") == 0)
    ret = DENEMO_TENOR_CLEF;
  else if (g_strcasecmp (str, "soprano") == 0)
    ret = DENEMO_SOPRANO_CLEF;
  g_free (str);
  return ret;
}

gint
get_widget_height (GtkWidget * w)
{
  GtkAllocation allocation;
  gtk_widget_get_allocation (w, &allocation);
  return allocation.height;
}

gint
get_widget_width (GtkWidget * w)
{
  GtkAllocation allocation;
  gtk_widget_get_allocation (w, &allocation);
  return allocation.width;
}

void
switch_back_to_main_window (void)
{
  if (Denemo.non_interactive)
    return;
  gtk_window_present (GTK_WINDOW (Denemo.window));
  gtk_widget_grab_focus (Denemo.scorearea);
}

/* set all labels in the hierarchy below widget to use markup */
void
use_markup (GtkWidget * widget)
{
  if (!widget)
    return;
  if (GTK_IS_LABEL (widget))
    {
      gtk_label_set_use_markup (GTK_LABEL (widget), TRUE);
    }
  else if (GTK_IS_CONTAINER (widget))
    {
      GList *g = gtk_container_get_children (GTK_CONTAINER (widget));
      for (; g; g = g->next)
        use_markup (g->data);
      if (GTK_IS_MENU_ITEM (widget))
        {
          use_markup (gtk_menu_item_get_submenu (GTK_MENU_ITEM (widget)));
        }
    }
}

// Help for beginners using keyboard shortcuts
static GtkWidget *KeyStrokes;
static GtkWidget *KeyStrokeLabel;
static GtkWidget *KeyStrokeHelp;


void
KeyStrokeAwait (gchar * first_keypress)
{
  KeyStrokeShow (first_keypress, DENEMO_NO_COMMAND, TwoKey);
}

void
KeyStrokeDecline (gchar * first_keypress)
{
  KeyStrokeShow (first_keypress, DENEMO_NO_COMMAND, SingleKey);
}

void MouseGestureShow (gchar *str, gchar *help, DenemoShortcutType type)
{
  gtk_label_set_text (GTK_LABEL (KeyStrokeHelp), help);
  KeyStrokeShow (str, DENEMO_NO_COMMAND, type);
}
void KeyPlusMouseGestureShow(gchar *str, gint command_idx)
{
  const gchar *label = lookup_label_from_idx (Denemo.map, command_idx);
  const gchar *tooltip = lookup_tooltip_from_idx (Denemo.map, command_idx);
  gchar *text = g_strdup_printf (_("Mouse shortcut <span font_desc=\"24\" foreground=\"blue\">%s</span>" " invokes command <span font_desc=\"24\" foreground=\"dark red\">%s</span>"), str, label);
            
  gtk_window_set_title (GTK_WINDOW (KeyStrokes),  _("Mouse Shortcut"));
  gtk_label_set_markup (GTK_LABEL (KeyStrokeLabel), text);
  gtk_label_set_text (GTK_LABEL (KeyStrokeHelp), tooltip);
  g_free (text);
  gtk_widget_show (KeyStrokes);
}

void
KeyStrokeShow (gchar * str, gint command_idx, DenemoShortcutType type)
{
  if (str != NULL)
    {
      gchar *text;
      if (command_idx != DENEMO_NO_COMMAND)
        {
          const gchar *label = lookup_label_from_idx (Denemo.map, command_idx);
          const gchar *tooltip = lookup_tooltip_from_idx (Denemo.map, command_idx);
          if (type)
            {
              text = g_strdup_printf (_("Key Press <span font_desc=\"24\" foreground=\"blue\">%s</span>" " invokes command <span font_desc=\"24\" foreground=\"dark red\">%s</span>"), str, label);

            }
          else
            {
              text = g_strdup_printf (_("Key Presses <span font_desc=\"24\" foreground=\"blue\">%s</span>" " invoke command <span font_desc=\"24\" foreground=\"dark red\">%s</span>"), str, label);
            }
          gtk_window_set_title (GTK_WINDOW (KeyStrokes), type==SingleKey ? _("Single Key Press") : _("Two Key Presses"));
          gtk_label_set_markup (GTK_LABEL (KeyStrokeLabel), text);
          gtk_label_set_text (GTK_LABEL (KeyStrokeHelp), tooltip);
          g_free (text);
        }
      else // no command
        {
          switch (type) {
            case SingleKey:
              gtk_window_set_title (GTK_WINDOW (KeyStrokes), _("Key Press"));
              gtk_label_set_text (GTK_LABEL (KeyStrokeHelp), "");
              text = g_strdup_printf (_("Key Press <span font_desc=\"24\" foreground=\"blue\">%s</span>" " Is not a shortcut.\n%s"), str,
              (Denemo.project->view != DENEMO_MENU_VIEW)?
              _("(The menus are now restored in case you are lost.)"):"");
              break;
            case TwoKey:
              gtk_window_set_title (GTK_WINDOW (KeyStrokes), _("First Key Press"));
              gtk_label_set_text (GTK_LABEL (KeyStrokeHelp), "");
              text = g_strdup_printf (_("Key Press <span font_desc=\"24\" foreground=\"blue\">%s</span>" " Awaiting continuation"), str);
              break;
            case MouseGesture:
              gtk_window_set_title (GTK_WINDOW (KeyStrokes), _("Mouse"));
              text = g_strdup_printf (_("Mouse <span font_desc=\"24\" foreground=\"blue\">%s</span>"), str);
              break;
            case KeyPlusMouse:
              gtk_window_set_title (GTK_WINDOW (KeyStrokes), _("Key + Mouse"));
              text = g_strdup_printf (_("Key Press <span font_desc=\"24\" foreground=\"blue\">%s</span>"), str);
              break;
            default:
              g_warning("bad call");
              return;
          }
          gtk_label_set_markup (GTK_LABEL (KeyStrokeLabel), text);
          g_free (text);
        }
    }
  gtk_widget_show (KeyStrokes);
}

static gboolean
toggle_show_keystroke_preference (void)
{
  Denemo.prefs.learning = FALSE;
  gtk_widget_hide (KeyStrokes);
  return TRUE;
}

void
initialize_keystroke_help (void)
{
  if (KeyStrokes == NULL)
    {
      KeyStrokes = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      g_signal_connect (G_OBJECT (KeyStrokes), "delete-event", G_CALLBACK (toggle_show_keystroke_preference), NULL);
      gtk_window_set_keep_above (GTK_WINDOW (KeyStrokes), TRUE);
      gtk_window_set_accept_focus (GTK_WINDOW (KeyStrokes), FALSE);
      KeyStrokeLabel = gtk_label_new ("");
      KeyStrokeHelp = gtk_label_new ("");
      GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
      gtk_container_add (GTK_CONTAINER (KeyStrokes), vbox);
      gtk_box_pack_start (GTK_BOX (vbox), KeyStrokeLabel, FALSE, TRUE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), KeyStrokeHelp, FALSE, TRUE, 0);
      gtk_label_set_use_markup (GTK_LABEL (KeyStrokeLabel), TRUE);
      gtk_widget_show_all (KeyStrokes);
    }
  gtk_widget_hide (KeyStrokes);
}

/**
 * find_dir_for_file:
 * @filename: The file to search
 * @dirs: A dir paths array, ending by NULL, where to search.
 *
 * Finds the first dir in the list that contains 'filename', and free the array.
 *
 * Returns: The dir path if found, NULL either
 **/
gchar*
find_dir_for_file(gchar* filename, gchar* dirs[])
{
  gchar *dir = NULL;
  gchar *path = NULL;
  gint i;

  for(i = 0; dirs[i]; i++)
  {
	  //g_debug("Searching %s\n", dirs[i]);
    if(!dir)
    {
      path = g_build_filename (dirs[i], filename, NULL);
      if(g_file_test (path, G_FILE_TEST_EXISTS))
        {
		 dir = g_strdup(dirs[i]);
			//g_debug("Found file %s\n", path);
		} else
			//g_debug("No file %s\n", path);
      g_free(path);
    }
    g_free(dirs[i]);
  }
  return dir;
}

/**
 * find_dir_for_files:
 * @files: The files to search
 * @dirs: A dir paths array, ending by NULL, where to search.
 *
 * Finds the first dir in the list that contains 'filename', and free the array.
 *
 * Returns: The dir path if found, NULL either
 **/
gchar*
find_dir_for_files(gchar* files[], gchar* dirs[])
{
  gchar *dir = NULL;
  gchar *path = NULL;
  gint d, f;

  for(d = 0; dirs[d]; d++)
  {
    for(f = 0; files[f]; f++)
    {
      if(!dir)
      {
        path = g_build_filename (dirs[d], files[f], NULL);
        if(g_file_test (path, G_FILE_TEST_EXISTS))
          dir = g_strdup(dirs[d]);
        g_free(path);
      }
    }
    g_free(dirs[d]);
  }
  return dir;
}

/**
 * find_path_for_file:
 * @filename: The file to search
 * @dirs: A dir paths array, ending by NULL, where to search.
 *
 * Finds the first dir in the list that contains 'filename', and free the array.
 *
 * Returns: The file path if found, NULL either
 **/
gchar*
find_path_for_file(gchar* filename, gchar* dirs[])
{
  gchar* dir = find_dir_for_file (filename, dirs);
  if(dir){
    gchar* path = g_build_filename(dir, filename, NULL);
    g_free(dir);
    return path;
  }
  return NULL;
}

const gchar*
get_local_dir(DenemoDirectory dir)
{
  switch(dir)
  {
    case DENEMO_DIR_COMMANDS:   return COMMANDS_DIR;
    case DENEMO_DIR_UI:         return UI_DIR;
    case DENEMO_DIR_SOUNDFONTS: return SOUNDFONTS_DIR;
    case DENEMO_DIR_PIXMAPS:    return PIXMAPS_DIR;
    case DENEMO_DIR_FONTS:      return FONTS_DIR;
    case DENEMO_DIR_LOCALE:     return LOCALE_DIR;
    default:         return NULL;
  }
}

gchar*
get_system_dir(DenemoDirectory dir)
{
  switch(dir)
  {
    case DENEMO_DIR_COMMANDS:
    case DENEMO_DIR_UI:
    case DENEMO_DIR_SOUNDFONTS:
    case DENEMO_DIR_FONTS:
      return g_build_filename(get_system_data_dir (), get_local_dir(dir), NULL);
    case DENEMO_DIR_PIXMAPS:
      return g_build_filename(get_system_data_dir (), PIXMAPS_DIR, NULL);
      break;
    case DENEMO_DIR_LOCALE:
      return g_strdup(get_system_locale_dir ());
      break;
    case DENEMO_DIR_BIN:
      return g_strdup(get_system_bin_dir ());
      break;
    default:
      return NULL;
  }
}

const gchar*
get_executable_dir ()
{
  static const gchar* dir = NULL;
  if(dir == NULL)
  {
    char path[1024];

#ifdef G_OS_WIN32
    GetModuleFileNameW(NULL, path, MAX_PATH);

#elif defined _MACH_O_
    char path[1024];
    guint size = sizeof (path);
    _NSGetExecutablePath (path, &size);

#else
   readlink("/proc/self/exe", path, sizeof(path));

#endif
    dir = g_path_get_dirname(path);
  }
  return dir;
}

/**
 * find:
 * @dir: The denemo directory where to search
 * @filename: The file to search
 *
 * Finds a file by searching:
 *  - in the local directory
 *  - in the executable parent directory
 *  - in the user directory
 *  - in the system directory
 **/
gchar*
find_denemo_file (DenemoDirectory dir, gchar* filename)
{
  //g_debug("find_denemo_file called with %d and %s\n", dir, filename);
  gchar* dirs[] = {
    g_build_filename(get_executable_dir (TRUE), "..", get_local_dir (dir), NULL),
    g_build_filename(get_user_data_dir (TRUE), get_local_dir (dir), NULL),
    g_strdup(get_system_dir(dir)),
    NULL
  };
 
  return find_path_for_file (filename, dirs);
}
