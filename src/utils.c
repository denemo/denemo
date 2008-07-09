/* utils.cpp
 * Functions useful across the different modules of
 * drawing and non-drawing code.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <stdio.h>
#include <gtk/gtk.h>
#include "accwidths.h"
#include <denemo/denemo.h>
#include "notewidths.h"
#include "utils.h"
#include <signal.h> /*for SIGTERM */

#include "config.h"
#ifdef G_OS_WIN32
#include "windows.h"
#else
#include "binreloc.h"
#endif

/**
 * Pops up a warning dialog and blocks until it is dismissed
 *  @param msg warning message to display
 * @return none
 */
void
warningdialog (gchar * msg)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
				   GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_WARNING, GTK_BUTTONS_CLOSE, msg);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/**
 * Displays information message to screen, not blocking.
 * User can destroy window when no longer needed.
 * @param msg message to display
 * @return none
 */
void
infodialog (gchar * msg)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
				   GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO, GTK_BUTTONS_NONE, msg);
  gtk_widget_show_all(dialog);
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
drawbitmapinverse (GdkPixmap * pixmap, GdkGC * gc, GdkBitmap * mask, gint x,
		   gint y, gint width, gint height)
{
  gdk_gc_set_clip_mask (gc, mask);
  gdk_gc_set_clip_origin (gc, x, y);
  gdk_draw_rectangle (pixmap, gc, TRUE, x, y, width, height);
  gdk_gc_set_clip_mask (gc, NULL);	/* Removes clip mask */
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
set_tuplefied_numticks (DenemoObject * theobj, gint numerator,
			gint denominator)
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
      power = 1 << ((chord *) theobj->object)->baseduration;
      withoutdots = WHOLE_NUMTICKS / power;
      addperdot = withoutdots / 2;
      theobj->basic_durinticks = withoutdots;
      for (i = 0; i < ((chord *) theobj->object)->numdots;
	   addperdot /= 2, i++)
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
      headtype = MIN (baseduration, 2);
      if (chordval.notes)
	{
	  /* if (theobj->isstart_beamgroup && theobj->isend_beamgroup &&
	     chordval.is_stemup)
	     theobj->minpixelsalloted = headwidths[headtype] + STEM_WIDTH;
	     else
	     theobj->minpixelsalloted = headwidths[headtype];  */
	  /* We can get away with that because the stems are narrower
	   * than even the narrowest notes; upstemmed notes are
	   * the unusual case here */

	  /* The above code will allow extra space for the stem of
	   * stemup notes. It's commented out 'cause we no longer want
	   * that behavior */
	  theobj->minpixelsalloted = headwidths[headtype];
	}
      else			/* a rest */
	theobj->minpixelsalloted = restwidths[baseduration];

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
	      theobj->space_before =
		MAX (theobj->space_before, thetone->position_of_accidental);
	  }
      if (chordval.is_reversealigned)
	if (chordval.is_stemup)
	  theobj->minpixelsalloted += headwidths[headtype];
	else if (!chordval.hasanacc)
	  /* Accidental positioning already accounts for the extra leading
	     space that we need for reverse-aligned noteheads, provided
	     the chord has an accidental in it somewhere. We only have to
	     remark upon noteheads to the left of the stem if there weren't
	     any accidentals to position.  */
	  theobj->space_before += headwidths[headtype];
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
      8;
#endif
      theobj->space_before = 0;
      break;
    case LILYDIRECTIVE:
      theobj->minpixelsalloted = 8;
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
      theobj->minpixelsalloted = 40;
      theobj->space_before = 0;
      break;
    default:
      theobj->minpixelsalloted = 0;
      theobj->space_before = 0;
      break;
    }
}

/**
 * Returns the height of a tone based on its mid_c_offset and the clef that it's in 
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
      break;			/* Probably gratuitous */
    case DENEMO_ALTO_CLEF:
      return 2 * LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_G_8_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset - 1);
      break;
    case DENEMO_BASS_CLEF:
      return -LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_TENOR_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * mid_c_offset;
      break;
    case DENEMO_SOPRANO_CLEF:
      return LINE_SPACE - HALF_LINE_SPACE * (mid_c_offset - 6);
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
 * converts the mid_c_offset to the correct letter name
 * @param mid_c_offset the mid_c_offset to convert
 * @return the character name of the mid_c_offset
 */
gchar
mid_c_offsettoname (gint mid_c_offset)
{
  int otn = offsettonumber (mid_c_offset);

  return ((otn + 2) % 7) + 'a';
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
    default:			/* needs to be up to date with enum in include/denemo/denemo.h */
      fprintf (stderr, "!!!!!unknown object type %x - see enum in denemo.h\n",
	       curObj->type);
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
  for (measure = firstmeasurenode (thestaff); measure;
       measure = measure->next)
    {
      fprintf (stderr, "*************Measure %d *************\n",
	       measurenum++);
      printmeasure (measure);
    }
}
G_GNUC_UNUSED void
printscoreinfo (DenemoScore * si)
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
  GError *error=NULL;
  if(!gbr_init (&error))
    {
      g_print ("BinReloc failed to initialize:\n");
      g_print ("Domain: %d (%s)\n",
               (int) error->domain,
               g_quark_to_string (error->domain));
      g_print ("Code: %d\n", error->code);
      g_print ("Message: %s\n", error->message);
      g_error_free (error);
      g_print ("----------------\n");
    }
#endif /* not G_OS_WIN32 */
}

const gchar *
get_data_dir ()
{
  static gchar *datadir = NULL;
  if (datadir == NULL)
  {
#ifdef G_OS_WIN32
    gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
    datadir = g_build_filename (rootdir, "share", "denemo", NULL);
    g_free (rootdir);
#else /* not G_OS_WIN32 */
  datadir = gbr_find_data_dir (PKGDATADIR);
#endif /* not G_OS_WIN32 */
  }
  return datadir;
}

const gchar *
get_plugin_dir ()
{
  static gchar *plugindir = NULL;
  if (plugindir == NULL)
  {
#ifdef G_OS_WIN32
    gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
    plugindir = g_build_filename (rootdir, "lib", "denemo", NULL);
    g_free (rootdir);
#else /* not G_OS_WIN32 */
    plugindir = gbr_find_lib_dir (PACKAGE_PLUGIN_DIR);
#endif /* not G_OS_WIN32 */
  }
  return plugindir;
}


const gchar *
get_locale_dir ()
{
  static gchar *localedir = NULL;
  if (localedir == NULL)
  {
#ifdef G_OS_WIN32
    gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
    localedir = g_build_filename (rootdir, "share", "locale", "denemo", NULL);
    g_free (rootdir);
#else /* not G_OS_WIN32 */
# ifndef ENABLE_BINRELOC
    /* it seems to be the standard way (no binreloc)
     * to set the path of translations this way:
     * messages are in $LOCALEDIR/$LANG/denemo
     */
    localedir = g_strdup (LOCALEDIR);
# else /* ENABLE_BINRELOC */
    /* binreloc says it is disabled even with built thanks to
     * --enable-binreloc... So, searhing falls back to
     *  $LOCALEDIR/denemo/$LANG which is not a valid path
     */
    gchar *localedir2 = gbr_find_locale_dir (LOCALEDIR);
    localedir = g_build_filename (localedir2, "denemo", NULL);
    g_free (localedir2);
# endif /* ENABLE_BINRELOC */
#endif /* not G_OS_WIN32 */
  }
  return localedir;
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

/* markup the passed string to be in the denemo music font
* caller must free the returned string
*/
gchar * music_font(gchar *str) {
  return g_strdup_printf("<span font_desc=\"Denemo 12\">%s</span>", str);
}

void  set_title_bar(DenemoGUI *gui) {
  gchar *title;
  if(gui->filename && gui->filename->len)
    title = gui->filename->str;
  else
    title = "(Untitled)";
  title = g_strdup_printf("Denemo - %s%c", title, gui->changecount?'*':' ');  
  gtk_notebook_set_tab_label_text (Denemo.notebook, gui->page, title); 
  g_free(title);
}

void score_status(DenemoGUI *gui, gboolean change) {
  if(change) { 
    gui->changecount++;
    if(gui->changecount==1)//just changed
      set_title_bar(gui);

  } else {
    gui->changecount=0;
    set_title_bar(gui);
  }
  write_status(gui);
}


/**
 * Display a message box asking primary & secondary messages
 * @return TRUE if the OK button was clicked or Enter pressed
 */
gboolean
confirm (gchar *primary, gchar *secondary)
{
  GtkWidget *dialog;
  gboolean r = 0;

  dialog = gtk_message_dialog_new (NULL,
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_QUESTION,
				   GTK_BUTTONS_YES_NO,
				   primary);

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    secondary);
  gtk_widget_show_all (dialog);
  r = (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_YES);
  gtk_widget_destroy (dialog);
  return r;
}

/* free a GString and the string it holds, and set the pointer to it to NULL */
void nullify_gstring (GString **s) {
  if(*s)
    g_string_free(*s, TRUE);
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
string_dialog_entry (DenemoGUI *gui, gchar *title, gchar *instruction, gchar *initial_value)
{
  string_dialog_entry_with_widget (gui, title, instruction, initial_value, NULL);
}

/* as string_dialog_entry() but with extra widget */
gchar *
string_dialog_entry_with_widget (DenemoGUI *gui, gchar *wlabel, gchar *direction, gchar *PreValue, GtkWidget *widget)
{

 	GtkWidget *dialog;
	GtkWidget *entry;
	GtkWidget *label;
	gchar *entry_string;
	GString *string;
	entry = gtk_entry_new ();

	dialog = gtk_dialog_new_with_buttons (wlabel,
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                       GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                        NULL);

	label = gtk_label_new (direction);
  	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		                        TRUE, TRUE, 0);
	if(widget)
	  	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), widget,
		                        TRUE, TRUE, 0);
	if (PreValue != NULL) {
            gtk_entry_set_text (GTK_ENTRY (entry), (gchar *) PreValue);
        }
  
	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), entry,
		                        TRUE, TRUE, 0);
    	gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
      	gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
        gtk_widget_grab_focus (entry);
  	gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
	gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
	gtk_widget_show_all (dialog);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT){ 
		entry_string = (gchar *) gtk_entry_get_text (GTK_ENTRY (entry));
		string = g_string_new(entry_string);
		gtk_widget_destroy (dialog);
		return g_string_free(string, FALSE);
	}
	else {  
		gtk_widget_destroy (dialog);
		return NULL;
	}

}
