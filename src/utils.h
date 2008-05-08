/* utils.h
 * Header file for functions useful across the different modules of
 * drawing and non-drawing code.
 *
 * also includes useful constants
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */
#ifndef UTILS_H
#define UTILS_H

#include <denemo/denemo.h>

#define LINE_SPACE 10
#define HALF_LINE_SPACE 5
#define NO_OF_LINES 5
#define STAFF_HEIGHT (LINE_SPACE * (NO_OF_LINES - 1))
#define LYRICS_HEIGHT (STAFF_HEIGHT/2)
#define MID_STAFF_HEIGHT 2*LINE_SPACE
#define LEFT_MARGIN 20
#define KEY_MARGIN (LEFT_MARGIN+35)
#define SPACE_FOR_TIME 30
#define RIGHT_MARGIN 20
#define SPACE_FOR_BARLINE 10
#define HALF_BARLINE_SPACE 5
#define WHOLE_NUMTICKS 1536
#define FONT "Sans 9"
#define TIMESIGFONT "Sans 24"
/* Wrapper macro for creating bitmaps */

#define bitmaphelper(widget, name) \
   gdk_bitmap_create_from_data (NULL, \
                                (gchar *) name##_bits, \
                                name##_width, name##_height)

/* Adds a callback that processes the "activate" signal coming from
 * a widget */

#define processenter(entry, callback,  cbdata, dialog)\
  gtk_signal_connect(GTK_OBJECT(entry), "activate", \
		     GTK_SIGNAL_FUNC(callback), \
		     &cbdata); \
  if (dialog) \
    gtk_signal_connect_object (GTK_OBJECT (entry), "activate", \
	  		       GTK_SIGNAL_FUNC(gtk_widget_destroy), \
			       GTK_OBJECT (dialog))
  

void
drawbitmapinverse (GdkPixmap *pixmap, GdkGC *gc, GdkBitmap *mask,
		   gint x, gint y, gint width, gint height);

/* Gives space after a note or rest */

gint
space_after (gint numticks, gint wholenotewidth);

/* Returns height of a note based on what the note is and and the current
 * clef context */

gint
calculateheight (gint mid_c_offset, gint dclef);

/* Translates a mid_c_offset into 0 (c) through 6 (b). Useful for
 * getting accidentals to persist */

gint
offsettonumber (gint n);

gchar
mid_c_offsettoname (gint mid_c_offset);

gint
mid_c_offsettooctave (gint mid_c_offset);

void 
set_grace_numticks(DenemoObject *theobj, gint multiplier);

void
set_tuplefied_numticks (DenemoObject *theobj, gint numerator,
			 gint denominator);

void
set_basic_numticks (DenemoObject *theobj);

void
setpixelmin (DenemoObject *theobj);

void
freeit (gpointer data, gpointer user_data);

void
warningdialog(gchar *msg);
void
infodialog(gchar *msg);



void 
headerfields(GtkWidget *dialog, GtkListStore *list_store, 
	     GtkTreeIter* iter, DenemoScore *si, gboolean isdialog); 
/* default isdialog shall be TRUE */


void initdir ();
const gchar *get_data_dir ();
const gchar *get_plugin_dir ();
const gchar *get_locale_dir ();

void kill_process (GPid pid);

gchar * music_font(gchar *str);

void  set_title_bar(DenemoGUI *gui);
void score_status(DenemoGUI *gui, gboolean change);
gboolean
confirm (gchar *primary, gchar *secondary);
void 
nullify_gstring (GString **s);

gchar *
string_dialog_entry (DenemoGUI *gui, gchar *wlabel, gchar *direction, gchar *PreValue);
	

#endif /* UTILS_H */
