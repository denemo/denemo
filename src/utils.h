/* utils.h
 * Header file for functions useful across the different modules of
 * drawing and non-drawing code.
 *
 * also includes useful constants
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, 2008, 2009 Richard Shann
 */
#ifndef UTILS_H
#define UTILS_H

#include <denemo/denemo.h>
#include <gdk/gdk.h>
#define HIGHLIGHT_OFFSET (20) /* Fairly arbitrary value to transform codes '0', '1' ... to a new range with the meaning highlight the whole-note, half-note, ..., glyph */ 
#define MAXEXTRASPACE (150) /* maximum space for ledger lines, for sanity */

#define LINE_SPACE 10
#define HALF_LINE_SPACE 5
#define NO_OF_LINES 5
#define STAFF_HEIGHT (LINE_SPACE * (NO_OF_LINES - 1))
#define LYRICS_HEIGHT (STAFF_HEIGHT/2)
#define MID_STAFF_HEIGHT 2*LINE_SPACE
#define LEFT_MARGIN 20
#define KEY_MARGIN (LEFT_MARGIN+35)
#define SPACE_FOR_TIME 35
#define RIGHT_MARGIN 20
#define SPACE_FOR_BARLINE 10
#define HALF_BARLINE_SPACE 5
#define WHOLE_NUMTICKS 1536
#define FONT "Sans 9"
#define TIMESIGFONT "Sans 24"

/* Adds a callback that processes the "activate" signal coming from
 * a widget */

#define processenter(entry, callback,  cbdata, dialog)\
  g_signal_connect(G_OBJECT(entry), "activate", \
		     G_CALLBACK(callback), \
		     &cbdata); \
  if (dialog) \
    g_signal_connect_object (G_OBJECT (entry), "activate", \
	  		       G_CALLBACK(gtk_widget_destroy), \
			       G_OBJECT (dialog), G_CONNECT_AFTER)
  

void
drawbitmapinverse_cr (cairo_t * cr, DenemoGraphic * mask, gint x,
		   gint y);

void
drawfetachar_cr (cairo_t * cr, gunichar uc, double x, double y);

//void
//setcairocolor (cairo_t * cr, GdkGC * gc);

void 
drawnormaltext_cr (cairo_t *cr, const char *text, double x, double y);

void 
drawlargetext_cr (cairo_t *cr, const char *text, double x, double y);

void 
drawtext_cr (cairo_t *cr, const char *text, double x, double y, double size);

gint draw_for_directives(cairo_t *cr, GList *directives, gint x, gint y);

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

gchar *
mid_c_offsettolily (int mid_c_offset, int enshift);

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
popup_menu(gchar *name);

void
warningmessage (gchar * msg);

void
warningdialog(gchar *msg);
void
infodialog(gchar *msg);

void
progressbar(gchar *msg);
void
progressbar_stop(void);

void 
headerfields(GtkWidget *dialog, GtkListStore *list_store, 
	     GtkTreeIter* iter, DenemoScore *si, gboolean isdialog); 
/* default isdialog shall be TRUE */


void initdir (void);
const gchar *get_data_dir (void);
const gchar *get_conf_dir (void);
const gchar *get_locale_dir (void);
const gchar *get_bin_dir (void);
void kill_process (GPid pid);

gchar * music_font(gchar *str);

void  set_title_bar(DenemoGUI *gui);
void score_status(DenemoGUI *gui, gboolean change);
void      write_status(DenemoGUI *gui);
gboolean
confirm (gchar *primary, gchar *secondary);
void 
nullify_gstring (GString **s);

gchar *
string_dialog_entry (DenemoGUI *gui, gchar *title, gchar *instruction, gchar *initial_value);

gchar *
string_dialog_entry_with_widget (DenemoGUI *gui, gchar *title, gchar *instruction, gchar *initial_value, GtkWidget *extra_widget);

void note2lilynotename(struct note *noteobject, GString *ret);
void note2lilyaccidental(struct note *noteobject, GString *ret);
void note2lilyoctave(struct note* noteobject, GString *ret);
void chord2lilybaseduration(struct chord *chordobject, GString *ret);
void chord2lilyduration(struct chord *chordobject, GString *ret);
void chord2lilynumdots(struct chord *chordobject, GString *ret);

#define UTILS_H_PARAM_ASSIGN(param_name) if( (str = g_strstr_len(values->str+i,strlen(values->str+i), #param_name)))\
nothing=FALSE,param_name = (*(str+strlen(#param_name))=='=')?str+strlen(#param_name)+1:NULL;

#define GET_1PARAM(action, param, param_name) \
gchar * query = NULL;\
gchar * param_name = NULL;\
  DenemoScriptParam dummy;\
  dummy.string=NULL;\
  if(param==NULL)\
    param = &dummy;\
  param->status = FALSE;\
if(!action && param){\
    gboolean nothing=TRUE;\
    GString *values = ((DenemoScriptParam *)param)->string;\
    if(values) {\
      gchar *str;\
      gint i;\
      for(i=0;i<values->len;i+=strlen(values->str+i)+1) {\
         UTILS_H_PARAM_ASSIGN(query)\
         UTILS_H_PARAM_ASSIGN(param_name)\
      }\
    }\
    if(param_name==NULL)\
       param_name = values?values->str:NULL;\
}

#define GET_2PARAMS(action, param, param_name1, param_name2) \
gchar * query = NULL;\
gchar * param_name1 = NULL;\
gchar * param_name2 = NULL;\
  DenemoScriptParam dummy;\
  dummy.string=NULL;\
  if(param==NULL)\
    param = &dummy;\
  param->status = FALSE;\
if(!action && param){\
    gboolean nothing=TRUE;\
    GString *values = ((DenemoScriptParam *)param)->string;\
    if(values) {\
     gchar *str;\
     gint i;\
       for(i=0;i<values->len;i+=strlen(values->str+i)+1) {\
          UTILS_H_PARAM_ASSIGN(query)\
          UTILS_H_PARAM_ASSIGN(param_name1)\
          UTILS_H_PARAM_ASSIGN(param_name2)\
       }\
     }\
     if(nothing)\
      param_name1=values?values->str:NULL;\
}
#define GET_3PARAMS(action, param, param_name1, param_name2, param_name3) \
gchar * query = NULL;\
gchar * param_name1 = NULL;\
gchar * param_name2 = NULL;\
gchar * param_name3 = NULL;\
  DenemoScriptParam dummy;\
  dummy.string=NULL;\
  if(param==NULL)\
    param = &dummy;\
  param->status = FALSE;\
if(!action && param){\
    gboolean nothing=TRUE;\
    GString *values = ((DenemoScriptParam *)param)->string;\
    if(values) {\
     gchar *str;\
     gint i;\
       for(i=0;i<values->len;i+=strlen(values->str+i)+1) {\
          UTILS_H_PARAM_ASSIGN(query)\
          UTILS_H_PARAM_ASSIGN(param_name1)\
          UTILS_H_PARAM_ASSIGN(param_name2)\
          UTILS_H_PARAM_ASSIGN(param_name3)\
       }\
     }\
     if(nothing)\
      param_name1=values?values->str:NULL;\
}
#define GET_4PARAMS(action, param, param_name1, param_name2, param_name3, param_name4) \
gchar * query = NULL;\
gchar * param_name1 = NULL;\
gchar * param_name2 = NULL;\
gchar * param_name3 = NULL;\
gchar * param_name4 = NULL;\
  DenemoScriptParam dummy;\
  dummy.string=NULL;\
  if(param==NULL)\
    param = &dummy;\
  param->status = FALSE;\
if(!action && param){\
    GString *values = ((DenemoScriptParam *)param)->string;\
    gboolean nothing=TRUE;\
    if(values) {\
     gchar *str;\
     gint i;\
       for(i=0;i<values->len;i+=strlen(values->str+i)+1) {\
          UTILS_H_PARAM_ASSIGN(query)\
          UTILS_H_PARAM_ASSIGN(param_name1)\
          UTILS_H_PARAM_ASSIGN(param_name2)\
          UTILS_H_PARAM_ASSIGN(param_name3)\
          UTILS_H_PARAM_ASSIGN(param_name4)\
       }\
     }\
     if(nothing)\
      param_name1=values?values->str:NULL;\
}

gchar * get_option(gchar *str, gint length);
void console_output(gchar *text);

gint get_override(GList *g);

void add_font_directory(gchar *fontpath);
void add_font_file(gchar *fontpath);

gboolean run_file_association(gchar *filenam);
gchar *make_temp_dir(void);
gchar *remove_extension(gchar *name);
gchar *substitute_extension(gchar *name, gchar *extension);
void init_denemo_notenames(void);
gint get_widget_height(GtkWidget *w);
gint get_widget_width(GtkWidget *w);
void switch_back_to_main_window(void);
#endif /* UTILS_H */
