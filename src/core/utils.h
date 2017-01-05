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
//FIXME, these two need to be an enum in denemo_types.h and the field showaccidental changed from boolean to this type.
#define DENEMO_REMINDER (2)
#define DENEMO_CAUTIONARY (3)

#define HIGHLIGHT_OFFSET (20)   /* Fairly arbitrary value to transform codes '0', '1' ... to a new range with the meaning highlight the whole-note, half-note, ..., glyph */
#define MAXEXTRASPACE (150)     /* maximum space for ledger lines, for sanity */

#define LINE_SPACE 10
#define HALF_LINE_SPACE 5
#define NO_OF_LINES 5
#define STAFF_HEIGHT (LINE_SPACE * (NO_OF_LINES - 1))
#define LYRICS_HEIGHT (STAFF_HEIGHT/2)
#define MID_STAFF_HEIGHT 2*LINE_SPACE
/*#define LEFT_MARGIN 20 now variable for braces to take space */
//#define KEY_MARGIN (LEFT_MARGIN+35)
#define SPACE_FOR_TIME 35
#define RIGHT_MARGIN 20
#define SPACE_FOR_BARLINE 10
#define HALF_BARLINE_SPACE 5
#define WHOLE_NUMTICKS 1536
#define FONT "Sans 9"
#define TIMESIGFONT "Sans 24"

#ifndef g_info
#ifdef G_HAVE_ISO_VARARGS
#define g_info(...) g_log(G_LOG_DOMAIN, G_LOG_LEVEL_INFO, __VA_ARGS__)
#elif defined(G_HAVE_GNUC_VARARGS)
#define g_info(format) g_log(G_LOG_DOMAIN, G_LOG_LEVEL_INFO, format)
#endif
#endif

const gchar *get_user_data_dir (gboolean create);

const gchar *locateprintdir (void);
void removeprintdir (void);
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


void drawbitmapinverse_cr (cairo_t * cr, DenemoGraphic * mask, gint x, gint y, gboolean invert);

void drawfetachar_cr (cairo_t * cr, gunichar uc, double x, double y);

//void
//setcairocolor (cairo_t * cr, GdkGC * gc);

void drawnormaltext_cr (cairo_t * cr, const char *text, double x, double y);

void drawlargetext_cr (cairo_t * cr, const char *text, double x, double y);

void drawtext_cr (cairo_t * cr, const char *text, double x, double y, double size);

void draw_staff_brace (cairo_t * cr, gboolean curly, gint x, gint y, gint height);

gint draw_for_directives (cairo_t * cr, GList * directives, gint x, gint y, gboolean at_cursor);

gchar *pretty_name (gchar * lilynote);

/* Gives space after a note or rest */

gint space_after (gint numticks, gint wholenotewidth);

/* Returns height of a note based on what the note is and and the current
 * clef context */

gint calculateheight (gint mid_c_offset, gint dclef);

/* Translates a mid_c_offset into 0 (c) through 6 (b). Useful for
 * getting accidentals to persist */

gint offsettonumber (gint n);

gchar *mid_c_offsettolily (int mid_c_offset, int enshift);

gchar mid_c_offsettoname (gint mid_c_offset);

gint mid_c_offsettooctave (gint mid_c_offset);

void set_grace_numticks (DenemoObject * theobj, gint multiplier);

void set_tuplefied_numticks (DenemoObject * theobj, gint numerator, gint denominator);

void set_basic_numticks (DenemoObject * theobj);

void setpixelmin (DenemoObject * theobj);

void freeit (gpointer data, gpointer user_data);

void popup_menu (gchar * name);

void warningmessage (gchar * msg);

void warningdialog (gchar * msg);
void infowarningdialog (gchar * msg, gboolean info);
GtkWidget *infodialog (gchar * msg);

GtkWindow *progressbar (gchar * msg, gpointer callback);
void progressbar_stop (void);
void busy_cursor (GtkWidget * area);
void normal_cursor (GtkWidget * area);
void headerfields (GtkWidget * dialog, GtkListStore * list_store, GtkTreeIter * iter, DenemoMovement * si, gboolean isdialog);
/* default isdialog shall be TRUE */


void initdir (void);
const gchar *get_system_data_dir (void);
/* UNUSED
const gchar *get_system_conf_dir (void);
*/
const gchar *get_system_locale_dir (void);
const gchar *get_system_bin_dir (void);
const gchar *get_system_font_dir (void);
const gchar *get_executable_dir (void);
/* get directory of current Denemo.project or home directory if untitled. User must free the returned string */
gchar *get_project_dir (void);
const gchar *get_local_dir (DenemoDirectory dir);
gchar *get_system_dir (DenemoDirectory dir);

void copy_files (gchar *source_dir, gchar *dest_dir);//copies all files in source_dir to dest_dir creating the latter if need be

void kill_process (GPid pid);

gchar *music_font (gchar * str);

void set_title_bar (DenemoProject * gui);
void score_status (DenemoProject * gui, gboolean change);
void write_status (DenemoProject * gui);
gboolean confirm (gchar * primary, gchar * secondary);
gboolean confirm_first_choice (gchar *title, gchar * primary, gchar * secondary);
gboolean choose_option (gchar * title, gchar * primary, gchar * secondary);
gint choose_option_or_cancel (gchar * title, gchar * primary, gchar * secondary, gboolean cancel_button);
void nullify_gstring (GString ** s);
gchar *choose_file (gchar * title, gchar * startdir, GList * extensions);
gchar *string_dialog_entry (DenemoProject * gui, gchar * title, gchar * instruction, gchar * initial_value);
gchar *notes_choice_dialog (gint number_of_notes /* 1 or 2 */, gchar *initial_value, gchar *meaning);
gchar *string_dialog_entry_with_widget (DenemoProject * gui, gchar * title, gchar * instruction, gchar * initial_value, GtkWidget * extra_widget);
gchar *string_dialog_editor_with_widget (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget, gpointer keypress_callback);
gchar *string_dialog_entry_with_widget_opt (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget, gboolean modal);
gchar *string_dialog_editor_with_widget_opt (DenemoProject * gui, gchar * wlabel, gchar * direction, gchar * PreValue, GtkWidget * widget, gboolean modal, gpointer keypress_callback);
gchar *get_multiline_input (gchar *title, gchar *instruction, gchar *initial);
void note2lilynotename (struct note *noteobject, GString * ret);
void note2lilyaccidental (struct note *noteobject, GString * ret);
void note2lilyoctave (struct note *noteobject, GString * ret);
void chord2lilybaseduration (struct chord *chordobject, GString * ret);
void chord2lilyduration (struct chord *chordobject, GString * ret);
void chord2lilynumdots (struct chord *chordobject, GString * ret);
gchar *get_fretdiagram_as_markup (void);
gchar *get_chord_notes (void);

#define UTILS_H_PARAM_ASSIGN(param_name) if( (str = g_strstr_len(values->str+i,strlen(values->str+i), #param_name)))\
nothing=FALSE,param_name = (*(str+strlen(#param_name))=='=')?str+strlen(#param_name)+1:NULL;

#define GET_1PARAM(action, param, param_name) \
  G_GNUC_UNUSED gchar * query = NULL;\
  gchar * param_name = NULL;\
  DenemoScriptParam dummy;\
  dummy.string=NULL;\
  if(param==NULL)\
    param = &dummy;\
  param->status = FALSE;\
if(!action && param){\
    G_GNUC_UNUSED gboolean nothing=TRUE;\
    GString *values = ((DenemoScriptParam *)param)->string;\
    if(values) {\
      gchar *str;\
      guint i;\
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
     guint i;\
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
     guint i;\
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
G_GNUC_UNUSED gchar * query = NULL;\
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
     guint i;\
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

gchar *get_option (gchar * title, gchar * str, gint length);
void console_output (gchar * text);

gint get_override (GList * g);

void add_font_directory (gchar * fontpath);
void add_font_file (gchar * fontpath);
const gchar *get_prefix_dir (void);
gboolean run_file_association (gchar * filenam);
gchar *remove_extension (gchar * name);
gchar *substitute_extension (gchar * name, gchar * extension);
void init_denemo_notenames (void);
gint get_widget_height (GtkWidget * w);
gint get_widget_width (GtkWidget * w);
void switch_back_to_main_window (void);
void use_markup (GtkWidget * widget);

typedef enum
{
  TwoKey = 0,
  SingleKey = 1,
  MouseGesture = 2,
  KeyPlusMouse = 3
} DenemoShortcutType;
void initialize_keystroke_help (void);
void init_gdk_cursors (void);
void KeyStrokeAwait (gchar * first_keypress);
void KeyStrokeDecline (gchar * first_keypress);
void KeyStrokeShow (gchar * str, gint command_idx, DenemoShortcutType type);
void MouseGestureShow (gchar * str, gchar * help, DenemoShortcutType type);
void KeyPlusMouseGestureShow (gchar * str, gint command_idx);

note *findnote (DenemoObject * curObj, gint cursory);
note *findnote_strict (DenemoObject * curObj, gint cursory);
#include "core/kbd-custom.h"
#define get_label_for_command(name) lookup_label_from_idx(Denemo.map, lookup_command_from_name(Denemo.map, name))
#define get_tooltip_for_command(name) lookup_tooltip_from_idx(Denemo.map, lookup_command_from_name(Denemo.map, name))
#define get_menu_path_for_command(name) lookup_menu_path_from_idx(Denemo.map, lookup_command_from_name(Denemo.map, name))

#ifdef FAKE_TOOLTIPS
gboolean show_tooltip (GtkWidget * w, GdkEvent * ev, gchar * text);
void free_tooltip (GtkWidget * w, gchar * text);
#endif

void write_input_status ();
enum clefs cleftypefromname (gchar * str);
gchar *find_dir_for_file (gchar * filename, GList * dirs);
gchar *find_dir_for_files (GList * files, GList * dirs);
gchar *find_path_for_file (gchar * filename, GList * dirs);
gchar *find_denemo_file (DenemoDirectory dir, gchar * filename);
gchar *escape_scheme (gchar * input);
gchar *time_spent_editing (void);
void reset_editing_timer (void);
gboolean shift_held_down (void);

#if GTK_MAJOR_VERSION == 2
#define GdkRGBA GdkColor
#define gtk_widget_override_color gtk_widget_modify_fg
#define gtk_widget_override_background_color gtk_widget_modify_bg
#define GTK_STATE_FLAG_NORMAL (0)
void get_color (GdkColor * color, gdouble r, gdouble g, gdouble b, gdouble a);
#else
void get_color (GdkRGBA * color, gdouble r, gdouble g, gdouble b, gdouble a);
#define gtk_widget_override_background_color(w,f,c) {gchar *color = gdk_rgba_to_string(c);set_background_color(w,color);g_free(color);}
#endif
void set_foreground_color(GtkWidget *w, gchar *color);
void set_background_color(GtkWidget *w, gchar *color);

gchar *format_tooltip (const gchar *tip);

#endif /* UTILS_H */
