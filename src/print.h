#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h>

void printall_cb (GtkAction * action, gpointer param);
void printmovement_cb (GtkAction * action, gpointer param);
void printpreview_cb (GtkAction * action, gpointer param);
void printselection_cb (GtkAction * action, gpointer param);
void printexcerptpreview_cb (GtkAction * action, gpointer param);
void printpart_cb (GtkAction * action, gpointer param);
void install_printpreview (GtkWidget * vbox);
void refresh_print_view (gboolean interactive);
gchar *get_lily_version_string (void);
void print_lily_cb (GtkWidget * item, DenemoGUI * gui);
void export_pdf (gchar * filename, DenemoGUI * gui);
void export_png (gchar * filename, GChildWatchFunc finish, DenemoGUI * gui);
void printpng_finished (GPid pid, gint status, GList * filelist);
gboolean create_thumbnail (gboolean async);
gchar *large_thumbnail_name (gchar * filepath);
gboolean stop_lilypond ();
void show_print_view (GtkAction * action, gpointer param);

gboolean get_offset (gdouble * x, gdouble * y);
gboolean get_positions (gdouble * neary, gdouble * fary, gboolean for_slur);
gboolean get_new_target (void);
gboolean get_reference_point (void);
gboolean get_control_point (gint which);
gboolean get_curve (gdouble * x1, gdouble * y1, gdouble * x2, gdouble * y2, gdouble * x3, gdouble * y3, gdouble * x4, gdouble * y4);

void typeset_part (void);
gboolean continuous_typesetting (void);
gboolean get_new_point (void);
int check_lily_version (gchar * version);
gboolean typeset_for_script (gchar * script);
gboolean print_typeset_pdf (void);
void typeset_current_layout (void);
#endif /*PRINT_H */
