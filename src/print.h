#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h> 

void
printall_cb(GtkAction *action, gpointer param);
void
printpreview_cb(GtkAction *action, gpointer param);
void
printselection_cb(GtkAction *action, gpointer param);
void
printexcerptpreview_cb(GtkAction *action, gpointer param);
void
printpart_cb(GtkAction *action, gpointer param);
gchar *
get_printfile_pathbasename(void);
void viewer(DenemoGUI *gui);
void export_pdf_action (GtkAction *action, gpointer param);
void export_png_action (GtkAction *action, gpointer param);
void install_printpreview(DenemoGUI *gui, GtkWidget *vbox);
void
refresh_print_view(gboolean interactive);
gchar *
get_lily_version_string (void);
void
run_lilypond(gchar *printfile, DenemoGUI *gui);
#endif /*PRINT_H*/	
