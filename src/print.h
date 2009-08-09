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
void
run_lilypond_and_viewer(gchar *basename, DenemoGUI *gui);
void export_pdf_action (GtkAction *action, gpointer param);


void install_printpreview(DenemoGUI *gui, GtkWidget *vbox);

#endif /*PRINT_H*/	
