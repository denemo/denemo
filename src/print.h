#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h> 

void
printall_cb(GtkAction *action, gpointer param);
void
printpreview_cb(GtkAction *action, gpointer param);
void
printexcerptpreview_cb(GtkAction *action, gpointer param);
void
printpart_cb(GtkAction *action, gpointer param);
gchar *
get_printfile_pathbasename(void);
void
run_lilypond_and_viewer(gchar *basename, DenemoGUI *gui);
void export_pdf_action (GtkAction *action, gpointer param);


#endif /*PRINT_H*/	
/* exportpdf.h
 * Header file for exporting PDF files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001 Eric Galluzzo
 */

#include <gtk/gtk.h>

