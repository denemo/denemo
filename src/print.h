#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h> 

void
printall_cb(GtkAction *action, DenemoGUI *gui);
void
printpreview_cb(GtkAction *action, DenemoGUI *gui);
void
printexcerptpreview_cb(GtkAction *action, DenemoGUI *gui);
void
printpart_cb(GtkAction *action, DenemoGUI *gui);
gchar *
get_printfile_pathbasename(void);
void
run_lilypond_and_viewer(gchar *basename, DenemoGUI *gui);
void export_pdf_action (GtkAction *action, DenemoGUI *gui);
void export_pdf (const gchar* filename, DenemoGUI *gui);

#endif /*PRINT_H*/	
/* exportpdf.h
 * Header file for exporting PDF files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001 Eric Galluzzo
 */

#include <gtk/gtk.h>

