/* exportpdf.h
 * Header file for exporting PDF files
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001 Eric Galluzzo
 */

#include <gtk/gtk.h>

void export_pdf_action (GtkAction *action, DenemoGUI *gui);
void export_pdf (const gchar* filename, DenemoGUI *gui);
