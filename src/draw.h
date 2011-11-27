/* draw.h
 * prototypes for actual drawing functions
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include <gtk/gtk.h>
#include <denemo/denemo.h> 

gint
scorearea_configure_event (GtkWidget * widget, GdkEventConfigure * event);
#if GTK_MAJOR_VERSION==3
gint
scorearea_draw_event (GtkWidget *widget, cairo_t *cr);
#else
gint
scorearea_expose_event (GtkWidget * widget, GdkEventExpose * event);
#endif
