/* draw.h
 * prototypes for actual drawing functions
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */
#ifndef DRAW_H
#define DRAW_H
#include <gtk/gtk.h>
#include <denemo/denemo.h>

void region_playhead ();

void initialize_playhead ();

gint scorearea_configure_event (GtkWidget * widget, GdkEventConfigure * event);
extern gint LEFT_MARGIN;
#if GTK_MAJOR_VERSION == 3
gint scorearea_draw_event (GtkWidget * widget, cairo_t * cr);
#else
gint scorearea_draw_event (GtkWidget * widget, GdkEventExpose * event);
#endif
void draw_score (cairo_t * cr);
void fix_start_end_ordering();
void draw_score_area();
#endif
