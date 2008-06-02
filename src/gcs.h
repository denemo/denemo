/* gcs.h
 * Header file for gc creators
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001 Matthew Hiller
 */

#include <gdk/gdk.h>

void gcs_init (GdkWindow * window);

GdkGC *gcs_blackgc ();

GdkGC *gcs_graygc ();

GdkGC *gcs_greengc ();

GdkGC *gcs_redgc ();

GdkGC *gcs_bluegc ();

GdkGC *gcs_purplegc ();

GdkGC *gcs_yellowgc ();

GdkGC *gcs_slategraygc ();

