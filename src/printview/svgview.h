#ifndef __SVG_VIEW_H__
#define __SVG_VIEW_H__
void
install_svgview (GtkWidget * top_vbox);

void display_svg (gdouble scale, gboolean part);
void attach_timings (void);
#endif
