#ifndef __SVG_VIEW_H__
#define __SVG_VIEW_H__
void
install_svgview (GtkWidget * top_vbox);

void display_svg (gdouble scale, gboolean part);
gboolean attach_timings (void);
DenemoObject *get_object_for_time (gdouble time, gboolean start);
#endif
