/* figure.h
 *  
 * (c)  2003 Richard Shann <richard.shann@virgin.net>
 */

#ifndef FIGURE_H

#define FIGURE_H

#include <denemo/denemo.h>

void 
insertfigure(GtkWidget *widget, gpointer data);
void 
figure_insert(GtkAction *action);
DenemoObject *
newfigure (gint baseduration, gint numdots, gchar *figs);

#endif
