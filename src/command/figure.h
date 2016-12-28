/* figure.h
 *
 * (c)  2003 Richard Shann <richard.shann@virgin.net>
 */

#ifndef FIGURE_H

#define FIGURE_H

#include <denemo/denemo.h>


void figure_insert (DenemoAction * action, DenemoScriptParam * param);
DenemoObject *newfigure (gint baseduration, gint numdots, gchar * figs);
void delete_figured_bass (DenemoAction * action, DenemoScriptParam * param);
void hide_figured_bass (DenemoAction * action, DenemoScriptParam * param);
void show_figured_bass (DenemoAction * action, DenemoScriptParam * param);
#endif
