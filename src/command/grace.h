/* grace.h
 *
 * Grace note function prototypes
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 2000, 2001 Adam Tee, 2010 Richard Shann
 */

#ifndef GRACEOPS_H
#define GRACEOPS_H

#include <glib.h>
#include <denemo/denemo.h>


DenemoObject *newgracestart ();
DenemoObject *newgraceend ();
void toggle_grace (GtkAction * action, DenemoScriptParam * param);
void toggle_acciaccatura (GtkAction * action, DenemoScriptParam * param);

#endif //GRACEOPS_H
