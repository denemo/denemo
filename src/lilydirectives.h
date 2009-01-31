/* lilydirectives.h
 *
 *
 * (c) 2000-2005 Adam Tee <eenajt@electeng.leeds.ac.uk>
 *
 */

#ifndef LILYDIRECTIVE_H

#define LILYDIRECTIVE_H

#include <denemo/denemo.h>


void
note_directive (GtkAction *action, gpointer param);
void
chord_directive (GtkAction *action, gpointer param);
void
standalone_directive (GtkAction *action, DenemoScriptParam *param);
#endif
