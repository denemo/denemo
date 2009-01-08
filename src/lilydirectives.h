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
lily_directive_attach_note (GtkAction *action, gpointer param);
void
lily_directive_attach_chord (GtkAction *action, gpointer param);
void
lily_directive_insert (GtkAction *action, DenemoScriptParam *param);
#endif
