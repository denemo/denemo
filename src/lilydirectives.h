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
lily_directive_postfix (GtkAction *action, gpointer param);
void
lily_directive_insert (GtkAction *action, DenemoScriptParam *param);

#ifdef DENEMO_DYNAMIC_MENU_ITEMS
void myactivate (GtkAction *action, gpointer param);
#endif
#endif
