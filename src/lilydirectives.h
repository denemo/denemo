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
lily_directive_postfix (GtkAction * action, DenemoGUI *gui);
void
lily_directive_insert (GtkAction * action, DenemoGUI *gui);

#ifdef DENEMO_DYNAMIC_MENU_ITEMS
void myactivate (GtkAction * action, DenemoGUI *gui);
#endif
#endif
