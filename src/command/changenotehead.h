/* changenotehead.h
 *
 * function prototypes for changing
 * the notehead.
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 2000-2005 Adam Tee
 */

#ifndef CHANGENOTEHEAD_H
#define CHANGENOTEHEAD_H

void insertnotehead (DenemoMovement * si, gchar * noteheadstring);
gint texttohead (gchar * text);
void set_notehead (GtkAction * action, DenemoScriptParam * param);

#endif
