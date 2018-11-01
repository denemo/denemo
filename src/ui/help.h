/* help.h
 * header file for help.c
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 * 2013 Richard Shann
 */
#ifndef HELP_H
#define HELP_H
void about (DenemoAction * action, DenemoScriptParam* param);
void browse_manual (DenemoAction * action, DenemoScriptParam* param);
void email_help (gchar *page);
void display_shortcuts (void);
#endif
