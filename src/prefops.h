/* prefops.h
 * header file for initializing preferences
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */
#ifndef PREFOPSH
#define PREFOPSH

#include <denemo/denemo.h>

#define INITIAL_WIDTH 1000
#define INITIAL_HEIGHT 500


void initprefs (void);
gint readxmlprefsString (gchar * xmlprefs);
gint writeXMLPrefs (DenemoPrefs * prefs);
void writeHistory (void);
gint readHistory (void);
void storeWindowState (void);
void loadWindowState (void);

gboolean get_bool_pref (gchar * prefname);
gint get_int_pref (gchar * prefname);
gchar *get_string_pref (gchar * prefname);

#endif
