//
// C++ Interface: keyboard
//
// Description: Load xml keymap file
//
//
// Author: Adam Tee <adam@ajtee.plus.com>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef KEYBOARDH
#define KEYBOARDH

#include <denemo/denemo.h>

void set_visibility_for_action (GtkAction * action, gboolean visible);
void add_ui (gchar * menupath, gchar * after, gchar * name);
void create_command(gchar* name,
                    gchar* label,
                    gchar* scheme,
                    gchar* tooltip,
                    gboolean hidden,
                    gchar* after,
                    gchar* menupath,
                    gchar* fallback,
                    gint merge,
                    GList* menupaths);
void create_binding(gchar* tmp, keymap * the_keymap, gint command_number);
void show_action_of_name (gchar * name);
void show_action_of_name (gchar * name);
gchar* translate_binding_dnm_to_gtk (const gchar * dnm_binding);
gchar* translate_binding_gtk_to_dnm (const gchar * gtk_binding);
gchar* extract_menupath (gchar * filename);

#endif //KEYBOARDH
