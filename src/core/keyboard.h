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

void set_visibility_for_action (DenemoAction * action, gboolean visible);
gint parse_paths (gchar * filename, DenemoProject * gui);
void add_ui (gchar * menupath, gchar * after, gchar * name);
void show_action_of_name (gchar * name);
void hide_action_of_name (gchar * name);
gchar * translate_binding_dnm_to_gtk (const gchar * dnm_binding);
gchar* translate_binding_gtk_to_dnm (const gchar * gtk_binding);
gchar *extract_menupath (gchar * filename);
void create_command(command_row *command);
#endif //KEYBOARDH
