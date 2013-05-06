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
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "utils.h"
#include "prefops.h"


gint load_xml_keymap (gchar * filename, gboolean interactive);
gint save_xml_keymap (gchar * filename);
gint save_xml_keybindings (gchar * filename);
gint load_xml_keybindings (gchar * filename);
void set_visibility_for_action (GtkAction * action, gboolean visible);
gint parse_paths (gchar * filename, DenemoGUI * gui);
gint save_script_as_xml (gchar * filename, gchar * myname, gchar * myscheme, gchar * mylabel, gchar * mytooltip, gchar * after);
void add_ui (gchar * menupath, gchar * after, gchar * name);
#endif //KEYBOARDH
