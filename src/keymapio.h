#ifndef KEYMAPIO_H
#define KEYMAPIO_H

#include <denemo/denemo.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "utils.h"
#include "prefops.h"

gint load_xml_keymap (gchar * filename, gboolean interactive);
gint save_xml_keymap (gchar * filename);
gint save_xml_keybindings (gchar * filename);
gint load_xml_keybindings (gchar * filename);
gint parse_paths (gchar * filename, DenemoGUI * gui);
gint save_script_as_xml (gchar * filename, gchar * myname, gchar * myscheme, gchar * mylabel, gchar * mytooltip, gchar * after);

#endif