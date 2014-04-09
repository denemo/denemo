#ifndef KEYMAPIO_H
#define KEYMAPIO_H

#include <string.h>
#include <denemo/denemo.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "core/utils.h"
#include "core/prefops.h"
#include "core/kbd-custom.h"
#include "core/keyboard.h"

gint load_xml_keymap (gchar * filename);
gint save_xml_keymap (gchar * filename);
gint save_xml_keybindings (gchar * filename);
gint load_xml_keybindings (gchar * filename);
gint save_command_metadata (gchar * filename, gchar * myname, gchar * mylabel, gchar * mytooltip, gchar * after);
gint save_command_data (gchar * filename, gchar * myscheme);
gchar* load_command_data (gint idx);

#define XML_ENCODING "UTF-8"

#define COMMANDXML_TAG_ROOT          (const xmlChar *) "Denemo"
#define COMMANDXML_TAG_MERGE         (const xmlChar *) "merge"
#define COMMANDXML_TAG_TITLE         (const xmlChar *) "title"
#define COMMANDXML_TAG_AUTHOR        (const xmlChar *) "author"
#define COMMANDXML_TAG_MAP           (const xmlChar *) "map"
#define COMMANDXML_TAG_ROW           (const xmlChar *) "row"
#define COMMANDXML_TAG_AFTER         (const xmlChar *) "after"
#define COMMANDXML_TAG_CURSORS       (const xmlChar *) "cursors"
#define COMMANDXML_TAG_ACTION        (const xmlChar *) "action"
#define COMMANDXML_TAG_HIDDEN        (const xmlChar *) "hidden"
#define COMMANDXML_TAG_MENUPATH      (const xmlChar *) "menupath"
#define COMMANDXML_TAG_TYPE          (const xmlChar *) "type"
#define COMMANDXML_TAG_LABEL         (const xmlChar *) "_label"
#define COMMANDXML_TAG_TOOLTIP       (const xmlChar *) "_tooltip"

#define COMMAND_TYPE_SCHEME          (const xmlChar *) "scheme"
#define COMMAND_TYPE_BUILTIN         (const xmlChar *) "builtin"

#define MENUXML_TAG_MENU             (const xmlChar *) "menu"
#define MENUXML_TAG_MENUBAR          (const xmlChar *) "menubar"
#define MENUXML_TAG_TOOLBAR          (const xmlChar *) "toolbar"
#define MENUXML_TAG_MENUITEM         (const xmlChar *) "menuitem"
#define MENUXML_PROP_ACTION          (const xmlChar *) "action"
#define MENUXML_PROP_NAME            (const xmlChar *) "name"

#define BINDINGXML_TAG_ACTION        (const xmlChar *) "action"
#define BINDINGXML_TAG_BIND          (const xmlChar *) "bind"
#define BINDINGXML_TAG_STATE         (const xmlChar *) "state"
#define BINDINGXML_TAG_CURSOR        (const xmlChar *) "cursor"
#define BINDINGXML_TAG_CURSORBINDING (const xmlChar *) "cursor-binding"

#endif
