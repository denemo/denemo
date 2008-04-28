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


gint load_xml_keymap(gchar *filename, keymap *the_keymap);
gint save_xml_keymap(gchar *filename, keymap *the_keymap);

#endif //KEYBOARDH
