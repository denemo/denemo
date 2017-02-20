/* kbd-interface.h
   Keyboard customization dialog - header file

   For Denemo, the GNU graphical music notation package
   (c) 2000-2005 Olivier Vermersch, Matthew Hiller */

#ifndef KBDINTERFACE_H
#define KBDINTERFACE_H

void configure_keyboard_dialog (DenemoAction * action, DenemoScriptParam * param);

void command_center_select_idx (DenemoAction * action, gint command_idx);

GtkWidget *get_command_view(void);
#endif
