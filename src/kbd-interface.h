/* kbd-interface.h
   Keyboard customization dialog - header file
   
   For Denemo, the GNU graphical music notation package
   (c) 2000-2005 Olivier Vermersch, Matthew Hiller */

#ifndef KBDINTERFACE_H
#define KBDINTERFACE_H

void configure_keyboard_dialog (GtkAction * action, gpointer param);

void configure_keyboard_dialog_init_idx (GtkAction * action, DenemoGUI * gui, gint command_idx);


#endif
