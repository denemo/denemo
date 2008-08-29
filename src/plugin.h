/* plugin.h
 * 
 * Denemo plugin handling
 *
 *  (c) 2001-2005 Adam Tee
 */

#ifndef _PLUGIN_H
#define _PLUGIN_H
#include <denemo/denemo.h>


void
init_plugins(DenemoGUI *gui);
void 
load_plugin(GtkAction *action, gpointer param);
void list_loaded_plugins(GtkAction *action, gpointer param);
void list_available_plugins(GtkAction *action, gpointer param);
void unloadplugins(GtkAction *action, gpointer param);
void denemo_plugin_init (const gchar * name, DenemoGUI * gui);
gint denemo_plugin_cleanup(const gchar *name, DenemoGUI * gui);
char *stripname (char *d_name);
#endif /* _PLUGIN_H */
