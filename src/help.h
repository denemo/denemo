/* help.h
 * header files for help.c
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 */

void about (GtkAction *action, gpointer callback_data);

void keybindings (gpointer callback_data,
		  guint callback_action, GtkWidget * widget);
void
browse_manual (GtkAction * action, gpointer data);
