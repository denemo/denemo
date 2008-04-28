/* mousing.h
   header for callbacks that handle mouse clicks, drags, etc.

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005  Matthew Hiller
*/

gint
scorearea_button_release (GtkWidget *widget, GdkEventButton *event, gpointer data);
gint
scorearea_button_press (GtkWidget *widget, GdkEventButton *event, gpointer data);
