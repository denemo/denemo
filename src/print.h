#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h> 

void
printall_cb(GtkAction *action, gpointer param);
void
printmovement_cb(GtkAction *action, gpointer param);
void
printpreview_cb(GtkAction *action, gpointer param);
void
printselection_cb(GtkAction *action, gpointer param);
void
printexcerptpreview_cb(GtkAction *action, gpointer param);
void
printpart_cb(GtkAction *action, gpointer param);
void 
install_printpreview(DenemoGUI *gui, GtkWidget *vbox);
void
refresh_print_view(gboolean interactive);
gchar *
get_lily_version_string (void);
void 
print_lily_cb (GtkWidget *item, DenemoGUI *gui);
void
export_pdf (gchar *filename, DenemoGUI * gui);
void
export_png (gchar *filename, GChildWatchFunc finish, DenemoGUI * gui);
void
printpng_finished(GPid pid, gint status, GList *filelist);
gboolean
create_thumbnail(gboolean async);
gchar *
large_thumbnail_name(gchar *filepath);
gboolean
stop_lilypond();
void show_print_view(GtkAction *action, gpointer param);
#endif /*PRINT_H*/	
