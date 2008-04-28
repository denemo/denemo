#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h> 

void
printall_cb(GtkAction *action, DenemoGUI *gui);
void
printpreview_cb(GtkAction *action, DenemoGUI *gui);
void
printpart_cb(GtkAction *action, DenemoGUI *gui);
gchar *
get_printfile_pathbasename(void);
void
run_lilypond_and_viewer(gchar *basename);

#endif /*PRINT_H*/	
