/* file.h
 * prototypes for file I/O routines
 * 
 * for Denemo, a gtk+ frontend to GNU Lilypond 
 * (c) 2000, 2001, Adam Tee
 * (c) 2000, 2001, University of Leeds
 */



gboolean confirmbox (DenemoGUI *gui);

void 
file_savepartswrapper(GtkAction *action, gpointer param);

void
file_open_with_check (GtkAction *action, DenemoScriptParam * param);

void
file_add_staffs (GtkAction *action, DenemoScriptParam * param);

void
file_add_movements (GtkAction *action, DenemoScriptParam * param);

gint
open_user_default_template(ImportType type);

void
system_template_open_with_check (GtkAction *action, DenemoScriptParam * param);
void
system_example_open_with_check (GtkAction *action, DenemoScriptParam * param);

void
local_template_open_with_check (GtkAction *action, DenemoScriptParam * param);

void
file_savewrapper (GtkAction *action, gpointer param);

void
file_saveaswrapper (GtkAction *action, gpointer param);

void
file_newwrapper (GtkAction *action, gpointer param);

gint
open_for_real (gchar *filename, DenemoGUI *gui, gboolean as_template, ImportType type);



void
file_save (GtkWidget * widget, DenemoGUI *gui);

void
file_saveas (DenemoGUI *gui, gboolean as_template);

void
template_save (GtkAction * action, gpointer param);

void
reload_lily_file (GtkWidget * widget, gpointer data);

gint
lyinput (gchar *filename, DenemoGUI *gui);

void
deletescore (GtkWidget * widget, DenemoGUI *gui);

void
dnm_deletescore (GtkWidget * widget, DenemoGUI *gui);



void
updatescoreinfo (DenemoGUI *gui);



/**
 * @return TRUE if the file does not exists or the user want it to be overwritten
 */
gboolean replace_existing_file_dialog(const gchar* filename, GtkWindow* parent_window, gint format_id);
