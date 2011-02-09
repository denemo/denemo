/* file.h
 * prototypes for file I/O routines
 * 
 * for Denemo, a gtk+ frontend to GNU Lilypond 
 * (c) 2000, 2001, Adam Tee
 * (c) 2000, 2001, University of Leeds
 */
#ifndef FILE_H
#define FILE_H


typedef enum
{ DENEMO_FORMAT = 0,
  DNM_FORMAT,
  MUDELA_FORMAT,
  PDF_FORMAT,
  PNG_FORMAT,
  ABC_FORMAT,
  MIDI_FORMAT,
  CSOUND_FORMAT
}
FileFormatNames;

gboolean confirmbox (DenemoGUI *gui);

void 
file_savepartswrapper(GtkAction *action, gpointer param);

void
file_open_with_check (GtkAction *action, DenemoScriptParam * param);
void
file_import_lilypond_with_check (GtkAction *action, DenemoScriptParam * param);
void
file_import_midi_with_check (GtkAction *action, DenemoScriptParam * param);
void
file_import_musicxml_with_check (GtkAction *action, DenemoScriptParam * param);

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
open_for_real (gchar *filename, DenemoGUI *gui, DenemoSaveType as_template, ImportType type);



void
file_save (GtkWidget * widget, DenemoGUI *gui);

void
file_saveas (DenemoGUI *gui,  DenemoSaveType as_template);

void
template_save (GtkAction * action, gpointer param);
void
file_copy_save (GtkAction * action, gpointer param); 
void export_mudela_action (GtkAction *action, DenemoScriptParam *param);
void export_pdf_action (GtkAction *action, DenemoScriptParam *param);
void export_png_action (GtkAction *action, DenemoScriptParam *param);
void export_midi_action (GtkAction *action, DenemoScriptParam *param);
void export_csound_action (GtkAction *action, DenemoScriptParam *param);
void export_pdf (gchar *filename, DenemoGUI * gui);
void export_png (gchar *filename, gboolean show_preview, DenemoGUI * gui);
void
paste_clipboard(GtkAction * action, gpointer param);

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

#endif /*FILE_H*/
