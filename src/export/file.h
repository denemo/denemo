/* file.h
 * prototypes for file I/O routines
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001, Adam Tee
 * (c) 2000, 2001, University of Leeds
 */
#ifndef FILE_H
#define FILE_H

gboolean confirmbox (DenemoProject * gui);

void file_savepartswrapper (DenemoAction * action, DenemoScriptParam * param);

void file_open_with_check (DenemoAction * action, DenemoScriptParam * param);
void file_import_lilypond_with_check (DenemoAction * action, DenemoScriptParam * param);
void file_import_midi_with_check (DenemoAction * action, DenemoScriptParam * param);
void file_import_musicxml_with_check (DenemoAction * action, DenemoScriptParam * param);

void file_add_staffs (DenemoAction * action, DenemoScriptParam * param);

void file_add_movements (DenemoAction * action, DenemoScriptParam * param);


void system_template_open_with_check (DenemoAction * action, DenemoScriptParam * param);

void system_example_open_with_check (DenemoAction * action, DenemoScriptParam * param);
void local_template_open_with_check (DenemoAction * action, DenemoScriptParam * param);

void file_savewrapper (DenemoAction * action, DenemoScriptParam * param);

void file_saveaswrapper (DenemoAction * action, DenemoScriptParam * param);

void file_newwrapper (DenemoAction * action, DenemoScriptParam * param);

void new_score_cb (DenemoAction * action, DenemoScriptParam * param);

gint file_save (GtkWidget * widget, DenemoProject * gui);

void file_saveas (DenemoSaveType as_template);

void template_save (DenemoAction * action, DenemoScriptParam * param);
void file_copy_save (DenemoAction * action, DenemoScriptParam * param);
void export_mudela_action (DenemoAction * action, DenemoScriptParam * param);
void export_pdf_action (DenemoAction * action, DenemoScriptParam * param);
void export_png_action (DenemoAction * action, DenemoScriptParam * param);
void export_midi_action (DenemoAction * action, DenemoScriptParam * param);
void paste_clipboard (DenemoAction * action, DenemoScriptParam * param);
void paste_comment (DenemoAction * action, DenemoScriptParam * param);
void openrecent (GtkWidget * widget, gchar * filename);
gint open_for_real (gchar * filename, DenemoProject * gui, DenemoSaveType template, ImportType type);

gchar *file_dialog (gchar * message, gboolean read, gchar * location, gchar *ext, GList *exts);
void set_project_filename (DenemoProject * gui, gchar * filename);

gint lyinput (gchar * filename);
gint open_source_file (void);
gint open_proof_file (void);
#endif /*FILE_H */
