#ifndef __SCHEME_CORE_H__
#define __SCHEME_CORE_H__

#include <denemo/denemo.h>
#include <libguile.h>

gchar *process_command_line (int argc, char **argv);
void create_scheme_identfiers ();
void install_scm_function (gchar * name, gpointer callback);
void init_keymap (void);

SCM scheme_highlight_cursor (SCM optional);
void define_scheme_constants (void);
void load_scheme_init (void);
gint eval_file_with_catch (gchar * filename);
void append_to_local_scheme_init (gchar * scheme);
void update_scheme_snippet_ids (void);

#define INIT_SCM "init.scm"
#define DENEMO_SCHEME_PREFIX "d-"

#endif