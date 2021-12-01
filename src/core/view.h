/* Header file for functions for creating new views of
 * the main window
 *
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005 Adam Tee
 */

#ifndef VIEW_H
#define VIEW_H
#include "command/commandfuncs.h"
#include "config.h"
#include "display/draw.h"       /* Which includes gtk.h */
#include <denemo/denemo.h>
#include "ui/dialogs.h"
#include "export/exportlilypond.h"
#include "export/file.h"
#include "core/kbd-custom.h"
#include "ui/kbd-interface.h"
#include "command/keyresponses.h"
#include "command/clef.h"
#include "command/keysig.h"
#include "command/timesig.h"
#include "command/tuplet.h"
#include "command/contexts.h"
#include "ui/help.h"
#include "audio/midi.h"
#include "ui/mousing.h"
#include "ui/moveviewport.h"
#include "core/prefops.h"
#include "command/score.h"
#include "command/select.h"
#include "command/staff.h"
#include "core/utils.h"
#include "command/lyric.h"
#include "command/figure.h"
#include "command/fakechord.h"
#include "command/changenotehead.h"
#include "export/print.h"
#include "ui/palettes.h"
#include "core/palettestorage.h"

#include <libguile.h>

#define EXTRA_ACCELS "extra.accels"
#define INIT_SCM "init.scm"

void newview (DenemoAction * action, DenemoScriptParam * param);
void free_movements (DenemoProject * gui);      /* frees all movement data (i.e. the DenemoMovement objects) from gui, leaving gui interface intact */
void addhistorymenuitem (gchar * filename);

//TODO Fix for automatic updating during operation
//

/* tool_buttons cannot be visually separated from the toolbar, which is ok for icons but not so good for pure text; to separate them we put the label of the tool_button into an event box and decorate that. This macro gets the label out.
 Unfortunately, this lead to problems on some builds, so it has been dropped and the macro re-defined.*/
void set_rhythm_label (RhythmPattern * r, gchar * text);
void delete_rhythm_pattern (RhythmPattern * r);
void highlight_rhythm (RhythmPattern * r);

void unhighlight_rhythm (RhythmPattern * r);
void highlight_duration (DenemoProject * gui, gint dur);
void highlight_rest (DenemoProject * gui, gint dur);

void highlight_audio_record (void);
void highlight_midi_record (void);
void toggle_recording_audio (void);
gboolean loadGraphicItem (gchar * name, DenemoGraphic ** pgraphic);


DenemoAction *activate_action (gchar * path);

GError *execute_script_file (gchar * filename);

gboolean activate_script (DenemoAction * action, DenemoScriptParam * param);
void create_scheme_function_for_script (gchar * name);
void *inner_main (void *data);

gchar *create_xbm_data_from_pixbuf (GdkPixbuf * pixbuf, int lox, int loy, int hix, int hiy);
gchar *get_icon_for_name (gchar * name, gchar * label);

void upload_edit_script (gchar * tag, gchar * script);
void execute_init_scripts (gchar * menupath);

void denemo_scheme_init (void);

void execute_scheme (DenemoAction * action, DenemoScriptParam * param);

gchar modifier_code (gpointer fn);
gboolean code_is_a_duration (gchar code);

gint call_out_to_guile (const char *script);

void set_playbutton (void);

void set_master_volume (DenemoMovement * si, gdouble volume);

void set_movement_tempo (gint tempo);
gint movement_tempo_from_user (void);
void toggle_to_drawing_area (gboolean show);

void ToggleReduceToDrawingArea (DenemoAction * action, DenemoScriptParam * param);

gchar *get_midi_control_command (guchar type, guchar value);
gchar *get_midi_pitch_bend_command (gint value);
gint hide_printarea_on_delete (void);
void set_midi_in_status (void);
void set_meantone_tuning (gint step);
void update_leadin_widget (gdouble secs);
void update_tempo_widget (gchar *newvalue);
void append_scheme_call (gchar * str);
SCM scheme_set_enharmonic_position (SCM position);
void define_scheme_variable (gchar * varname, gchar * value, gchar * tooltip);
void define_scheme_int_variable (gchar * varname, gint value, gchar * tooltip);
void define_scheme_double_variable (gchar * varname, gdouble value, gchar * tooltip);
void define_scheme_bool_variable (gchar * varname, gint value, gchar * tooltip);
SCM scheme_next_note (SCM optional);
void define_scheme_literal_variable (const gchar * varname, const gchar * value, gchar * tooltip);
gboolean show_midi_record_control (void);
gint eval_file_with_catch (gchar * filename);

GtkWidget *get_playalong_button ();
GtkWidget *get_conduct_button ();
GtkWidget *get_record_button ();

void destroy_local_scheme_init (void);
void delete_recording (void);

void pb_playalong (GtkWidget * button);
gboolean pb_record (GtkWidget *button, gchar * callback);
void pb_conduct (GtkWidget * button);
void pb_midi_delete (void);
void install_button_for_pattern (RhythmPattern * r, gchar * thelabel);
gboolean append_rhythm (RhythmPattern * r, gpointer fn);
gint insert_pattern_in_toolbar (RhythmPattern * r, gboolean highlight);
void select_rhythm_pattern (RhythmPattern * r);

void toggle_toolbar (DenemoAction * action, gpointer param);
void toggle_playback_controls (DenemoAction * action, gpointer param);
void toggle_midi_in_controls (DenemoAction * action, gpointer param);
void openinnew (DenemoAction * action, DenemoScriptParam * param);
gboolean close_gui_with_check (DenemoAction * action, DenemoScriptParam * param);
void closewrapper (DenemoAction * action, DenemoScriptParam * param);
void morecommands (DenemoAction * action, DenemoScriptParam * param);
void mycommands (DenemoAction * action, DenemoScriptParam * param);
void fetchcommands (DenemoAction * action, DenemoScriptParam * param);



void create_rhythm_cb (DenemoAction * action, DenemoScriptParam * param);
void delete_rhythm_cb (DenemoAction * action, DenemoScriptParam * param);
void midi_in_adjust (gint value);
gboolean insert_nth_rhythm (gint n);
void create_rhythm (RhythmPattern * r, gboolean from_selection);
GError *execute_script_file (gchar * filename);
void popup_help_for_action (DenemoAction * action);
void save_accels (void);

#endif
