/* Header file for functions for creating new views of 
 * the main window
 * 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2003-2005 Adam Tee
 */

#ifndef VIEW_H
#define VIEW_H
#include "commandfuncs.h"
#include "config.h"
#include "draw.h"               /* Which includes gtk.h */
#include <denemo/denemo.h>
#include "dialogs.h"
#include "exportlilypond.h"
#include "file.h"
#include "kbd-custom.h"
#include "kbd-interface.h"
#include "keyresponses.h"
#include "contexts.h"
#include "help.h"
#include "midi.h"
#include "mousing.h"
#include "moveviewport.h"
#include "prefops.h"
#include "scoreops.h"
#include "selectops.h"
#include "staffops.h"
#include "utils.h"
#include "dynamic.h"
#include "lyric.h"
#include "figure.h"
#include "fakechord.h"
#include "changenotehead.h"
#include "articulations.h"
#include "print.h"
#include "barline.h"
#include "runsilent.h"

#define EXTRA_ACCELS "extra.accels"


void newview (GtkAction * action, gpointer param);
void free_movements (DenemoGUI * gui);  /* frees all movement data (i.e. the DenemoScore objects) from gui, leaving gui interface intact */
void addhistorymenuitem (gchar * filename);

//TODO Fix for automatic updating during operation
//

/* tool_buttons cannot be visually separated from the toolbar, which is ok for icons but not so good for pure text; to separate them we put the label of the tool_button into an event box and decorate that. This macro gets the label out.
 Unfortunately, this lead to problems on some builds, so it has been dropped and the macro re-defined.*/
#define LABEL(a) (gtk_tool_button_get_label_widget((a)))

void highlight_rhythm (RhythmPattern * r);


void unhighlight_rhythm (RhythmPattern * r);
void highlight_duration (DenemoGUI * gui, gint dur);
void highlight_rest (DenemoGUI * gui, gint dur);

gboolean loadGraphicItem (gchar * name, DenemoGraphic ** pgraphic);


GtkAction *activate_action (gchar * path);

GError *execute_script_file (gchar * filename);

gboolean activate_script (GtkAction * action, DenemoScriptParam * param);
void create_scheme_function_for_script (gchar * name);
void inner_main (void *closure, int argc, char **argv);

gchar *create_xbm_data_from_pixbuf (GdkPixbuf * pixbuf, int lox, int loy, int hix, int hiy);
gchar *get_icon_for_name (gchar * name, gchar * label);

void upload_edit_script (gchar * tag, gchar * script);
void execute_init_scripts (gchar * menupath);

void denemo_scheme_init (void);

void execute_scheme (GtkAction * action, DenemoScriptParam * param);

gchar modifier_code (gpointer fn);
gboolean code_is_a_duration (gchar code);

gint call_out_to_guile (const char *script);

void set_playbutton (gboolean pause);

void set_master_volume (DenemoScore * si, gdouble volume);

void set_master_tempo (DenemoScore * si, gdouble tempo);

void toggle_to_drawing_area (gboolean show);

void ToggleReduceToDrawingArea (GtkAction * action, DenemoScriptParam * param);

gchar *get_midi_control_command (guchar type, guchar value);
gchar *get_midi_pitch_bend_command (gint value);
gint hide_printarea_on_delete (void);
void set_midi_in_status (void);
void set_meantone_tuning (gint step);
void update_leadin_widget (gdouble secs);
void
append_scheme_call (gchar * str);
#endif
