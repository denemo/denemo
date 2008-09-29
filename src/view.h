/* view.h
 * Header file for functions for creating new views of 
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
#include "draw.h"		/* Which includes gtk.h */
#include <denemo/denemo.h>
#include "dialogs.h"
#include "exportlilypond.h"
#include "file.h"
#include "gcs.h"
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
//#include "csoundplayback.h"
#include "print.h"
#include "barline.h"
#include "plugin.h"
#include "runsilent.h"

#define EXTRA_ACCELS "extra.accels"


void newview(GtkAction*, gpointer param);

void free_gui(DenemoGUI *gui);/* frees all movement data from gui, leaving gui interface intact */
void addhistorymenuitem(gchar *filename);

//TODO Fix for automatic updating during operation
//

/* tool_buttons cannot be visually separated from the toolbar, which is ok for icons but not so good for pure text; to separate them we put the label of the tool_button into an event box and decorate that. This macro gets the label out. */
#define LABEL(a) (gtk_bin_get_child(GTK_BIN(gtk_tool_button_get_label_widget((a)))))

void	  highlight_rhythm(RhythmPattern *r);


void	  unhighlight_rhythm(RhythmPattern *r);
void	  highlight_duration(DenemoGUI *gui, gint dur);
void	  highlight_rest(DenemoGUI *gui, gint dur);
void      write_status(DenemoGUI *gui);


void
activate_script (GtkAction *action, gpointer param);
void 
inner_main(void*closure, int argc, char **argv);
#endif
