#ifndef PLAYBACKH
#define PLAYBACKH
#include <gtk/gtk.h>


void
playback_local (gpointer callback_data, guint callback_action,
		GtkWidget * widget);
void
ext_midi_playback (GtkAction *action, DenemoGUI *gui);
void
stop_midi_playback (GtkAction *action, DenemoGUI *gui);

#endif //PLAYBACKH
