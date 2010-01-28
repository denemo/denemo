#ifndef PLAYBACKH
#define PLAYBACKH
#include <gtk/gtk.h>

gchar *
get_midi_audio_pointer(gchar *audio_device);


void
ext_midi_playback (GtkAction *action, DenemoScriptParam *param);
void
stop_midi_playback (GtkAction *action, gpointer param);

#endif //PLAYBACKH
