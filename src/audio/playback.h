#ifndef PLAYBACKH
#define PLAYBACKH
#include <gtk/gtk.h>
#include "denemo/denemo_types.h"

gchar *get_midi_audio_pointer (gchar * audio_device);

void set_tempo (void);
void ext_midi_playback (DenemoAction * action, DenemoScriptParam * param);
void stop_midi_playback (DenemoAction * action, DenemoScriptParam* param);
void playback_panic (void);
void PlaybackRangeDialog ();
void MasterVolumeDialog ();
void restart_play();
#endif //PLAYBACKH
