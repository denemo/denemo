#ifndef _ALSAPLAYBACK_H
#define _ALSAPLAYBACK_H
#include "../config.h"
#include <denemo/denemo.h>
#ifdef HAVEALSA

#include <alsa/asoundlib.h>
void alsaplayback(GtkAction *action);
int open_seq();
int close_seq();
#endif

#endif /* _ALSAPLAYBACK_H */
