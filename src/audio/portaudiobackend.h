/*
 * portaudiobackend.h
 * PortAudio backend.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef PORTAUDIOBACKEND_H
#define PORTAUDIOBACKEND_H

#include "audio/audiointerface.h"

extern backend_t portaudio_backend;
#ifdef _HAVE_RUBBERBAND_
void set_playback_speed (double speed);
#endif

#endif // PORTAUDIOBACKEND_H
