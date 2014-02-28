/*
 * portaudioutil.h
 * PortAudio utility functions.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef PORTAUDIOUTIL_H
#define PORTAUDIOUTIL_H

#include <glib.h>
#include <portaudio.h>

/**
 * Returns a list of available PortAudio device names.
 */
GList *get_portaudio_devices ();

/**
 * Frees a list returned by get_portaudio_devices()
 */
void free_portaudio_devices (GList * list);

/**
 * Returns the device index for the given device name.
 */
PaDeviceIndex get_portaudio_device_index (char const *name);


#endif // PORTAUDIOUTIL_H
