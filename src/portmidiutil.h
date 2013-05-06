/*
 * portmidiutil.h
 * PortMidi utility functions.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef PORTMIDIUTIL_H
#define PORTMIDIUTIL_H

#include <glib.h>
#include <portmidi.h>


/**
 * A wrapper around Pm_Initialize() that may be called more than once.
 */
PmError Pm_InitializeWrapper ();

/**
 * A wrapper around Pm_Terminate() that may be called more than once.
 */
PmError Pm_TerminateWrapper ();


/**
 * Returns a list of available PortMidi device names.
 *
 * @param output  if TRUE returns output ports, otherwise input ports.
 */
GList *get_portmidi_devices (gboolean output);

/**
 * Frees a list returned by get_portmidi_devices()
 */
void free_portmidi_devices (GList * list);

/**
 * Returns the device id for the given device name.
 */
PmDeviceID get_portmidi_device_id (char const *name, gboolean output);


#endif // PORTMIDIUTIL_H
