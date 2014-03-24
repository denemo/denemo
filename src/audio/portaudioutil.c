#ifdef _HAVE_PORTAUDIO_
/*
 * portaudioutil.c
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

#include "audio/portaudioutil.h"

#include <portaudio.h>


GList *
get_portaudio_devices ()
{
  GList *list = NULL;

  PaError err = Pa_Initialize ();
  if (err != paNoError)
    {
      return NULL;
    }

  list = g_list_append (list, g_strdup ("default"));

  PaDeviceIndex num = Pa_GetDeviceCount ();

  if (num < 0)
    {
      goto ret;
    }

  PaDeviceIndex i;

  for (i = 0; i < num; ++i)
    {
      PaDeviceInfo const *info = Pa_GetDeviceInfo (i);
      char const *api_name = Pa_GetHostApiInfo (info->hostApi)->name;

      char *s = g_strdup_printf ("%s: %s", api_name, info->name);

      list = g_list_append (list, s);
    }

ret:
  Pa_Terminate ();

  return list;
}


void
free_portaudio_devices (GList * list)
{
  GList *p = list;
  while (p)
    {
      g_free (p->data);
      p = g_list_next (p);
    }

  g_list_free (list);
}


PaDeviceIndex
get_portaudio_device_index (char const *name)
{
  if (g_strcmp0 (name, "default") == 0)
    {
      return Pa_GetDefaultOutputDevice ();
    }

  GList *list = get_portaudio_devices ();

  if (!list)
    {
      return paNoDevice;
    }

  GList *item = g_list_find_custom (list, name, (GCompareFunc) g_strcmp0);
  PaDeviceIndex index = g_list_position (list, item);

  if (index == -1)
    {
      index = paNoDevice;
    }

  free_portaudio_devices (list);

  return index - 1;
}
#endif //_HAVE_PORTAUDIO_
