#ifdef _HAVE_PORTMIDI_
/*
 * portmidiutil.c
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

#include "audio/portmidiutil.h"

#include <portmidi.h>


/*
 * Unlike PortAudio, PortMidi may only be initialized once. We get around this
 * limitation by counting PortMidi initializations ourselves, and only calling
 * Pm_Terminate() when the counter reaches zero.
 */
static int init_count = 0;


PmError
Pm_InitializeWrapper ()
{
  if (init_count++ == 0)
    {
      return Pm_Initialize ();
    }
  else
    {
      return pmNoError;
    }
}

PmError
Pm_TerminateWrapper ()
{
  if (--init_count == 0)
    {
      return Pm_Terminate ();
    }
  else
    {
      return pmNoError;
    }
}


GList *
get_portmidi_devices (gboolean output)
{
  GList *list = NULL;

  PmError err = Pm_InitializeWrapper ();
  if (err != pmNoError)
    {
      return NULL;
    }

  list = g_list_append (list, g_strdup ("none"));
  list = g_list_append (list, g_strdup ("default"));

  int num = Pm_CountDevices ();

  int i;
  for (i = 0; i < num; ++i)
    {
      PmDeviceInfo const *info = Pm_GetDeviceInfo (i);

      // skip inputs if we're looking for outputs, skip outputs if we're looking
      // for inputs
      if ((output && !info->output) || (!output && !info->input))
        {
          continue;
        }

      char *s = g_strdup_printf ("%s: %s", info->interf, info->name);

      list = g_list_append (list, s);
    }

  Pm_TerminateWrapper ();

  return list;
}


void
free_portmidi_devices (GList * list)
{
  GList *p = list;
  while (p)
    {
      g_free (p->data);
      p = g_list_next (p);
    }

  g_list_free (list);
}


PmDeviceID
get_portmidi_device_id (char const *name, gboolean output)
{
  PmError err = Pm_InitializeWrapper ();
  if (err != pmNoError)
    {
      return pmNoDevice;
    }

  PmDeviceID ret = pmNoDevice;

  if (g_strcmp0 (name, "default") == 0)
    {
      if (output)
        {
          ret = Pm_GetDefaultOutputDeviceID ();
          goto out;
        }
      else
        {
          ret = Pm_GetDefaultInputDeviceID ();
          goto out;
        }
    }

  int num = Pm_CountDevices ();

  int i;
  for (i = 0; i < num; ++i)
    {
      PmDeviceInfo const *info = Pm_GetDeviceInfo (i);

      char *s = g_strdup_printf ("%s: %s", info->interf, info->name);

      // check if the device type (input/output) and name matches
      if (((output && info->output) || (!output && info->input)) && g_strcmp0 (name, s) == 0)
        {
          ret = i;
          g_free (s);
          break;
        }

      g_free (s);
    }

out:
  Pm_TerminateWrapper ();

  return ret;
}
#endif //_HAVE_PORTMIDI_
