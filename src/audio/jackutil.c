#ifdef _HAVE_JACK_
/*
 * jackutil.c
 * JACK utility functions.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Copyright (C) 2011  Dominic Sacré
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "audio/jackutil.h"

#include <jack/jack.h>


GList *
get_jack_ports (gboolean midi, gboolean output)
{
  GList *list = NULL;

  // open a temporary connection to the JACK server
  jack_client_t *client = jack_client_open ("denemo", JackNoStartServer, NULL);

  if (!client)
    {
      return NULL;
    }

  char const *type = midi ? JACK_DEFAULT_MIDI_TYPE : JACK_DEFAULT_AUDIO_TYPE;
  unsigned long flags = output ? JackPortIsOutput : JackPortIsInput;

  // get all input port names of the given type
  char const **ports = jack_get_ports (client, NULL, type, flags);

  if (!ports)
    {
      return NULL;
    }

  // copy all port names to our own list
  char const **p = ports;
  while (*p)
    {
      char *s = g_strdup (*p++);
      list = g_list_append (list, s);
    }

  jack_free (ports);
  jack_client_close (client);

  return list;
}


void
free_jack_ports (GList * list)
{
  GList *p = list;
  while (p)
    {
      g_free (p->data);
      p = g_list_next (p);
    }

  g_list_free (list);
}

#endif //_HAVE_JACK_
