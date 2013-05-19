//      sourceaudio.c
//      
//      Copyright 2013 Richard Shann <rshann@virgin.net>
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 3 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.


#include <stdio.h>
#include <sndfile.h>
#include <fcntl.h>
#include "audiofile.h"
#include "file.h"
#include "prefops.h"
#include <string.h>

const gchar *
recorded_audio_filename (void)
{
  static gchar *filename;
  if (filename == NULL)
    filename = g_build_filename (locatedotdenemo (), "denemo_audio.1channel-floats", NULL);
  return filename;
}

gboolean
export_recorded_audio ()
{
  const gchar *filename = recorded_audio_filename ();
  SF_INFO sfinfo;
  SF_INFO out;
  sfinfo.channels = 1;
  sfinfo.format = SF_FORMAT_RAW | SF_FORMAT_FLOAT | SF_ENDIAN_LITTLE;
  sfinfo.samplerate = 44100;
  if (filename)
    {
      gpointer sndfile = sf_open (filename, SFM_READ, &sfinfo);
      if (sndfile)
        {
          gchar *outfile = file_dialog ("Give output audio file name, with .ogg or .wav extension", TRUE, (gchar *) locatedotdenemo ());
          if (outfile)
            {
              float data;
              gint len = strlen (outfile);
              if (len > 4)
                {
                  if (*(outfile + len - 3) == 'o')
                    {
                      out.format = SF_FORMAT_VORBIS | SF_FORMAT_OGG;
                    }
                  else
                    {
                      out.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
                    }
                  out.channels = 1;
                  out.samplerate = 44100;
                  gpointer outsnd = sf_open (outfile, SFM_WRITE, &out);
                  g_print ("Returned with %p\n", outsnd);
                  if (outsnd)
                    {
                      while (sf_read_float (sndfile, &data, 1) == 1)
                        sf_write_float (outsnd, &data, 1);
                      sf_close (outsnd);
                      sf_close (sndfile);
                      return TRUE;
                    }
                  else
                    {
                      g_warning ("Unable to open file %s for writing this format\n", outfile);
                    }
                  g_free (outfile);
                }
            }
          sf_close (sndfile);
        }
    }
  return FALSE;
}
