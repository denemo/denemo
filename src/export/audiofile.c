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
#include <math.h>
#include <sndfile.h>
#include <fcntl.h>
#include <string.h>
#include "export/audiofile.h"
#include "export/file.h"
#include "core/prefops.h"
#include "core/utils.h"

const gchar *
recorded_audio_filename (void)
{
  static gchar *filename;
  if (filename == NULL)
    filename = g_build_filename (get_user_data_dir (TRUE), "denemo_audio.1channel-floats", NULL);
  return filename;
}

gboolean
export_recorded_audio (const gchar *outname)
{
  const gchar *filename = recorded_audio_filename ();
  SF_INFO out;
  if (filename)
    {
      gsize length;
      float *data;
      if (g_file_get_contents (filename, (gchar **)&data, &length, NULL) && (length>0))
        {
		  gchar *outfile;
		  GList *exts = g_list_append (NULL, "*.ogg");
		  exts = g_list_append (exts, "*.wav");
          if (outname==NULL)
			outfile = file_dialog ("Give output audio file name, with .ogg or .wav extension", FALSE, Denemo.prefs.denemopath->str, NULL, exts);
		  else
			outfile = g_strdup (outname);
		  g_list_free (exts);
          if (outfile)
            {
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
                  out.channels = 2;
                  out.samplerate = 44100;
                  gpointer outsnd = sf_open (outfile, SFM_WRITE, &out);
                  if (outsnd)
                    {
					  gsize i;
					  gboolean silence = TRUE;
					  
					  progressbar (_("Saving audio .. please wait"), NULL);
					  for (i=0;i<length/sizeof(float);i++)
						{
							float out[2];
							out[0] = out [1] = *(data+i);
							if (silence && (fabs(*out) < 0.0001))
								continue;
							else
								silence = FALSE;
							sf_writef_float (outsnd, out, 1);
							if (!(i%44100))
								keep_alive ();
						}
                      g_free(data);
                      sf_close (outsnd);
                      progressbar_stop ();
                      return TRUE;
                    }
                  else
                    {
                      g_warning ("Unable to open file %s for writing this format", outfile);
                    }
                  g_free (outfile);
                }
            }
        }
        else
        {
          if (Denemo.prefs.maxrecordingtime)
            warningdialog (_("No audio recording has been made.\nSee Playback Controls - Record Button"));
          else
            warningdialog (_("The preference set for recording time is 0 - nothing is recorded.\nSee Edit → Change Preferences Audio/Midi Tab"));
        }
    }
  return FALSE;
}

