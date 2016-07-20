/*
 * train.c
 * 
 * Copyright 2016 Richard Shann <richard@rshann.plus.com>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

//gcc `pkg-config --cflags --libs fann` -o train train.c
#include <denemo/denemo.h>
#include <fann.h>
#include "core/utils.h"
static gdouble desired_error = 0.01;
static gint max_neurons = 1000;
static gint neurons_between_reports = 10;


gboolean train(gint num, gint in_nodes /* 6 */, gint out_nodes /* 4 */, gchar *data)
{
    gchar *filename = g_build_filename (get_user_data_dir (FALSE), "Training.data", NULL);
    gchar *output = g_build_filename (get_user_data_dir (FALSE), "fann_config.data", NULL);
    struct fann *ann;
    gint number_samples = 0, in = 6, out = 4, tail = 0;
    gchar *contents, *old_data, *new_data;
    GError *error = NULL;

     if  (g_file_test (filename, G_FILE_TEST_EXISTS))
        {                 
            g_file_get_contents (filename, &contents, NULL, &error);
            if (error)
                {
                    g_warning ("Bad file %s found in train()", filename);
                    g_free (contents);
                    return FALSE;
                }
            sscanf (contents, "%d%d%d%n", &number_samples, &in, &out, &tail);// return value may or may not be 3, C compilers disagree 
            if ((in != in_nodes) || (out != out_nodes))
                {
                    g_warning ("Bad call to train()");
                    g_free (contents);
                    return FALSE;
                }
            if (num>0)
            {
                old_data = contents + tail;
                new_data = g_strdup_printf ("%d %d %d\n%s\n%s\n", number_samples + num, in, out, old_data, data); 
                g_file_set_contents (filename, new_data, -1, &error);
                g_free (new_data);
            }
            g_free (contents);
            if (error) return FALSE;
    }
    else
    {
         if (num>0)
            {
                new_data = g_strdup_printf ("%d %d %d\n%s\n",  num, in, out, data); 
                g_file_set_contents (filename, new_data, -1, &error);
                g_free (new_data);
            }
          else
            {
              g_warning ("No Training data %s", filename);
              return FALSE;
            }
    }

  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    {
        ann = fann_create_shortcut (2, in_nodes /* 2 * number of notes of context provided, including the one to classify */ , out_nodes /* number of classifications ie neurons in the output layer*/);
        fann_cascadetrain_on_file(ann, filename, max_neurons, neurons_between_reports, desired_error);
        fann_save (ann, output);
        return TRUE;
    }
    return FALSE;
}
