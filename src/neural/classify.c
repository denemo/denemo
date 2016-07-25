/*
 * classify.c
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

//gcc `pkg-config --cflags --libs fann` -o classify classify.c
#include <denemo/denemo.h>
#include <fann.h>
#include "core/utils.h"
//(d-Classify " 0 0 0 268 370 624 ")
static gchar *filename = "Training.data";

gchar *classify(gchar *data)
{
    gchar *theann = g_build_filename (get_user_data_dir (FALSE), "fann_config.data", NULL);
    struct fann *ann = fann_create_from_file (theann);
  
    float array_data[6];
    if (6 == sscanf (data, "%f%f%f%f%f%f", array_data, array_data+1, array_data+2, array_data+3, array_data+4, array_data+5))
        {
            float *output = fann_run (ann, array_data);
            return g_strdup_printf ("%.1f %.1f %.1f %.1f %.1f %.1f %.1f %.1f %.1f", output[0], output[1], output[2], output[3], output[4], output[5], output[6], output[7], output[8], output[9]);  
        }
    return NULL;
}

