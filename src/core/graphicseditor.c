/*
 * graphicsedit.c
 *
 * Copyright 2014 Richard Shann <rshann@debian-box>
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


#include <stdio.h>
#include <string.h>
#include <denemo/denemo.h>
#include "utils.h"

#define GPID_NONE (-1)


static gchar *get_extension (gchar *filename) {
   if(filename==NULL) return NULL;
   gchar *c = filename+strlen(filename);
   for(;c != filename; c--)
        {
          if(*c=='.')
          {
             return (c+1);
          }
        }
    return NULL;
}
static gchar *choose_graphic_file (void)
{
  GtkWidget *dialog;
  gchar *filename = NULL;
  gchar *system_dir = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "graphics", NULL);
  gchar *title =  _("Graphics File");
  GList *extensions = NULL;
  extensions = g_list_append (extensions, (gpointer) "*.eps");
  extensions = g_list_append (extensions, (gpointer) "*.EPS");
  extensions = g_list_append (extensions, (gpointer) "*.svg");
  extensions = g_list_append (extensions, (gpointer) "*.SVG");

  filename = choose_file (title, system_dir, extensions);
  g_free(system_dir);
  return filename;
}
static gchar *create_editable_file (gchar *filename, gchar *newname)
{
   gchar *ret, *contents, *outname;
   gsize length;
   gchar *temp;
   if(newname) newname = g_strdup_printf ("%s.%s", newname, get_extension (filename));
   temp = g_path_get_basename (newname?newname:filename);
   gchar * current_directory = newname?
        g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "graphics", NULL):
        get_project_dir ();

  if (!g_file_test (current_directory, G_FILE_TEST_EXISTS))
    g_mkdir_with_parents (current_directory, 0770);
   outname = g_build_filename (current_directory, temp, NULL);
   g_free(temp);g_print("outname is %s for %s %s\n", outname, newname, get_user_data_dir (TRUE));
   if(g_file_get_contents (filename, &contents, &length, NULL))
    {
        if(g_file_set_contents (outname, contents, length, NULL))
            {
                //g_free (filename);
                return outname;
            } else
            {
                    g_warning ("Could not write %s\n", outname);
            }
    }
    g_free (filename);
    if(newname) g_free(current_directory);
    return NULL;
}

static gboolean try_for_svg (gchar *filename) {
    gchar *extension = get_extension (filename);
    if(extension)
        {
        if(!strcmp (extension, "eps"))
             {
                        *extension = 's';
                        *(extension+1) = 'v';
                        *(extension+2) = 'g';
            }
        if(!g_file_test (filename, G_FILE_TEST_EXISTS))
            {
                        *extension = 'e';
                        *(extension+1) = 'p';
                        *(extension+2) = 's';
                        return FALSE;
            }
        }
    return TRUE;
}
/* filename is an svg or eps file, if NULL allows the user to choose an svg or eps file from
 * the system files and the file is copied to the project directory (template==FALSE) or
 * to the users actions/graphics directory (template=TRUE)
 * prefs->graphicseditor run on the .svg file of the same name (if present).
 * the filename being edited (without the extension) is returned
 * returns the filename chosen which should be freed by the caller
 */
gchar *edit_graphics_file (gchar *filename, gchar *newname)
{
   GPid pid = GPID_NONE;
   GError *err = NULL;
   gchar *choice = NULL;
   if(filename==NULL)
        {
            choice = choose_graphic_file();
            if(choice)
                choice = create_editable_file (choice, newname);
        } else
        choice = strdup (filename);

   if(try_for_svg(choice))
    g_info("Opening SVG for eps");
   if(choice && g_file_test (choice, G_FILE_TEST_EXISTS))
    {
        gchar *arguments[] = {
                                Denemo.prefs.graphicseditor->str,
                                choice,
                                NULL};
        g_spawn_async_with_pipes (NULL,  /* dir */
                                    arguments, NULL,    /* env */
                                    G_SPAWN_SEARCH_PATH,        /* search in path for executable */
                                    NULL,       /* child setup func */
                                    NULL,       /* user data */
                                    &pid,      /* FIXME &pid see g_spawn_close_pid(&pid) */
                                    NULL, NULL, NULL, &err);

     //drop extension
     if(choice)
        {
            gchar *c;
            for (c= choice + strlen(choice);c !=choice; c--)
                if(*c == '.')
                    {
                        *c = 0;
                        break;
                    }
        }
    }
   return choice;
}
