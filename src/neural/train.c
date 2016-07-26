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
//(d-Train 0 6 10 "")
#include <denemo/denemo.h>
#include <fann.h>
#include "core/utils.h"
#include "neural/train.h"
#define TRAINING_DATA "Training.data"
static gdouble desired_error = 0.025;//0.00001;
static gint max_neurons = 1000; //1000;
static gint neurons_between_reports = 1;
static GtkWidget *dialog = NULL;

static gchar *message_head; 
static gchar *message_tail;


static void destroy_dialog (void)
{
    if (dialog)
        gtk_widget_destroy (dialog);
    dialog = NULL;
}
static void create_dialog (gint epochs)
{
   if (Denemo.non_interactive)
    {gchar *dummy = "dummy"; dialog = (GtkWidget*)dummy;}
  else
    {
      dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_CANCEL, "%s\n%d %s",message_head, epochs, message_tail);
      g_signal_connect (dialog, "response", G_CALLBACK (destroy_dialog), NULL);
      gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (Denemo.window));
      gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
      gtk_widget_show_all (dialog);
    }  
}

static int FANN_API training_callback(struct fann *ann, struct fann_train_data *train,
                           unsigned int max_epochs, unsigned int epochs_between_reports,
                           float desired_error, unsigned int epochs)
{

   if (dialog!=NULL)
    {
     gchar *msg = g_strdup_printf ("%s\n%d %s %.2f",message_head, epochs, message_tail, fann_get_MSE(ann) - desired_error  );
     gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG(dialog), msg);
     g_free (msg);
    }
    while (gtk_events_pending ())
        gtk_main_iteration ();
   
   g_print ("Epochs %8d. MSE: %.5f. Desired-MSE: %.5f\n", epochs, fann_get_MSE(ann), desired_error);
   if (dialog == NULL)
    return -1;
   return 0;
}


gboolean train(gint num, gint in_nodes /* 6 */, gint out_nodes /* 10 */, gchar *data)
{
    gchar *filename;
    gchar *output = g_build_filename (get_user_data_dir (FALSE), DENEMO_NET, NULL);
    struct fann *ann;
    gint number_samples = 0, in = 6, out = 10, tail = 0;
    gchar *contents, *old_data, *new_data;
    GError *error = NULL;
    
    
    
    
    filename = g_build_filename (get_user_data_dir (FALSE), COMMANDS_DIR, TRAINING_DATA, NULL);
    if  (!g_file_test (filename, G_FILE_TEST_EXISTS))
        {
             g_free (filename);
             filename = g_build_filename (get_system_data_dir (), COMMANDS_DIR, TRAINING_DATA, NULL);
         }
    message_head  = _("Training in progress, click to cancel");
    message_tail  = _("Epochs so far, remaining error: ");
    create_dialog(0);
    
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
                g_free (filename);
                filename = g_build_filename (get_user_data_dir (FALSE), COMMANDS_DIR, TRAINING_DATA, NULL);
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
                g_free (filename);
                filename = g_build_filename (get_user_data_dir (FALSE), COMMANDS_DIR, TRAINING_DATA, NULL); 
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
        fann_set_callback(ann, training_callback);
        fann_cascadetrain_on_file(ann, filename, max_neurons, neurons_between_reports, desired_error);
        fann_save (ann, output);
        destroy_dialog ();
        return TRUE;
    }
    return FALSE;
}
