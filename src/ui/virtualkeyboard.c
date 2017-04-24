/*
 * virtualkeyboard.c
 *
 * Copyright 2017 Richard Shann <rshann@virgin.net>
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
#include "ui/virtualkeyboard.h"
#include "core/view.h"
#include "core/utils.h"


#if GTK_MAJOR_VERSION==2
void create_virtual_keyboard (void) {
    warningdialog (_("Virtual Keyboard requires Denemo to be built with GTK3 or higher"));
  
}

#else

gboolean sharp_order[12] = {
                        0,1,0,1,0,0, // c cis d ees e f
                        1,0,1,0,1,0 // fis g gis a bes
                    };
    
static void noteon (gint key)
    {
        g_print ("Now emit MIDI on for %d", key);
    }
static GtkWidget *sharp_button (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = gtk_label_new ("   \n \n \n");
    gtk_container_add (GTK_CONTAINER(eventbox), label);
    gtk_widget_add_events (eventbox, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK ));
    g_signal_connect_swapped (G_OBJECT(eventbox), "button-press-event", G_CALLBACK (noteon), GINT_TO_POINTER(i));
    //gtk_widget_set_margin_end (GTK_WIDGET(label), 1);
    set_background_color (label, "#000000");
    gtk_widget_set_hexpand  (label, TRUE);
    return eventbox;
} 
static GtkWidget *natural_button (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = gtk_label_new ("   \n \n \n");
    gtk_container_add (GTK_CONTAINER(eventbox), label);
    gtk_widget_add_events (eventbox, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK ));

    g_signal_connect_swapped (G_OBJECT(eventbox), "button-press-event", G_CALLBACK (noteon), GINT_TO_POINTER(i));
    gtk_widget_set_margin_end (GTK_WIDGET(label), 1);
     set_background_color (label, "#FFFFFF");
    
    gtk_widget_set_hexpand  (label, TRUE);
    return eventbox;
}
 static GtkWidget *natural_head (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = gtk_label_new ("         \n \n");
    gtk_container_add (GTK_CONTAINER(eventbox), label);
    gtk_widget_add_events (eventbox, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK ));

    g_signal_connect_swapped (G_OBJECT(eventbox), "button-press-event", G_CALLBACK (noteon), GINT_TO_POINTER(i));
    gtk_widget_set_margin_end (GTK_WIDGET(label), 1);
    set_background_color (label, "#FFFFFF");
    gtk_widget_set_hexpand  (label, TRUE);
    return eventbox;
}  
static add_sharps (GtkWidget *sharps)
{
   gint i;
   for (i=0;i<12; i++)
    {
       gtk_container_add (GTK_CONTAINER(sharps), sharp_order[i]? sharp_button(i):natural_button(i)); 
        
    }
    
}
static add_naturals (GtkWidget *sharps)
{
   gint i;
   for (i=0;i<12; i++)
    {
        if (!sharp_order[i])
            gtk_container_add (GTK_CONTAINER(sharps), natural_head(i)); 
    }
    
}

void create_virtual_keyboard (gint octaves)
{
    GtkWidget *keyboard_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    GtkWidget *keyboard = gtk_grid_new();

    set_background_color (keyboard_window, "#000000");
    gtk_window_set_title (GTK_WINDOW (keyboard_window), _("Virtual MIDI keyboard"));
    gtk_container_add (GTK_CONTAINER(keyboard_window), keyboard);
    gtk_orientable_set_orientation ((GtkOrientable *)keyboard, GTK_ORIENTATION_HORIZONTAL);                                
    

    

    gint i;
    for (i=0;i<octaves;i++)
         {   
            GtkWidget *sharps = gtk_grid_new();
            GtkWidget *naturals = gtk_grid_new();
            GtkWidget *octave = gtk_grid_new(); 
            
            gtk_container_add (GTK_CONTAINER(keyboard), octave);
            gtk_orientable_set_orientation ((GtkOrientable *)octave, GTK_ORIENTATION_VERTICAL);

              
            gtk_container_add (GTK_CONTAINER(octave), sharps);
            gtk_orientable_set_orientation ((GtkOrientable *)sharps, GTK_ORIENTATION_HORIZONTAL); 
            gtk_container_add (GTK_CONTAINER(octave), naturals);
            gtk_orientable_set_orientation ((GtkOrientable *)naturals, GTK_ORIENTATION_HORIZONTAL);        
             
            add_sharps (sharps);
            add_naturals (naturals);
         }
    gtk_widget_show_all (keyboard_window);
}











#endif //GTK3 and above
