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
#include "command/keyresponses.h"


#if GTK_MAJOR_VERSION==2
void create_virtual_keyboard (void) {
    warningdialog (_("Virtual Keyboard requires Denemo to be built with GTK3 or higher"));
  
}

#else

gboolean sharp_order[12] = {
                        0,1,0,1,0,0, // c cis d ees e f
                        1,0,1,0,1,0 // fis g gis a bes
                    };
    
static gboolean noteon (GtkWidget *widget, GdkEventButton  *event, gint key)
    {
        char buf[3] = {0x90, key, 0xFF}; //NOTEON
        gboolean right = (event->button == 3);
        if (right)
            {
                
                    Denemo.keyboard_state |= (CHORD_MASK | ADDING_MASK);
        }
        else
            Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
        //if (Denemo.project->midi_destination & MIDIRECORD)
			handle_midi_event (buf);
		//else
		//	process_midi_event (buf);
        return TRUE;
    }

static gboolean noteoff (GtkWidget *widget, GdkEventButton  *event, gint key)
    {
        char buf[3] = {0x80, key, 0xFF}; //NOTEOFF
        gboolean right = (event->button == 3);
        if (right)
            Denemo.keyboard_state &= ~(CHORD_MASK | ADDING_MASK);
        //if (Denemo.project->midi_destination & MIDIRECORD)
			handle_midi_event (buf);
		//else
		//	process_midi_event (buf);
        //return TRUE;
    }
      
    
static void connect_signals_to_eventbox (GtkWidget* eventbox, gint i)
{
    gtk_widget_add_events (eventbox, (GDK_BUTTON_RELEASE_MASK | GDK_BUTTON_PRESS_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK ));
    gtk_widget_set_can_focus (eventbox, TRUE);
    g_signal_connect (G_OBJECT(eventbox), "button-press-event", G_CALLBACK (noteon), GINT_TO_POINTER(i));
    g_signal_connect (G_OBJECT(eventbox), "button-release-event", G_CALLBACK (noteoff), GINT_TO_POINTER(i));
    g_signal_connect (G_OBJECT(eventbox), "key-press-event", G_CALLBACK (scorearea_keypress_event), NULL);
    g_signal_connect (G_OBJECT(eventbox), "key-release-event", G_CALLBACK (scorearea_keyrelease_event), NULL);   
    
}    
    
static GtkWidget *sharp_button (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = gtk_label_new ("   \n \n \n");
    gtk_container_add (GTK_CONTAINER(eventbox), label);
    connect_signals_to_eventbox (eventbox, i);
    set_background_color (label, "#000000");
    gtk_widget_set_hexpand  (label, TRUE);
    return eventbox;
} 


static void add_label_to_eventbox (GtkWidget *eventbox, GtkWidget *label)
{  
#if ((GTK_MAJOR_VERSION==3) && (GTK_MINOR_VERSION>=12))
    gtk_container_add (GTK_CONTAINER(eventbox), label);
    gtk_widget_set_margin_end (GTK_WIDGET(label), 1);
#else
    GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 1);
    set_background_color (box, "#0000FF");
    gtk_container_add (GTK_CONTAINER(eventbox), box);                                
    GtkWidget *sep = gtk_separator_new (GTK_ORIENTATION_VERTICAL);
    gtk_container_add (GTK_CONTAINER(box), sep);
    set_foreground_color (sep, "#000000");
    gtk_container_add (GTK_CONTAINER(box), label);
#endif
}    


static GtkWidget *natural_button (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = gtk_label_new ("   \n \n \n");
    set_background_color (label, "#FFFFFF");
    gtk_widget_set_hexpand  (label, TRUE);
    add_label_to_eventbox (eventbox, label);
 
    connect_signals_to_eventbox (eventbox, i);

    return eventbox;
}


 static GtkWidget *natural_head (gint i)
{
    GtkWidget *eventbox = gtk_event_box_new ();
    GtkWidget *label = i==60?  gtk_label_new (" \n   C   \n") :
                                ((i%12)==4)? gtk_label_new (" \n        \n") :          gtk_label_new ("         \n \n");
    set_background_color (label, "#FFFFFF");
    gtk_widget_set_hexpand  (label, TRUE);
    add_label_to_eventbox (eventbox, label);
    
    connect_signals_to_eventbox (eventbox, i);

    return eventbox;
}  
static void add_sharps (GtkWidget *sharps, gint octave)
{
   gint i;
   for (i=0;i<12; i++)
    {
       gtk_container_add (GTK_CONTAINER(sharps), sharp_order[i]? sharp_button(i + 12*octave):natural_button(i + 12*octave)); 
        
    }
    
}
static void add_naturals (GtkWidget *sharps, gint octave)
{
   gint i;
   for (i=0;i<12; i++)
    {
        if (!sharp_order[i])
            gtk_container_add (GTK_CONTAINER(sharps), natural_head(i + 12*octave)); 
    }
    
}

void create_virtual_keyboard (gint octaves)
{
    GtkWidget *keyboard_window = gtk_window_new (GTK_WINDOW_TOPLEVEL); 
    GtkWidget *keyboard = gtk_grid_new();
    set_background_color (keyboard_window, "#000000");
    gtk_window_set_title (GTK_WINDOW (keyboard_window), _("Virtual MIDI keyboard"));
    //gtk_window_set_decorated (GTK_WINDOW (keyboard_window), FALSE);
    gtk_window_set_default_size (GTK_WINDOW (keyboard_window), 800, 200);
   
    GtkAdjustment *hadj = gtk_adjustment_new (200.0, 0.0, 400.0, 0.0, 0.0, 0.0);
    GtkWidget *sw = gtk_scrolled_window_new (hadj , NULL);

 
    gtk_orientable_set_orientation ((GtkOrientable *)keyboard, GTK_ORIENTATION_HORIZONTAL);                                

    gint i;
    for (i=5-octaves/2;i<(5 + octaves - octaves/2);i++)
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
             
            add_sharps (sharps, i);
            add_naturals (naturals, i);
         }
   
#if (GTK_MAJOR_VERSION==3 && GTK_MINOR_VERSION<8) 
           gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), keyboard);
#else          
          gtk_container_add (GTK_CONTAINER(sw), keyboard);
#endif   
    gtk_container_add (GTK_CONTAINER(keyboard_window), sw);    
    gtk_widget_show_all (keyboard_window);
    gdouble lower, upper, page;
    lower = gtk_adjustment_get_lower (hadj);
    upper = gtk_adjustment_get_upper (hadj);
    page = gtk_adjustment_get_page_size (hadj);
    //g_print ("Lower ... %f %f %f\n", lower, upper, page);
    gtk_adjustment_set_value (hadj, (upper-page - lower)/2);
}











#endif //GTK3 and above
