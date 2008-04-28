#include <denemo/denemo.h>
#include <denemo/denemo_version.h>
#include "../src/objops.h"
#include "../src/file.h"
#include "../src/importmidi.h"
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#ifdef HAVEALSA 
 #include <alsa/asoundlib.h>
 snd_seq_t *open_seq();
 void midi_action(snd_seq_t *seq_handle);
 void *pullalsa();
#endif


	void denemo_plugin_init(DenemoGUI *si, PluginData *pd);
	void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd);
	void *filter_note_on(void *ptr);
	void midiinput_pref_dialog (GtkAction * action, DenemoGUI *si);
	#define STATUS_MASK 0x0F0
	#define NOTE_ON 0x90
	
 	static GtkWidget *root_menu = NULL;			
	struct scoreinfo *si2;
	char *device_name;
	gboolean stepentry;

struct callbackdata
{ 
  	GtkWidget *deviceentry;
      	GtkWidget *step;
        DenemoGUI *si;
};

typedef struct midinote {
	char notenum;
	char veloc;
} midinote;


static struct callbackdata cbdata;

void denemo_plugin_init(DenemoGUI *si, PluginData *pd){
	
		/*Set plugin Name in list and increment counter*/
		pd->title = g_strdup("Midi Input");
		pd->clean_up = denemo_plugin_clean_up;
		si->plugincounter++;
		
		GtkWidget *menu_item1;
		GtkWidget *new_menu;

			 
         /*		
	 * create a menu checkbox item that allows turning off and on midi input
	 * [] midi step entry 
	 *
	 */
		new_menu = gtk_menu_new();
		menu_item1 = gtk_menu_item_new_with_label("Midi step entry");
		gtk_menu_append(GTK_MENU(new_menu), menu_item1);
		gtk_widget_show(menu_item1);
		gtk_signal_connect(GTK_OBJECT(menu_item1), "activate",
				    GTK_SIGNAL_FUNC(midiinput_pref_dialog),(struct scoreinfo *)si);
		
		root_menu = gtk_menu_item_new_with_label("Midi_input");
		gtk_widget_show(root_menu);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(root_menu), new_menu);
		gtk_menu_bar_append(GTK_MENU_BAR(si->menubar), root_menu);
}
	
   		


void 
midiinput_pref_dialog (GtkAction * action, DenemoGUI *si){
	GtkWidget *dialog;
	GtkWidget *label;
	GtkWidget *table;
	GtkWidget *deviceentry;
	GtkWidget *step;
	pthread_t thread1;
	int rc2;

	dialog = gtk_dialog_new_with_buttons (_("Midi Input"), NULL,
	                                 (GtkDialogFlags) (GTK_DIALOG_MODAL |
	                                  GTK_DIALOG_DESTROY_WITH_PARENT),
	                                   GTK_STOCK_OK, GTK_RESPONSE_OK,
	                                  GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
	                                  NULL);

	table = gtk_table_new (3, 3, TRUE);
#ifndef HAVEALSA
	label = gtk_label_new (_("Midi device path"));
	gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 0, 1);
	gtk_widget_show (label); 

	deviceentry = gtk_entry_new ();
	gtk_entry_set_text (GTK_ENTRY (deviceentry), "/dev/midi"); 
	gtk_table_attach_defaults (GTK_TABLE (table), deviceentry, 1, 2, 0, 1);
	gtk_widget_show (deviceentry);
#endif

	label = gtk_label_new (_("Midi Step entry"));
	gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, 1, 2);
	gtk_widget_show (label);
	
	step = gtk_check_button_new ();
	//gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (step) , cbdata.step);  
	gtk_table_attach_defaults (GTK_TABLE (table), step, 1, 2, 1, 2);
	gtk_widget_show (step);

	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), table, TRUE, TRUE,
					0);
	gtk_widget_show (table);

#ifndef HAVEALSA 	
	cbdata.deviceentry = deviceentry;
	device_name = (gchar *) gtk_entry_get_text (GTK_ENTRY (deviceentry));
	gchar *message = (gchar*)device_name;

#endif
	cbdata.si = si;
	cbdata.step = step;
#ifndef HAVEALSA
	gtk_widget_grab_focus (deviceentry);
#endif
	gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
	gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
	gtk_widget_show (dialog);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK)
		printf("\nokey dokey\n");

	stepentry = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (step));
	
#ifndef HAVEALSA
	if (stepentry == 1)
		rc2 = pthread_create( &thread1, NULL, filter_note_on, (void*) message);
#endif
#ifdef HAVEALSA
	if (stepentry == 1)
		rc2 = pthread_create( &thread1, NULL, pullalsa, NULL);
#endif
	gtk_widget_destroy (dialog);
}

void *filter_note_on(void *ptr)
{
	gchar *device = (gchar*)ptr;
	unsigned char inpacket = 0;
	int status = 0;
	midinote noteon;
	int position = 0;
	printf("\nOpening device %s to read step entry\n",device);
	int fd=open(device, O_RDONLY);
	
	while (stepentry != 0){ 
		read(fd, &inpacket, sizeof(inpacket));
		if (inpacket > 127){	
			status = ((inpacket & STATUS_MASK) == NOTE_ON);
		}
		if (inpacket < 127 && status == 1){
			noteon.notenum = inpacket;
			read(fd, &inpacket, sizeof(inpacket));
			noteon.veloc = inpacket;
			if (noteon.veloc != 0){
				printf("note = %i velocity = %i\n",noteon.notenum, noteon.veloc);
				//donoteon(si2,noteon.notenum,120,position);
				//position += 192;	
				//donoteon (si2, noteon.notenum, 0, position);
				//gtk_widget_draw (si2->scorearea, NULL);	
			}
		}		
	}
}

#ifdef HAVEALSA
snd_seq_t *open_seq() {

  snd_seq_t *seq_handle;
  int portid;

  if (snd_seq_open(&seq_handle, "default", SND_SEQ_OPEN_INPUT, 0) < 0) {
    fprintf(stderr, "Error opening ALSA sequencer.\n");
    exit(1);
  }
  snd_seq_set_client_name(seq_handle, "Denemo");
  if ((portid = snd_seq_create_simple_port(seq_handle, "Denemo",
            SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
            SND_SEQ_PORT_TYPE_APPLICATION)) < 0) {
    fprintf(stderr, "Error creating sequencer port.\n");
    exit(1);
  }
  return(seq_handle);
}

void midi_action(snd_seq_t *seq_handle) {

  snd_seq_event_t *ev;

  do {
    snd_seq_event_input(seq_handle, &ev);
    switch (ev->type) {
      case SND_SEQ_EVENT_CONTROLLER: 
        fprintf(stderr, "Control event on Channel %2d: %5d       \r",
                ev->data.control.channel, ev->data.control.value);
        break;
      case SND_SEQ_EVENT_PITCHBEND:
        fprintf(stderr, "Pitchbender event on Channel %2d: %5d   \r", 
                ev->data.control.channel, ev->data.control.value);
        break;
      case SND_SEQ_EVENT_NOTEON:
        fprintf(stderr, "Note On event on Channel %2d: %5d       \r",
                ev->data.control.channel, ev->data.note.note);
	break;        
      case SND_SEQ_EVENT_NOTEOFF: 
        fprintf(stderr, "Note Off event on Channel %2d: %5d      \r",         
                ev->data.control.channel, ev->data.note.note);  
        break;        
    }
    snd_seq_free_event(ev);
  } while (snd_seq_event_input_pending(seq_handle, 0) > 0);
}
#endif
#ifdef HAVEALSA
void *pullalsa() {

  snd_seq_t *seq_handle;
  int npfd;
  struct pollfd *pfd;
    
  seq_handle = open_seq();
  npfd = snd_seq_poll_descriptors_count(seq_handle, POLLIN);
  pfd = (struct pollfd *)alloca(npfd * sizeof(struct pollfd));
  snd_seq_poll_descriptors(seq_handle, pfd, npfd, POLLIN);
  while (1) {
    if (poll(pfd, npfd, 100000) > 0) {
      midi_action(seq_handle);
    }  
  }
}

#endif

void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd){
		g_free(pd->title);
}
