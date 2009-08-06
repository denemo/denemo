
#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <denemo/denemo.h>
#include "file.h"

#ifdef WITH_LASH
#include <lash/lash.h>
lash_client_t   *lash_client;

void
save_data()
{
	DenemoGUI *gui = Denemo.gui;
	lash_config_t *config;

	//g_print("\n*** Lash Save filename = %s ***\n",gui->filename->str);
	config = lash_config_new_with_key("filename");
	lash_config_set_value_string(config, gui->filename->str);
	lash_send_config(lash_client, config);
}

void
restore_data(lash_config_t * config)
{
	DenemoGUI *gui = Denemo.gui;
	const char *key;
	key = lash_config_get_key(config);


	if (strcmp(key, "filename") == 0) {
		g_print("\n*** Lash Open filename = %s ***\n",lash_config_get_value_string(config));
		open_for_real ((gchar *)lash_config_get_value_string(config), 
				gui, FALSE, REPLACE_SCORE);
		return;
	}
}


static gboolean
lash_callback(gpointer notused)
{
	lash_event_t	*event;
	lash_config_t   *config;

	while ((event = lash_get_event(lash_client))) {
		switch (lash_event_get_type(event)) {
			case LASH_Save_Data_Set:
				save_data();
				lash_send_event(lash_client, event);
				break;
			case LASH_Restore_Data_Set:
				lash_send_event(lash_client, event);
				break;
			case LASH_Quit:
				g_warning("Exiting due to LASH request.");
				lash_event_destroy(event);
				break;
				//stop_midi_output = 1; //Is this needed?
				//add stuff to exit denemo here
				//break;
			//case LASH_Server_Lost:
			//	return 1;
			default:
				printf("%s: receieved unknown LASH event of type %d",
						    __FUNCTION__, lash_event_get_type(event));
				lash_event_destroy(event);
				break;

		}
		while ((config = lash_get_config(lash_client))) {
		        restore_data(config);
			lash_config_destroy(config);
		}
	}
	return TRUE;
}

static void
init_lash()
{
	lash_event_t *event;
	/* XXX: Am I doing the right thing wrt protocol version? */
        //int flags = 0;	
	//flags = LASH_Config_Data_Set;
	//flags |= LASH_Terminal;

	//lash_client = lash_init(args, PROGRAM_NAME, flags, LASH_PROTOCOL(2, 0));

	if (lash_server_connected(lash_client)) {
		g_print("\n\t\t*** Lash Initialized ***\n");
	} 
	else {
		g_critical("Cannot initialize LASH.  Continuing anyway.");
		/* exit(EX_UNAVAILABLE); */
		return;
	}	
	#define PROGRAM_NAME            "denemo"
	event = lash_event_new_with_type(LASH_Jack_Client_Name);
	lash_event_set_string(event, PROGRAM_NAME);
	lash_send_event(lash_client, event);

	/* Schedule a function to process LASH events, ten times per second. */
	g_timeout_add(100, lash_callback, NULL);
}

void
start_init_lash(lash_client_t *client)
{
  lash_client = client;
  init_lash();
}
#endif /* WITH_LASH */

