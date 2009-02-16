
#include <glib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#ifdef WITH_LASH
#include <lash/lash.h>
lash_client_t   *lash_client;

static gboolean
lash_callback(gpointer notused)
{
	lash_event_t	*event;

	while ((event = lash_get_event(lash_client))) {
		switch (lash_event_get_type(event)) {
			case LASH_Save_File:
				printf("Asked to save data in %s, but we don’t have any\n",
			 	lash_event_get_string(event));
				lash_send_event(lash_client, event);
			    	continue;

			case LASH_Restore_File:
				printf("Asked to restore data from %s, but we don’t have any\n",
							   lash_event_get_string(event));
				lash_send_event(lash_client, event);
			 	continue;

			case LASH_Quit:
				g_warning("Exiting due to LASH request.");
				//stop_midi_output = 1; //Is this needed?
				//add stuff to exit denemo here
				//break;
			default:
				g_warning("Receieved unknown LASH event of type %d.", lash_event_get_type(event));
		}
	  lash_event_destroy(event);
		
	}
	return TRUE;
}

static void
init_lash()
{
	/* XXX: Am I doing the right thing wrt protocol version? */
	//lash_client = lash_init(args, PROGRAM_NAME, LASH_Config_Data_Set, LASH_PROTOCOL(2, 0));

	if (lash_server_connected(lash_client)) {
		g_print("\n\t\t*** Lash Initialized ***\n");
	} 
	else {
		g_critical("Cannot initialize LASH.  Continuing anyway.");
		/* exit(EX_UNAVAILABLE); */
		return;
	}	
	
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

