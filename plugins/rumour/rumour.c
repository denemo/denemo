#include <denemo/denemo.h>
#include <denemo/denemo_version.h>

#ifdef __cplusplus
extern "C" 
{
#endif
	
	void denemo_plugin_init(DenemoGUI *si, PluginData *pd);
	void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" 
{
#endif
	void denemo_plugin_init(DenemoGUI *si, PluginData *pd)
	{
		/*Set plugin Name in list and increment counter*/
		pd->title = g_strdup("Rumour");
		pd->clean_up = denemo_plugin_clean_up;
		si->plugincounter++;
	}
	
   void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd)
	{
		g_free(pd->title);
		
	}
	
#ifdef __cplusplus
}
#endif
