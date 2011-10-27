#include "scorewizard.h"
#include "utils.h"
#include <glib.h>
#include <string.h>
#include "parseinstruments.h"
#include "staffops.h"
#include "dialogs.h"
#include <gtk/gtk.h>

/* 	TODO
 * 	insert timesignature/keysignature page
 */

/**
 * Instrument properties
 */


typedef struct instproperties
{
	gint *clef;
	gint numstaffs;
	gint *transposition;
}instproperties;


/**
 * Lilyponds supported font sizes
 */
static gchar *fontsizes[8] =
  {
    "11", "13", "14", "16", "18", "20", "23","26"
  };

/**
 * Lilyponds supported paper sizes
 */
static gchar *papersizes[6] =
  {
    "a4", "a6", "a5", "legal", "letter", "tabloid"
  };

/**
 * Textual representation of instrument types
 */
static gchar *instrumenttypes[8] =
  {
    "Woodwinds" ,"Brass","Strings","Vocals", "Pitched Percussion","Plucked Strings","Keyboards", "None"
  };

#define KEYNAME_ARRAY_OFFSET 7

static gchar *majorkeys[15] =
  { "C flat", "G flat", "D flat", "A flat", "E flat", "B flat", "F",
  "C", "G", "D", "A", "E", "B", "F sharp", "C sharp"
};

static gchar *minorkeys[15] =
  { "A flat", "E flat", "B flat", "F", "C", "G", "D",
  "A", "E", "B", "F sharp", "C sharp", "G sharp", "D sharp", "A sharp"
};

static gchar *modes[7] =
  { "lydian", "ionian", "mixolydian", "dorian", "aeolian", "phrygian",
"locrain" };

/**
 * create a staff with that instrument name
 *
 */

static void create_staff_instruments(wizarddata *wdata, gchar *instrument_name,
				     instproperties *inst)
{
  gint numstaffsforinstrument = 1; //(gint) inst->numstaffs;
  /*gint staffnumber = (gint) wdata->staffnumber; 
    gint currentstaffnumber = (gint) wdata->currentstaffnumber;*/
  DenemoStaff *curstaffstruct = 
    (DenemoStaff *) wdata->gui->si->currentstaff->data;
  //DenemoStaff *curstaffstruct  = g_list_last(wdata->gui->si->currentstaff);

  //g_printf("clef # in create_staff_instrument = %i\n", inst->clef);
  gtk_widget_queue_draw (Denemo.scorearea);
  /* printf("\nlocation before switch currentstaffnum = %i\n",  */
/* 	 wdata->gui->si->currentstaffnum); */
  switch((gint) inst->numstaffs)
    {
    case 1:
      {
	if ((wdata->gui->si->currentstaffnum != 1) || 
	    (wdata->currentstaffnumber > 1)) 
	  //problem is it is not creating staves for multiple instruments
	  {
	    wdata->gui->si->currentstaffnum++;
	    newstaff (wdata->gui, 
		      ADDFROMLOAD, DENEMO_NONE);
	    
/* 	    printf("\nlocation b before switch currentstaffnum = %i\n",  */
/* 		   wdata->gui->si->currentstaffnum); */
	    update_vscrollbar (wdata->gui);

	    displayhelper(wdata->gui);
	  }
	wdata->gui->si->currentstaff = g_list_last (wdata->gui->si->thescore);
	curstaffstruct = (DenemoStaff *) wdata->gui->si->currentstaff->data;
	dnm_setinitialclef(wdata->gui->si, curstaffstruct, 
			   (int) ((instproperties *) inst)->clef);
	//strcpy(curstaffstruct->denemo_name->str, instrument);

	g_string_assign(curstaffstruct->denemo_name, instrument_name);

	set_lily_name (curstaffstruct->denemo_name,curstaffstruct->lily_name);
	//strcpy(curstaffstruct->midi_instrument->str, instrument);
	g_string_assign(curstaffstruct->midi_instrument, instrument_name);

	curstaffstruct->transposition = (int) inst->transposition;
	
	
	wdata->staffnumber--;
	wdata->currentstaffnumber++;
	break;
      }
    case 2:
      {
	while (numstaffsforinstrument <= inst->numstaffs){ 
	  
	  if ((wdata->gui->si->currentstaffnum != 1) || 
	      (numstaffsforinstrument == 2) || (wdata->currentstaffnumber > 1))
	    {
	      wdata->gui->si->currentstaffnum++;
	      newstaff (wdata->gui, ADDFROMLOAD, 
			DENEMO_NONE);
	      printf("\ninside case 2 first if currentstaffnum = %i\n", 
		     wdata->gui->si->currentstaffnum); 
	      update_vscrollbar(wdata->gui);
	      wdata->gui->si->currentstaff = 
		g_list_last (wdata->gui->si->thescore);
	      displayhelper(wdata->gui);
	      
	    }
	  
	  if (numstaffsforinstrument == 1){
	    wdata->gui->si->currentstaff = 
	      g_list_last (wdata->gui->si->thescore);
	    curstaffstruct = 
	      (DenemoStaff *) wdata->gui->si->currentstaff->data;
	    dnm_setinitialclef(wdata->gui->si, curstaffstruct, 
			       DENEMO_TREBLE_CLEF);
	    curstaffstruct->context = DENEMO_PIANO_START;

	    g_string_printf(curstaffstruct->denemo_name, "%s%s", instrument_name, "rh");


	    set_lily_name (curstaffstruct->denemo_name,
			   curstaffstruct->lily_name);
	    //strcpy(curstaffstruct->midi_instrument->str, instrument);
	    g_string_assign(curstaffstruct->midi_instrument, instrument_name);
	  }
	  if (numstaffsforinstrument == 2){
	    wdata->gui->si->currentstaff = 
	      g_list_last (wdata->gui->si->thescore);
	    curstaffstruct = 
	      (DenemoStaff *) wdata->gui->si->currentstaff->data;
/* 	    printf("\ninside case 2 third if currentstaffnum = %i\n",  */
/* 		   wdata->gui->si->currentstaffnum); */
	    dnm_setinitialclef(wdata->gui->si, curstaffstruct, 
			       DENEMO_BASS_CLEF);
	    curstaffstruct->context = DENEMO_PIANO_END;

	    g_string_printf(curstaffstruct->denemo_name, "%s%s", instrument_name, "lh");


	    set_lily_name (curstaffstruct->denemo_name,
			   curstaffstruct->lily_name);
	    //strcpy(curstaffstruct->midi_instrument->str, instrument);
	    
	  }
	  numstaffsforinstrument++;
	}
	wdata->staffnumber--;//FIXME should be a for loop
	break;
      }
    case 3:
      {
	if (wdata->staffnumber != 1)
	  {
	    wdata->gui->si->currentstaffnum++;
	    newstaff (wdata->gui, 
		      ADDFROMLOAD, DENEMO_NONE);
	    update_vscrollbar(wdata->gui);
	    wdata->gui->si->currentstaff = 
	      g_list_last (wdata->gui->si->thescore);
	    update_vscrollbar (wdata->gui);
	    staffdown(NULL);
	    displayhelper(wdata->gui);
	  }
	dnm_setinitialclef(wdata->gui->si, curstaffstruct, 
			   (int) ((instproperties *) inst)->clef);
	break;
      }
    };
  
/*   printf("\nlocation c currentstaffnum = %i\n",  */
/* 	 wdata->gui->si->currentstaffnum); */
/*   printf("\nstaffnumger = %i\n",wdata->staffnumber); */
}



/**
 * Add instrument to score view
 *
 *
 */
static void addinstrument(GtkButton *button, wizarddata *data)
{
  
  data->sdata->selection =
    gtk_tree_view_get_selection(GTK_TREE_VIEW(data->instrument_list));
  GtkTreeIter iter;
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(data->sdata->selection), 
				  (GtkTreeModel **)&data->sdata->list_store, 
				     &iter))
    return;
  gchar *name;

  gtk_tree_model_get(GTK_TREE_MODEL(data->sdata->list_store), &iter, 0, 
		     &name, -1);
#ifdef DEBUG

  g_print("Instrument %s \n", name);
#endif

  gtk_list_store_append (data->sdata->score_list, &iter);
  gtk_list_store_set (data->sdata->score_list, &iter,  0, _(name), -1);
}


static instproperties * lookup_instrument_properties(gchar *name, wizarddata *wdata)
{
  /*need to have a sting comparison here*/
  GList *instruments = wdata->icbdata->instruments;
  GList *tmp = instruments;
  instproperties *instr = (instproperties *) g_malloc0(sizeof(instproperties));
  //instproperties *instr;
  //int clef, transposition;
  while(tmp)
    {

      GList *inst = ((InstrumentList *) (tmp)->data)->instruments;
      while (inst)
        {
          if (!strcmp(((InstrumentConfig *)inst->data)->name->str, name))
            {
              //g_print("string comparison returns %i\n", (!strcmp(((InstrumentConfig *)inst->data)->name->str,"Flute")));
              instr->clef = (gint *) ((InstrumentConfig *)inst->data)->sclef;
              instr->transposition = (gint *) ((InstrumentConfig *)inst->data)->transposition;
	      instr->numstaffs = ((InstrumentConfig *)inst->data)->numstaffs;
	      g_debug("instrument name inst->data)->name->str = %s\n", ((InstrumentConfig *)inst->data)->name->str);
              g_debug("number of staves for instrument = %i\n", (gint) ((InstrumentConfig *)inst->data)->numstaffs);
	      g_debug("instrument staff inst->data)->sclef = %i\n", (gint) instr->clef);
	      g_debug("transposition = %i\n", (gint) instr->transposition);
	      break;
            }
          inst = g_list_next(inst);
        }
      tmp = g_list_next(tmp);
    }
  return instr;
}

static gboolean
foreach_func (GtkTreeModel *model,
              GtkTreePath  *path,
              GtkTreeIter  *iter,
              gpointer      user_data)
{
  wizarddata *wdata = (wizarddata *) user_data;
  gchar *name;
  gchar *tree_path_str = gtk_tree_path_to_string(path);
  gtk_tree_model_get (model, iter, 0, &name,-1);
  //g_print ("Row %s: %s staff #= %i\n", tree_path_str,name,staffnumber);
  instproperties *instr =  (instproperties *) lookup_instrument_properties(name, wdata);
  create_staff_instruments(wdata, name, instr);
  g_free(name);
  return FALSE;
}
/**
 * Remove instrument from score view
 *
 */
static void removeinstrument(GtkButton *button, wizarddata *wdata)
{
  //scoredata *scored = (scoredata *) data;
  wdata->sdata->selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(wdata->score_list));
  GtkTreeIter iter;
  if(gtk_tree_selection_get_selected(GTK_TREE_SELECTION(wdata->sdata->selection), (GtkTreeModel **)&wdata->sdata->score_list, &iter))
    gtk_list_store_remove((GtkListStore *) wdata->sdata->score_list, &iter);
}
/**
 * Callback to add the specific instruments to the tree view 
 *
 */
static gboolean
cell_clicked(GtkTreeSelection *selection,
             GtkTreeModel     *model,
             GtkTreePath      *path,
             gboolean          path_currently_selected,
             gpointer          user_data)
{

  gboolean ret = FALSE;
  //g_print("\nIn cell_clicked callback\n");
  wizarddata *wdata = (wizarddata *)user_data;
  instsdata *cbdata = (instsdata *) wdata->cbdata;
  GList *instruments = wdata->icbdata->instruments;
  GtkTreeIter iter;


  //get the current selected path
  if (gtk_tree_model_get_iter(model, &iter, path))
    {
      gchar *name;

      gtk_tree_model_get(model, &iter, 0, &name, -1);

      if (!path_currently_selected)
        {
          gtk_list_store_clear(cbdata->list_store); //clear list store
          //add instruments to the tree views second column
          InstrumentType type = lookuptype(name);
#ifdef DEBUG
	  g_print("Name %s,  Type %d\n", name, type);
#endif
          //g_print("Entire Instruments List length %d\t", g_list_length((data->instruments)->data)->instruments);
          GList *tmp = instruments;

          //g_print("Entire Instruments List length %d\t", g_list_length(instruments));

          while (tmp)
            {
              //g_print("Individual List type %i \t ", ((InstrumentList *) tmp->data)->type);
              if (type == ((InstrumentList *) (tmp)->data)->type)
                {
                  GList *inst = ((InstrumentList *) (tmp)->data)->instruments;
                  while (inst)
                    {
                      gtk_list_store_append (cbdata->list_store, &iter);
                      /* get instrument name from InstrumentConfig structure */
                      gtk_list_store_set (cbdata->list_store, &iter,  0, _(((InstrumentConfig *)inst->data)->name->str), -1);
                      //	g_print("instrument name inst->data)->name->str = %s\n", _(((InstrumentConfig *)inst->data)->name->str) );
                      //	g_print("instrument staff inst->data)->sclef = %i\n", (int *) ((InstrumentConfig *)inst->data)->sclef );
                      inst = g_list_next(inst);
                    }
                  break;
                }
              tmp = g_list_next(tmp);
            }
        }
      g_free(name);
    }
  return ret;
}

/**
 * Debug function to print the elements of the instruments 
 * to the screen
 */
static void printInstruments(GList *instruments)
{
  GList *tmp;
  for(tmp=instruments; tmp; tmp=tmp->next)
    {
      GList *insts;
      for(insts = ((InstrumentList *) tmp->data)->instruments; insts; insts=insts->next)
        {
          g_print("(printInstrument) Name %s \n ",  ((InstrumentConfig *) insts->data)->name->str);
        }
    }
}

/**
 * Adds page to the notebook for selecting the instruments
 * that will be in the score.
 */
void
instrumentsetup(wizarddata *wdata)
{
  GtkWidget *list;
  GtkListStore *list_store;
  GtkListStore *score_store;
  
  GtkTreeIter iter;
  GList *tmp;
  wdata->cbdata = (instsdata *) g_malloc0(sizeof(instsdata));
  instsdata *cbdata = wdata->cbdata;
  wdata->instruments = parseInstruments(wdata->instruments);

  GtkWidget *hbox = gtk_hbox_new(TRUE, 5);


  list_store = gtk_list_store_new (1, G_TYPE_STRING);	/* label */
  list = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list_store));
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (list),
      0, "Instrument Type",
      gtk_cell_renderer_text_new (),
      "text", 0, NULL);

  /* this populates the first column */
  for(tmp=wdata->instruments; tmp; tmp=tmp->next)
    {
      gtk_list_store_append (list_store, &iter);
      gtk_list_store_set (list_store, &iter,
                          0, _(instrumenttypes[((InstrumentList *)tmp->data)->type]), -1);
    }
  gtk_box_pack_start (GTK_BOX (hbox), list, TRUE, TRUE, 0);


  //Instruments view
  GtkListStore *instrument_store = gtk_list_store_new (1, G_TYPE_STRING);	/* label */
  wdata->instrument_list = gtk_tree_view_new_with_model (GTK_TREE_MODEL (instrument_store));
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (wdata->instrument_list),
      0, "Instrument", gtk_cell_renderer_text_new (), "text", 0, NULL);

  gtk_box_pack_start (GTK_BOX (hbox), wdata->instrument_list, TRUE, TRUE, 0);

  wdata->cbdata->list_store = instrument_store;
  wdata->cbdata->instruments = wdata->instruments;

  GtkWidget *vbox = gtk_vbox_new(FALSE, 5);
  gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE,0);

  GtkWidget *addbutton = gtk_button_new_with_label(">");
  gtk_box_pack_start(GTK_BOX(vbox), addbutton, FALSE, TRUE,0);

  GtkWidget *removebutton = gtk_button_new_with_label("<");
  gtk_box_pack_start(GTK_BOX(vbox), removebutton, FALSE, TRUE,0);


  score_store =  gtk_list_store_new (1, G_TYPE_STRING);
  wdata->score_list =  gtk_tree_view_new_with_model (GTK_TREE_MODEL (score_store));
  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (wdata->score_list),
      0, "Score", gtk_cell_renderer_text_new (), "text", 0, NULL);

  gtk_box_pack_start (GTK_BOX (hbox), wdata->score_list, TRUE, TRUE, 0);


  wdata->sdata = (scoredata *)g_malloc0(sizeof(scoredata));
  wdata->sdata->selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(wdata->instrument_list));
  wdata->sdata->list_store = &instrument_store;
  wdata->sdata->score_list = score_store;

  g_signal_connect(G_OBJECT(addbutton), "clicked", G_CALLBACK(addinstrument), (gpointer) wdata); //add button callback
  g_signal_connect(G_OBJECT(removebutton), "clicked", G_CALLBACK(removeinstrument), (gpointer) wdata); //remove button callback
  GtkTreeSelection  *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(list));
  gtk_tree_selection_set_select_function(selection, cell_clicked, (gpointer) wdata, NULL);
  gtk_notebook_append_page (GTK_NOTEBOOK (wdata->notebook), hbox, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (wdata->notebook), hbox,  _("Instrument Setup"));
}

/**
 * Creates a page to setup the printed scores properties
 * @param notebook widget to attach the table to either GtkNotebook or Dialog->vbox
 * @param gui pointer to the DenemoGUI structure to get current information
 * @param isnotebook specifies if the widget passed is a notebook. Default is TRUE
 * @return the new paper setup 
 */
papersetupcb *
papersetup(GtkWidget *notebook, DenemoGUI *gui, gboolean isnotebook)
{
  gint i;

  papersetupcb *setup = (papersetupcb *) g_malloc0(sizeof(papersetupcb));
  GtkWidget *table = gtk_table_new(3, 4 , FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  GtkWidget *label = gtk_label_new(_("Paper Size"));
  gtk_table_attach(GTK_TABLE(table), label, 0, 1,0 ,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  GtkWidget *papersize = gtk_combo_box_entry_new_text();
  for(i=0; i < 6; i++)
    {
      gtk_combo_box_append_text(GTK_COMBO_BOX(papersize), papersizes[i]);
    }

  gtk_entry_set_text(GTK_ENTRY(GTK_BIN(papersize)->child), 
		    gui->lilycontrol.papersize->len? gui->lilycontrol.papersize->str:"");
  gtk_table_attach(GTK_TABLE(table), papersize,1,2,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new(_("Font Size"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  GtkWidget *fontsize = gtk_combo_box_entry_new_text();
  for(i=0; i < 8; i++)
    {
      gtk_combo_box_append_text(GTK_COMBO_BOX(fontsize), fontsizes[i]);
    }
  gchar *tmp;
  //tmp = g_strdup_printf( "%d", gui->lilycontrol.fontsize);
  gtk_entry_set_text (GTK_ENTRY (GTK_BIN(fontsize)->child),  gui->lilycontrol.staffsize->len?gui->lilycontrol.staffsize->str:"");
  //g_free(tmp);
  gtk_table_attach(GTK_TABLE(table), fontsize, 1,2,1,2,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new(_("Lilypond Version"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  GtkWidget *lilyversion = gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), lilyversion, 1,2,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  gtk_entry_set_text(GTK_ENTRY(lilyversion), gui->lilycontrol.lilyversion->len?
		     gui->lilycontrol.lilyversion->str:"");






  GtkWidget *vbox = gtk_vbox_new(FALSE,0);
  GtkWidget *portraitradio = 
    gtk_radio_button_new_with_label(NULL, _("Portrait"));
  gtk_box_pack_start(GTK_BOX(vbox), portraitradio, TRUE, TRUE,0);
  
  GtkWidget *landscaperadio = 
    gtk_radio_button_new_with_label
    (gtk_radio_button_group (GTK_RADIO_BUTTON (portraitradio)),_("Landscape"));
  gtk_box_pack_start(GTK_BOX(vbox), landscaperadio, TRUE, TRUE,0);

  if(gui->lilycontrol.orientation)
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(portraitradio), TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(landscaperadio), TRUE);
  gtk_table_attach(GTK_TABLE(table), vbox, 2,3,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  if(isnotebook)
    {
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
      gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
                                       _("Paper Setup"));
    }
  else
    {
#ifdef _USE_GTK3_
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(notebook)), table, TRUE, TRUE,0);
#else
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(notebook)->vbox), table, TRUE, TRUE,0);
#endif
    }

  setup->papersize = papersize;
  setup->fontsize = fontsize;
  setup->portrait = portraitradio;
  setup->lilyversion = lilyversion;

  score_status(gui, TRUE);
  return setup;
}

timekeysigcb *
timekeysig(wizarddata *wdata, gboolean isnotebook)
{
  DenemoGUI *gui = wdata->gui;
  GtkWidget *notebook = wdata->notebook;  
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  timekeysigcb *timekeysigdata = 
    (timekeysigcb *) g_malloc0(sizeof(timekeysigcb));
  keysig_callbackdata *cbdata = 
    (keysig_callbackdata *) g_malloc0(sizeof(keysig_callbackdata));
  modedata *mdata = cbdata->mdata = (modedata *) g_malloc0(sizeof(modedata)); 
  //combobox to hold pitches for modes
  GtkWidget *pitchescombo = gtk_combo_new (); 
  GtkWidget *combobox = gtk_combo_new ();	
  gint i;
  GList *majorlist = NULL;
  GList *minorlist = NULL;
  GList *modelist = NULL;

  if (!majorlist)
    for (i = 0; i < 15; i++)
      {
	majorlist = g_list_append (majorlist, majorkeys[i]);
	minorlist = g_list_append (minorlist, minorkeys[i]);
      }
  if (!modelist)
    for (i = 0; i < 7; i++)
      modelist = g_list_append (modelist, modes[i]);



  mdata->majorlist = majorlist;
  mdata->minorlist = minorlist;
  mdata->modelist = modelist;

  GtkWidget *table = gtk_table_new (3, 3 , FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);


  GtkWidget *label = gtk_label_new(_("Tempo"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  GtkWidget *tempo = gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), tempo, 1,2,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  //gtk_entry_set_text(GTK_ENTRY(lilyversion), gui->lilycontrol.lilyversion->str);
  /*timesig*/
  label = gtk_label_new (_("Enter desired time signature:"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  label = gtk_label_new (_("Numerator"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  GtkWidget *numerator = gtk_spin_button_new_with_range (1, 16, 1.0);
  gtk_table_attach(GTK_TABLE(table), numerator, 1,2,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  label = gtk_label_new (_("Denominator"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (numerator), 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (numerator),
			     (gdouble) curstaffstruct->timesig.time1);
  gtk_entry_set_activates_default (GTK_ENTRY (numerator), TRUE);

  GtkWidget *denominator = gtk_spin_button_new_with_range (1, 16, 1.0);
  gtk_table_attach(GTK_TABLE(table), denominator, 1,2,3,4,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (denominator), 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (denominator),
			     (gdouble) curstaffstruct->timesig.time2);
  gtk_entry_set_activates_default (GTK_ENTRY (denominator), TRUE);

  /*key signature*/
  
  mdata->combobox = combobox;
  mdata->dialog = table;
  mdata->pitchcombo = pitchescombo;

  label = gtk_label_new (_("Key Signature Setup:"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  GtkWidget *radiobutton1 = gtk_radio_button_new_with_label (NULL, _("Major"));
  gtk_signal_connect (G_OBJECT (radiobutton1), "clicked",
		      GTK_SIGNAL_FUNC (majorcallback), cbdata->mdata);
  gtk_table_attach(GTK_TABLE(table), radiobutton1, 0,1,5,6,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 

  gtk_widget_show (radiobutton1);

  GtkWidget *radiobutton2 = gtk_radio_button_new_with_label
    (gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1)), _("Minor"));
  gtk_signal_connect (G_OBJECT (radiobutton2), "clicked",
		      GTK_SIGNAL_FUNC (minorcallback), cbdata->mdata);
  gtk_table_attach(GTK_TABLE(table), radiobutton2, 1,2,5,6,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  gtk_widget_show (radiobutton2);

  GtkWidget *radiobutton3 = gtk_radio_button_new_with_label
    (gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1)), _("Mode"));

  gtk_signal_connect (G_OBJECT (radiobutton3), "clicked",
		      GTK_SIGNAL_FUNC (modedialog), cbdata->mdata);
  gtk_table_attach(GTK_TABLE(table), radiobutton3, 2,3,5,6,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
 
  gtk_widget_show (radiobutton3);
  
  gtk_table_attach(GTK_TABLE(table), combobox, 0,1,6,7,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  gtk_table_attach(GTK_TABLE(table), pitchescombo, 0,1,7,8,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  gtk_widget_hide (mdata->pitchcombo); 
    
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton1), TRUE);
  majorcallback (NULL, mdata);
  gtk_entry_set_text(GTK_ENTRY (GTK_COMBO (combobox)->entry),
		     majorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET]);
   
  if(isnotebook)
    {
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
      gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
                                       _("Key/Time signature"));
    }
  else
    {
#ifdef _USE_GTK3_
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(notebook)), table, TRUE, TRUE,0);
#else 
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(notebook)->vbox), table, TRUE, TRUE,0);
#endif
    }
    
  timekeysigdata->tempo = tempo;
  timekeysigdata->numerator = numerator;
  timekeysigdata->denominator = denominator;

  cbdata->gui = gui;
  cbdata->curstaffstruct = curstaffstruct;
  cbdata->combobox = combobox;
  cbdata->radiobutton2 = radiobutton2;
  cbdata->radiobutton3 = radiobutton3;
  cbdata->mode = pitchescombo;
  cbdata->mdata = mdata; 
  timekeysigdata->cbdata = cbdata;
  
  return timekeysigdata;
}



/**
 * Function to set the printed score parameters
 *
 */
void setpaperconfig(papersetupcb *cbdata, DenemoGUI *gui)
{
  g_string_assign(gui->lilycontrol.papersize, 
		  (gchar *) gtk_entry_get_text 
		  (GTK_ENTRY (GTK_BIN (cbdata->papersize)->child)));
  g_string_assign(gui->lilycontrol.lilyversion, 
		  (gchar *)gtk_entry_get_text 
		  (GTK_ENTRY (cbdata->lilyversion)));

  g_string_assign(gui->lilycontrol.staffsize,
    (gchar *) gtk_entry_get_text 
	 (GTK_ENTRY (GTK_BIN (cbdata->fontsize)->child)));
  if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cbdata->portrait)))
    {
      gui->lilycontrol.orientation = TRUE;
    }
  else
    gui->lilycontrol.orientation = FALSE;

  //g_print(" %s %s %d %d \n", gui->lilycontrol.papersize->str, gui->lilycontrol.lilyversion->str, gui->lilycontrol.fontsize, gui->lilycontrol.orientation);
  score_status(gui, TRUE);
}

/*
 * Create new staffs and label them with instrument names. 
 * 
 */
static void setstaffconfig(wizarddata *wdata, DenemoScore *si)
{
  wdata->staffnumber = gtk_tree_model_iter_n_children 
    (GTK_TREE_MODEL(wdata->sdata->score_list), NULL);
  gtk_tree_model_foreach(GTK_TREE_MODEL(wdata->sdata->score_list), 
			 foreach_func, wdata);
}

static void applykeytimesig_settings(wizarddata *wdata)
{
  DenemoScore *si = wdata->gui->si;
  timekeysigcb *tsetup = wdata->tsetup;
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;
  
  /*apply Tempo*/
  si->tempo = atoi( (gchar *) gtk_entry_get_text (GTK_ENTRY (tsetup->tempo)));
  /*apply timesig*/

  curstaffstruct->timesig.time1 = 
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (tsetup->numerator));
  curstaffstruct->timesig.time2 = 
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (tsetup->denominator));

  // wdata->gui->haschanged = TRUE;???????? why was this set???
  //barlength = PPQN * 4 * curstaffstruct->stime1 / curstaffstruct->stime2;

  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active 
    (GTK_TOGGLE_BUTTON (tsetup->cbdata->radiobutton2)) ?
    1 :
    gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (tsetup->cbdata->radiobutton3)) ?
    2 : 0;
  tokey = findkey (tsetup->cbdata->combobox, tsetup->cbdata->mdata, isminor);
 
  //printf("\nisminor = %i tokey = %i\n",isminor,tokey);
  if (isminor == 2)
    mode = findmode (tsetup->cbdata->mode, tsetup->cbdata->mdata);
  dnm_setinitialkeysig (curstaffstruct, tokey - mode, isminor);

}


/**
 * Set up the score based upon the configuration given
 * @param button the button that was pressed
 * @param data   the score wizard data
 */

static void setupscore(GtkButton *button, wizarddata *wdata)
{
  setpaperconfig(wdata->paper, wdata->gui);
  //applyheader_settings(wdata->hsetup,wdata->gui);
  applykeytimesig_settings(wdata);
  setstaffconfig(wdata, wdata->gui->si);
  score_status(wdata->gui, TRUE);
  set_bottom_staff(wdata->gui);      
  displayhelper(wdata->gui);
  /*free memory*/
  //g_free(wdata->tsetup->cbdata->mdata);
  g_free(wdata->tsetup->cbdata);
  g_free(wdata->tsetup);
  g_free(wdata->sdata);
  g_free(wdata->icbdata);
  g_free(wdata);
  
}

void notebook_page(gpointer data)
{
  wizarddata *wdata = (wizarddata *) data;
  gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(wdata->notebook));
  //printf("current page # = %i\n",page);
  gint max_pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(wdata->notebook));

  if (page == (max_pages -1)){
    gtk_widget_set_sensitive (wdata->nextbutton, FALSE);
    gtk_widget_set_sensitive (wdata->finishbutton, TRUE);
    }
  if (page < (max_pages -1)) 
    gtk_widget_set_sensitive (wdata->finishbutton, FALSE);
  if ((page >=0) && (page < (max_pages -1)))
    gtk_widget_set_sensitive (wdata->nextbutton, TRUE);
  if (page == 0)
    gtk_widget_set_sensitive (wdata->backbutton, FALSE);
  if (page > 0)
    gtk_widget_set_sensitive (wdata->backbutton, TRUE);
}

/**
 * Top-level function for score creation 
 *
 */
void scorewizard(GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if (gui->notsaved)
    {
      if (confirmbox (gui))
        deletescore (NULL, gui);
      else
        return;
    }
  else
    deletescore (NULL, gui);

  wizarddata *wdata = (wizarddata *) g_malloc0 (sizeof (wizarddata));
  wdata->icbdata = (instsdata *) g_malloc0 (sizeof (instsdata));


  wdata->gui = (DenemoGUI *) gui;
  wdata->staffnumber = 0;
  wdata->currentstaffnumber = 1;
  wdata->instruments = NULL;
  wdata->icbdata->instruments = parseInstruments (wdata->instruments);

  GtkWidget *dialog = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog), _("Score Configuration Wizard"));
  wdata->notebook = gtk_notebook_new ();
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK (wdata->notebook), FALSE);
#ifdef _USE_GTK3_  
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)), wdata->notebook,
                      TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), wdata->notebook,
                      TRUE, TRUE, 0);
#endif
  /* create pages of notebook */
  wdata->paper = papersetup (wdata->notebook, gui, TRUE);
  //wdata->hsetup = headersetup (wdata->notebook, gui, TRUE);
  instrumentsetup(wdata);
  wdata->tsetup = timekeysig (wdata, TRUE);

  /* create buttons underneath*/
  wdata->backbutton = gtk_button_new_from_stock ("gtk-go-back");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), 
		     wdata->backbutton, FALSE, FALSE,0);
  g_signal_connect_swapped(G_OBJECT(wdata->backbutton), "clicked", 
			   G_CALLBACK (gtk_notebook_prev_page), 
			   G_OBJECT(wdata->notebook) );
  g_signal_connect_swapped(G_OBJECT(wdata->backbutton), "clicked", 
			   G_CALLBACK (notebook_page), (gpointer) wdata );
  gtk_widget_set_sensitive (wdata->backbutton, FALSE);
  //start the backbutton shadwoed out

  wdata->nextbutton = gtk_button_new_from_stock ("gtk-go-forward");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), 
		     wdata->nextbutton, FALSE, FALSE,0);
  g_signal_connect_swapped(G_OBJECT(wdata->nextbutton), "clicked", 
			   G_CALLBACK (gtk_notebook_next_page), 
			   G_OBJECT(wdata->notebook) );
  g_signal_connect_swapped(G_OBJECT(wdata->nextbutton), "clicked", 
			   G_CALLBACK (notebook_page), (gpointer) wdata);


  wdata->finishbutton = gtk_button_new_from_stock ("gtk-apply");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), 
		     wdata->finishbutton, FALSE, FALSE,0);
  g_signal_connect(G_OBJECT(wdata->finishbutton), "clicked", 
		   G_CALLBACK (setupscore),  (gpointer)wdata);
  g_signal_connect_swapped(G_OBJECT(wdata->finishbutton), "clicked",
			   G_CALLBACK (gtk_widget_destroy), G_OBJECT(dialog));
  gtk_widget_set_sensitive (wdata->finishbutton, FALSE);
  
  GtkWidget *cancelbutton = gtk_button_new_from_stock ("gtk-cancel");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area), 
		     cancelbutton, FALSE, FALSE,0);
  g_signal_connect_swapped(G_OBJECT(cancelbutton), "clicked", 
			   G_CALLBACK (gtk_widget_destroy), G_OBJECT(dialog));

 
  gtk_widget_queue_draw (Denemo.scorearea);
  gtk_widget_show_all(dialog);
     
  g_list_free(wdata->instruments);
  score_status(gui, FALSE);/* clear the change count */
}


