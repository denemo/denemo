/* texteditors.c
 * text editors for editing scripts, adding comments etc
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2009  Richard Shann
 * 
 */


#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include "texteditors.h"
#include "view.h"


/* returns newly allocated string containing current Scheme in the ScriptView
 caller must free
*/
gchar *getSchemeText(void) {
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  return gtk_text_buffer_get_text(buffer, &startiter, &enditer, FALSE);

}

/* execute the script that is in the Scheme script window */
void executeScript(void) {
  gchar *text = getSchemeText();
  g_debug("Calling script %s\n", text);
  (void)call_out_to_guile(text);
  g_free(text);
}

/* Return number of characters in Scheme script */
gint getNumCharsSchemeText(void) {
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  return gtk_text_buffer_get_char_count(buffer);
}

void deleteSchemeText(void) {
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
    gtk_text_buffer_delete (buffer, &startiter, &enditer);
}

void appendSchemeText(gchar *text) {
  GtkTextIter enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer((GtkTextView*)(Denemo.ScriptView));
  gtk_text_buffer_get_end_iter (buffer,  &enditer);
  gtk_text_buffer_insert(buffer, &enditer, text, -1);
}

static gint
hide_scheme (GtkAction * action, GdkEvent*event,  GtkWidget *w) {
  activate_action("/MainMenu/ViewMenu/ToggleScript");
  return TRUE;
}


static void save_scheme_text_as(GtkWidget *widget, GtkWidget *textview) {
  gchar **pfilename = g_object_get_data(G_OBJECT(textview), "pfilename");
  gchar *text = getSchemeText();
  GtkWidget *label;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  GtkWidget *dialog = gtk_file_chooser_dialog_new("Save Scheme Text as...",
						  NULL/*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.ScriptView), GTK_TEXT_WINDOW_WIDGET))*/,
						  GTK_FILE_CHOOSER_ACTION_SAVE,
						  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						  GTK_STOCK_SAVE, GTK_RESPONSE_OK, NULL);

  if(gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK) {
       g_free(*pfilename);
	*pfilename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

	if(g_file_test(*pfilename, G_FILE_TEST_EXISTS)) {
		gtk_widget_destroy(dialog);
		dialog = gtk_dialog_new_with_buttons("File already exists",
						     NULL/*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.ScriptView), GTK_TEXT_WINDOW_WIDGET))*/,
						     GTK_DIALOG_DESTROY_WITH_PARENT,
						     GTK_STOCK_OK,
						     GTK_RESPONSE_OK,
						     GTK_STOCK_CANCEL,
						     GTK_RESPONSE_CANCEL,
						     NULL);
		gchar *labeltext = g_strconcat("\nThe file ", *pfilename, " already exists.\n Do you want to overwrite it?\n\n", NULL);
		label = gtk_label_new(labeltext);
		g_free(labeltext);
		gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
		gtk_widget_show_all(dialog);

		if(gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_CANCEL) {
			g_free(text);
			gtk_widget_destroy(dialog);
			return;
		}
	}			
	g_file_set_contents(*pfilename, text, -1, NULL);
	gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(buffer), FALSE);
  }
  g_free(text);
  gtk_widget_destroy(dialog);
}


static void save_scheme_text(GtkWidget *widget, GtkWidget *textview) {
  gchar **pfilename = g_object_get_data(G_OBJECT(textview), "pfilename");	  
  GtkTextBuffer *buffer;
  if(*pfilename == NULL)
    save_scheme_text_as(NULL, textview);
  else {
    gchar *text = getSchemeText();
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
    g_file_set_contents(*pfilename, text, -1, NULL);
    gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(buffer), FALSE);
    g_free(text);
  }
}

gboolean save_scheme_dialog(GtkTextBuffer *buffer, GtkWidget *textview) {
  gchar **pfilename = g_object_get_data(G_OBJECT(textview), "pfilename");
  GtkWidget *dialog;
  GtkWidget *label;
  gint response;
  GtkWidget *contentArea;

  if(gtk_text_buffer_get_modified(buffer)) {
	  dialog = gtk_dialog_new_with_buttons("Scheme text changed",
					       NULL/*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.ScriptView), GTK_TEXT_WINDOW_WIDGET))*/,
						GTK_DIALOG_DESTROY_WITH_PARENT,
						GTK_STOCK_YES,
						GTK_RESPONSE_YES,
						GTK_STOCK_NO,
						GTK_RESPONSE_NO,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_CANCEL,
						NULL);
	  contentArea = GTK_DIALOG(dialog)->vbox/*gtk_dialog_get_content_area(GTK_DIALOG(dialog))*/;
	  if(*pfilename == NULL) 
		  label = gtk_label_new("\nDo you want to save the changes in a new file?\n\n");
	  else {
	    gchar *labeltext = g_strconcat("\nDo you want to save the changes in ", *pfilename, "?\n\n", NULL);
	    label = gtk_label_new(labeltext);
	    g_free(labeltext);
	  }
	  gtk_container_add(GTK_CONTAINER(contentArea), label);
	  gtk_widget_show_all(dialog);


	  response = gtk_dialog_run(GTK_DIALOG(dialog));
	  gtk_widget_destroy(dialog);
	  if(response == GTK_RESPONSE_YES)
		  save_scheme_text(NULL, textview);
	  else if(response == GTK_RESPONSE_CANCEL)
		  return FALSE;

  }
  return TRUE;
}

	

static void load_scheme_from_file(GtkWidget *widget, GtkWidget *textview) {
  gchar **pfilename = g_object_get_data(G_OBJECT(textview), "pfilename");	
  gchar *text = NULL;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  GtkWidget *dialog = gtk_file_chooser_dialog_new("Open File",
						  NULL/*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.ScriptView),GTK_TEXT_WINDOW_WIDGET))*/,
						GTK_FILE_CHOOSER_ACTION_OPEN,
						GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						GTK_STOCK_OPEN, GTK_RESPONSE_OK, NULL);

  if(!save_scheme_dialog(buffer, textview)) {
	  return;
  }


  if(gtk_dialog_run((GTK_DIALOG(dialog))) == GTK_RESPONSE_OK) { 
         if(*pfilename) g_free(*pfilename);
	  *pfilename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
	  g_file_get_contents(*pfilename, &text, NULL, NULL);
	  gtk_text_buffer_set_text(GTK_TEXT_BUFFER(buffer), text, -1);
	  gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(buffer), FALSE);
  }

  g_free(text);
  gtk_widget_destroy(dialog);

}

void clear_scheme_window(GtkWidget *widget, GtkWidget *textview) {
  gchar **pfilename = g_object_get_data(G_OBJECT(textview), "pfilename");
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(Denemo.ScriptView));
  if(!save_scheme_dialog(buffer, textview))
    return;
  
  g_free(*pfilename);
  *pfilename = NULL; 
  gtk_text_buffer_set_text(GTK_TEXT_BUFFER(buffer), "", 0);
  gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(buffer), FALSE);
}

/*
 create_editor_window()
 create a text window for editing

*/

static GtkWidget * create_editor_window(void) {
  GtkWidget *TextView;
  GtkSourceBuffer *buffer;
  GtkSourceLanguageManager *LanguageManager = gtk_source_language_manager_get_default();
  //GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *menu, *menuBar, *fileMenu, *item;
  gchar **filename = g_malloc0(sizeof(gchar *));

  
  buffer = gtk_source_buffer_new (NULL); 
  gtk_source_buffer_set_highlight_syntax(GTK_SOURCE_BUFFER(buffer), TRUE);
  gtk_source_buffer_set_language(GTK_SOURCE_BUFFER(buffer),
				 gtk_source_language_manager_get_language(LanguageManager, "scheme"));
  gtk_source_buffer_set_highlight_matching_brackets(GTK_SOURCE_BUFFER(buffer), TRUE);

  TextView = GTK_WIDGET(gtk_source_view_new_with_buffer(GTK_SOURCE_BUFFER(buffer)));

  g_object_set_data(G_OBJECT(TextView), "pfilename", filename);
  gtk_source_view_set_show_line_numbers(TextView, TRUE);
  gtk_source_view_set_auto_indent(TextView, TRUE);
  gtk_source_view_set_indent_on_tab(TextView, TRUE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(TextView), GTK_WRAP_CHAR);


  GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (w), "Denemo Scheme Script");
  //gtk_window_set_resizable (GTK_WINDOW (w), TRUE);
  g_signal_connect(G_OBJECT(w), "delete-event", G_CALLBACK(hide_scheme/*gtk_widget_hide_on_delete*/), w);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (w), main_vbox);
  
  w = gtk_button_new_with_label("Execute Script");
  g_signal_connect(G_OBJECT(w), "clicked",  G_CALLBACK(executeScript), NULL);
  gtk_box_pack_start (GTK_BOX (main_vbox), w, FALSE, TRUE, 0);

    menu = gtk_menu_new();

  item = gtk_menu_item_new_with_label("New");
  gtk_menu_append(GTK_MENU(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(clear_scheme_window), (gpointer)TextView);

  item = gtk_menu_item_new_with_label("Open");
  gtk_menu_append(GTK_MENU(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(load_scheme_from_file), (gpointer)TextView);

  item = gtk_menu_item_new_with_label("Save");
  gtk_menu_append(GTK_MENU(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(save_scheme_text), (gpointer)TextView);

  item = gtk_menu_item_new_with_label("Save as...");
  gtk_menu_append(GTK_MENU(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(save_scheme_text_as), (gpointer)TextView);

  fileMenu = gtk_menu_item_new_with_label("File");
  gtk_widget_show(fileMenu);

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(fileMenu), menu);
  gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(buffer), FALSE);

  menuBar = gtk_menu_bar_new();

  gtk_box_pack_start(GTK_BOX(main_vbox), GTK_WIDGET(menuBar), FALSE, TRUE, 0);
  gtk_widget_show(menuBar);
  gtk_menu_bar_append(GTK_MENU_BAR(menuBar), fileMenu);


  GtkWidget *wid = gtk_check_button_new();
  gtk_action_connect_proxy(gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/RecordScript"), wid);
  gtk_box_pack_start (GTK_BOX (main_vbox), wid, FALSE, TRUE, 0);
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (main_vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), TextView);
  return TextView;
}


void create_scheme_window(void) {
  Denemo.ScriptView = create_editor_window ();
}
