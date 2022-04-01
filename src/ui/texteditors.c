/* texteditors.c
 * text editors for editing scripts, adding comments etc
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2009  Richard Shann
 *
 */

#include <gtksourceview/gtksource.h>

#if GTK_MAJOR_VERSION==3
#include <gdk/gdkkeysyms-compat.h>      //FIXME Look for something more gtk3 like
#else
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourcestyleschememanager.h>
#include <gtksourceview/gtksourceprintcompositor.h>
#include <gtksourceview/gtksourceiter.h>
#include <gtksourceview/gtksourcebuffer.h>
#endif
#include "ui/texteditors.h"
#include "core/view.h"
#include "scripting/scheme-callbacks.h"
#include "core/menusystem.h"

static GtkWidget *SchemeWindow;
static void find_cb (DenemoAction * action, gpointer user_data);

static void replace_cb (DenemoAction * action, gpointer user_data);

/* returns newly allocated string containing current Scheme in the script_view
 caller must free
*/
gchar *
get_script_view_text (void)
{
  GtkTextIter startiter, enditer;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
  gtk_text_buffer_get_start_iter (buffer, &startiter);
  gtk_text_buffer_get_end_iter (buffer, &enditer);
  return gtk_text_buffer_get_text (buffer, &startiter, &enditer, FALSE);
}
/* create a palette button for the script that is in the Scheme script window */
static void
createButton (void)
{
  gchar *text = Denemo.project->script;
  if(text && *text)
     {
        DenemoPalette *pal = NULL;
        gchar *name = choose_palette_by_name (TRUE, FALSE);
        if (name)
            pal = create_palette (name, FALSE, TRUE);
        if(pal) {
            gchar *button_name = _("My Script");
            gchar *label = string_dialog_entry (Denemo.project, _("Palette Button Creation"), _("Give a (unique) name for the button"), button_name);
            if (label)
                {
                    if (!palette_add_button (pal, label, _("Creates a button for the script"), text))
                        warningdialog (_("Could not create a button of that name in that palette"));
                }
            else
                warningdialog (_("Cancelled"));
            g_free (label);
            gtk_widget_show_all (gtk_widget_get_parent(pal->box));
            }
    }
    else
        g_warning (_("Empty scheme script"));
}
static void
helpCreateMenuItem (void)
{
  gchar *text = Denemo.project->script;
  if(text && *text)
		infodialog (_("To create a new command in the menu system that will execute the Scheme script in the pane below follow these steps:\n\
 1) Choose an already existing command in the menu system which you want your new command to come after.\n\
 2) Right Click on that command and select the option to create a new menu item\n\
 3) Give a unique name for this command (hint: prefix the command with your own initials)\n\
 4) Give the label and then the tooltip for the command\n\
 5) When exiting Denemo choose to save you new command(s)."));
	else 
		warningdialog (_("No script in the Scheme Window"));
}
/* execute the script that is in the Scheme script window */
void
executeScript (void)
{
  gchar *text = Denemo.project->script;
  if(text && *text)
    {
    g_debug ("Calling script %s\n", text);
    stage_undo (Denemo.project->movement, ACTION_STAGE_END);        //undo is a queue so this is the end :)
    (void) call_out_to_guile (text);
    stage_undo (Denemo.project->movement, ACTION_STAGE_START);
    }
    else
        g_warning ("Trying to execute empty scheme script");
}


/* execute the line of scheme script that is in the Scheme CLI */
static void
executeCLI (GtkWidget * button, GtkEntry * entry)
{
  gchar *display = NULL;
  if (entry)
    {
#ifndef POPUP_RESULT_VIA_GUI      
      display = g_strdup_printf ("%s%s%s", "(format #t \"~%=> ~A~%\"", gtk_entry_get_text (entry), ")\n");
#else
      display = g_strdup_printf ("(d-WarningDialog (format #f \"The expression evaluates to:~%~A\" %s))",  gtk_entry_get_text (entry));
#endif
     //g_print ("Passing to scheme: %s\n", display);
      (void) call_out_to_guile (display);
      g_free (display);
    }
  else
    g_critical ("entry is NULL!!!!");
}

/* Return number of characters in Scheme script */
//TODO: Avoid to use gtk widgets for this since it can be used in non
//interactive mode.
gint
getNumCharsSchemeText (void)
{
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
  return gtk_text_buffer_get_char_count (buffer);
}

void
deleteSchemeText (void)
{
  if(!Denemo.non_interactive){
    GtkTextIter startiter, enditer;
    GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
    gtk_text_buffer_get_start_iter (buffer, &startiter);
    gtk_text_buffer_get_end_iter (buffer, &enditer);
    gtk_text_buffer_delete (buffer, &startiter, &enditer);
  }

 g_free(Denemo.project->script);
 Denemo.project->script = NULL;
 Denemo.project->has_script = FALSE;
}

void appendSchemeText (gchar * text)
{
  if(!Denemo.non_interactive){
    GtkTextIter enditer;
    GtkTextBuffer *buffer = gtk_text_view_get_buffer ((GtkTextView *) (Denemo.script_view));
    gtk_text_buffer_get_end_iter (buffer, &enditer);
    while (g_ascii_isspace (*text)) text++;
    gtk_text_buffer_insert (buffer, &enditer, text, -1);
  }

  else{
    if(Denemo.project->script){
      gchar* old_script = Denemo.project->script;
      Denemo.project->script = g_strconcat(old_script, text, NULL);
      g_free(old_script);
    }
    else
      Denemo.project->script = text;
  }
}

static gint
hide_scheme (DenemoAction * action, GdkEvent * event, GtkWidget * w)
{

  if(Denemo.ScriptRecording) {
        denemo_action_activate (denemo_menusystem_get_action (RecordScript_STRING));
        infodialog(_("Turning off Recording scheme"));
    }
  set_toggle (ToggleScript_STRING, FALSE);//              toggle_scheme ();//activate_action ("ToggleScript");
  return TRUE;
}


static void
save_scheme_text_as (GtkWidget * widget, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  gchar *text = get_script_view_text ();
  GtkWidget *label;
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
  GtkWidget *dialog = gtk_file_chooser_dialog_new (_("Save Scheme Text as..."),
                                                   NULL /*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.script_view), GTK_TEXT_WINDOW_WIDGET)) */ ,
                                                   GTK_FILE_CHOOSER_ACTION_SAVE,
                                                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                                                   _("_Save"), GTK_RESPONSE_OK, NULL);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK)
    {
      g_free (*pfilename);
      *pfilename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

      if (g_file_test (*pfilename, G_FILE_TEST_EXISTS))
        {
          gtk_widget_destroy (dialog);
          dialog = gtk_dialog_new_with_buttons (_("File already exists"),  //FIXME I think there is a function to do this already.
                                                NULL /*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.script_view), GTK_TEXT_WINDOW_WIDGET)) */ ,
                                                GTK_DIALOG_DESTROY_WITH_PARENT, _("_OK"), GTK_RESPONSE_OK, _("_Cancel"), GTK_RESPONSE_CANCEL, NULL);
          gchar* labeltext = g_strdup_printf(_("The file %s already exists.\nDo you want to overwrite it?"), *pfilename);
          label = gtk_label_new (labeltext);
          g_free (labeltext);
          GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
          gtk_container_add (GTK_CONTAINER (content_area), label);
          gtk_widget_show_all (dialog);

          if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_CANCEL)
            {
              g_free (text);
              gtk_widget_destroy (dialog);
              return;
            }
        }
      g_file_set_contents (*pfilename, text, -1, NULL);
      gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
    }
  g_free (text);
  gtk_widget_destroy (dialog);
}


static void
save_scheme_text (GtkWidget * widget, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  GtkTextBuffer *buffer;
  if (*pfilename == NULL)
        {
          warningdialog (_ ("If you have loaded this script from a menu item or palette button then you must save it using a right click on that same menu item or palette button (and choosing \"Save Script from Scheme Window\").\nOtherwise use \"Save As\" from this menu."));
        }
  else
    { 
		
      gchar *text = get_script_view_text ();
      		if (confirm (*pfilename, (text && *text) ? _("Overwrite this file?") : _("Blank out this file?")))
      		{
			  buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
			  g_file_set_contents (*pfilename, text, -1, NULL);
			  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
			  infodialog (_("Saved File"));
			 } else
			 warningdialog (_("Cancelled"));
      g_free (text);
    }
}

gboolean
save_scheme_dialog (GtkTextBuffer * buffer, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  GtkWidget *dialog;
  GtkWidget *label;
  gint response;
  GtkWidget *content_area;

  if (gtk_text_buffer_get_modified (buffer))
    {
      dialog = gtk_dialog_new_with_buttons (_("Scheme text changed"), NULL /*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.script_view), GTK_TEXT_WINDOW_WIDGET)) */ ,
                                            GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_YES, GTK_RESPONSE_YES, GTK_STOCK_NO, GTK_RESPONSE_NO, _("_Cancel"), GTK_RESPONSE_CANCEL, NULL);


      if (*pfilename == NULL)
        label = gtk_label_new (_("\nDo you want to save the changes in a new file?\n\n"));
      else
        {
          gchar* labeltext = g_strdup_printf(_("\nDo you want to save the changes in %s ?\n\n"), *pfilename);
          label = gtk_label_new (labeltext);
          g_free (labeltext);
        }

      content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
      gtk_container_add (GTK_CONTAINER (content_area), label);

      gtk_widget_show_all (dialog);

      response = gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      if (response == GTK_RESPONSE_YES)
        save_scheme_text (NULL, textview);
      else if (response == GTK_RESPONSE_CANCEL)
        return FALSE;

    }
  return TRUE;
}

static void load_script_file (gchar *filename, GtkTextBuffer *buffer, gchar *init)
        {
          gchar *text = NULL;
          g_file_get_contents (filename, &text, NULL, NULL);
          if (text==NULL)
            {
              if (init==NULL)
                {
                  g_critical ("bad call to load_script_file");
                  return;
                }
              text = g_strdup(init);
            }
          gtk_window_set_title (GTK_WINDOW (SchemeWindow), g_strdup_printf("%s ; %s", _("Denemo Scheme Script"), filename));
          gtk_text_buffer_set_text (GTK_TEXT_BUFFER (buffer), text, -1);
          gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
          g_free (text);
        }

static void
load_scheme_from_file (GtkWidget * widget, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
 
  if (*pfilename && (!save_scheme_dialog (buffer, textview)))
    {
      return;
    } 
  
  GtkWidget *dialog = gtk_file_chooser_dialog_new (_("Open File"),
                                                   NULL /*GTK_WINDOW(gtk_text_view_get_window(GTK_TEXT_VIEW(Denemo.script_view),GTK_TEXT_WINDOW_WIDGET)) */ ,
                                                   GTK_FILE_CHOOSER_ACTION_OPEN,
                                                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                                                   _("_Open"), GTK_RESPONSE_OK, NULL);

  if (gtk_dialog_run ((GTK_DIALOG (dialog))) == GTK_RESPONSE_OK)
    {
      if (*pfilename)
        g_free (*pfilename);
      *pfilename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      load_script_file (*pfilename, buffer, NULL);
    }
  gtk_widget_destroy (dialog);
}
static void
open_initialization_script (GtkWidget * widget, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
  if (*pfilename && (!save_scheme_dialog (buffer, textview)))
    {
      return;
    }
  g_free (*pfilename);
  *pfilename = g_build_filename (get_user_data_dir (TRUE), "actions", "denemo.scm", NULL);
  load_script_file (*pfilename, buffer, ";this script will be run each time you open a score or on starting a new, blank score\n");

}




void
clear_scheme_window (GtkWidget * widget, GtkWidget * textview)
{
  gchar **pfilename = g_object_get_data (G_OBJECT (textview), "pfilename");
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (Denemo.script_view));
  if (*pfilename && (!save_scheme_dialog (buffer, textview)))
    return;
  g_free (*pfilename);
  *pfilename = NULL;
  gtk_window_set_title (GTK_WINDOW (SchemeWindow), g_strdup_printf("%s", _("Denemo Scheme Script")));
  gtk_text_buffer_set_text (GTK_TEXT_BUFFER (buffer), "", 0);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
}


static gboolean
keypress (GtkEntry * w, GdkEventKey * event)
{
  if (event->keyval == GDK_Return)
    executeCLI (NULL, w);
  //let the normal handler have the keypress
  return FALSE;
}

static void
scheme_changed_cb (GtkSourceBuffer *buffer){
  if(Denemo.project->script)
    g_free(Denemo.project->script);
  Denemo.project->script = get_script_view_text();
}


static void
toggle_record_script (DenemoAction * action, gpointer param)
{
  if (!gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.script_view)))
    toggle_scheme ();
  Denemo.ScriptRecording = !Denemo.ScriptRecording;
}

/*
 create_editor_window()
 create a text window for editing Scheme
*/

static GtkWidget *
create_editor_window (void)
{
  GtkWidget *TextView;
  GtkSourceBuffer *buffer;
  GtkSourceLanguageManager *LanguageManager = gtk_source_language_manager_get_default ();
  //GtkWidget *w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *menu, *menuBar, *fileMenu, *item;
  gchar **filename = g_malloc0 (sizeof (gchar *));
  GtkSourceLanguage* language = NULL;


  buffer = gtk_source_buffer_new (NULL);
  g_signal_connect (G_OBJECT (buffer), "changed", G_CALLBACK (scheme_changed_cb), NULL);

  gtk_source_buffer_set_highlight_syntax (GTK_SOURCE_BUFFER (buffer), TRUE);
  language = gtk_source_language_manager_get_language (LanguageManager, "scheme");
  gtk_source_buffer_set_language (GTK_SOURCE_BUFFER (buffer), language);
  gtk_source_buffer_set_highlight_matching_brackets (GTK_SOURCE_BUFFER (buffer), TRUE);

  TextView = GTK_WIDGET (gtk_source_view_new_with_buffer (GTK_SOURCE_BUFFER (buffer)));

  g_object_set_data (G_OBJECT (TextView), "pfilename", filename);
  gtk_source_view_set_show_line_numbers (GTK_SOURCE_VIEW (TextView), TRUE);
  gtk_source_view_set_auto_indent (GTK_SOURCE_VIEW (TextView), TRUE);
  gtk_source_view_set_indent_on_tab (GTK_SOURCE_VIEW (TextView), TRUE);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (TextView), GTK_WRAP_CHAR);

  GtkWidget *w = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  SchemeWindow = w;
  gtk_window_set_title (GTK_WINDOW (w), _("Denemo Scheme Script"));
  //gtk_window_set_resizable (GTK_WINDOW (w), TRUE);
  g_signal_connect (G_OBJECT (w), "delete-event", G_CALLBACK (hide_scheme /*gtk_widget_hide_on_delete */ ), w);
  g_signal_connect (G_OBJECT (SchemeWindow), "key_press_event", G_CALLBACK (window_keypress_event), NULL);

  
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (w), main_vbox);

  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  w = gtk_button_new_with_label ("CLI: ");
  gtk_widget_set_tooltip_text (w, _( "A Scheme command line interface. Type an expression here and press Enter to evaluate and display the result in the terminal. (On windows, use denemo-console.exe to get a terminal)"));
  GtkWidget *button = w;
  //gtk_widget_set_can_default(w, TRUE);
  //GTK_WIDGET_SET_FLAGS(window, GTK_CAN_DEFAULT);
  //gtk_window_set_default (window, w);
  gtk_box_pack_start (GTK_BOX (hbox), w, FALSE, TRUE, 0);
  w = gtk_entry_new ();
  GtkWidget *entry = w;
  //gtk_entry_set_activates_default (w,TRUE);
  gtk_box_pack_start (GTK_BOX (hbox), w, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (executeCLI), entry);
  g_signal_connect (G_OBJECT (entry), "key-press-event", G_CALLBACK (keypress), NULL);

  GtkWidget *inner_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), inner_hbox, FALSE, TRUE, 0);

  w = gtk_button_new_with_label (_("Execute Script"));
  gtk_widget_set_tooltip_text (w, _("Executes the Scheme in the script window below. If an error is thrown this will give a message in the terminal."));
  g_signal_connect (G_OBJECT (w), "clicked", G_CALLBACK (executeScript), NULL);
  gtk_box_pack_start (GTK_BOX (inner_hbox), w, FALSE, FALSE, 0);

  w = gtk_button_new_with_label (_("Create Button"));
  gtk_widget_set_tooltip_text (w, _("Create a palette button for the Scheme in the script window below."));
  g_signal_connect (G_OBJECT (w), "clicked", G_CALLBACK (createButton), NULL);
  gtk_box_pack_start (GTK_BOX (inner_hbox), w, FALSE, FALSE, 0);

  w = gtk_button_new_with_label (_("Create Menu Item"));
  gtk_widget_set_tooltip_text (w, _("Steps you through creation of a Denemo command in the menu system to execute the Scheme script."));
  g_signal_connect (G_OBJECT (w), "clicked", G_CALLBACK (helpCreateMenuItem), NULL);
  gtk_box_pack_start (GTK_BOX (inner_hbox), w, FALSE, FALSE, 0);
  menu = gtk_menu_new ();

  item = gtk_menu_item_new_with_label (_("New"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);  
  gtk_widget_set_tooltip_text (item, _( "Erase the current script"));

  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (clear_scheme_window), (gpointer) TextView);

  item = gtk_menu_item_new_with_label (_("Open"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_set_tooltip_text (item, _( "Load a script file"));

  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (load_scheme_from_file), (gpointer) TextView);

  item = gtk_menu_item_new_with_label (_("Open Initialization Script"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_set_tooltip_text (item, _( "Load the script file which will be executed on each newly opened score"));

  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (open_initialization_script), (gpointer) TextView);

  item = gtk_menu_item_new_with_label (_("Save"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_set_tooltip_text (item, _( "Save this script to the file it was loaded from"));

  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (save_scheme_text), (gpointer) TextView);

  item = gtk_menu_item_new_with_label (_("Save as…"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_set_tooltip_text (item, _( "Save this as a new file"));

  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (save_scheme_text_as), (gpointer) TextView);

  item = gtk_menu_item_new_with_label (_("Find"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (find_cb), (gpointer) TextView);
  item = gtk_menu_item_new_with_label (_("Replace"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (replace_cb), (gpointer) TextView);



  fileMenu = gtk_menu_item_new_with_label (_("File"));
  gtk_widget_show (fileMenu);

  gtk_menu_item_set_submenu (GTK_MENU_ITEM (fileMenu), menu);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);

  menuBar = gtk_menu_bar_new ();

  gtk_box_pack_start (GTK_BOX (main_vbox), menuBar, FALSE, FALSE, 0);
  gtk_widget_show (menuBar);
  gtk_menu_shell_append (GTK_MENU_SHELL (menuBar), fileMenu);

  inner_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), inner_hbox, FALSE, FALSE, 0);
  GtkWidget *wid = gtk_check_button_new_with_label (_("Record Scheme Script"));
  gtk_widget_set_tooltip_text (wid, _("Start recording commands into the Scheme script text window"));
  g_signal_connect (G_OBJECT (wid), "toggled", G_CALLBACK (toggle_record_script), NULL);

  gtk_box_pack_start (GTK_BOX (inner_hbox), wid, FALSE, FALSE, 0);

  GtkWidget *sw = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  gtk_box_pack_start (GTK_BOX (main_vbox), sw, TRUE, TRUE, 0);

  gtk_container_add (GTK_CONTAINER (sw), TextView);
  return TextView;
}


void
create_scheme_window (void)
{
  Denemo.script_view = create_editor_window ();
}



/* Buffer action callbacks ------------------------------------------------------------ */
#if  GTK_MAJOR_VERSION==3
static struct
{
  char *what;
  char *replacement;
  GtkTextSearchFlags flags;
} search_data =
{
NULL, NULL, GTK_TEXT_SEARCH_TEXT_ONLY};
#else
static struct
{
  char *what;
  char *replacement;
  GtkSourceSearchFlags flags;
} search_data =
{
NULL, NULL, GTK_SOURCE_SEARCH_CASE_INSENSITIVE};
#endif
#if GTK_MAJOR_VERSION==3
static gboolean
search_dialog (GtkWidget * widget, gboolean replace, char **what_p, char **replacement_p, GtkTextSearchFlags * flags_p)
#else
static gboolean
search_dialog (GtkWidget * widget, gboolean replace, char **what_p, char **replacement_p, GtkSourceSearchFlags * flags_p)
#endif
{
  GtkWidget *dialog;
  GtkEntry *entry1, *entry2;

  dialog = gtk_dialog_new_with_buttons (replace ? _("Replace") : _("Find"), GTK_WINDOW (gtk_widget_get_toplevel (widget)), GTK_DIALOG_MODAL, _("_Cancel"), GTK_RESPONSE_CANCEL, _("_OK"), GTK_RESPONSE_OK, NULL);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  entry1 = g_object_new (GTK_TYPE_ENTRY, "visible", TRUE, "text", search_data.what ? search_data.what : "", "activates-default", TRUE, NULL);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (content_area), vbox);
  gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET (entry1), TRUE, TRUE, 0);
  entry2 = g_object_new (GTK_TYPE_ENTRY, "visible", replace, "text", search_data.replacement ? search_data.replacement : "", "activates-default", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET (entry2), TRUE, TRUE, 0);

#if GTK_MAJOR_VERSION==2
  GtkToggleButton *case_sensitive;
  case_sensitive = g_object_new (GTK_TYPE_CHECK_BUTTON, "visible", TRUE, "label", "Case sensitive", "active", !(search_data.flags & GTK_SOURCE_SEARCH_CASE_INSENSITIVE), NULL);

  gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET (case_sensitive), FALSE, FALSE, 0);
#endif
  gtk_widget_show_all (dialog);

  while (TRUE)
    {
      if (gtk_dialog_run (GTK_DIALOG (dialog)) != GTK_RESPONSE_OK)
        {
          gtk_widget_destroy (dialog);
          return FALSE;
        }

      if (*gtk_entry_get_text (entry1))
        break;
    }

  g_free (search_data.what);
  *what_p = search_data.what = g_strdup (gtk_entry_get_text (entry1));
  g_free (search_data.replacement);
  *replacement_p = search_data.replacement = g_strdup (gtk_entry_get_text (entry2));
#if GTK_MAJOR_VERSION==2
  *flags_p = search_data.flags = gtk_toggle_button_get_active (case_sensitive) ? 0 : GTK_SOURCE_SEARCH_CASE_INSENSITIVE;
#else
  *flags_p = search_data.flags;
#endif
  gtk_widget_destroy (dialog);
  return TRUE;
}

static void
do_search_replace (GtkTextView * view, gboolean replace)
{
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (view);
  GtkTextIter iter;
  char *what, *replacement;
#if GTK_MAJOR_VERSION==3
  GtkTextSearchFlags flags;
#else
  GtkSourceSearchFlags flags;
#endif

  if (!search_dialog (GTK_WIDGET (view), replace, &what, &replacement, &flags))
    return;

  if (replace)
    {
      gtk_text_buffer_get_iter_at_offset (buffer, &iter, 0);

      while (TRUE)
        {
          GtkTextIter match_start, match_end;
#if GTK_MAJOR_VERSION==3
          if (!gtk_text_iter_forward_search (&iter, what, flags, &match_start, &match_end, NULL))
#else
          if (!gtk_source_iter_forward_search (&iter, what, flags, &match_start, &match_end, NULL))
#endif
            {
              break;
            }

          gtk_text_buffer_delete (buffer, &match_start, &match_end);
          gtk_text_buffer_insert (buffer, &match_start, replacement, -1);
          iter = match_start;
        }
    }
  else
    {
      GtkTextIter match_start, match_end;

      gtk_text_buffer_get_iter_at_mark (buffer, &iter, gtk_text_buffer_get_insert (buffer));
#if GTK_MAJOR_VERSION==3
      if (gtk_text_iter_forward_search (&iter, what, flags, &match_start, &match_end, NULL))
#else
      if (gtk_source_iter_forward_search (&iter, what, flags, &match_start, &match_end, NULL))
#endif
        {
          gtk_text_buffer_select_range (buffer, &match_start, &match_end);
        }
      else
        {
          GtkTextIter insert = iter;
          gtk_text_buffer_get_start_iter (buffer, &iter);
#if GTK_MAJOR_VERSION==3
          if (gtk_text_iter_forward_search (&iter, what, flags, &match_start, &match_end, &insert))
#else
          if (gtk_source_iter_forward_search (&iter, what, flags, &match_start, &match_end, &insert))
#endif
            gtk_text_buffer_select_range (buffer, &match_start, &match_end);
        }
    }
}

static void
find_cb (DenemoAction * action, gpointer user_data)
{
  do_search_replace (user_data, FALSE);
}

static void
replace_cb (DenemoAction * action, gpointer user_data)
{
  do_search_replace (user_data, TRUE);
}
