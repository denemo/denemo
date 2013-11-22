/* 
 * generate_source.c
 *
 * Program for generating source code from the old unmenued commands
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C) 2007 Richard Shann
 *
 * License: this file may be used under the FSF GPL version 2
 */

#include <stdio.h>
#include <glib.h>
#include <glib/gi18n.h>

#define NOTE0 "\\xF0\\x9D\\x85\\x9D"
#define NOTE1 "\\xF0\\x9D\\x85\\x9E"
#define NOTE2 "\\xF0\\x9D\\x85\\x9F"
#define NOTE3 "\\xF0\\x9D\\x85\\xA0"
#define NOTE4 "\\xF0\\x9D\\x85\\xA1"
#define NOTE5 "\\xF0\\x9D\\x85\\xA2"
#define NOTE6 "\\xF0\\x9D\\x85\\xA3"
#define NOTE7 "\\xF0\\x9D\\x85\\xA4"
#define NOTE8 "\\xF0\\x9D\\x85\\xA5"

#define REST0 "\\xF0\\x9D\\x84\\xBB"
#define REST1 "\\xF0\\x9D\\x84\\xBC"
#define REST2 "\\xF0\\x9D\\x84\\xBD"
#define REST3 "\\xF0\\x9D\\x84\\xBE"
#define REST4 "\\xF0\\x9D\\x84\\xBF"
#define REST5 "\\xF0\\x9D\\x85\\x80"
#define REST6 "\\xF0\\x9D\\x85\\x81"
#define REST7 "\\xF0\\x9D\\x85\\x82"
#define REST8 "\\xF0\\x9D\\x85\\x83"

char *NOTES[] = { NOTE0, NOTE1, NOTE2, NOTE3, NOTE4, NOTE5, NOTE6, NOTE7, NOTE8 };
char *RESTS[] = { REST0, REST1, REST2, REST3, REST4, REST5, REST6, REST7, REST8 };

#define CMD_CATEGORY_NAVIGATION		0
#define CMD_CATEGORY_NOTE_ENTRY		1
#define CMD_CATEGORY_REST_ENTRY		2
#define CMD_CATEGORY_ARTICULATION	3
#define CMD_CATEGORY_EDIT		4
#define CMD_CATEGORY_MEASURE		5
#define CMD_CATEGORY_STAFF		6
#define CMD_CATEGORY_PLAYBACK		7
#define CMD_CATEGORY_OTHER		8

#define CMD_CLASS(cat) (cat&0xFF)
#define CMD_CATEGORY_DIRECT		0x100   /* does not require a wrapper to the callback */
#define CMD_CATEGORY_BOOLEAN		0x200   /* call returns boolean */

struct name_and_function
{
  unsigned category;
  /** Command name */
  char *icon;
  //char *menu_label;
  char *tooltip;
  char *name;
  char *function;
  char *menu_label;
  char *initial_setting;        /*of radio/check items */
};

FILE *callbacks, *entries, *xml, *scheme, *scheme_cb, *register_commands;

#define ni denemo_commands[i].name
#define ii denemo_commands[i].icon
#define ml denemo_commands[i].menu_label
#define ti denemo_commands[i].tooltip
#define fi denemo_commands[i].function
#define mi denemo_commands[i].category

void parse_menu_commands(){
  #include "menu.c"

  int i;
  int n_denemo_commands = (sizeof (denemo_commands) / sizeof (struct name_and_function));

  for (i = 0; i < n_denemo_commands; i++)
    {
      if (fi != NULL)
        {
          if (!(mi & CMD_CATEGORY_DIRECT))
            {
              fprintf (callbacks, "/*%s %s*/\n", ni, fi);
              /*******************   create a callback for calling from a menuitem *******************/
              fprintf (callbacks,
                       "static void %s_cb (GtkAction *action, DenemoScriptParam *param) {\n"
                       "  %s (param);\n"
                       "%s"
                       "}\n",
                       fi, fi, (CMD_CLASS (mi) == CMD_CATEGORY_NAVIGATION) ? "gtk_widget_queue_draw(Denemo.scorearea);\n" : "  displayhelper (Denemo.gui);\n  score_status(Denemo.gui, TRUE);\n");
            }

          /*******************   create a procedure d-<name> in scheme to call scheme_<name>  *******************/
          fprintf (scheme, "/*%s %s*/\n", ni, fi);
          fprintf (scheme, 
                   "SCM scheme_%s(SCM optional);\n"
                   "install_scm_function (0, NULL, \"d-%s\", scheme_%s);\n", ni, ni, ni);  // for direct callback via (scheme_xxx)

          /*******************   create a callback scheme_<name> for calling from a scheme procedure d-<name>  *******************/
          fprintf (scheme_cb, 
                   "SCM scheme_%s (SCM optional) {\n"
                   "  return scheme_call_callback(optional, %s%s);\n"
                   "}\n", 
                   ni, fi, !(mi & CMD_CATEGORY_DIRECT) ? "_cb" : "");

          /****************** install the command in the hash table of commands (keymap) **************/
          fprintf (register_commands, "register_command(\"%s\", _(\"%s\"), _(\"%s\"), %s);\n", ni, ml ? ml : ni, ti ? ti : ni, fi);

          /****************** install the command as an action in the menu system **************************/
          fprintf (entries, "{\"%s\", %s, N_(\"%s\"), \"\"," "N_(\"%s\")," "G_CALLBACK (%s%s)},\n", ni, ii ? ii : "NULL", ml ? ml : ni, ti ? ti : ni, fi, (mi & CMD_CATEGORY_DIRECT) ? "" : "_cb");
        }
      else                      //no callback function - a menu rather than a menu item. It still needs to be added as an action in the menu system.
        fprintf (entries, "{\"%s\", %s, N_(\"%s\"), \"\"," "N_(\"%s\")},\n", ni, ii ? ii : "NULL", ml ? ml : ni, ti ? ti : ni);
    }
}

int
main ()
{
  scheme_cb = fopen ("scheme_cb.h", "w");
  scheme = fopen ("scheme.h", "w");
  callbacks = fopen ("callbacks.h", "w");
  entries = fopen ("entries.h", "w");
  register_commands = fopen ("register_commands.h", "w");
  if (!callbacks || !entries || !scheme || !scheme_cb || !register_commands)
    return -1;
  fprintf (callbacks, "/******** generated automatically from generate_source. See generate_source.c */\n");
  fprintf (entries, "/******** generated automatically from generate_source. See generate_source.c */\n");

  parse_menu_commands ();
  
  int i;

  /* generate source for duration callbacks - these were intercepted when
     typed at the keyboard to set prevailing rhythm, so the callback has to
     include code for this */

  for (i = 0; i < 9; i++)
    {
      fprintf (entries, "\n#define NOTE%d \"%s\"\n", i, NOTES[i]);
      fprintf (entries, "\n#define REST%d \"%s\"\n", i, RESTS[i]);


      /* menu_entries for the mode sensitive duration actions, Dur0,1,2 ... */
      fprintf (entries, "{\"%d\", \"NULL\", NOTE%d, NULL, \"Inserts a note at the cursor with duration \"NOTE%d\", or \\n(if appending) appends this duration\\nIf MIDI-in is active the note will be pitchless (displays yellow, percussion-sounding)\\n - the MIDI keyboard will provide the pitch. Changes prevailing duration.\",\n" "G_CALLBACK (Dur%d)},\n" "{\"Change%d\", \"NULL\", NOTE%d, NULL, \"Change current note to a \"NOTE%d,\n" "G_CALLBACK (ChangeDur%d)},\n"
               //"{\"ChangeRest%d\", NULL, \"Change duration\", NULL, \"Change duration of current rest\",\n"
               // "G_CALLBACK (ChangeRest%d)},\n"
               "{\"Insert%d\", NULL, \"Insert a \"NOTE%d\"\", NULL, \"Inserts a \"NOTE%d\" at cursor position\\nSets prevailing duration to \"NOTE%d,\n"
               "G_CALLBACK (InsertDur%d)},\n" "{\"InsertRest%d\", NULL, \"Insert a \"REST%d\"rest\", NULL, \"Inserts a rest at cursor position\\nSets prevailing duration to \"NOTE%d,\n" "G_CALLBACK (InsertRest%d)},\n" "{\"Set%d\", NULL, \"Set Duration to \"NOTE%d\"\", NULL, \"Sets prevailing duration to \"NOTE%d\" (subsequent notes entered will have this duration)\", \n" "G_CALLBACK (SetDur%d)},\n"
               /* ,i, i, i , i, i*/, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i);


    }

  /* menu_entries for the mode    note name    */
  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (entries,
               " {\"Insert%c\", NULL, \"Insert %c\", NULL, \"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "  G_CALLBACK (Insert%c)},\n"
               " {\"AddNote%c\", NULL, \"Insert %c After\", NULL, \"Inserts note %c after note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "  G_CALLBACK (AddNote%c)},\n"
               " {\"Add%c\", NULL, \"Add %c to Chord\", NULL, \"Adds note %c to chord at cursor\\nCursor determines which octave\",\n"
               "  G_CALLBACK (Add%c)},\n"
               "  {\"ChangeTo%c\", NULL, \"Change current note to %c\", NULL, \"Changes current note to the %c nearest cursor or (if no current note) inserts the note %c\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\",\n"
               "   G_CALLBACK (ChangeTo%c)},\n" "  {\"MoveTo%c\", NULL, \"Move cursor to step %c\", NULL, \"Moves the cursor to the %c nearest cursor\\nCurrent cursor position determines which octave.\",\n" "   G_CALLBACK (MoveTo%c)},\n", i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i);

    }

  for (i = 'A'; i <= 'G'; i++)
    {
      fprintf (register_commands, "register_command(\"Insert%c\", _(\"Insert %c\"),_(\"Inserts note %c before note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),  Insert%c);\n", i, i, i, i);
      fprintf (scheme, "SCM scheme_Insert%c(SCM optional);\ninstall_scm_function (0, NULL, \"d-Insert%c\", scheme_Insert%c);\n", i, i, i);       // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_Insert%c (SCM optional) {\nInsert%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);
      fprintf (register_commands, "register_command(\"AddNote%c\", _(\"Insert %c After\"),_(\"Inserts note %c after note at cursor\\nCursor determines which octave\\nNote is inserted in the prevailing rhythm\"),  AddNote%c);\n", i, i, i, i);
      fprintf (scheme, "SCM scheme_AddNote%c(SCM optional);\ninstall_scm_function (0, NULL, \"d-AddNote%c\", scheme_AddNote%c);\n", i, i, i);    // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_AddNote%c (SCM optional) {\nAddNote%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);


      fprintf (register_commands, "register_command(\"Add%c\", _(\"Add %c\"),_(\"Adds note %c to the chord at cursor\\nCursor height determines which octave\"),  Add%c);\n", i, i, i, i);
      fprintf (scheme, "SCM scheme_Add%c(SCM optional);\ninstall_scm_function (0, NULL, \"d-Add%c\", scheme_Add%c);\n", i, i, i);        // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_Add%c (SCM optional) {\nAdd%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (register_commands, "register_command(\"ChangeTo%c\", _(\"Change to %c\"),_(\"Changes note at cursor to nearest note %c\\nRhythm is unchanged\"),  ChangeTo%c);\n", i, i, i, i);
      fprintf (scheme, "SCM scheme_ChangeTo%c(SCM optional);\ninstall_scm_function (0, NULL, \"d-ChangeTo%c\", scheme_ChangeTo%c);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_ChangeTo%c (SCM optional) {\nChangeTo%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);


      fprintf (register_commands, "register_command(\"MoveTo%c\", _(\"Move to %c\"),_(\"Moves cursor to nearest note %c\"),  MoveTo%c);\n", i, i, i, i);
      fprintf (scheme, "SCM scheme_MoveTo%c(SCM optional);\ninstall_scm_function (0, NULL, \"d-MoveTo%c\", scheme_MoveTo%c);\n", i, i, i);       // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_MoveTo%c (SCM optional) {\nMoveTo%c (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

    }



  for (i = 0; i < 9; i++)
    {
      /* registering commands for mode independent duration actions InsertRest0,1,2... ChangeRest0,1,2... InsertDur,ChangeDur0,1,2...
       *
       * !!! FIXME what is ChangeRestn???? seems to be Changen ... now dropped. */

      fprintf (register_commands, "register_command(\"%d\", _(\"Insert/Append a %s\"), _(\"When appending, appends a %s \\nWith the cursor on a note inserts a %s  before the current note\\nIf MIDI-in is active, the note will be pitchless (displays yellow, percussion-sounding)\\n - the MIDI keyboard will provide the pitch. Changes prevailing duration.\"), Dur%d);\n", i, NOTES[i], NOTES[i], NOTES[i], i);

      fprintf (register_commands, "register_command(\"Change%d\", _(\"Change to %s\"), _(\"Change the current note to a %s\"), ChangeDur%d);\n", i, NOTES[i], NOTES[i], i);

      fprintf (register_commands, "register_command(\"Insert%d\", _(\"%s\"), _(\"Insert a %s\"), InsertDur%d);\n", i, NOTES[i], NOTES[i], i);

      fprintf (register_commands, "register_command(\"InsertRest%d\",  _(\"Insert a %s\") ,  _(\"Inserts a rest at cursor position\\nSets prevailing rhythm to %s\"), InsertRest%d);\n", i, RESTS[i], NOTES[i], i);

      //  fprintf(register_commands, 
      //    "register_command(Denemo.map, gtk_action_group_get_action(action_group, \"ChangeRest%d\"), \"ChangeRest%d\",  _(\"Change a %s\") ,  _(\"Changes a rest at cursor position\\nSets prevailing rhythm to %s\"), ChangeRest%d);\n", i, i, RESTS[i], NOTES[i], i);

      fprintf (register_commands, "register_command(\"Set%d\", _(\"Set Prevailing Duration to %s\"), _(\"Set the prevailing duration to %s (subsequent notes entered will have this duration)\"), SetDur%d);\n", i, NOTES[i], NOTES[i], i);

      fprintf (register_commands, "\n#undef NOTE%d\n", i);
      fprintf (register_commands, "\n#undef REST%d\n", i);

      fprintf (scheme, "/*%d */\n", i);

      fprintf (scheme, "SCM scheme_%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-%d\", scheme_%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_%d (SCM optional) {\nDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      fprintf (scheme, "SCM scheme_InsertDur%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-Insert%d\", scheme_InsertDur%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_InsertDur%d (SCM optional) {\nInsertDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (scheme, "SCM scheme_ChangeDur%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-Change%d\", scheme_ChangeDur%d);\n", i, i, i); // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_ChangeDur%d (SCM optional) {\nChangeDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      fprintf (scheme, "SCM scheme_SetDur%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-Set%d\", scheme_SetDur%d);\n", i, i, i);  // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_SetDur%d (SCM optional) {\nSetDur%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);



      fprintf (scheme, "SCM scheme_InsertRest%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-InsertRest%d\", scheme_InsertRest%d);\n", i, i, i);   // for direct callback via (scheme_xxx)
      fprintf (scheme_cb, "SCM scheme_InsertRest%d (SCM optional) {\nInsertRest%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i, i);

      // fprintf(scheme, "SCM scheme_ChangeRest%d(SCM optional);\ninstall_scm_function (0, NULL, \"d-ChangeRest%d\", scheme_ChangeRest%d);\n", i, i, i);// for direct callback via (scheme_xxx)
      // fprintf(scheme_cb, "SCM scheme_ChangeRest%d (SCM optional) {\nChangeRest%d (NULL, NULL);\nreturn SCM_BOOL(TRUE);\n}\n", i,  i);
    }



#ifdef GENERATE_XML_FRAGMENT
  xml = fopen ("xml.fragment", "w");
  if(!xml)
    return -1
    char *catname[9] = { 
      N_("Navigation"),
      N_("Note entry"),
      N_("Rest entry"),
      N_("Articulation"),
      N_("Edit"),
      N_("Measure"),
      N_("Staff"),
      N_("Playback"),
      N_("Other")
    };

  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Change%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Insert%d\"/>\n", i);
  //for (i = 0; i < 9; i++)
    //fprintf(xml, "<menuitem action=\"ChangeRest%d\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"InsertRest%d\"/>\n", i);
  for (i = 'A'; i <= 'G'; i++)
    fprintf (xml, "<menuitem action=\"Insert%c\"/>\n", i);
  for (i = 'A'; i <= 'G'; i++)
    fprintf (xml, "<menuitem action=\"ChangeTo%c\"/>\n", i);
  for (i = 0; i < 9; i++)
    fprintf (xml, "<menuitem action=\"Set%d\"/>\n", i);

  fprintf (xml, "<menu action=\"AllOther\">\n");
  for (j = 0; j < 9; j++)
    {
      fprintf (xml, "<menu action=\"%s\">\n", catname[j]);
      for (i = 0; i < n_denemo_commands; i++)
        {
          if (mi == j)
            {
              fprintf (xml, "<menuitem action=\"%s\"/>\n", ni);
            }

        }
      fprintf (xml, "</menu>\n");
    }
  fprintf (xml, "</menu>\n");
#endif
  return 0;
}
