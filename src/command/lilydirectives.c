/* lilydirectives.c
 * Implements lilydirectives which are not notes
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * Richard Shann 2009, 2010, 2011
 * A Tee  (c) 2000-2005
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "command/lilydirectives.h"
#include "command/chord.h"
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include "ui/dialogs.h"
#include "display/draw.h"
#include "command/object.h"
#include "command/staff.h"
#include "core/utils.h"
#include "core/prefops.h"
#include "core/view.h"
#include "core/menusystem.h"
#include "ui/texteditors.h"
//#if GTK_MAJOR_VERSION==2
//#define GDK_KEY_Escape GDK_Escape
//#define GDK_KEY_Return GDK_Return
//#define GDK_KEY_Tab GDK_Tab
//#define GDK_KEY_BackSpace GDK_BackSpace
//#endif


static gboolean text_edit_directive (DenemoDirective * directive, gchar * what);

static GHashTable *action_scripts;


gchar * difference_of_directive (DenemoDirective *d1, DenemoDirective *d2)
{
  if (d1==d2) return NULL;
  if (d1 && d2)
    {
#define NEQ(a) if(!compare_gstring((GString*)d1->a, (GString*)d2->a)) return g_strdup_printf ("Directive tagged differently %s - %s", d1->tag->str, d2->tag->str);
      NEQ(tag);
#undef NEQ
#define NEQ(a) if(!compare_gstring((GString*)d1->a, (GString*)d2->a)) return g_strdup_printf ("Directives tagged %s differ", d1->tag->str);
      
      NEQ(prefix);
      NEQ(postfix);
      NEQ(display);
      NEQ(midibytes);
      NEQ(graphic_name);
      NEQ(grob);
      NEQ(data);
#undef NEQ
#define NEQ(a) if(!(d1 && d2 && (d1->a == d2->a))) return g_strdup_printf ("Directives tagged %s differ", d1->tag->str);
      NEQ(tx);
      NEQ(ty);
      NEQ(gx);
      NEQ(gy);
      NEQ(minpixels);
      NEQ(flag);
      NEQ(locked);
      NEQ(override);
#undef NEQ
      if (!compare_glists (d1->layouts, d2->layouts))
        return g_strdup_printf ("Directives tagged %s apply to different layouts", d1->tag->str);
    }
  return NULL;
}
gboolean compare_directive (DenemoDirective *d1, DenemoDirective *d2)
{
  gchar *ret = difference_of_directive (d1, d2);
  g_free (ret);
  return ret == NULL;
}


gboolean compare_directive_lists (GList *dlist1, GList *dlist2)
{
  gchar *ret = difference_of_directive_lists (dlist1, dlist2);
  g_free (ret);
  return ret == NULL;
}


gchar *difference_of_directive_lists (GList *dlist1, GList *dlist2)
{
  if (!dlist1 && !dlist2)
    return NULL;
  while (dlist1 && dlist2)
    {
      gchar *diff = difference_of_directive (dlist1->data, dlist2->data);
      if (diff) 
        return diff;
        dlist1=dlist1->next;
        dlist2=dlist2->next;
    }
  if (dlist1 || dlist2)
    return g_strdup (_("Different number of directives"));
  return NULL;
}


static void
action_script_table_insert (gchar * name, gchar * script)
{
  if (!action_scripts)
    action_scripts = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  g_hash_table_insert (action_scripts, g_strdup (name), script);
}

void
set_action_script_for_tag (gchar * tag, gchar * script)
{
  action_script_table_insert (tag, script);
}


gchar *
get_action_script (gchar * name)
{
  if (action_scripts)
    return (gchar *) g_hash_table_lookup (action_scripts, name);
  return NULL;
}


static void
gtk_menu_item_set_label_text (GtkMenuItem * item, gchar * text)
{
  GtkWidget *label = (GtkWidget *) gtk_bin_get_child (GTK_BIN (item));
  if (label)
    gtk_label_set_text (GTK_LABEL (label), text);
}




static void
toggle_locked (GtkWidget * widget, gboolean * locked)
{
  //g_debug("Called with %d\n", *locked);
  *locked = !*locked;
}

/* lookup a directive tagged with TAG in a list DIRECTIVES and return it.
   if TAG is NULL or "" return the first directive
   else return NULL
   DEPRECATED * If TAG has two lines the first only is matched, while the second is
   DEPRECATED* interpreted as a number selecting which matching directive to return
   * USE d-DirectiveGetNthTag-##what## instead.
   * */
DenemoDirective *
find_directive (GList * directives, gchar * tag)
{
  DenemoDirective *directive = NULL;
  if (tag && *tag)
    {
      GList *g;
      gchar *newline;
      gint number = 0;          //number of matching directive required 1 is first matching
      gint count = 0;           //count of directives with matching name
      if (*tag == '\n')
        return NULL;

      for (newline = tag; *newline; newline++)
        {
          if (*newline == '\n')
            {
              number = atoi (newline + 1);
              if (number)
                *newline = 0;
              break;
            }
        }

      for (g = directives; g; g = g->next)
        {
          directive = (DenemoDirective *) g->data;
          if (directive->tag && (number ? g_str_has_prefix (directive->tag->str, tag) : !strcmp (tag, directive->tag->str)))
            {
              if (number == 0)
                return directive;
              count++;
              if (number == count)
                {
                  if (newline != tag)
                    *newline = '\n';
                  return directive;
                }
            }
          directive = NULL;
        }
    }
  else
    directive = (DenemoDirective *) directives->data;
  return directive;
}

static DenemoDirective *
find_directive_number (GList * directives, gint num)
{
  return g_list_nth_data (directives, num - 1);
}

static gboolean
delete_directive (GList ** directives, gchar * tag)
{
  DenemoDirective *directive = NULL;
  if (tag)
    {
      GList *g;
      for (g = *directives; g; g = g->next)
        {
          directive = (DenemoDirective *) g->data;
          if (directive->tag && !strcmp (tag, directive->tag->str))
            {
              *directives = g_list_remove (*directives, directive);
              free_directive (directive);
              score_status (Denemo.project, TRUE);
              displayhelper (Denemo.project);
              return TRUE;
            }
        }
    }
  return FALSE;
}

/* free a list of directives and set to NULL */
void
delete_directives (GList ** directives)
{
  DenemoDirective *directive = NULL;    //FIXME use free_directives
  if (directives)
    while (*directives)
      {
        directive = (DenemoDirective *) (*directives)->data;
        *directives = g_list_remove (*directives, directive);
        free_directive (directive);
      }
}



static DenemoDirective *
new_directive (gchar * tag)
{
  DenemoDirective *directive = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
  if (tag)
    directive->tag = g_string_new (tag);
  return directive;
}




typedef enum attach_type
{ ATTACH_NOTE, ATTACH_CHORD } attach_type;
/**
 * Denemo directive attach or edit.
if interactive: Allows user to attach a lilypond directive
else attache the passed strings as lilypond directive
attachment is to chord ( attach is ATTACH_CHORD) or to the note at the cursor
 */
static void
attach_directive (attach_type attach, gchar * postfix, gchar * prefix, gchar * display, gchar * tag, gboolean interactive)
{
  gchar *prefixstring = NULL, *postfixstring = NULL, *displaystring = NULL;
  DenemoProject *gui = Denemo.project;
  note *curnote = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj == NULL)
    {
      if (interactive)
        warningdialog (_("You must put the cursor on a chord to attach LilyPond"));     //FIXME find a note and ask
      return;
    }
  chord *thechord = NULL;
  thechord = (chord *) curObj->object;
  if (curObj->type != CHORD)
    {
      if (interactive)
        warningdialog (_("You must put the cursor on a chord to attach LilyPond"));
      return;
    }

  curnote = findnote (curObj, gui->movement->cursor_y);
  if (attach == ATTACH_NOTE && (curnote == NULL))
    {
      if (interactive)
        warningdialog (_("You must put the cursor on a note to attach LilyPond to the note"));  //FIXME find a note and ask
      return;
    }
  if (tag==NULL)
    {
        if (attach==ATTACH_CHORD)
            tag = "AttachLilyToChord";
        else
         tag = "AttachLilyToNote";
    }
  // setup directive to be data from thechord->directives or curnote->directives which has matching tag, or first if tag is NULL.
  DenemoDirective *directive = NULL;
  switch (attach)
    {
    case ATTACH_CHORD:
      if (thechord->directives == NULL)
        {
          directive = new_directive (tag);
          thechord->directives = g_list_append (NULL, directive);
        }
      else
        {
          directive = find_directive (thechord->directives, tag);
          if (directive == NULL)
            {
              if (tag)
                {
                  directive = new_directive (tag);
                  thechord->directives = g_list_append (thechord->directives, directive);
                }
            }
        }
      break;
    case ATTACH_NOTE:
      if (curnote->directives == NULL)
        {
          directive = new_directive (tag);
          curnote->directives = g_list_append (NULL, directive);
        }
      else
        {
          directive = find_directive (curnote->directives, tag);
          if (directive == NULL)
            {
              if (tag)
                {
                  directive = new_directive (tag);
                  curnote->directives = g_list_append (curnote->directives, directive);
                }
            }
        }
      break;
    default:
      g_warning (_("Error in attach type"));
      return;
    }

  if (interactive)
    {
      if (directive->postfix)
        postfixstring = directive->postfix->str;
      if (directive->prefix)
        prefixstring = directive->prefix->str;
      if (directive->display)
        displaystring = directive->display->str;

      prefixstring = string_dialog_entry (gui, _("Attach LilyPond"), _("Give text to place before the note"), prefixstring);
      postfixstring = string_dialog_entry (gui, curnote ? _("Attach LilyPond to Note") : _("Attach LilyPond to Chord"), curnote ? _("Give LilyPond text to postfix to note of chord") : _("Give LilyPond text to postfix to chord"), postfixstring);
      displaystring = string_dialog_entry (gui, _("Attach LilyPond"), _("Give Display text if required"), displaystring);
    }
  else
    {                           //not interactive
      if (prefix)
        prefixstring = g_strdup (prefix);
      if (postfix)
        postfixstring = g_strdup (postfix);
      if (display)
        displaystring = g_strdup (display);
    }

#define STRINGASSIGN(field, val) \
     if(val && *val) {\
     if(directive->field) g_string_assign(directive->field, val);\
     else directive->field=g_string_new(val);}
  STRINGASSIGN (postfix, postfixstring);
  STRINGASSIGN (prefix, prefixstring);
  STRINGASSIGN (display, displaystring);

#undef STRINGASSIGN

  score_status (gui, TRUE);
  displayhelper (gui);
  g_free (postfixstring);
  g_free (displaystring);
  g_free (prefixstring);
}

static void
create_directives (GList ** directives, gchar * tag)
{
  *directives = g_list_append (NULL, new_directive (tag));
}

static void
get_lily_parameter (gchar * query, DenemoScriptParam * param)
{
  DenemoObject *curObj = (DenemoObject *) Denemo.project->movement->currentobject ? (DenemoObject *) Denemo.project->movement->currentobject->data : NULL;
  param->status = curObj && curObj->type == LILYDIRECTIVE;
#define ASSIGN_PARAM(field)  if(!strcmp(#field, query))\
  g_string_assign(param->string, lilyobj->field->str);
  if (param->status)
    {
      lilydirective *lilyobj = (lilydirective *) curObj->object;
      ASSIGN_PARAM (postfix);
      ASSIGN_PARAM (display);
      if (!strcmp ("minpixels", query))
        g_string_printf (param->string, "%d", curObj->minpixelsalloted);
    }
#undef ASSIGN_PARAM
}


static void
insert_lily_directive (gchar * postfix, gchar * display, gboolean locked, gint minpixels)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  DenemoObject *lily;
  lilydirective *lilyobj = NULL;        /* a lily directive object */
  DenemoObject *curObj = (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
  if (postfix == NULL)
    postfix = "";
  gboolean is_new = FALSE;
  if (curObj && curObj->type == LILYDIRECTIVE)
    {
      g_string_assign ((lilyobj = (lilydirective *) curObj->object)->postfix, postfix);
      setpixelmin (curObj);     //curObj->minpixelsalloted = minpixels;
    }
  else
    {
      lily = lily_directive_new (postfix);
      is_new = TRUE;
      lilyobj = (lilydirective *) lily->object;
      setpixelmin (lily);       //lily->minpixelsalloted = minpixels;//g_debug("min pixels %d\n", lily->minpixelsalloted);
    }
  if (lilyobj)
    {
      lilyobj->locked = locked;
      if (*postfix == '%')
        {                       //append newline if directive starts with a LilyPond comment indicator
          g_string_append (lilyobj->postfix, "\n");
        }
      if (display)
        {
          if (lilyobj->display)
            g_string_assign (lilyobj->display, display);
          else
            lilyobj->display = g_string_new (display);
        }
    }
  if (is_new)
    object_insert (gui, lily);
  score_status (gui, TRUE);
  displayhelper (gui);
}


/* Run a dialog to get a lily directive from the user
 the values returned must be freed by the caller */
static gboolean
get_lily_directive (gchar ** directive, gchar ** display, gboolean * locked)
{
  DenemoProject *gui = Denemo.project;
  GtkToggleButton *button = NULL;
  button = (GtkToggleButton *) gtk_check_button_new_with_label ("locked");
  g_signal_connect (button, "toggled", G_CALLBACK (toggle_locked), locked);
  if (*locked)
    gtk_toggle_button_set_active (button, *locked), *locked = TRUE;     //FIXME how is this supposed to be done?
  *directive = string_dialog_entry_with_widget (gui, _("Insert LilyPond"), _("Give LilyPond text to insert"), *directive, GTK_WIDGET (button));
  if (!*directive)
    return FALSE;
  *display = string_dialog_entry (gui, _("Insert LilyPond"), _("Give Display text if required"), *display);
  return TRUE;
}

/* return the directive whose tag is prefixed with tag if present at cursor postion
 if tag is NULL, return any directive at current position*/
static DenemoDirective *
get_standalone_directive (gchar * tag)
{
  DenemoObject *curObj = (DenemoObject *) Denemo.project->movement->currentobject ? (DenemoObject *) Denemo.project->movement->currentobject->data : NULL;
  if (curObj && curObj->type == LILYDIRECTIVE)
    {
      DenemoDirective *ret = (DenemoDirective *) curObj->object;
      if (tag == NULL)
        return ret;
      if (*tag == 0)
        return ret;
      if (ret && ret->tag && !g_str_has_prefix (ret->tag->str, tag))
        ret = NULL;
      return ret;
    }
  return NULL;
}


static DenemoObject *
get_chordobject (void)
{
  DenemoObject *curObj = get_object ();
  if (curObj == NULL)
    return NULL;
  if (curObj->type != CHORD)
    {
      return NULL;
    }
  return curObj;
}

static chord *
get_chord (void)
{
  DenemoObject *curObj = get_chordobject ();
  if (curObj == NULL)
    return NULL;
  return (chord *) curObj->object;
}

static DenemoLilyControl *
get_score (void)
{
  return &Denemo.project->lilycontrol;
}


static note *
get_note (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj = get_chordobject ();
  if (curObj == NULL)
    return NULL;
  return findnote (curObj, gui->movement->cursor_y);
}

static note *
get_strict_note (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoObject *curObj = get_chordobject ();
  if (curObj == NULL)
    return NULL;
  return findnote_strict (curObj, gui->movement->cursor_y);
}

static DenemoStaff *
get_staff (void)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return NULL;
  return Denemo.project->movement->currentstaff->data;
}

#define get_voice get_staff


//block for new type of directive
static clef *
get_clef (void)
{
  clef *ret = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj && curObj->type == CLEF)
    {
      ret = ((clef *) curObj->object);
    }
  else
    {
      DenemoStaff *curstaff = get_staff ();
      if (curstaff)
        ret = &curstaff->clef;
    }
  return ret;
}

static DenemoDirective *
get_clef_directive (gchar * tag)
{
  clef *curclef = get_clef ();
  if (curclef == NULL || (curclef->directives == NULL))
    return NULL;
  return find_directive (curclef->directives, tag);
}

gboolean
delete_clef_directive (gchar * tag)
{
  clef *curclef = get_clef ();
  if (curclef == NULL || (curclef->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_clef_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curclef->directives, tag);
}

// end of block for new type of directive


static keysig *
get_keysig (void)
{
  keysig *ret = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj && curObj->type == KEYSIG)
    {
      ret = ((keysig *) curObj->object);
    }
  else
    {
      DenemoStaff *curstaff = get_staff ();
      if (curstaff)
        ret = &curstaff->keysig;
    }
  return ret;
}

static DenemoDirective *
get_keysig_directive (gchar * tag)
{
  keysig *curkeysig = get_keysig ();
  if (curkeysig == NULL || (curkeysig->directives == NULL))
    return NULL;
  return find_directive (curkeysig->directives, tag);
}

gboolean
delete_keysig_directive (gchar * tag)
{
  keysig *curkeysig = get_keysig ();
  if (curkeysig == NULL || (curkeysig->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_keysig_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curkeysig->directives, tag);
}

static timesig *
get_timesig (void)
{
  timesig *ret = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj && curObj->type == TIMESIG)
    {
      ret = ((timesig *) curObj->object);
    }
  else
    {
      DenemoStaff *curstaff = get_staff ();
      if (curstaff)
        ret = &curstaff->timesig;
    }
  return ret;
}

static DenemoDirective *
get_timesig_directive (gchar * tag)
{
  timesig *curtimesig = get_timesig ();
  if (curtimesig == NULL || (curtimesig->directives == NULL))
    return NULL;
  return find_directive (curtimesig->directives, tag);
}

gboolean
delete_timesig_directive (gchar * tag)
{
  timesig *curtimesig = get_timesig ();
  if (curtimesig == NULL || (curtimesig->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_timesig_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curtimesig->directives, tag);
}

static tuplet *
get_tuplet (void)
{
  tuplet *ret = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj && (curObj->type == TUPOPEN || curObj->type == TUPCLOSE))
    {
      ret = ((tuplet *) curObj->object);
    }
  return ret;
}

static DenemoDirective *
get_tuplet_directive (gchar * tag)
{
  tuplet *curtuplet = get_tuplet ();
  if (curtuplet == NULL || (curtuplet->directives == NULL))
    return NULL;
  return find_directive (curtuplet->directives, tag);
}

gboolean
delete_tuplet_directive (gchar * tag)
{
  tuplet *curtuplet = get_tuplet ();
  if (curtuplet == NULL || (curtuplet->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_tuplet_directive (tag);
  if (directive == NULL)
    return FALSE;
  return delete_directive (&curtuplet->directives, tag);
}

static stemdirective *
get_stemdirective (void)
{
  stemdirective *ret = NULL;
  DenemoObject *curObj = get_object ();
  if (curObj && (curObj->type == STEMDIRECTIVE))
    {
      ret = ((stemdirective *) curObj->object);
    }
  return ret;
}

static DenemoDirective *
get_stemdirective_directive (gchar * tag)
{
  stemdirective *curstemdirective = get_stemdirective ();
  if (curstemdirective == NULL || (curstemdirective->directives == NULL))
    return NULL;
  return find_directive (curstemdirective->directives, tag);
}

gboolean
delete_stemdirective_directive (gchar * tag)
{
  stemdirective *curstemdirective = get_stemdirective ();
  if (curstemdirective == NULL || (curstemdirective->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_stemdirective_directive (tag);
  if (directive == NULL)
    return FALSE;
  return delete_directive (&curstemdirective->directives, tag);
}

static scoreheader *
get_scoreheader (void)
{
  return &Denemo.project->scoreheader;
}

static DenemoDirective *
get_scoreheader_directive (gchar * tag)
{
  scoreheader *curscoreheader = get_scoreheader ();
  if (curscoreheader == NULL || (curscoreheader->directives == NULL))
    return NULL;
  return find_directive (curscoreheader->directives, tag);
}

gboolean
delete_scoreheader_directive (gchar * tag)
{
  scoreheader *curscoreheader = get_scoreheader ();
  if (curscoreheader == NULL || (curscoreheader->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_scoreheader_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curscoreheader->directives, tag);
}


static paper *
get_paper (void)
{
  return &Denemo.project->paper;
}

static DenemoDirective *
get_paper_directive (gchar * tag)
{
  paper *curpaper = get_paper ();
  if (curpaper == NULL || (curpaper->directives == NULL))
    return NULL;
  return find_directive (curpaper->directives, tag);
}

gboolean
delete_paper_directive (gchar * tag)
{
  paper *curpaper = get_paper ();
  if (curpaper == NULL || (curpaper->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_paper_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curpaper->directives, tag);
}

static layout *
get_layout (void)
{
  return &Denemo.project->movement->layout;
}

static DenemoDirective *
get_layout_directive (gchar * tag)
{
  layout *curlayout = get_layout ();
  if (curlayout == NULL || (curlayout->directives == NULL))
    return NULL;
  return find_directive (curlayout->directives, tag);
}

gboolean
delete_layout_directive (gchar * tag)
{
  layout *curlayout = get_layout ();
  if (curlayout == NULL || (curlayout->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_layout_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curlayout->directives, tag);
}


static movementcontrol *
get_movementcontrol (void)
{
  return &Denemo.project->movement->movementcontrol;
}

DenemoDirective *
get_movementcontrol_directive (gchar * tag)
{
  movementcontrol *curmovementcontrol = get_movementcontrol ();
  if (curmovementcontrol == NULL || (curmovementcontrol->directives == NULL))
    return NULL;
  return find_directive (curmovementcontrol->directives, tag);
}

gboolean
delete_movementcontrol_directive (gchar * tag)
{
  movementcontrol *curmovementcontrol = get_movementcontrol ();
  if (curmovementcontrol == NULL || (curmovementcontrol->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_movementcontrol_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curmovementcontrol->directives, tag);
}


static header *
get_header (void)
{
  return &Denemo.project->movement->header;
}

static DenemoDirective *
get_header_directive (gchar * tag)
{
  header *curheader = get_header ();
  if (curheader == NULL || (curheader->directives == NULL))
    return NULL;
  return find_directive (curheader->directives, tag);
}

gboolean
delete_header_directive (gchar * tag)
{
  header *curheader = get_header ();
  if (curheader == NULL || (curheader->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_header_directive (tag);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curheader->directives, tag);
}


DenemoDirective *
get_note_directive (gchar * tag)
{
  note *curnote = get_note ();
  if (curnote == NULL || (curnote->directives == NULL))
    return NULL;
  return find_directive (curnote->directives, tag);
}


DenemoDirective *
get_note_directive_number (gint num)
{
  note *curnote = get_note ();
  if (curnote == NULL || (curnote->directives == NULL))
    return NULL;
  return find_directive_number (curnote->directives, num);
}

static DenemoDirective *
get_chord_directive (gchar * tag)
{
  DenemoObject *curObj = get_chordobject ();
  if (curObj == NULL)
    return NULL;
  chord *thechord = (chord *) curObj->object;
  if (thechord->directives == NULL)
    return NULL;
  return find_directive (thechord->directives, tag);
}

static DenemoDirective *
get_object_directive (gchar * tag)
{
  DenemoObject *curObj = get_object ();
  if (curObj == NULL)
    return NULL;
  if (curObj->directives == NULL)
    return NULL;
  return find_directive (curObj->directives, tag);
}

gboolean
delete_object_directive (gchar * tag)
{
  DenemoObject *curObj = get_object ();
  if (curObj == NULL)
    return FALSE;
  if (curObj->directives == NULL)
    return FALSE;
  return delete_directive (&curObj->directives, tag);
}

DenemoDirective *
get_score_directive (gchar * tag)
{

  return find_directive (Denemo.project->lilycontrol.directives, tag);
}

static DenemoDirective *
get_staff_directive (gchar * tag)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  //FIXME return NULL if not primary staff
  if (curstaff == NULL || curstaff->staff_directives == NULL)
    return NULL;
  return find_directive (curstaff->staff_directives, tag);
}

static DenemoDirective *
get_voice_directive (gchar * tag)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  if (curstaff == NULL || curstaff->voice_directives == NULL)
    return NULL;
  return find_directive (curstaff->voice_directives, tag);
}

gboolean
delete_staff_directive (gchar * tag)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return FALSE;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  if (curstaff == NULL || curstaff->staff_directives == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curstaff->staff_directives, tag);
}


gboolean
delete_initialclef_directive (gchar * tag)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return FALSE;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  if (curstaff == NULL || curstaff->clef.directives == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curstaff->clef.directives, tag);
}


gboolean
delete_voice_directive (gchar * tag)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return FALSE;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  if (curstaff == NULL || curstaff->voice_directives == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&curstaff->voice_directives, tag);
}

gboolean
delete_note_directive (gchar * tag)
{
  note *curnote = get_note ();
  if (curnote == NULL || (curnote->directives == NULL))
    return FALSE;
  DenemoDirective *directive = get_note_directive (tag);
  if (directive == NULL)
    return FALSE;
  return delete_directive (&curnote->directives, tag);
}

gboolean
delete_chord_directive (gchar * tag)
{
  DenemoObject *curObj = get_chordobject ();
  if (curObj == NULL)
    return FALSE;
  chord *thechord = (chord *) curObj->object;
  if (thechord->directives == NULL)
    return FALSE;
  DenemoDirective *directive = get_chord_directive (tag);
  if (directive == NULL)
    return FALSE;
  return delete_directive (&thechord->directives, tag);
}

gboolean
delete_score_directive (gchar * tagname)
{
  DenemoDirective *directive = get_score_directive (tagname);
  if (directive == NULL)
    return FALSE;
  signal_structural_change (Denemo.project);
  return delete_directive (&Denemo.project->lilycontrol.directives, tagname);
}


#define GET_TAG_FUNC(what)\
gchar *\
what##_directive_get_tag(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive && directive->tag)\
    return directive->tag->str;\
  else directive = NULL;/* get_##what##_directive(NULL)*/;  \
  if(directive && directive->tag)\
    return directive->tag->str;\
  return NULL;\
}



#define GET_STR_FIELD_FUNC(what, field)\
gchar *\
what##_directive_get_##field(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive && directive->field)\
    return directive->field->str;\
  return NULL;\
}
//typdefs to make the macros defined below pick up the right structure for staff, voice and score as chord & note do
typedef DenemoStaff staff;
typedef DenemoStaff voice;
typedef DenemoLilyControl score;
typedef DenemoObject object;


     //note I think you cannot change the graphic once you have set it.
#define PUT_GRAPHIC_NAME(what, directives) gboolean \
what##_directive_put_graphic(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(Denemo.project->movement->currentobject)\
  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
  if(current->directives==NULL)\
       create_directives (&current->directives, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->directives = g_list_append(current->directives, directive);\
    }\
  loadGraphicItem(value, (DenemoGraphic **)&directive->graphic);\
  if(directive->graphic_name)\
     g_string_assign(directive->graphic_name, value);\
  else\
      directive->graphic_name = g_string_new(value);\
  return TRUE;\
}
#define PUT_GRAPHIC(what) PUT_GRAPHIC_NAME(what, directives)
PUT_GRAPHIC (chord);
PUT_GRAPHIC (note);

PUT_GRAPHIC (keysig);
PUT_GRAPHIC (timesig);
PUT_GRAPHIC (tuplet);
PUT_GRAPHIC (stemdirective)
#define PUT_STR_FIELD_FUNC_NAME(what, field, name)\
gboolean \
what##_directive_put_##field(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(Denemo.project->movement->currentobject)\
  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  if(directive->field)\
    g_string_assign(directive->field, value);\
  else\
    directive->field = g_string_new(value);\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->name);\
  }\
  return TRUE;\
}
#define PUT_STR_FIELD_FUNC(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, directives)
#define PUT_STR_FIELD_FUNCS(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, staff_directives)
#define PUT_STR_FIELD_FUNCV(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, voice_directives)
  GET_TAG_FUNC (object);
GET_TAG_FUNC (standalone);
GET_TAG_FUNC (chord);
GET_TAG_FUNC (note);
GET_TAG_FUNC (staff);
GET_TAG_FUNC (voice);
GET_TAG_FUNC (score);
GET_TAG_FUNC (clef);
GET_TAG_FUNC (timesig);
GET_TAG_FUNC (tuplet);
GET_TAG_FUNC (stemdirective);
GET_TAG_FUNC (keysig);
GET_TAG_FUNC (scoreheader);
GET_TAG_FUNC (header);
GET_TAG_FUNC (paper);
GET_TAG_FUNC (layout);
GET_TAG_FUNC (movementcontrol);


#undef GET_TAG_FUNC




GET_STR_FIELD_FUNC (score, midibytes);
GET_STR_FIELD_FUNC (movementcontrol, midibytes);
GET_STR_FIELD_FUNC (note, midibytes);
GET_STR_FIELD_FUNC (chord, midibytes);
GET_STR_FIELD_FUNC (keysig, midibytes);
GET_STR_FIELD_FUNC (timesig, midibytes);
GET_STR_FIELD_FUNC (tuplet, midibytes);
GET_STR_FIELD_FUNC (clef, midibytes);
GET_STR_FIELD_FUNC (staff, midibytes);
GET_STR_FIELD_FUNC (voice, midibytes);
GET_STR_FIELD_FUNC (standalone, midibytes);
PUT_STR_FIELD_FUNC (note, midibytes);
PUT_STR_FIELD_FUNC (chord, midibytes)
PUT_STR_FIELD_FUNC (keysig, midibytes)
PUT_STR_FIELD_FUNC (timesig, midibytes)
PUT_STR_FIELD_FUNC (tuplet, midibytes)
PUT_STR_FIELD_FUNC (clef, midibytes)

//cloned for grob
  GET_STR_FIELD_FUNC (score, grob);
GET_STR_FIELD_FUNC (movementcontrol, grob);
GET_STR_FIELD_FUNC (note, grob);
GET_STR_FIELD_FUNC (chord, grob);
GET_STR_FIELD_FUNC (staff, grob);
GET_STR_FIELD_FUNC (voice, grob);
GET_STR_FIELD_FUNC (clef, grob);
GET_STR_FIELD_FUNC (timesig, grob);
GET_STR_FIELD_FUNC (keysig, grob);
GET_STR_FIELD_FUNC (tuplet, grob);
GET_STR_FIELD_FUNC (stemdirective, grob);
GET_STR_FIELD_FUNC (standalone, grob);
GET_STR_FIELD_FUNC (standalone, graphic_name);
GET_STR_FIELD_FUNC (chord, graphic_name);
GET_STR_FIELD_FUNC (note, graphic_name);
GET_STR_FIELD_FUNC (clef, graphic_name);
GET_STR_FIELD_FUNC (keysig, graphic_name);
GET_STR_FIELD_FUNC (timesig, graphic_name);
GET_STR_FIELD_FUNC (tuplet, graphic_name);

PUT_STR_FIELD_FUNC (score, grob)
//PUT_STR_FIELD_FUNC(staff, grob)
//PUT_STR_FIELD_FUNC(voice, grob)
  PUT_STR_FIELD_FUNC (note, grob);
PUT_STR_FIELD_FUNC (chord, grob);
PUT_STR_FIELD_FUNC (clef, grob);
PUT_STR_FIELD_FUNC (timesig, grob);
PUT_STR_FIELD_FUNC (keysig, grob);
PUT_STR_FIELD_FUNC (tuplet, grob);
PUT_STR_FIELD_FUNC (stemdirective, grob)
//PUT_STR_FIELD_FUNC(standalone, grob)
//end of clone for grob

//this set for the "data" field is complete I think. For some reason others fields have incomplete sets of get/put functions.
//In particular, the S and V versions of the macros, that enable staff and voice directives to be accessed were commented out for some reason and needed to be put back for this set.
  GET_STR_FIELD_FUNC (score, data)
  GET_STR_FIELD_FUNC (scoreheader, data)
  GET_STR_FIELD_FUNC (header, data)
  GET_STR_FIELD_FUNC (paper, data)
  GET_STR_FIELD_FUNC (layout, data)
  GET_STR_FIELD_FUNC (movementcontrol, data)
  GET_STR_FIELD_FUNC (note, data)
  GET_STR_FIELD_FUNC (chord, data)
  GET_STR_FIELD_FUNC (staff, data)
  GET_STR_FIELD_FUNC (voice, data)
  GET_STR_FIELD_FUNC (clef, data)
  GET_STR_FIELD_FUNC (timesig, data)
  GET_STR_FIELD_FUNC (keysig, data);
GET_STR_FIELD_FUNC (tuplet, data);
GET_STR_FIELD_FUNC (stemdirective, data);
GET_STR_FIELD_FUNC (standalone, data)


  PUT_STR_FIELD_FUNC (score, data)
  PUT_STR_FIELD_FUNC (scoreheader, data)
  PUT_STR_FIELD_FUNC (header, data)
  PUT_STR_FIELD_FUNC (paper, data)
  PUT_STR_FIELD_FUNC (layout, data)
  PUT_STR_FIELD_FUNCS(staff, data)
  PUT_STR_FIELD_FUNCV(voice, data)
  PUT_STR_FIELD_FUNC (movementcontrol, data)
  PUT_STR_FIELD_FUNC (note, data);
PUT_STR_FIELD_FUNC (chord, data);
PUT_STR_FIELD_FUNC (clef, data);
PUT_STR_FIELD_FUNC (timesig, data);
PUT_STR_FIELD_FUNC (keysig, data);
PUT_STR_FIELD_FUNC (tuplet, data);
PUT_STR_FIELD_FUNC (stemdirective, data)
//PUT_STR_FIELD_FUNC(standalone, data) // done separately below...
//end of set for data


  GET_STR_FIELD_FUNC (chord, prefix)
GET_STR_FIELD_FUNC (chord, postfix)
GET_STR_FIELD_FUNC (chord, display)
PUT_STR_FIELD_FUNC (chord, prefix)
PUT_STR_FIELD_FUNC (chord, postfix)
PUT_STR_FIELD_FUNC (chord, display)
GET_STR_FIELD_FUNC (note, prefix)
GET_STR_FIELD_FUNC (note, postfix)
GET_STR_FIELD_FUNC (note, display)
PUT_STR_FIELD_FUNC (note, prefix)
PUT_STR_FIELD_FUNC (note, postfix)
PUT_STR_FIELD_FUNC (note, display);
GET_STR_FIELD_FUNC (standalone, prefix);
GET_STR_FIELD_FUNC (standalone, postfix);
GET_STR_FIELD_FUNC (standalone, display);
GET_STR_FIELD_FUNC (score, prefix);
GET_STR_FIELD_FUNC (score, postfix);
GET_STR_FIELD_FUNC (score, display);
GET_STR_FIELD_FUNC (staff, prefix);
GET_STR_FIELD_FUNC (staff, postfix);
GET_STR_FIELD_FUNC (staff, display);
GET_STR_FIELD_FUNC (voice, prefix);
GET_STR_FIELD_FUNC (voice, postfix);
GET_STR_FIELD_FUNC (voice, display)
#undef staff
#define PUT_INT_FIELD_FUNC_NAME(what, field, name)\
gboolean \
what##_directive_put_##field(gchar *tag, gint value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(Denemo.project->movement->currentobject)\
  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  directive->field = value;\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->name);\
  }\
  return TRUE;\
}
static GList *add_layout (GList *layouts, guint id)
{
    if (g_list_index (layouts, GUINT_TO_POINTER(id))<0)
        return g_list_append (layouts, GUINT_TO_POINTER(id));
    return layouts;
}
static GList *remove_layout (GList *layouts, guint id)
{
   return g_list_remove (layouts, GUINT_TO_POINTER(id));
}
static void action_ignore (DenemoDirective *directive, guint value) {
  if (value)
      {
       if(directive->layouts==NULL)
          {
          directive->flag = DENEMO_IGNORE_FOR_LAYOUTS;
          directive->layouts = add_layout (directive->layouts, value);//g_print("Made %x the ignored layouts\n", value);
          } else
          {

            if (directive->flag == DENEMO_IGNORE_FOR_LAYOUTS)
                {
                 directive->layouts = add_layout (directive->layouts, value);//g_print("Added %x to ignored layouts\n", value);
                }
            else {
                    directive->layouts = remove_layout (directive->layouts, value);//g_print("Removed %x from allowed layouts\n", value);
                    if(directive->layouts == NULL) directive->flag = 0;//, g_print("No conditions left\n");
                }
            }
     } else
        {
           g_list_free (directive->layouts);//g_print("Removed conditions\n");
           directive->layouts = NULL;
           directive->flag = 0;
        }
}
static void action_allow (DenemoDirective *directive, guint value) {
  if (value)
      {
        if(directive->layouts==NULL)
          {
              directive->flag = DENEMO_ALLOW_FOR_LAYOUTS;
              directive->layouts = add_layout (directive->layouts, value);//g_print("Made %x the allowed layout\n", value);
          } else
          {
          if (directive->flag == DENEMO_ALLOW_FOR_LAYOUTS)
            {
                directive->layouts = add_layout (directive->layouts, value);//g_print("Added %x to allowed layouts\n", value);
            }
          else {
            directive->layouts = remove_layout (directive->layouts, value);//g_print("Removed %x from ignored layouts\n", value);
            if(directive->layouts == NULL) directive->flag = 0;//, g_print("No conditions left\n");
            }
        }
    } else
    {
       g_list_free (directive->layouts);//g_print("Removed conditions\n");
       directive->layouts = NULL;
       directive->flag = 0;
    }
}




#define PUT_LAYOUT_IGNORE_FUNC_NAME(what, directives) \
gboolean \
what##_directive_put_ignore(gchar *tag, guint value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(Denemo.project->movement->currentobject)\
  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
  if(current->directives==NULL)\
       create_directives (&current->directives, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->directives = g_list_append(current->directives, directive);\
    }\
   action_ignore (directive, value);\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->directives);\
  }\
  return TRUE;\
}
#define PUT_LAYOUT_ALLOW_FUNC_NAME(what, directives) \
gboolean \
what##_directive_put_allow(gchar *tag, guint value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(Denemo.project->movement->currentobject)\
  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
  if(current->directives==NULL)\
       create_directives (&current->directives, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->directives = g_list_append(current->directives, directive);\
    }\
  action_allow (directive, value);\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->directives);\
  }\
  return TRUE;\
}

#define PUT_LAYOUT_IGNORE_FUNC(what) PUT_LAYOUT_IGNORE_FUNC_NAME(what, directives)
#define PUT_LAYOUT_ALLOW_FUNC(what) PUT_LAYOUT_ALLOW_FUNC_NAME(what, directives)
#define PUT_LAYOUT_IGNORE_FUNCS(what) PUT_LAYOUT_IGNORE_FUNC_NAME(what, staff_directives)
#define PUT_LAYOUT_IGNORE_FUNCV(what) PUT_LAYOUT_IGNORE_FUNC_NAME(what, voice_directives)
#define PUT_LAYOUT_ALLOW_FUNCS(what) PUT_LAYOUT_ALLOW_FUNC_NAME(what, staff_directives)
#define PUT_LAYOUT_ALLOW_FUNCV(what) PUT_LAYOUT_ALLOW_FUNC_NAME(what, voice_directives)

#define PUT_INT_FIELD_FUNC(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, directives)
//#define PUT_INT_FIELD_FUNCS(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, staff_directives)
//#define PUT_INT_FIELD_FUNCV(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, voice_directives)
#define GET_INT_FIELD_FUNC(what, field)\
gint \
what##_directive_get_##field(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive)\
    return directive->field;\
  return 0;\
}
#define GET_INT_GRAPHIC_FIELD_FUNC(what, field)\
gint \
what##_directive_get_##field(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive)\
    return directive->graphic->field;\
  return 0;\
}
  PUT_GRAPHIC (object);
PUT_INT_FIELD_FUNC (object, minpixels);
GET_INT_FIELD_FUNC (object, minpixels)
  /* block which can be copied for new int fields */
  PUT_INT_FIELD_FUNC (note, minpixels);
PUT_INT_FIELD_FUNC (chord, minpixels)
  //PUT_INT_FIELD_FUNCS(staff, minpixels)
  //PUT_INT_FIELD_FUNCV(voice, minpixels)
  //PUT_INT_FIELD_FUNC(score, minpixels)
  PUT_INT_FIELD_FUNC (clef, minpixels);
PUT_INT_FIELD_FUNC (timesig, minpixels);
PUT_INT_FIELD_FUNC (tuplet, minpixels);
PUT_INT_FIELD_FUNC (stemdirective, minpixels);
PUT_INT_FIELD_FUNC (keysig, minpixels)
  //PUT_INT_FIELD_FUNC(scoreheader, minpixels)
  //PUT_INT_FIELD_FUNC(header, minpixels)
  //PUT_INT_FIELD_FUNC(paper, minpixels)
  //PUT_INT_FIELD_FUNC(layout, minpixels)
  //PUT_INT_FIELD_FUNC(movementcontrol, minpixels)
  //standalone needs different code for "put" see STANDALONE_PUT* below
  GET_INT_FIELD_FUNC (note, minpixels)
GET_INT_FIELD_FUNC (chord, minpixels)
GET_INT_FIELD_FUNC (staff, minpixels)
GET_INT_FIELD_FUNC (voice, minpixels)
GET_INT_FIELD_FUNC (score, minpixels);
GET_INT_FIELD_FUNC (clef, minpixels);
GET_INT_FIELD_FUNC (keysig, minpixels);
GET_INT_FIELD_FUNC (timesig, minpixels);
GET_INT_FIELD_FUNC (tuplet, minpixels);
GET_INT_FIELD_FUNC (stemdirective, minpixels);
GET_INT_FIELD_FUNC (scoreheader, minpixels);
GET_INT_FIELD_FUNC (header, minpixels);
GET_INT_FIELD_FUNC (paper, minpixels);
GET_INT_FIELD_FUNC (layout, minpixels);
GET_INT_FIELD_FUNC (movementcontrol, minpixels);
GET_INT_FIELD_FUNC (standalone, minpixels)
  /* end block which can be copied for new int fields */
  PUT_INT_FIELD_FUNC (note, override)
PUT_INT_FIELD_FUNC (chord, override)
GET_INT_FIELD_FUNC (note, override)
GET_INT_FIELD_FUNC (chord, override)
GET_INT_FIELD_FUNC (staff, override)
GET_INT_FIELD_FUNC (voice, override)
GET_INT_FIELD_FUNC (score, override)
PUT_LAYOUT_ALLOW_FUNC (note)
PUT_LAYOUT_ALLOW_FUNC (chord)
PUT_LAYOUT_ALLOW_FUNCS (staff)
PUT_LAYOUT_ALLOW_FUNCV (voice)
PUT_LAYOUT_IGNORE_FUNC (note)
PUT_LAYOUT_IGNORE_FUNC (chord)
PUT_LAYOUT_IGNORE_FUNCS (staff)
PUT_LAYOUT_IGNORE_FUNCV (voice)
PUT_INT_FIELD_FUNC (note, tx)
PUT_INT_FIELD_FUNC (chord, tx)
GET_INT_FIELD_FUNC (note, tx)
GET_INT_FIELD_FUNC (chord, tx)
GET_INT_FIELD_FUNC (staff, tx)
GET_INT_FIELD_FUNC (voice, tx)
PUT_INT_FIELD_FUNC (note, ty)
PUT_INT_FIELD_FUNC (chord, ty)
GET_INT_FIELD_FUNC (note, ty)
GET_INT_FIELD_FUNC (chord, ty)
GET_INT_FIELD_FUNC (staff, ty)
GET_INT_FIELD_FUNC (voice, ty)
PUT_INT_FIELD_FUNC (note, gx)
PUT_INT_FIELD_FUNC (chord, gx)
GET_INT_FIELD_FUNC (note, gx)
GET_INT_FIELD_FUNC (chord, gx)
GET_INT_FIELD_FUNC (staff, gx)
GET_INT_FIELD_FUNC (voice, gx)
PUT_INT_FIELD_FUNC (note, gy)
PUT_INT_FIELD_FUNC (chord, gy)
GET_INT_FIELD_FUNC (note, gy)
GET_INT_FIELD_FUNC (chord, gy)
GET_INT_FIELD_FUNC (staff, gy)
GET_INT_FIELD_FUNC (voice, gy)
GET_INT_FIELD_FUNC (standalone, override)
GET_INT_FIELD_FUNC (standalone, tx)
GET_INT_FIELD_FUNC (standalone, ty)
GET_INT_FIELD_FUNC (standalone, gx)
GET_INT_FIELD_FUNC (standalone, gy)
PUT_LAYOUT_ALLOW_FUNC (score)
PUT_LAYOUT_IGNORE_FUNC (score)
GET_INT_FIELD_FUNC (score, tx)
GET_INT_FIELD_FUNC (score, ty)
GET_INT_FIELD_FUNC (score, gx)
GET_INT_FIELD_FUNC (score, gy)
  /* width and height of graphic (if any), read only */
  GET_INT_GRAPHIC_FIELD_FUNC (note, width)
GET_INT_GRAPHIC_FIELD_FUNC (chord, width);
GET_INT_GRAPHIC_FIELD_FUNC (staff, width);
GET_INT_GRAPHIC_FIELD_FUNC (voice, width);
GET_INT_GRAPHIC_FIELD_FUNC (standalone, width);
GET_INT_GRAPHIC_FIELD_FUNC (score, width);
GET_INT_GRAPHIC_FIELD_FUNC (note, height);
GET_INT_GRAPHIC_FIELD_FUNC (chord, height);
GET_INT_GRAPHIC_FIELD_FUNC (staff, height);
GET_INT_GRAPHIC_FIELD_FUNC (voice, height);
GET_INT_GRAPHIC_FIELD_FUNC (standalone, height);
GET_INT_GRAPHIC_FIELD_FUNC (score, height)


/* return a full path to an editscript for directive or NULL if there is none */

gchar *
get_editscript_filename (gchar * tag)
{
  gchar *basename = g_strconcat (tag, ".scm", NULL);
  GList* dirs = NULL;
  dirs = g_list_append(dirs, g_build_filename (get_user_data_dir (FALSE), COMMANDS_DIR, "editscripts", NULL));
  dirs = g_list_append(dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, "editscripts", NULL));
  return find_path_for_file(basename, dirs);
}


/*
instead of the "activate"  "button-release-event" signal

gboolean            user_function                      (GtkWidget      *widget,
                                                        GdkEventButton *event,
                                                        gpointer        user_data)      : Run Last
                                                        * PROBLEM cannot use gtk_widget_activate ... d-DirectiveActivate ...
the look at event to see if left or right button pressed
and allow advanced edit if right button.

*/
static gboolean text_edit_directive_by_fn (DenemoDirective * directive, gpointer fn);

static gboolean swallow_button_press (void)
    { return TRUE;} //prevent other handlers seeing this.
static gboolean
button_callback (GtkWidget * widget, GdkEventButton * event, DenemoDirective * directive)
{
  // !!!!! clicking on a staff tools menu item comes thru here - but if you break gdb here as the menu item is still up your mouse is grabbed.
  gboolean left = TRUE;
  
  signal_structural_change (Denemo.project); //FIXME this is only needed for some directives
  
  if (event != NULL)
    left = !((event->button != 1));
  if (left && (directive->override & DENEMO_OVERRIDE_EDITOR))
    {
      GtkWidget *texteditor = (GtkWidget *) g_object_get_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG);
      if (texteditor)
        {
          //FIXME position at cursor if a toplevel window
          gtk_widget_show_all (gtk_widget_get_toplevel (texteditor));
          gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (texteditor)));
        }
    }
  else
    {
      gchar *script = get_action_script (directive->tag->str);
      if (left && script)
        {
          stage_undo (Denemo.project->movement, ACTION_STAGE_END);    //undo is a queue so this is the end :)
          call_out_to_guile (script);
          stage_undo (Denemo.project->movement, ACTION_STAGE_START);
        }
      else
        {
          if (left && (directive->override & DENEMO_OVERRIDE_TAGEDIT))
            script = get_editscript_filename (directive->tag->str);
          else
            script = NULL;
          if (script)
            execute_script_file (script);
          else
            {
              /* if there is an action of this tag with scheme script, run it again
                 else do text edit of the directives fields
               */
              DenemoAction *action;
              if (left && ((action = lookup_action_from_name ((gchar *) directive->tag->str)) != NULL) && (directive->override & DENEMO_OVERRIDE_TAGEDIT))
                denemo_action_activate (action);
              else
                {
                  if (left && action)
                    {
                      gchar *name = (gchar *) denemo_action_get_name (action);
                      gint idx = lookup_command_from_name (Denemo.map, name);
                      if (idx > 0)
                        {
                          gpointer fn = (widget != NULL) ? g_object_get_data (G_OBJECT (widget), "fn") : NULL;

                          gchar *label = (gchar *) lookup_label_from_idx (Denemo.map, idx);
                          if (confirm (label, _("Repeat the command?\n(Hold Shift for advanced edit)")))
                            if(shift_held_down())
                                {
                                GList **directives = (GList **) g_object_get_data (G_OBJECT (widget), "directives-pointer");
                                gboolean delete = !text_edit_directive (directive, fn);
                                if (delete)
                                    {
                                    if (directives)
                                        delete_directive (directives, directive->tag->str);
                                      else
                                        g_warning ("Could not get directives list to delete from");
                                    }
                                }
                            else
                                denemo_action_activate (action);
                        }
                    }
                  else
                    {
                      gpointer fn = (widget != NULL) ? g_object_get_data (G_OBJECT (widget), "fn") : NULL;
                      if (fn)
                        {
                          gboolean delete = !text_edit_directive_by_fn (directive, fn);
                          if (delete)
                            {
                              GList **directives = (GList **) g_object_get_data (G_OBJECT (widget), "directives-pointer");
                              if (directives)
                                delete_directive (directives, directive->tag->str);
                              else
                                g_warning ("Could not get directives list to delete from");
                            }
                        }
                    }
                }
            }
        }
    }
    return TRUE;
}


static void
button_activate_callback (GtkWidget * w, DenemoDirective * d)
{
  button_callback (w, NULL, d);
}

/* return a GtkTextView which has been installed inside a scrolled window */
static GtkWidget *
create_text_window (void)
{
  GtkWidget *textview = gtk_text_view_new ();
  GtkWidget *w = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (w), _("Denemo Editor:Newline to update, Esc for Advanced Edit"));
  gtk_window_set_default_size (GTK_WINDOW (w), 600, 400);
  gtk_window_set_position (GTK_WINDOW (w), GTK_WIN_POS_MOUSE);
  g_signal_connect (G_OBJECT (w), "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), w);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (w), main_vbox);

  GtkWidget *sw = gtk_scrolled_window_new (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0), gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (main_vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), textview);
  return textview;
}

static void
assign_text (GtkWidget * w, gchar * text)
{
  GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (w));
  if (textbuffer)
    gtk_text_buffer_set_text (textbuffer, text, -1);
}

static gchar *
get_label_text (DenemoDirective * directive, gchar * text)
{
  if (directive->override & DENEMO_OVERRIDE_MARKUP)
    return g_strdup (text);
  return g_markup_escape_text (text, -1);
}

/* create a label.
Use the display string up to the first newline, if it is long enough
else use tag
*/
static void
set_directive_graphic_label (DenemoDirective * directive)
{
  if(Denemo.non_interactive)
    return;
  gchar *value;
  if (directive->display && directive->display->len > 0)
    value = get_label_text (directive, directive->display->str);
  else
    value = get_label_text (directive, directive->tag->str);
  gchar *c;
  for (c = value; *c; c++)
    if (*c == '\n')
      {
        *c = 0;
        break;
      }
  if (GTK_IS_MENU_ITEM (directive->widget))
    gtk_menu_item_set_label_text ((GtkMenuItem *) directive->widget, value);
  else
    gtk_label_set_markup ((GtkLabel *) gtk_bin_get_child (GTK_BIN (directive->widget)), value);
  g_free (value);
}


static gboolean
editor_keypress (GtkWidget * w, GdkEventKey * event, DenemoDirective * directive)
{
  GtkTextIter startiter, enditer;
  GtkTextBuffer *textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (w));
  gtk_text_buffer_get_start_iter (textbuffer, &startiter);
  gtk_text_buffer_get_end_iter (textbuffer, &enditer);
  gchar *text = gtk_text_buffer_get_text (textbuffer, &startiter, &enditer, FALSE);
  if (directive->display)
    g_string_assign (directive->display, text);
  else
    directive->display = g_string_new (text);
  //if the GdkEventKey is newline, run the editscript for the directive

  //FIXME use switch
  if (event->keyval == GDK_KEY_Escape)
    if (!text_edit_directive (directive, "unknown"))
      {
        /* I have used "unknown" here because we would need to get the name e.g. "score" "movementcontrol" etc from fn, but this is only used for the create a script thing...
           g_object_get_data(G_OBJECT(directive->widget), "fn")
         */
        GList **directives_ptr = g_object_get_data (G_OBJECT (directive->widget), "directives-pointer");
        if (directives_ptr)
          delete_directive (g_object_get_data (G_OBJECT (directive->widget), "directives-pointer"), directive->tag->str);
        else
          warningdialog (_("Cannot delete via this mechanism, sorry"));
        return TRUE;
      }
  if (event->keyval == GDK_KEY_Return)
    {
      gchar *filename = get_editscript_filename (directive->tag->str);
      if (filename)
        execute_script_file (filename);
    }
  set_directive_graphic_label (directive);
  score_status (Denemo.project, TRUE);
  return TRUE;
}

static void
attach_textedit_widget (DenemoDirective * directive)
{
  if (directive->override & DENEMO_OVERRIDE_EDITOR)
    {
      GtkWidget *texteditor = create_text_window ();
      //    g_signal_connect (G_OBJECT (texteditor), "key-press-event",
      //                G_CALLBACK (editor_keypress), directive);
      g_signal_connect_after (G_OBJECT (texteditor), "key-release-event", G_CALLBACK (editor_keypress), directive);
      if (directive->display == NULL)
        directive->display = g_string_new ("");
      assign_text (texteditor, directive->display->str);
      // g_object_set_data(texteditor, "gstring", directive->display);
      g_object_set_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG, texteditor);
    }
}

/*
  widget_for_directive()
  if directive does not have widget:

     creates a widget (button or menu depending on fn) for editing/actioning directive, point directive->widget to it and attach a callback to edit/action this directive, passing fn as data to it (to say what sort of directive it is) or the directive itself (for actionscripts/editscripts).

if directive is non-DenemoObject directive it  places the widget in the appropriate buttonbox/menu, the directives attached to DenemoObjects have menus created dynamically. (fn gives the type of directive: it determines where the widget goes (score or movement level, DenemoProject or DenemoMovement respectively, or in staff or voice menu))

     set  the label for the widget from directive->display or the tag if no display text
     set  the visibility for the widget from directive->override
*/

void
widget_for_directive_menu (DenemoDirective * directive, void fn (), GtkMenu * menu)
{
  if(Denemo.non_interactive) return;
 //if (menu==NULL) return; this kills the titles bar!
  GtkWidget *box;
  gchar *value = "";
  //FIXME we don't need value now...
  if (directive->override & DENEMO_OVERRIDE_EDITOR)
    {
      value = directive->tag->str;
    }
  else if (directive->display)
    value = directive->display->str;
  value = get_label_text (directive, value);
  if (!Denemo.non_interactive && directive->widget == NULL)
    {
      //FIXME at this point you could allow the user to specify a custom button box for his directive - some property of the directive saying which button box it should be in. We could even allow the directive to create a toolitem of a toolbar or menuitem on a menu bar???

      if (fn == (void (*)()) score_directive_put_graphic || fn == (void (*)()) scoreheader_directive_put_graphic || fn == (void (*)()) paper_directive_put_graphic || fn == (void (*)()) layout_directive_put_graphic)
        box = Denemo.project->buttonbox;
      else if (fn == (void (*)()) movementcontrol_directive_put_graphic || fn == (void (*)()) header_directive_put_graphic)
        box = Denemo.project->movement->buttonbox;
      else
        box = NULL;

      if ((fn == (void (*)()) staff_directive_put_graphic) || (fn == (void (*)()) voice_directive_put_graphic))
        {
#if 0
voice and staff directives no longer have these popup menus, instead the staff voice editor is used.
however, at least the rest of this code expects a valid GtkWidget...
#endif
         //g_print("Doing the staff or voice case");
          directive->widget = GTK_WIDGET (gtk_menu_item_new_with_label (value));        //WARNING _with_label is important
//          attach_textedit_widget (directive);
//          g_signal_connect (G_OBJECT (directive->widget), "button-release-event", G_CALLBACK (button_callback), directive);
//         gtk_menu_shell_append (GTK_MENU_SHELL (menu), GTK_WIDGET (directive->widget));

        }
      else if (box)
        {
          //g_debug("Doing the score and movement cases starting from %p", directive->widget);
          directive->widget = GTK_WIDGET (gtk_button_new_with_label (value));
          set_foreground_color (directive->widget, box==Denemo.project->buttonbox?"#107000":"#001070");
          gchar *tooltip;
          const gchar *label = get_label_for_command (directive->tag->str);
          const gchar *help = get_tooltip_for_command (directive->tag->str);
          if (label)
            tooltip = g_strdup_printf (_("Command: %s.\n(%s)" "\nLeft click to run the command or right click for further options"), label, help ? help : "");
          else
            tooltip = g_strdup_printf (_("This button was created for the Denemo Directive whose tag is %s." " Usually you click on it to alter the setting made or perform the action it is labelled with"), directive->tag->str);     //FIXME enable scripters to pass a tooltip in???
          gtk_widget_set_tooltip_text (directive->widget, tooltip);
          g_free (tooltip);

          {
            GtkWidget *label = gtk_bin_get_child (GTK_BIN (directive->widget));
            gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
          }
          attach_textedit_widget (directive);
          g_signal_connect (G_OBJECT (directive->widget),  "button-release-event", G_CALLBACK (button_callback), directive);
          g_signal_connect (G_OBJECT (directive->widget),  "button-press-event", G_CALLBACK (swallow_button_press), directive);

          if (box)
            {
              gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (directive->widget), FALSE, TRUE, 0);
              gtk_widget_show (box);
            }
        }
      else
        {
          directive->widget = gtk_menu_item_new_with_label (value);
          attach_textedit_widget (directive);
          g_signal_connect (G_OBJECT (directive->widget), "button-release-event", G_CALLBACK (button_callback), directive);
        }
      g_object_set_data (G_OBJECT (directive->widget), "directive", (gpointer) directive);
      g_object_set_data (G_OBJECT (directive->widget), "fn", (gpointer) fn);
      //GTK_WIDGET_UNSET_FLAGS(directive->widget, GTK_CAN_FOCUS);
      gtk_widget_set_can_focus (directive->widget, FALSE);
    }                           //end of no widget

  (directive->override & DENEMO_OVERRIDE_GRAPHIC) ? gtk_widget_show (GTK_WIDGET (directive->widget)) : gtk_widget_hide (GTK_WIDGET (directive->widget));

  // here handle the case where widget has a GtkTextView editing the text in value
  if (directive->display)
    {
      GtkWidget *texteditor = (GtkWidget *) g_object_get_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG);
      if (texteditor)
        assign_text (texteditor, directive->display->str);
    }
  set_directive_graphic_label (directive);
  g_free (value);
}

void
widget_for_directive (DenemoDirective * directive, void fn ())
{
  if(Denemo.non_interactive) return;
  GtkMenu *menu = NULL;
  if (Denemo.project->movement)
    {
      if (fn == (void (*)()) staff_directive_put_graphic)
        {
          menu = ((DenemoStaff *) Denemo.project->movement->currentstaff->data)->staffmenu;
        }
      if (fn == (void (*)()) voice_directive_put_graphic)
        {
          menu = ((DenemoStaff *) Denemo.project->movement->currentstaff->data)->voicemenu;
        }
    }
  widget_for_directive_menu (directive, fn, menu);
}

void
widget_for_staff_directive (DenemoDirective * directive, GtkMenu * menu)
{
  return widget_for_directive_menu (directive, (void (*)()) staff_directive_put_graphic, menu);
}

void
widget_for_voice_directive (DenemoDirective * directive, GtkMenu * menu)
{
  return widget_for_directive_menu (directive, (void (*)()) voice_directive_put_graphic, menu);
}

void
widget_for_movementcontrol_directive (DenemoDirective * directive)
{
  return widget_for_directive_menu (directive, (void (*)()) movementcontrol_directive_put_graphic, NULL);
}

void
widget_for_header_directive (DenemoDirective * directive)
{
  return widget_for_directive_menu (directive, (void (*)()) header_directive_put_graphic, NULL);
}

void
widget_for_layout_directive (DenemoDirective * directive)
{
  return widget_for_directive_menu (directive, (void (*)()) layout_directive_put_graphic, NULL);
}

// create a directive for non-DenemoObject directive #what
// assigning the string VALUE to the field ##field
// also create a button or menuitem ( if it does not already exist) as the directive->widget, this will be used to edit/action the directive
// Compare this with the macros above which create the what##_directive_put_##field() without calling widget_for_directive() and so do not create a widget in the graphic field, except via the user setting graphic_name.
// FIXME this comment above seems out of date. the macros above also call widget_for_directive(), the only difference is that these don't call  store_for_undo_change () for the currentobject.
// So it seems these should be used for non-object directives and the other for object directives.
#define PUT_GRAPHIC_WIDGET_STR(field, what, name) \
gboolean \
what##_directive_put_##field(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  if(directive->field)\
    g_string_assign(directive->field, value);\
  else\
    directive->field = g_string_new(value);\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", (gpointer)&current->name); \
  }\
  return TRUE;\
}


#define PUT_GRAPHIC_WIDGET_INT(field, what, name)\
gboolean \
what##_directive_put_##field(gchar *tag, gint value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  directive->field = value;\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->name);\
  }\
  return TRUE;\
}


//As the above (for string and int) but for the graphic name field
//FIXME this is just storing the graphic name, any bitmap of that name could be placed on the button/menu item as an icon
#define PUT_GRAPHIC_WIDGET_GRAPHIC(what, name) gboolean \
what##_directive_put_graphic(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  if(directive->graphic_name==NULL) \
    directive->graphic_name = g_string_new(value);\
  else\
    g_string_assign(directive->graphic_name, value);\
  if(!Denemo.non_interactive){\
    widget_for_directive(directive, (void(*)())what##_directive_put_graphic);\
    if (directive->widget) g_object_set_data(G_OBJECT(directive->widget), "directives-pointer", &current->name);\
  }\
  return directive != NULL;\
}

PUT_GRAPHIC_WIDGET_GRAPHIC (score, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (scoreheader, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (header, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (paper, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (layout, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (movementcontrol, directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (staff, staff_directives)
PUT_GRAPHIC_WIDGET_GRAPHIC (voice, voice_directives)
PUT_GRAPHIC_WIDGET_STR (display, score, directives)
PUT_GRAPHIC_WIDGET_STR (display, scoreheader, directives)
PUT_GRAPHIC_WIDGET_STR (display, header, directives)
PUT_GRAPHIC_WIDGET_STR (display, paper, directives)
PUT_GRAPHIC_WIDGET_STR (display, layout, directives)
PUT_GRAPHIC_WIDGET_STR (display, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_STR (display, staff, staff_directives)
PUT_GRAPHIC_WIDGET_STR (display, voice, voice_directives)
PUT_GRAPHIC_WIDGET_STR (prefix, score, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, scoreheader, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, header, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, paper, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, layout, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_STR (prefix, staff, staff_directives)
PUT_GRAPHIC_WIDGET_STR (prefix, voice, voice_directives)
PUT_GRAPHIC_WIDGET_STR (postfix, score, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, scoreheader, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, header, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, paper, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, layout, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_STR (postfix, staff, staff_directives)
PUT_GRAPHIC_WIDGET_STR (postfix, voice, voice_directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, score, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, scoreheader, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, header, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, paper, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, layout, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, staff, staff_directives)
PUT_GRAPHIC_WIDGET_STR (midibytes, voice, voice_directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, score, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, header, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, paper, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, layout, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (minpixels, voice, voice_directives)
#if 0
PUT_GRAPHIC_WIDGET_INT (x, score, directives)
PUT_GRAPHIC_WIDGET_INT (x, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (x, header, directives)
PUT_GRAPHIC_WIDGET_INT (x, paper, directives)
PUT_GRAPHIC_WIDGET_INT (x, layout, directives)
PUT_GRAPHIC_WIDGET_INT (x, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (x, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (x, voice, voice_directives)
PUT_GRAPHIC_WIDGET_INT (y, score, directives)
PUT_GRAPHIC_WIDGET_INT (y, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (y, header, directives)
PUT_GRAPHIC_WIDGET_INT (y, paper, directives)
PUT_GRAPHIC_WIDGET_INT (y, layout, directives)
PUT_GRAPHIC_WIDGET_INT (y, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (y, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (y, voice, voice_directives)
#endif
PUT_GRAPHIC_WIDGET_INT (tx, score, directives)
PUT_GRAPHIC_WIDGET_INT (tx, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (tx, header, directives)
PUT_GRAPHIC_WIDGET_INT (tx, paper, directives)
PUT_GRAPHIC_WIDGET_INT (tx, layout, directives)
PUT_GRAPHIC_WIDGET_INT (tx, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (tx, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (tx, voice, voice_directives)
PUT_GRAPHIC_WIDGET_INT (ty, score, directives)
PUT_GRAPHIC_WIDGET_INT (ty, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (ty, header, directives)
PUT_GRAPHIC_WIDGET_INT (ty, paper, directives)
PUT_GRAPHIC_WIDGET_INT (ty, layout, directives)
PUT_GRAPHIC_WIDGET_INT (ty, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (ty, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (ty, voice, voice_directives)
PUT_GRAPHIC_WIDGET_INT (gx, score, directives)
PUT_GRAPHIC_WIDGET_INT (gx, scoreheader, directives)
PUT_GRAPHIC_WIDGET_INT (gx, header, directives)
PUT_GRAPHIC_WIDGET_INT (gx, paper, directives)
PUT_GRAPHIC_WIDGET_INT (gx, layout, directives)
PUT_GRAPHIC_WIDGET_INT (gx, movementcontrol, directives)
PUT_GRAPHIC_WIDGET_INT (gx, staff, staff_directives)
PUT_GRAPHIC_WIDGET_INT (gx, voice, voice_directives)
PUT_GRAPHIC_WIDGET_INT (gy, score, directives);
PUT_GRAPHIC_WIDGET_INT (gy, scoreheader, directives);
PUT_GRAPHIC_WIDGET_INT (gy, header, directives);
PUT_GRAPHIC_WIDGET_INT (gy, paper, directives);
PUT_GRAPHIC_WIDGET_INT (gy, layout, directives);
PUT_GRAPHIC_WIDGET_INT (gy, movementcontrol, directives);
PUT_GRAPHIC_WIDGET_INT (gy, staff, staff_directives);
PUT_GRAPHIC_WIDGET_INT (gy, voice, voice_directives);
PUT_GRAPHIC_WIDGET_INT (override, score, directives);
PUT_GRAPHIC_WIDGET_INT (override, scoreheader, directives);
PUT_GRAPHIC_WIDGET_INT (override, header, directives);
PUT_GRAPHIC_WIDGET_INT (override, paper, directives);
PUT_GRAPHIC_WIDGET_INT (override, layout, directives);
PUT_GRAPHIC_WIDGET_INT (override, movementcontrol, directives);
PUT_GRAPHIC_WIDGET_INT (override, staff, staff_directives);
PUT_GRAPHIC_WIDGET_INT (override, voice, voice_directives);
#undef PUT_GRAPHIC_WIDGET_STR
#undef PUT_GRAPHIC_WIDGET_INT
gboolean
standalone_directive_put_graphic (gchar * tag, gchar * value)
{
  DenemoDirective *directive = get_standalone_directive (tag);
  if (directive && directive->graphic)
    {

      // directive->graphic = NULL; FIXME should we do this...
      //g_string_free(directive->graphic_name, TRUE);
    }
  if (!directive)
    {
      DenemoObject *obj = lily_directive_new (" ");
      directive = (DenemoDirective *) obj->object;
      directive->tag = g_string_new (tag);
      object_insert (Denemo.project, obj);
      displayhelper (Denemo.project);
    }
  if (loadGraphicItem (value, &directive->graphic))
    {
      if (directive->graphic_name)
        g_string_assign (directive->graphic_name, value);
      else
        directive->graphic_name = g_string_new (value);
      return TRUE;
    }
  else
    {
      directive->graphic = NULL;
      directive->graphic_name = NULL;
      return FALSE;
    }
}



#define STANDALONE_PUT_STR_FIELD_FUNC(field)\
gboolean \
standalone_directive_put_##field(gchar *tag, gchar *value) {\
  DenemoDirective *directive = get_standalone_directive(tag);\
  if(directive && directive->field){\
    store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
    g_string_assign(directive->field, value);}              \
  else if(directive)\
    directive->field = g_string_new(value);\
  else {\
    DenemoObject *obj = lily_directive_new (" ");\
        directive = (DenemoDirective*)obj->object;\
        directive->tag = g_string_new(tag);\
        directive->field = g_string_new(value);\
    object_insert(Denemo.project, obj);\
    displayhelper(Denemo.project);\
   }\
  return TRUE;\
}

STANDALONE_PUT_STR_FIELD_FUNC (prefix);
STANDALONE_PUT_STR_FIELD_FUNC (postfix);
STANDALONE_PUT_STR_FIELD_FUNC (display);
STANDALONE_PUT_STR_FIELD_FUNC (midibytes);

STANDALONE_PUT_STR_FIELD_FUNC (grob);

STANDALONE_PUT_STR_FIELD_FUNC (data);


gboolean
standalone_directive_put_allow(gchar *tag, gint id) {
  DenemoDirective *directive = get_standalone_directive(tag);
  if(directive) {
    store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
    action_allow (directive, id);
    return TRUE;
    }
 return FALSE; //can only make an already existing directive conditional
}

gboolean
standalone_directive_put_ignore(gchar *tag, gint id) {
  DenemoDirective *directive = get_standalone_directive(tag);
  if(directive) {
    store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
    action_ignore (directive, id);
    return TRUE;
    }
 return FALSE; //can only make an already existing directive conditional
}


#define STANDALONE_PUT_INT_FIELD_FUNC(field)\
gboolean \
standalone_directive_put_##field(gchar *tag, gint value) {\
  DenemoDirective *directive = get_standalone_directive(tag);\
  if(directive){\
    store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);\
    directive->field = value;}\
  else {\
        DenemoObject *obj = lily_directive_new (" ");\
        directive = (DenemoDirective*)obj->object;\
        directive->tag = g_string_new(tag);\
        directive->field = value;\
    object_insert(Denemo.project, obj);\
   }\
  return TRUE;\
}

//STANDALONE_PUT_INT_FIELD_FUNC(minpixels); special case

STANDALONE_PUT_INT_FIELD_FUNC (tx);
STANDALONE_PUT_INT_FIELD_FUNC (ty);
STANDALONE_PUT_INT_FIELD_FUNC (gx);
STANDALONE_PUT_INT_FIELD_FUNC (gy);

STANDALONE_PUT_INT_FIELD_FUNC (override);

void put_standalone_directive (gchar *tag, gint value) {
  DenemoObject *obj = lily_directive_new (" ");
  DenemoDirective *directive = (DenemoDirective *) obj->object;
  directive->tag = g_string_new (tag);
  obj->minpixelsalloted = directive->minpixels = value;
  object_insert (Denemo.project, obj);
}

gboolean
standalone_directive_put_minpixels (gchar * tag, gint value)
{
  DenemoDirective *directive = get_standalone_directive (tag);
  if (directive)
    {
      directive->minpixels = value;     //This field is not actually useful for standalone directives.
      DenemoObject *obj = get_object ();
      store_for_undo_change (Denemo.project->movement, obj);
      obj->minpixelsalloted = value;
    }
  else
    {
      put_standalone_directive (tag, value);
    }
  return TRUE;
}



static gboolean
tag_choice (GtkWidget * widget, DenemoDirective ** response)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
    *response = g_object_get_data (G_OBJECT (widget), "choice");
  return TRUE;
}

static void
tag_none (GtkWidget * widget, DenemoDirective ** response)
{
  *response = NULL;
}

#define UNKNOWN_TAG "<Unknown Tag>"

/* pack radio buttons for directive choice */
static gint
pack_buttons (GtkWidget * vbox, GList * directives, DenemoDirective ** response)
{
  GList *g;
  gint count;
  GtkWidget *widget = NULL, *widget2;
  for (count = 0, g = directives; g; g = g->next)
    {
      DenemoDirective *directive = (DenemoDirective *) g->data;
      if (directive->tag == NULL)
        directive->tag = g_string_new (UNKNOWN_TAG);
      count++;
      if (*response == NULL)
        *response = directive;
      if (widget == NULL)
        {
          widget = gtk_radio_button_new_with_label (NULL, directive->tag->str); //FIXME get_label_for_tag() and get_tooltip_for_tag() here!!!
          g_object_set_data (G_OBJECT (widget), "choice", directive);
          g_signal_connect (G_OBJECT (widget), "toggled", G_CALLBACK (tag_choice), response);
          gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);
        }
      else
        {
          widget2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (widget), directive->tag->str);
          g_object_set_data (G_OBJECT (widget2), "choice", directive);
          g_signal_connect (G_OBJECT (widget2), "toggled", G_CALLBACK (tag_choice), response);
          gtk_box_pack_start (GTK_BOX (vbox), widget2, FALSE, TRUE, 0);
        }
    }
  return count;
}


/* let the user choose from a list of directives */
static DenemoDirective *
select_directive (gchar * instr, GList * directives)
{

  GtkWidget *dialog = gtk_dialog_new_with_buttons (_("Select Directive"),
                                                   GTK_WINDOW (Denemo.window),
                                                   (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
                                                   _("_OK"), GTK_RESPONSE_ACCEPT,
                                                   NULL);

  GtkWidget *cancelbutton = gtk_dialog_add_button (GTK_DIALOG(dialog),_("Cancel"), GTK_RESPONSE_REJECT);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  GtkWidget *content_area;
  if(g_list_length(directives)>Denemo.prefs.max_menu_size) //this doesn't avoid menu running off screen but allows the user to cancel in that case
    {
    content_area = gtk_widget_get_parent (cancelbutton); //get the action area - this will break if GTK decides to implement the dialog widget in some strange fashion
    gtk_box_pack_start (GTK_BOX (content_area), vbox, FALSE, TRUE, 0);
    } else
    {
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    gtk_container_add (GTK_CONTAINER (content_area), vbox);
    }

  DenemoDirective *response = NULL;

/*
    void                user_function                      (GtkDialog *arg0,
                                                        gpointer   user_data)      : Action
    The ::close signal is a keybinding signal which gets emitted when the user uses a keybinding to close the dialog.
    The default binding for this signal is the Escape key.
*/
  g_signal_connect (G_OBJECT (dialog), "close", G_CALLBACK (tag_none), &response);



  gint count;                   //count tagged directives
  GtkWidget *widget;
  widget = gtk_label_new (instr);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);
  count = pack_buttons (vbox, directives, &response);

  if (count > 0)
    {
      gtk_widget_show_all (dialog);
      if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_REJECT)
        {
          response = NULL;
        }
    }
  gtk_widget_destroy (dialog);
  //if(response && response->tag)
  //g_debug("Came back with response %s\n", response->tag->str);
  return response;
}


/* let the user choose from the directives at the cursor */
static void
user_select_directive_at_cursor (gchar ** what, GList *** pdirectives, DenemoDirective ** pdirective)
{
  *pdirectives = NULL;
  *pdirective = get_standalone_directive (NULL);

  if (*pdirective)
    return;                     //FIXME is this needed???? a return will be done anyway

  {
    tuplet *curtuplet = get_tuplet ();
    if (curtuplet && curtuplet->directives)
      {
        gchar *instr = get_object()->type==TUPOPEN? _("Select a directive attached to the tuplet start object"):
                    _("Select a directive attached to the tuplet end object");
        *pdirectives = &curtuplet->directives;
        *what = "tuplet";
        *pdirective = select_directive (instr, **pdirectives);
      }
  }
  {
    gchar *instr = _("Select a directive attached to the stem control object");
    stemdirective *curstemdir = get_stemdirective ();
    if (curstemdir && curstemdir->directives)
      {
        *pdirectives = &curstemdir->directives;
        *what = "stemdir";
        *pdirective = select_directive (instr, **pdirectives);
      }

  }

  gchar *name = NULL;
  note *curnote = get_note ();
  if (curnote != NULL)
    {
      name = mid_c_offsettolily (curnote->mid_c_offset, curnote->enshift);
      if (curnote->mid_c_offset == Denemo.project->movement->cursor_y)
        if (curnote->directives)
          {
            *pdirectives = &curnote->directives;
            *what = "note";
            gchar *instr = g_strdup_printf (_("Select a directive attached to the note \"%s\""), name);
            if (g_list_length (curnote->directives) == 1)
                *pdirective = (DenemoDirective*)(curnote->directives->data);
            else
                *pdirective = select_directive (instr, **pdirectives);
            g_free (instr);
            if (*pdirective)
              {
                g_free (name);
                return;
              }
          }
    }


  {
    // not exactly on a note, offer any chord directives
    gchar *instr = _("Select a directive attached to the chord");
    chord *curchord = get_chord ();
    if (curchord && curchord->directives)
      {
        *pdirectives = &curchord->directives;
        *what = "chord";
        if (g_list_length (curchord->directives) == 1)
            *pdirective = (DenemoDirective*)(curchord->directives->data);
        else
            *pdirective = select_directive (instr, **pdirectives);
      }
  }
  if (*pdirective == NULL && curnote)   //try nearest note
    if (curnote->directives && curnote->mid_c_offset != Denemo.project->movement->cursor_y)
      {
        *pdirectives = &curnote->directives;
        *what = "note";
        gchar *instr = g_strdup_printf (_("Select a directive attached to the note \"%s\""), name);
        if (g_list_length (curnote->directives) == 1)
            *pdirective = (DenemoDirective*)(curnote->directives->data);
        else
            *pdirective = select_directive (instr, **pdirectives);
        g_free (instr);
        if (*pdirective && (g_list_length (**pdirectives) == 1))
          {
            /* seek confirmation of the choice of this directive since it is on a note not pointed at and
               has been chosen automatically. */
            gchar *name = mid_c_offsettolily (curnote->mid_c_offset, curnote->enshift);
            gchar *msg = g_strdup_printf (_("Select the directive %s on note \"%s\"?"), (*pdirective)->tag->str, name);

            if (!confirm (_("Select Directive"), msg))
              *pdirective = NULL;
            g_free (name);
            g_free (msg);
          }
      }
  g_free (name);
  return;
}

gboolean choose_tag_at_cursor (gchar **ptag) {
    gchar *what;
    GList **ppdirectives;
    DenemoDirective *pdirective;
    user_select_directive_at_cursor (&what, &ppdirectives, &pdirective);
    if (pdirective && (pdirective->tag == NULL))
        pdirective->tag = g_string_new ("<Unknown Tag>");
    *ptag = pdirective?pdirective->tag->str:NULL;
    return g_strcmp0 (what, "chord");
}

static void
populate_menu_for_directive (GtkWidget * menu, DenemoDirective * directive)
{
  GtkWidget *item = gtk_menu_item_new_with_label (directive->tag->str);
  gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), GTK_WIDGET (item));
  g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (button_activate_callback), directive);
  gtk_widget_show (item);

}

static void
populate_menu_for_directives (GtkWidget * menu, GList * directives)
{
  g_object_set_data (G_OBJECT (menu), "directives", directives);
  //g_debug("setting directives %p for menu %p\n", directives, menu);
  for (; directives; directives = directives->next)
    {
      populate_menu_for_directive (menu, directives->data);
    }
}

/* callback for deactivate signal installed at startup on the NoteEditPopup menu
   it removes the menu items for the specific note
 */
gboolean
unpopulate_menu (GtkWidget * menu)
{
  GList *directives = g_object_get_data (G_OBJECT (menu), "directives");
  //g_debug("removing directives %p for menu %p\n", directives, menu);
  for (; directives; directives = directives->next)
    {
      DenemoDirective *directive = directives->data;
      //g_debug("now remove %p\n", directive->widget);
      if (directive->widget)
        gtk_container_remove (GTK_CONTAINER (menu), directive->widget);
    }
  g_object_set_data (G_OBJECT (menu), "directives", NULL);
  return FALSE;
}

// edit the object at the cursor based on its type
void
edit_object_type (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoObject *obj = get_object ();
  if (obj == NULL)
    {
      warningmessage (_("No object here to edit"));
      return;
    }
  switch (obj->type)
    {
    case LILYDIRECTIVE:
      edit_object_directive (NULL, NULL);
      return;
    case CLEF:
      {
        popup_menu ("ClefMenu");
      }
      return;
    case KEYSIG:
      {
        popup_menu ("Key");
      }
      return;
    case TIMESIG:
      {
        popup_menu ("TimeSig");
      }
      return;
    case CHORD:
      {

          popup_menu ("NotesRests");


      }
      return;

    case STEMDIRECTIVE:
      {
        GList *directives = ((stemdirective *) obj->object)->directives;
        if (directives)
          {
            GtkWidget *menu = gtk_menu_new ();
            populate_menu_for_directives (menu, directives);
            gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
          }
        else
          {
            infodialog (_("Nothing to edit on this stem direction control object - use controls in Staffs → Voices menu"));
          }
      }
      return;
    case TUPOPEN:
      popup_menu ("Tuplets");
      return;
    case TUPCLOSE:
      infodialog (_("This marks the end of a tuplet (that is triplets etc) - it should come in the same measure as the tuplet start marker."));
      return;
    default:
      warningdialog (_("No method for editing this type of object"));
      return;
    }
}


static gboolean
set_gstring (GtkWidget * widget, GdkEventKey * event, GString * gstr)
{
  g_string_assign (gstr, (gchar *) gtk_entry_get_text (GTK_ENTRY (widget)));
  return TRUE;
}

static gboolean
set_int (GtkSpinButton * widget, gint * val)
{
  *val = gtk_spin_button_get_value_as_int (widget);
  return TRUE;
}

static gboolean
set_uint (GtkSpinButton * widget, guint * val)
{
  *val = (guint) gtk_spin_button_get_value (widget);
  return TRUE;
}


static gchar *
quote_scheme (gchar * s)
{
  GString *dest = g_string_new ("");
  gchar *c;
  for (c = s; *c; c++)
    {
      if (*c == '"' || *c == '\\')
        g_string_append_c (dest, '\\');
      g_string_append_c (dest, *c);
    }
  return g_string_free (dest, FALSE);
}

gchar *get_script_for_directive (DenemoDirective* directive, gchar * what)
 {
  GString *scheme = g_string_new ("");
  g_string_append_printf (scheme, "(let ((tag \"%s\"))\n", directive->tag->str);
  if (what == NULL)
      what = "standalone";

  if (!strcmp (what, "standalone"))
      g_string_append (scheme, "(d-Directive-standalone tag)\n");
#define ADD_TEXT(field)\
if(directive->field && directive->field->len)\
  {gchar *quote = quote_scheme(directive->field->str);\
   g_string_append_printf(scheme, "(d-DirectivePut-%s-%s tag \"%s\")\n",\
       what, #field, quote);\
   g_free(quote);}
  ADD_TEXT (prefix);
  ADD_TEXT (postfix);
  ADD_TEXT (display);
  ADD_TEXT (grob);
  ADD_TEXT (midibytes);
  ADD_TEXT (data);
 //graphic_name is exceptional, the graphic field is filled in from it
if(directive->graphic_name && directive->graphic_name->len)
  {
    gchar *quote = quote_scheme(directive->graphic_name->str);
    g_string_append_printf(scheme, "(d-DirectivePut-%s-graphic tag \"%s\")\n",
       what, quote);
    g_free(quote);
   }
#undef ADD_TEXT
#define ADD_INTTEXT(field)\
if(directive->field)\
  g_string_append_printf(scheme, "(d-DirectivePut-%s-%s tag %d)\n",\
       what, #field, directive->field);
  ADD_INTTEXT (minpixels);
  ADD_INTTEXT (override);
//  ADD_INTTEXT (x); FIXME would it be good for the script to make the directive conditional if the original is? ie to install the flag and layouts?
//  ADD_INTTEXT (y);
  ADD_INTTEXT (tx);
  ADD_INTTEXT (ty);
  ADD_INTTEXT (gx);
  ADD_INTTEXT (gy);

#undef ADD_INTTEXT
  if (!strcmp (what, "note"))
      g_string_append (scheme, "(d-Chordize)(d-SetSaved #f))\n");
  else if (!strcmp (what, "standalone"))
            g_string_append (scheme, "(d-SetSaved #f)(d-RefreshDisplay))\n");
        else
            g_string_append (scheme, "(d-SetSaved #f))\n");
 return g_string_free (scheme, FALSE);
}

static void
create_script (DenemoDirective * directive, gchar * what)
{
  gchar *scheme = get_script_for_directive (directive, what);
  appendSchemeText (scheme);
  g_free (scheme);
}


/* callback to get an upload script of name tag */
#ifdef UPLOAD_TO_DENEMO_DOT_ORG
static void
upload_edit_script_cb (GtkWidget * widget, gchar * tag)
{
  gchar *filename = get_editscript_filename (tag);
  if (filename)
    {
      GError *error = NULL;
      gchar *script;
      if (g_file_get_contents (filename, &script, NULL, &error))
        upload_edit_script (tag, script);
      g_free (script);
      g_free (filename);
    }
}
#endif

/* callback to get an edit script of name tag into the Scheme Script window */
static void
get_edit_script (GtkWidget * widget, gchar * tag)
{
  gchar *filename = get_editscript_filename (tag);
  if (filename)
    {
      GError *error = NULL;
      gchar *script;
      if (g_file_get_contents (filename, &script, NULL, &error))
        appendSchemeText (script);
      else
        g_warning (_("Could not get contents of %s"), filename);
      g_free (script);
      g_free (filename);
    }
}


/* callback to save the scheme script text buffer as an edit script of name tag in the user's local denemo directory */
static void
put_edit_script (GtkWidget * widget, gchar * tag)
{
  gchar *tagscm = g_strconcat (tag, ".scm", NULL);
  gchar *filename = g_build_filename (get_user_data_dir (TRUE), COMMANDS_DIR, "editscripts", tagscm, NULL);
  if ((!g_file_test (filename, G_FILE_TEST_EXISTS)) || confirm (_("There is already an edit script for this tag"), _("Do you want to replace it?")))
    {
      gchar *scheme = (gchar *) get_script_view_text ();
      if (scheme && *scheme)
        {
          FILE *fp = fopen (filename, "w");
          if (fp)
            {
              fprintf (fp, "%s", scheme);
              fclose (fp);
              infodialog (_("Wrote edit script file to ~/.denemo/editscripts"));
            }
          g_free (scheme);
        }
    }
  g_free (tagscm);
  g_free (filename);
}

static gboolean
activate_directive (DenemoDirective * directive, gchar * what)
{
  if (directive->widget && GTK_IS_WIDGET (directive->widget))
    {
      g_debug ("Activate");
      gtk_widget_activate (directive->widget);
      //g_signal_emit!!!!!!!!!!!!!!! what do we do!!!!!!!!!!!(directive->widget, "button-release-event");
      return TRUE;
    }
  return FALSE;
}

static void
help_for_conditional (gchar *help)
{
    warningdialog (help);

}
/* text_edit_directive
   textually edit the directive via a dialog.
   return FALSE if the user requests deletion of the directive.
*/
static gboolean
text_edit_directive (DenemoDirective * directive, gchar * what)
{
  gboolean ret = TRUE;
#define CREATE_SCRIPT (2)
  DenemoDirective *clone = clone_directive (directive); //for reset

  GtkWidget *dialog = gtk_dialog_new_with_buttons (_("Low Level Denemo Directive Edit"),
                                                   GTK_WINDOW (Denemo.window),
                                                   (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
                                                   _("_OK"), GTK_RESPONSE_ACCEPT,
                                                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                                                   NULL);
 // gtk_window_set_title (GTK_WINDOW (dialog), _("Denemo Object Editor"));

  gtk_dialog_add_button (GTK_DIALOG (dialog), _("Delete Directive"), GTK_RESPONSE_REJECT);
  gtk_dialog_add_button (GTK_DIALOG (dialog), _("Create Script"), CREATE_SCRIPT);

  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), vbox);

  GtkWidget *hbox;
  GString *entrycontent = g_string_new ("");
  GtkWidget *entrywidget;
  GtkWidget *label;
  GtkWidget *button;
#define TEXTENTRY(thelabel, field) \
  G_GNUC_UNUSED GtkWidget *field;\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_entry_new ();\
  g_string_sprintf (entrycontent, "%s", directive->field?directive->field->str:"");\
  gtk_entry_set_text (GTK_ENTRY (entrywidget), entrycontent->str);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  if(directive->field==NULL) directive->field=g_string_new("");\
  g_signal_connect(G_OBJECT(entrywidget), "key-release-event", G_CALLBACK(set_gstring), directive->field);\
  g_string_assign(entrycontent, "");

#define NEWINTENTRY(thelabel, field)\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_spin_button_new_with_range (-(gdouble)G_MAXINT, (gdouble)G_MAXINT, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (entrywidget), directive->field);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  g_signal_connect(G_OBJECT(entrywidget), "value-changed", G_CALLBACK(set_int), &directive->field);
#define NEWUINTENTRY(thelabel, field)\
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_spin_button_new_with_range (0.0, (gdouble)G_MAXUINT, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (entrywidget), *(guint*)&directive->field);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  g_signal_connect(G_OBJECT(entrywidget), "value-changed", G_CALLBACK(set_uint), &directive->field);
#define ADDINTENTRY(thelabel, fieldx, fieldy)\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  label = gtk_label_new (_(" x:"));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_spin_button_new_with_range (-(gdouble)G_MAXINT, (gdouble)G_MAXINT, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (entrywidget), directive->fieldx);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  g_signal_connect(G_OBJECT(entrywidget), "value-changed", G_CALLBACK(set_int), &directive->fieldx);\
  label = gtk_label_new (_(" y:"));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  entrywidget = gtk_spin_button_new_with_range (-(gdouble)G_MAXINT, (gdouble)G_MAXINT, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (entrywidget), directive->fieldy);\
  gtk_box_pack_start (GTK_BOX (hbox), entrywidget, TRUE, TRUE, 0);\
  g_signal_connect(G_OBJECT(entrywidget), "value-changed", G_CALLBACK(set_int), &directive->fieldy);

  TEXTENTRY (_("Postfix"), postfix);
  TEXTENTRY (_("Prefix"), prefix);
  TEXTENTRY (_("Display text"), display);
  ADDINTENTRY (_("Text Position"), tx, ty);
  TEXTENTRY (_("Graphic"), graphic_name);
  ADDINTENTRY (_("Graphic Position"), gx, gy);
  TEXTENTRY (_("Tag"), tag);
  TEXTENTRY (_("LilyPond Grob Name"), grob);
  TEXTENTRY (_("Scheme Data"), data);
  TEXTENTRY (_("MidiBytes"), midibytes);
  NEWUINTENTRY (_("Override Mask"), override);
  NEWINTENTRY (_("Horizontal Display Space"), minpixels);
#undef TEXTENTRY

  if (directive->layouts == NULL)
    {
        button = gtk_button_new_with_label (_("Applies to all layouts"));
        //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
        //GdkRGBA color;
        //get_color (&color, 0.0, 1.0, 0.0, 1.0);
        //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
        
        set_foreground_color(button, "#00ff00");
        
        
        g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (help_for_conditional), _("This directive is honored by all layouts. Use the Score/Movement/Staff/Voice/Object Editor to make it conditional on the Current Layout, the Default Layout or the Default Layout for the current part."));
    }
  else
    {
        gboolean wrong = wrong_layout (directive, Denemo.project->layout_id);//g_print ("Current layout %x directive->flag %d and directive->layouts->data %x which is wrong = %d\n", Denemo.project->layout_id, directive->flag, directive->layouts->data, wrong);
        if (directive->flag == DENEMO_ALLOW_FOR_LAYOUTS)
            {
              button = gtk_button_new_with_label (wrong?
                                _("Applies only to certain layouts, excluding the current one.")
                                :_("Applies only to certain layouts, including the current one."));
              //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
              //GdkRGBA color;
              //get_color (&color, wrong?1:0, wrong?0:1, 0.0, 1.0);
              //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
              
              set_foreground_color(button, wrong?"#ff0000":"#00ff00");
              g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (help_for_conditional), _("This directive is honored only by certain layouts. Use the Score/Movement/Staff/Voice/Object Editor to alter this behavior."));
            }
        else
            {
              button = gtk_button_new_with_label (wrong?
                                _("Excludes the current layout.")
                                :_("Excludes certain layouts, but applies to the current one."));
              //GtkWidget *labelwidget = (GtkWidget *) gtk_bin_get_child (GTK_BIN (button));
              //GdkRGBA color;
              //get_color (&color, wrong?1:0, wrong?0:1, 0.0, 1.0);
              //gtk_widget_override_color (labelwidget, GTK_STATE_FLAG_NORMAL, &color);
              set_foreground_color(button, wrong?"#ff0000":"#00ff00");
              
              
              g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (help_for_conditional), _("This directive is disregarded by certain layouts. Use the Score/Movement/Staff/Voice/Object Editor to alter this behavior."));
            }
    }
    
    if (directive->locked)
      {
              button = gtk_button_new_with_label (_("Locked Directive"));
              set_foreground_color(button, "#a01000");
              g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (warningdialog), _("This directive is locked - usually to offer options other than just delete. If needed you can get rid of the directive by selecting it and using Edit->Cut."));
      }
    
 gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Get Edit Script"));
  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (get_edit_script), directive->tag->str);
  button = gtk_button_new_with_label (_("Put Edit Script"));
  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (put_edit_script), directive->tag->str);
#ifdef UPLOAD_TO_DENEMO_DOT_ORG
//disabled until website can take uploading again
  button = gtk_button_new_with_label (_("Upload Edit Script"));
  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (upload_edit_script_cb), directive->tag->str);
#endif
#ifdef EXTRA_WORK
  button = gtk_check_button_new_with_label (_("Show Current Script"));
  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);
 this should set_toggle scheme or some such... gtk_activatable_set_related_action (GTK_ACTIVATABLE (button), gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ViewMenu/ToggleScript"));
#endif
  gtk_widget_show_all (dialog);
  gint response = gtk_dialog_run (GTK_DIALOG (dialog));
  //g_debug("Got response %d\n", response);


  if (response == GTK_RESPONSE_CANCEL || response == GTK_RESPONSE_DELETE_EVENT || response == GTK_RESPONSE_REJECT)
    {
      GtkWidget *ref_widget = directive->widget;        //not cloned
      directive->widget = NULL; //prevent any button being destroyed
      free_directive_data (directive);
      memcpy (directive, clone, sizeof (DenemoDirective));
      directive->widget = ref_widget;
      if (response == GTK_RESPONSE_REJECT)
        {
          ret = FALSE;          //that is it may be deleted, we ensure it has not been changed first,as the tag is used for delelet
        }
    }
  else
    {
      clone->widget = NULL;     //prevent any button being destroyed FIXME ???
      free_directive (clone);
      score_status (Denemo.project, TRUE);
    }
#define REMOVEEMPTIES(field)\
if(directive->field && directive->field->len==0) g_string_free(directive->field, TRUE), directive->field=NULL;
  REMOVEEMPTIES (postfix);
  REMOVEEMPTIES (prefix);
  REMOVEEMPTIES (display);
  REMOVEEMPTIES (graphic_name);

  //REMOVEEMPTIES(tag); don't allow NULL tag
#undef REMOVEEMPTIES

  if (directive->tag && directive->tag->len == 0)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (directive->widget)
    {
      if (GTK_IS_WIDGET (directive->widget))
        widget_for_directive (directive, NULL /* no need to pass fn in as it is only needed if there is not widget, g_object_get_data(directive->widget, "fn") */ );
    }
  if (directive->graphic_name && directive->graphic)
    {
      loadGraphicItem (directive->graphic_name->str, (DenemoGraphic **) & directive->graphic);
    }
  gtk_widget_destroy (dialog);
  if (what && (response == CREATE_SCRIPT))
    create_script (directive, what);    //g_debug("(d-DirectivePut-%s \"%s\")\n", what, directive->tag->str);
  if (what && strcmp(what, "note"))
      directive->minpixels = abs(directive->minpixels);// negative values for note directives specify the space is to be before the note
  {
    GList *curObj = Denemo.project->movement->currentobject;
    if (curObj && (DenemoObject *) curObj->data)
      setpixelmin ((DenemoObject *) curObj->data);
  }
 
  find_xes_in_all_measures (Denemo.project->movement);

  return ret;
}
gboolean low_level_directive_edit (DenemoDirective *directive)
{
  return text_edit_directive (directive, NULL);

}
#define TEXT_EDIT_IF(what)\
  if(fn == (void(*)())what##_directive_put_graphic)\
    return text_edit_directive(directive, #what);
static gboolean
text_edit_directive_by_fn (DenemoDirective * directive, gpointer fn)
{
  TEXT_EDIT_IF (note);
  TEXT_EDIT_IF (chord);
  TEXT_EDIT_IF (staff);
  TEXT_EDIT_IF (voice);
  TEXT_EDIT_IF (score);
  TEXT_EDIT_IF (clef);
  TEXT_EDIT_IF (timesig);
  TEXT_EDIT_IF (tuplet);
  TEXT_EDIT_IF (stemdirective);
  TEXT_EDIT_IF (keysig);
  TEXT_EDIT_IF (scoreheader);
  TEXT_EDIT_IF (header);
  TEXT_EDIT_IF (paper);
  TEXT_EDIT_IF (layout);
  TEXT_EDIT_IF (movementcontrol);
  TEXT_EDIT_IF (standalone);
  return TRUE;
}

#undef TEXT_EDIT_IF



/* allow edit of a directive, either via script or textually if no script exists
   return FALSE if user confirms a request to delete the directive
*/
static gboolean
edit_directive (DenemoDirective * directive, gchar * what)
{
  gboolean ret = TRUE;
  gchar *filename = get_editscript_filename (directive->tag->str);
  if (filename == NULL)
    {
      DenemoAction *action;
      gchar *eol;
      gboolean chopped = FALSE;
      for (eol = directive->tag->str; *eol; eol++)
        {
          if (*eol == '\n')
            {
              *eol = 0;
              chopped = TRUE;
              break;
            }
        }

      action = lookup_action_from_name (directive->tag->str);
      if (chopped)
        *eol = '\n';
      if (action && !shift_held_down())
        {
          DenemoScriptParam param;
          param.string = g_string_new ("edit");
          g_debug ("Script can look for params \"edit\" - a string to catch this");
          activate_script (action, &param);
          g_string_free (param.string, TRUE);
        }
      else
        { if (what || shift_held_down())
            {
                ret = (text_edit_directive (directive, what) || !confirm (_("Directive Delete"), _("Are you sure you want to delete the directive?")));
                score_status (Denemo.project, TRUE);
            }
        else
            edit_object();
        }
      return ret;
    }
  GError *error = (GError *) execute_script_file (filename);
  if (error)
    g_warning ("%s", error->message);
  g_free (filename);
  return ret;
}


/**
 * callback for EditDirective on directive attached to an object.
 */
void
edit_object_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive;
  GList **directives;
  gchar *what = NULL;
  user_select_directive_at_cursor (&what, &directives, &directive);

  if (directive == NULL)
    {
      if (directives && *directives != NULL)
        infodialog (_("Cancelled"));
      else
        warningdialog (_("Use the ObjectMenu to modify this object - there are no directives here"));
      return;
    }
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!(param ? text_edit_directive (directive, what) : edit_directive (directive, what)))
    {
      if (directives && *directives)
        {
          delete_directive (directives, directive->tag->str);
        }
      else
        {                       //standalone directive
          dnm_deleteobject (Denemo.project->movement);
        }
    }
  if (Denemo.project->movement->currentobject)        //for standalone directive
    setpixelmin (Denemo.project->movement->currentobject->data);
}

/**
 * callback for DeleteDirective
 */
void
delete_chord_or_note_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive;
  GList **directives;
  gchar *what = NULL;
  user_select_directive_at_cursor (&what, &directives, &directive);
  //g_debug("Got directive %p in list %p\n", directive, directives);
  if (directives == NULL)
    {
      warningdialog (_("No directives here"));
      return;
    }
  if (directive == NULL)
    {
      warningdialog (_("No directive selected"));
      return;
    }
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (confirm (_("Directive Delete"), _("Are you sure you want to delete the directive?")))
    delete_directive (directives, directive->tag->str);
  else
    warningdialog (_("Operation cancelled"));
}

static GList *all_directives = NULL;
static GList *directive_types = NULL;
static void
append_directives (DenemoDirective *direc, gchar *type)
{
    all_directives = g_list_append (all_directives, direc);
    directive_types = g_list_append (directive_types, type);//g_print("tag %s\n", direc->tag->str);
}

static gint
select_system_directive (void)
{

    g_list_foreach (Denemo.project->lilycontrol.directives, (GFunc)append_directives, "lilycontrol");
    g_list_foreach (Denemo.project->scoreheader.directives, (GFunc)append_directives, "scoreheader");
    g_list_foreach (Denemo.project->paper.directives, (GFunc)append_directives, "paper");
    g_list_foreach (Denemo.project->movement->header.directives, (GFunc)append_directives, "header");
    g_list_foreach (Denemo.project->movement->layout.directives, (GFunc)append_directives, "layout");
    g_list_foreach (Denemo.project->movement->movementcontrol.directives, (GFunc)append_directives, "movementcontrol");
    if(all_directives)
        {
            DenemoDirective *d;
            d = select_directive (_("Select a score or movement directive for advanced (low-level) edit.\nNote: these directives can be edited normally using Score/Movement Properties Editor from the Score or Movement menus."), all_directives);
            if (d)
                return g_list_index (all_directives, d);
        }
    return -1;
}
void edit_system_directive (void)
{
    gint index = select_system_directive ();
    if(index >= 0)
        {
            gchar *type =  g_list_nth_data (directive_types, index);
            DenemoDirective *directive = g_list_nth_data (all_directives, index);
            gboolean delete = !text_edit_directive (directive, type);
            if (delete)
                {
                    GList **directives = NULL;
                    if (!strcmp(type, "lilycontrol"))
                        directives = &Denemo.project->lilycontrol.directives;
                    else if (!strcmp(type, "scoreheader"))
                        directives = &Denemo.project->scoreheader.directives;
                    else if (!strcmp(type, "paper"))
                        directives = &Denemo.project->paper.directives;
                    else if (!strcmp(type, "header"))
                        directives = &Denemo.project->movement->header.directives;
                    else if (!strcmp(type, "layout"))
                        directives = &Denemo.project->movement->layout.directives;
                    else if (!strcmp(type, "movementcontrol"))
                        directives = &Denemo.project->movement->movementcontrol.directives;
                    if (directives)
                            delete_directive (directives, directive->tag->str);
                    else
                            g_warning ("Could not get directives list to delete from");
                }
            signal_structural_change (Denemo.project);
        }
    g_list_free (all_directives);
    g_list_free (directive_types);
    all_directives = NULL;
    directive_types = NULL;
}

static DenemoDirective *
select_score_directive (void)
{
  if (Denemo.project->lilycontrol.directives == NULL)
    return NULL;
  return select_directive (_("Select a score directive - use Shift for advanced edit"), Denemo.project->lilycontrol.directives);
}
static DenemoDirective *
select_scoreheader_directive (void)
{
  if (Denemo.project->scoreheader.directives == NULL)
    return NULL;
  return select_directive (_("Select a score header block directive - use Shift for advanced edit"), Denemo.project->scoreheader.directives);
}

static DenemoDirective *
select_paper_directive (void)
{
  if (Denemo.project->paper.directives == NULL)
    return NULL;
  return select_directive (_("Select a score paper block directive - use Shift for advanced edit"), Denemo.project->paper.directives);
}


static DenemoDirective *
select_header_directive (void)
{
  if (Denemo.project->movement->header.directives == NULL)
    return NULL;
  return select_directive (_("Select a movement header block directive - use Shift for advanced edit"), Denemo.project->movement->header.directives);
}

static DenemoDirective *
select_layout_directive (void)
{
  if (Denemo.project->movement->layout.directives == NULL)
    return NULL;
  return select_directive (_("Select a movement layout block directive - use Shift for advanced edit"), Denemo.project->movement->layout.directives);
}

static DenemoDirective *
select_movementcontrol_directive (void)
{
  if (Denemo.project->movement->movementcontrol.directives == NULL)
    return NULL;
  return select_directive (_("Select a movement control directive - use Shift for advanced edit"), Denemo.project->movement->movementcontrol.directives);
}

static DenemoDirective *
select_clef_directive (void)
{
  clef *curclef = get_clef ();
  if (curclef == NULL || curclef->directives == NULL)
    return NULL;
  return select_directive (_("Select a clef directive - use Shift for advanced edit"), curclef->directives);
}

static DenemoDirective *
select_keysig_directive (void)
{
  keysig *curkeysig = get_keysig ();
  if (curkeysig == NULL || curkeysig->directives == NULL)
    return NULL;
  return select_directive (_("Select a key signature directive - use Shift for advanced edit"), curkeysig->directives);
}

static DenemoDirective *
select_timesig_directive (void)
{
  timesig *curtimesig = get_timesig ();
  if (curtimesig == NULL || curtimesig->directives == NULL)
    return NULL;
  return select_directive (_("Select a time signature directive - use Shift for advanced edit"), curtimesig->directives);
}

static DenemoDirective *
select_tuplet_directive (void)
{
  tuplet *curtuplet = get_tuplet ();
  if (curtuplet == NULL || curtuplet->directives == NULL)
    return NULL;
  return select_directive (_("Select a time signature directive - use Shift for advanced edit"), curtuplet->directives);
}

static DenemoDirective *
select_stemdirective_directive (void)
{
  stemdirective *curstemdirective = get_stemdirective ();
  if (curstemdirective == NULL || curstemdirective->directives == NULL)
    return NULL;
  return select_directive (_("Select a time signature directive - use Shift for advanced edit"), curstemdirective->directives);
}

static DenemoDirective *
select_staff_directive (void)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  //FIXME return NULL if not primary staff
  if (curstaff == NULL || curstaff->staff_directives == NULL)
    return NULL;
  return select_directive (_("Select a staff directive - use Shift for advanced edit"), curstaff->staff_directives);
}

static DenemoDirective *
select_voice_directive (void)
{
  if (Denemo.project->movement->currentstaff == NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.project->movement->currentstaff->data;
  if (curstaff == NULL || curstaff->voice_directives == NULL)
    return NULL;
  return select_directive (_("Select a voice directive - use Shift for advanced edit"), curstaff->voice_directives);
}


/**
 * callback for EditVoiceDirective
 */
void
edit_voice_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_voice_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "voice"))
    delete_voice_directive (directive->tag->str);
  signal_structural_change (Denemo.project);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditStaffDirective
 */
void
edit_staff_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_staff_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "staff"))
    delete_staff_directive (directive->tag->str);
  signal_structural_change (Denemo.project);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditClefDirective
 */
void
edit_clef_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_clef_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "clef"))
    delete_clef_directive (directive->tag->str);
  signal_structural_change (Denemo.project);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditKeysigDirective
 */
void
edit_keysig_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_keysig_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "keysig"))
    delete_keysig_directive (directive->tag->str);
  signal_structural_change (Denemo.project);
  score_status (Denemo.project, TRUE);
}


/**
 * callback for EditTimesigDirective
 */
void
edit_timesig_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_timesig_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "timesig"))
    delete_timesig_directive (directive->tag->str);
  signal_structural_change (Denemo.project);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditTupletDirective
 */
void
edit_tuplet_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_tuplet_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "tuplet"))
    delete_tuplet_directive (directive->tag->str);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditStemdirectiveDirective
 */
void
edit_stemdirective_directive (DenemoAction * action, DenemoScriptParam * param)
{
  //g_debug("Edit directive called\n");
  DenemoDirective *directive = select_stemdirective_directive ();
  //g_debug("Got directive %p\n", directive);
  if (directive == NULL)
    return;
  if (directive->tag == NULL)
    directive->tag = g_string_new (UNKNOWN_TAG);
  if (!edit_directive (directive, "stemdirective"))
    delete_stemdirective_directive (directive->tag->str);
  score_status (Denemo.project, TRUE);
}

/**
 * callback for EditScoreDirective
 */
void
edit_score_directive (DenemoAction * action, DenemoScriptParam * param)
{
  signal_structural_change (Denemo.project);
#define ScoreDirectives  _("ScoreDirectives")
#define ScoreHeaderBlockDirectives  _("Score Header Block Directives")
#define ScorePaperBlockDirectives  _("Score Paper Block Directives")
#define HeaderBlockDirectives  _("Movement Header Block Directives")
#define LayoutBlockDirectives  _("Layout Block Directives")

#define STRINGAPPEND(field)  g_string_append_printf(options,"%s%c", field,'\0')
  GString *options = g_string_new ("");
  gchar *option;
  if (Denemo.project->lilycontrol.directives)
    STRINGAPPEND (ScoreDirectives);
  if (Denemo.project->scoreheader.directives)
    STRINGAPPEND (ScoreHeaderBlockDirectives);
  if (Denemo.project->paper.directives)
    STRINGAPPEND (ScorePaperBlockDirectives);
  if (Denemo.project->movement->header.directives)
    STRINGAPPEND (HeaderBlockDirectives);
  if (Denemo.project->movement->layout.directives)
    STRINGAPPEND (LayoutBlockDirectives);

  if (strlen (options->str) != options->len)
    {
      option = get_option (NULL, options->str, options->len);
      if (option == NULL)
        {
          g_string_free (options, TRUE);
          return;
        }
    }
  else
    option = options->str;
#define EDITTYPE(type, what)\
  if(!strcmp(option, type)) {\
    DenemoDirective *directive = select_##what##_directive();\
    if(directive==NULL)\
      return;\
    if(directive->tag == NULL)\
      directive->tag = g_string_new(UNKNOWN_TAG);\
    if(!edit_directive(directive, #what))\
      delete_##what##_directive(directive->tag->str);\
  score_status (Denemo.project, TRUE);\
  }


  EDITTYPE (ScoreDirectives, score);
  EDITTYPE (ScoreHeaderBlockDirectives, scoreheader);
  EDITTYPE (ScorePaperBlockDirectives, paper);
  EDITTYPE (HeaderBlockDirectives, header);
  EDITTYPE (LayoutBlockDirectives, layout);

  //g_debug("option was %s\n",option);
  g_string_free (options, TRUE);
#undef EDITTYPE
#undef STRINGAPPEND
}



/**
 * callback for EditMovementDirective
 */
void
edit_movement_directive (DenemoAction * action, DenemoScriptParam * param)
{
  signal_structural_change (Denemo.project);

#define LayoutDirectives  _("Layout Directives")
#define MovementDirectives  _("Movement Directives")
#define HeaderBlockDirectives  _("Movement Header Block Directives")

#define STRINGAPPEND(field)  g_string_append_printf(options,"%s%c", field,'\0')
  GString *options = g_string_new ("");
  gchar *option;
  if (Denemo.project->movement->layout.directives)
    STRINGAPPEND (LayoutDirectives);
  if (Denemo.project->movement->movementcontrol.directives)
    STRINGAPPEND (MovementDirectives);

  if (Denemo.project->movement->header.directives)
    STRINGAPPEND (HeaderBlockDirectives);

  if (strlen (options->str) != options->len)
    {
      option = get_option (NULL, options->str, options->len);
      if (option == NULL)
        {
          g_string_free (options, TRUE);
          return;
        }
    }
  else
    option = options->str;
#define EDITTYPE(type, what)\
  if(!strcmp(option, type)) {\
    DenemoDirective *directive = select_##what##_directive();\
    if(directive==NULL)\
      return;\
    if(directive->tag == NULL)\
      directive->tag = g_string_new(UNKNOWN_TAG);\
    if(!edit_directive(directive, #what))\
      delete_##what##_directive(directive->tag->str);\
  score_status (Denemo.project, TRUE);\
  }



  EDITTYPE (HeaderBlockDirectives, header);
  EDITTYPE (LayoutDirectives, layout);
  EDITTYPE (MovementDirectives, movementcontrol);

  g_string_free (options, TRUE);
#undef EDITTYPE
#undef STRINGAPPEND
}




/* block which can be copied for type of directive (minpixels is done as sample for new int fields */
PUT_LAYOUT_ALLOW_FUNC (clef)
PUT_LAYOUT_IGNORE_FUNC (clef)
PUT_INT_FIELD_FUNC (clef, tx)
PUT_INT_FIELD_FUNC (clef, ty)
PUT_INT_FIELD_FUNC (clef, gx)
PUT_INT_FIELD_FUNC (clef, gy)
PUT_INT_FIELD_FUNC (clef, override)
GET_INT_FIELD_FUNC (clef, tx)
GET_INT_FIELD_FUNC (clef, ty)
GET_INT_FIELD_FUNC (clef, gx)
GET_INT_FIELD_FUNC (clef, gy)
GET_INT_FIELD_FUNC (clef, override);
GET_INT_GRAPHIC_FIELD_FUNC (clef, width);
GET_INT_GRAPHIC_FIELD_FUNC (clef, height);
PUT_GRAPHIC (clef);
PUT_STR_FIELD_FUNC (clef, prefix);
PUT_STR_FIELD_FUNC (clef, postfix);
PUT_STR_FIELD_FUNC (clef, display);
GET_STR_FIELD_FUNC (clef, prefix);
GET_STR_FIELD_FUNC (clef, postfix);
GET_STR_FIELD_FUNC (clef, display)
/* end block which can be copied for type of directive */
PUT_LAYOUT_ALLOW_FUNC (keysig)
PUT_LAYOUT_IGNORE_FUNC (keysig)
PUT_INT_FIELD_FUNC (keysig, tx)
PUT_INT_FIELD_FUNC (keysig, ty)
PUT_INT_FIELD_FUNC (keysig, gx)
PUT_INT_FIELD_FUNC (keysig, gy)
PUT_INT_FIELD_FUNC (keysig, override)
GET_INT_FIELD_FUNC (keysig, tx)
GET_INT_FIELD_FUNC (keysig, ty)
GET_INT_FIELD_FUNC (keysig, gx)
GET_INT_FIELD_FUNC (keysig, gy)
GET_INT_FIELD_FUNC (keysig, override)
GET_INT_GRAPHIC_FIELD_FUNC (keysig, width)
GET_INT_GRAPHIC_FIELD_FUNC (keysig, height)
PUT_STR_FIELD_FUNC (keysig, prefix)
PUT_STR_FIELD_FUNC (keysig, postfix)
PUT_STR_FIELD_FUNC (keysig, display)
GET_STR_FIELD_FUNC (keysig, prefix)
GET_STR_FIELD_FUNC (keysig, postfix)
GET_STR_FIELD_FUNC (keysig, display)
PUT_LAYOUT_ALLOW_FUNC (timesig)
PUT_LAYOUT_IGNORE_FUNC (timesig)
PUT_INT_FIELD_FUNC (timesig, tx)
PUT_INT_FIELD_FUNC (timesig, ty)
PUT_INT_FIELD_FUNC (timesig, gx)
PUT_INT_FIELD_FUNC (timesig, gy)
PUT_INT_FIELD_FUNC (timesig, override)
GET_INT_FIELD_FUNC (timesig, tx)
GET_INT_FIELD_FUNC (timesig, ty)
GET_INT_FIELD_FUNC (timesig, gx)
GET_INT_FIELD_FUNC (timesig, gy)
GET_INT_FIELD_FUNC (timesig, override)
GET_INT_GRAPHIC_FIELD_FUNC (timesig, width)
GET_INT_GRAPHIC_FIELD_FUNC (timesig, height)
PUT_STR_FIELD_FUNC (timesig, prefix)
PUT_STR_FIELD_FUNC (timesig, postfix)
PUT_STR_FIELD_FUNC (timesig, display)
GET_STR_FIELD_FUNC (timesig, prefix)
GET_STR_FIELD_FUNC (timesig, postfix)
GET_STR_FIELD_FUNC (timesig, display)
PUT_LAYOUT_ALLOW_FUNC (tuplet)
PUT_LAYOUT_IGNORE_FUNC (tuplet)
PUT_INT_FIELD_FUNC (tuplet, tx)
PUT_INT_FIELD_FUNC (tuplet, ty)
PUT_INT_FIELD_FUNC (tuplet, gx)
PUT_INT_FIELD_FUNC (tuplet, gy)
PUT_INT_FIELD_FUNC (tuplet, override)
GET_INT_FIELD_FUNC (tuplet, tx)
GET_INT_FIELD_FUNC (tuplet, ty)
GET_INT_FIELD_FUNC (tuplet, gx)
GET_INT_FIELD_FUNC (tuplet, gy)
GET_INT_FIELD_FUNC (tuplet, override)
GET_INT_GRAPHIC_FIELD_FUNC (tuplet, width)
GET_INT_GRAPHIC_FIELD_FUNC (tuplet, height)
PUT_STR_FIELD_FUNC (tuplet, prefix)
PUT_STR_FIELD_FUNC (tuplet, postfix)
PUT_STR_FIELD_FUNC (tuplet, display)
GET_STR_FIELD_FUNC (tuplet, prefix)
GET_STR_FIELD_FUNC (tuplet, postfix)
GET_STR_FIELD_FUNC (tuplet, display)
PUT_LAYOUT_ALLOW_FUNC (stemdirective)
PUT_LAYOUT_IGNORE_FUNC (stemdirective)
PUT_INT_FIELD_FUNC (stemdirective, tx)
PUT_INT_FIELD_FUNC (stemdirective, ty)
PUT_INT_FIELD_FUNC (stemdirective, gx)
PUT_INT_FIELD_FUNC (stemdirective, gy)
PUT_INT_FIELD_FUNC (stemdirective, override)
GET_INT_FIELD_FUNC (stemdirective, tx)
GET_INT_FIELD_FUNC (stemdirective, ty)
GET_INT_FIELD_FUNC (stemdirective, gx)
GET_INT_FIELD_FUNC (stemdirective, gy)
GET_INT_FIELD_FUNC (stemdirective, override)
GET_INT_GRAPHIC_FIELD_FUNC (stemdirective, width)
GET_INT_GRAPHIC_FIELD_FUNC (stemdirective, height)
PUT_STR_FIELD_FUNC (stemdirective, prefix)
PUT_STR_FIELD_FUNC (stemdirective, postfix)
PUT_STR_FIELD_FUNC (stemdirective, display)
GET_STR_FIELD_FUNC (stemdirective, prefix)
GET_STR_FIELD_FUNC (stemdirective, postfix)
GET_STR_FIELD_FUNC (stemdirective, display)
PUT_LAYOUT_ALLOW_FUNC (scoreheader)
PUT_LAYOUT_IGNORE_FUNC (scoreheader)
GET_INT_FIELD_FUNC (scoreheader, tx)
GET_INT_FIELD_FUNC (scoreheader, ty)
GET_INT_FIELD_FUNC (scoreheader, gx)
GET_INT_FIELD_FUNC (scoreheader, gy)
GET_INT_FIELD_FUNC (scoreheader, override)
GET_INT_GRAPHIC_FIELD_FUNC (scoreheader, width)
GET_INT_GRAPHIC_FIELD_FUNC (scoreheader, height)
GET_STR_FIELD_FUNC (scoreheader, prefix)
GET_STR_FIELD_FUNC (scoreheader, postfix)
GET_STR_FIELD_FUNC (scoreheader, display)
PUT_LAYOUT_ALLOW_FUNC (header)
PUT_LAYOUT_IGNORE_FUNC (header)
GET_INT_FIELD_FUNC (header, tx)
GET_INT_FIELD_FUNC (header, ty)
GET_INT_FIELD_FUNC (header, gx)
GET_INT_FIELD_FUNC (header, gy)
GET_INT_FIELD_FUNC (header, override)
GET_INT_GRAPHIC_FIELD_FUNC (header, width)
GET_INT_GRAPHIC_FIELD_FUNC (header, height)
GET_STR_FIELD_FUNC (header, prefix)
GET_STR_FIELD_FUNC (header, postfix)
GET_STR_FIELD_FUNC (header, display)
PUT_LAYOUT_ALLOW_FUNC (paper)
PUT_LAYOUT_IGNORE_FUNC (paper)
GET_INT_FIELD_FUNC (paper, tx)
GET_INT_FIELD_FUNC (paper, ty)
GET_INT_FIELD_FUNC (paper, gx)
GET_INT_FIELD_FUNC (paper, gy)
GET_INT_FIELD_FUNC (paper, override)
GET_INT_GRAPHIC_FIELD_FUNC (paper, width)
GET_INT_GRAPHIC_FIELD_FUNC (paper, height)
GET_STR_FIELD_FUNC (paper, prefix)
GET_STR_FIELD_FUNC (paper, postfix)
GET_STR_FIELD_FUNC (paper, display)
PUT_LAYOUT_ALLOW_FUNC (layout)
PUT_LAYOUT_IGNORE_FUNC (layout)
GET_INT_FIELD_FUNC (layout, tx)
GET_INT_FIELD_FUNC (layout, ty)
GET_INT_FIELD_FUNC (layout, gx)
GET_INT_FIELD_FUNC (layout, gy)
GET_INT_FIELD_FUNC (layout, override)
GET_INT_GRAPHIC_FIELD_FUNC (layout, width)
GET_INT_GRAPHIC_FIELD_FUNC (layout, height)
GET_STR_FIELD_FUNC (layout, prefix)
GET_STR_FIELD_FUNC (layout, postfix)
GET_STR_FIELD_FUNC (layout, display)
PUT_LAYOUT_ALLOW_FUNC (movementcontrol)
PUT_LAYOUT_IGNORE_FUNC (movementcontrol)
GET_INT_FIELD_FUNC (movementcontrol, tx)
GET_INT_FIELD_FUNC (movementcontrol, ty)
GET_INT_FIELD_FUNC (movementcontrol, gx)
GET_INT_FIELD_FUNC (movementcontrol, gy)
GET_INT_FIELD_FUNC (movementcontrol, override)
GET_INT_GRAPHIC_FIELD_FUNC (movementcontrol, width)
GET_INT_GRAPHIC_FIELD_FUNC (movementcontrol, height)
GET_STR_FIELD_FUNC (movementcontrol, prefix)
GET_STR_FIELD_FUNC (movementcontrol, postfix)
GET_STR_FIELD_FUNC (movementcontrol, display)
#undef STANDALONE_PUT_INT_FIELD_FUNC
#undef PUT_GRAPHIC
#undef PUT_INT_FIELD_FUNC
#undef GET_INT_FIELD_FUNC
#undef PUT_STR_FIELD_FUNC
#undef GET_STR_FIELD_FUNC
     gchar *
     get_scoretitle (void)
{
  gchar *scoretitle = NULL;
  GList *first = Denemo.project->movements;
  if (first)
    {
      DenemoMovement *si = (DenemoMovement *) first->data;
      if (si)
        {
          DenemoDirective *directive = find_directive (si->header.directives, "Movement-title");
          if (directive && directive->display)
            scoretitle = directive->display->str;
        }
    }
  return scoretitle;
}

#define ACTIVATE_DIRECTIVE(what)\
gboolean activate_##what##_directive(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive)\
    return activate_directive(directive, #what);\
  return FALSE;\
}
#define TEXT_EDIT_DIRECTIVE(what)\
gboolean text_edit_##what##_directive(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive)\
    return text_edit_directive(directive, #what);\
  return FALSE;\
}\
ACTIVATE_DIRECTIVE(what)

TEXT_EDIT_DIRECTIVE (note);
TEXT_EDIT_DIRECTIVE (chord);
TEXT_EDIT_DIRECTIVE (staff);
TEXT_EDIT_DIRECTIVE (voice);
TEXT_EDIT_DIRECTIVE (score);
TEXT_EDIT_DIRECTIVE (clef);
TEXT_EDIT_DIRECTIVE (timesig);
TEXT_EDIT_DIRECTIVE (tuplet);
TEXT_EDIT_DIRECTIVE (stemdirective);
TEXT_EDIT_DIRECTIVE (keysig);
TEXT_EDIT_DIRECTIVE (scoreheader);
TEXT_EDIT_DIRECTIVE (header);
TEXT_EDIT_DIRECTIVE (paper);
TEXT_EDIT_DIRECTIVE (layout);
TEXT_EDIT_DIRECTIVE (movementcontrol);
TEXT_EDIT_DIRECTIVE (standalone);


#undef TEXT_EDIT_DIRECTIVE

#define GET_NTH_TAG(what, name)\
gchar *get_nth_##what##_tag(gint n) {\
  what *current = get_##what();\
  if(current==NULL) return NULL;\
  GList *g = g_list_nth(current->name, n);\
  if(g==NULL) return NULL;\
  DenemoDirective *directive = (DenemoDirective *)g->data;\
  return directive->tag->str;}

GET_NTH_TAG (note, directives);
GET_NTH_TAG (chord, directives);
GET_NTH_TAG (staff, staff_directives);
GET_NTH_TAG (voice, voice_directives);
GET_NTH_TAG (score, directives);
GET_NTH_TAG (clef, directives);
GET_NTH_TAG (timesig, directives);
GET_NTH_TAG (tuplet, directives);
GET_NTH_TAG (stemdirective, directives);
GET_NTH_TAG (keysig, directives);
GET_NTH_TAG (scoreheader, directives);
GET_NTH_TAG (header, directives);
GET_NTH_TAG (paper, directives);
GET_NTH_TAG (layout, directives);
GET_NTH_TAG (movementcontrol, directives);
#undef GET_NTH_TAG

gchar *get_nth_strict_note_tag(gint n)
    {
      note *current = get_strict_note();
      if(current==NULL) return NULL;
      GList *g = g_list_nth(current->directives, n);
      if(g==NULL) return NULL;
      DenemoDirective *directive = (DenemoDirective *)g->data;
      if (directive->tag==NULL) directive->tag = g_string_new (UNKNOWN_TAG);
      return directive->tag->str;
  }

const gchar *strict_note_directive_get_tag (gchar *tag)
    {
      note *current = get_strict_note();
      if(current==NULL) return NULL;
      GList *g = current->directives;
      for(;g; g=g->next)
          {
            DenemoDirective *directive = (DenemoDirective *)g->data;
            if(tag == NULL)
                return directive->tag?directive->tag->str:NULL;
            if (directive->tag && !strcmp (directive->tag->str, tag))
                return tag;
          }
      return NULL;
    }


/* gets the directive at the cursor a further call on the same object gets the next directive unless called on another object of the same type
 * which causes it to reset to the first directive */
DenemoDirective *get_next_directive_at_cursor (void)
{
  GList *directives = NULL;
  DenemoDirective *directive = NULL;
  note *current = get_strict_note();
  if(current)
    {static GList *last;
        directives = current->directives;
        if(directives)
            {
                if(last && (g_list_position(directives, last)>=0))
                {
                    last = last->next;
                    if(!last) last = directives;
                } else
                last = directives;
            directive =  last->data;
            }
    }
  if(directives==NULL)
    {
       chord *curchord = get_chord ();
       if(curchord)
           {static GList *last;
            directives = curchord->directives;
            if(directives)
                {
                    if(last && (g_list_position(directives, last)>=0))
                    {
                        last = last->next;
                        if(!last) last = directives;
                    } else
                    last = directives;
                directive =  last->data;
                }
           }
    }
  if(directives==NULL)
    {
        DenemoObject *currentobject = get_object ();
        if (currentobject)
            {static GList *last;
                if(currentobject->type == LILYDIRECTIVE)
                    {
                       directive = currentobject->object;
                    } else {
                        gpointer obj = currentobject->object;
                        directives = (currentobject->type==KEYSIG)?((keysig*)obj)->directives:
                        (currentobject->type==TIMESIG)?((timesig*)obj)->directives:
                        (currentobject->type==CLEF)?((clef*)obj)->directives:
                        (currentobject->type==STEMDIRECTIVE)?((stemdirective*)obj)->directives:
                        (currentobject->type==TUPOPEN)?((tuplet*)obj)->directives:
                        (currentobject->type==TUPCLOSE)?((tuplet*)obj)->directives:NULL;

                        if(directives)
                            {
                                if(last && (g_list_position(directives, last)>=0))
                                {
                                    last = last->next;
                                    if(!last) last = directives;
                                } else
                                last = directives;
                            directive = last->data;
                            }
                    }
            }
    }
    return directive;
}

static GList *move_to_front (GList *directives, gchar *tag)
{
    GList *el;
    gpointer data = NULL;
    for (el = directives;el;el=el->next)
    {
        DenemoDirective *directive = el->data;
        if (!(g_strcmp0 (tag, (gchar*) directive->tag->str)))
            {
                data = el->data;
                directives = g_list_remove (directives, data);
                break;
            }
    }
    if(data)
        return g_list_prepend (directives, data);
    return NULL;
}
#define REORDER_TAG(what, name)\
gboolean prioritize_##what##_tag (gchar *tag) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  GList *g = move_to_front (current->name, tag);\
  if(g==NULL) return FALSE;\
  current->name = g;\
  return TRUE;}

REORDER_TAG (note, directives);
REORDER_TAG (chord, directives);
REORDER_TAG (staff, staff_directives);
REORDER_TAG (voice, voice_directives);
REORDER_TAG (score, directives);
REORDER_TAG (clef, directives);
REORDER_TAG (timesig, directives);
REORDER_TAG (tuplet, directives);
REORDER_TAG (stemdirective, directives);
REORDER_TAG (keysig, directives);
REORDER_TAG (scoreheader, directives);
REORDER_TAG (header, directives);
REORDER_TAG (paper, directives);
REORDER_TAG (layout, directives);
REORDER_TAG (movementcontrol, directives);
#undef REORDER_TAG

gboolean wrong_layout (DenemoDirective *directive, guint id)
{
 if (Denemo.pending_layout_id)
  id = Denemo.pending_layout_id;
 if (directive->layouts)
    {
     if(directive->flag == DENEMO_ALLOW_FOR_LAYOUTS)
        {
           if (g_list_index (directive->layouts, GUINT_TO_POINTER(id))<0)
             return TRUE;
          return FALSE;
        }
       if(directive->flag == DENEMO_IGNORE_FOR_LAYOUTS)
        {
         if (g_list_index (directive->layouts, GUINT_TO_POINTER(id))>=0)
            return TRUE;
        }
    }
 return FALSE;
}
