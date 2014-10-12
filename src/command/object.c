/* object.cpp
 * functions that do operations to mudela objects
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <denemo/denemo.h>
#include "command/chord.h"
#include "core/utils.h"
#include "command/commandfuncs.h"
#include "command/object.h"
#include "command/staff.h"
#include "command/tuplet.h"
#include "command/select.h"
#include "audio/pitchentry.h"
#include <string.h>

void initkeyaccs (gint * accs, gint number);
/**
 * Free the given object
 * @param mudobj the DenemoObject to free
FIXME is this failing to free the object field???
 */
void
freeobject (DenemoObject * mudobj)
{
  if (mudobj == NULL)
    return;
  if (mudobj->midi_events)
    g_list_free (mudobj->midi_events);  //do not free the data it belongs to libsmf
  if (mudobj->lilypond)
    g_free (mudobj->lilypond);
  switch (mudobj->type)
    {
    case CHORD:
      freechord (mudobj);       /* Which also frees mudobj itself */
      break;
    case CLEF:
      free_directives (((clef *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    case KEYSIG:
      free_directives (((keysig *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    case TIMESIG:
      free_directives (((timesig *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;

    case TUPOPEN:
    case TUPCLOSE:
      free_directives (((tuplet *) mudobj->object)->directives);
      g_free (mudobj->object);
      g_free (mudobj);
      break;
    default:
      g_free (mudobj);
      break;
    }
}

void
set_modeaccs (gint * accs, gint number, gint mode)
{

  g_debug ("Mode %d : %d \n", number, mode);
  if (mode == 0)
    {
      switch (number)
        {
        case 11:
          number -= 7;
          break;
        }
      initkeyaccs (accs, number);

    }
  else if (mode == 2)
    initkeyaccs (accs, 0);
  else if (number == 1 && mode == 1)
    initkeyaccs (accs, 0);
}

/** 
 * This function initializes the accidental-context array associated with
 * a key signature or a staff to that appropriate for _number_ 
 */
void
initkeyaccs (gint * accs, gint number)
{
  int index;
  memset (accs, 0, SEVENGINTS);
  if (number > 0)
    for (index = 3; number; number--, index = (index + 4) % 7)
      accs[index] = 1;
  else if (number < 0)
    for (index = 6; number; number++, index = (index + 3) % 7)
      accs[index] = -1;

}

/**
 * Create a new measure break object
 * @return the measurebreak
 */
DenemoObject *
newmeasurebreakobject ()
{
  DenemoObject *ret;

  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = MEASUREBREAK;
  return ret;
}

/**
 * Create a new staff break object
 * @return the staffbreak
 */
DenemoObject *
newstaffbreakobject ()
{
  DenemoObject *ret;
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = STAFFBREAK;
  return ret;
}

/* clone the directive, excluding the widget */
DenemoDirective *
clone_directive (DenemoDirective * directive)
{
  DenemoDirective *ret = (DenemoDirective *) g_malloc0 (sizeof (DenemoDirective));
  memcpy (ret, directive, sizeof (DenemoDirective));    //BEWARE all pointers in DenemoDirective require code, as follows:
#define CLONE(field) \
      if(directive->field && directive->field->len)\
        ret->field = g_string_new(directive->field->str);\
      else\
        ret->field = NULL;
  CLONE (tag);
  CLONE (prefix);
  CLONE (postfix);
  CLONE (display);
  CLONE (graphic_name);
  CLONE (grob);
  CLONE (midibytes);
#undef CLONE
  if (directive->graphic)
    {
      ret->graphic = directive->graphic;        //alternatively could load it via loadGraphicItem, is the same
    }
  if (directive->widget)
    {
      //  gpointer fn = g_object_get_data(G_OBJECT(directive->widget), "fn");
      ret->widget = NULL;       //FIXME call widget_for_directive here???
      //   widget_for_directive(ret, fn);
    }
  return ret;
}


GList *
clone_directives (GList * directives)
{
  GList *ret = NULL;
  for (; directives; directives = directives->next)
    ret = g_list_append (ret, clone_directive (directives->data));
  return ret;
}

void
free_directive_data (DenemoDirective * directive)
{
#define DFREE(field) if(directive->field) g_string_free(directive->field, TRUE);
  DFREE (tag);
  DFREE (display);
  DFREE (prefix);
  DFREE (postfix);
  DFREE (graphic_name);
  DFREE (grob);
#undef DFREE

  if (directive->widget && !G_IS_OBJECT (directive->widget))
    {
      g_debug ("Found non-gobject widget %p\n", directive->widget);
    }
  if (directive->widget && G_IS_OBJECT (directive->widget))
    {
      //g_debug("We should destroy the widget now ");
      GtkWidget *texteditor = (GtkWidget *) g_object_get_data (G_OBJECT (directive->widget), DENEMO_TEXTEDITOR_TAG);
      if (texteditor)
        gtk_widget_destroy (texteditor);        //FIXME we may need to destroy its parents
      gtk_widget_destroy ((GtkWidget *) directive->widget);
    }



}

void
free_directive (DenemoDirective * directive)
{
  free_directive_data (directive);
  g_free (directive);
}

void
free_directives (GList * directives)
{
  for (; directives; directives = directives->next)
    {
      DenemoDirective *directive = directives->data;
      free_directive (directive);
    }
  g_list_free (directives);
}

/**
 * Create a clone of the given object
 * @param orig the object to clone
 * @return the cloned object
 */
DenemoObject *
dnm_clone_object (DenemoObject * orig)
{
  DenemoObject *ret = NULL;
  if (orig != NULL)
    {
      switch (orig->type)
        {
        case CHORD:
          ret = clone_chord (orig);
          break;

        case TUPOPEN:
          ret = (DenemoObject *) tuplet_open_new (((tupopen *) orig->object)->numerator, ((tupopen *) orig->object)->denominator);
          ((tupopen *) ret->object)->directives = clone_directives (((tupopen *) orig->object)->directives);

          break;
        case TUPCLOSE:
          ret = (DenemoObject *) tuplet_close_new ();
          ((tupopen *) ret->object)->directives = clone_directives (((tupopen *) orig->object)->directives);

          break;
        case CLEF:
          ret = clef_new (((clef *) orig->object)->type);
          ((clef *) ret->object)->directives = clone_directives (((clef *) orig->object)->directives);
          break;
        case TIMESIG:
          ret = dnm_newtimesigobj (((timesig *) orig->object)->time1, ((timesig *) orig->object)->time2);
          ((timesig *) ret->object)->directives = clone_directives (((timesig *) orig->object)->directives);

          break;
        case KEYSIG:
          ret = dnm_newkeyobj (((keysig *) orig->object)->number, ((keysig *) orig->object)->isminor, ((keysig *) orig->object)->mode);
          ((keysig *) ret->object)->directives = clone_directives (((keysig *) orig->object)->directives);

          break;
          break;
        case STEMDIRECTIVE:
          ret = dnm_stem_directive_new (((stemdirective *) orig->object)->type);

          ((stemdirective *) ret->object)->directives = clone_directives (((stemdirective *) orig->object)->directives);

          break;
        case MEASUREBREAK:
          ret = newmeasurebreakobject ();
          break;
        case STAFFBREAK:
          ret = newstaffbreakobject ();
          break;
        case LILYDIRECTIVE:
          {
            lilydirective *curlily = (lilydirective *) orig->object;
            ret = directive_object_new (clone_directive (curlily));
            ret->durinticks = orig->durinticks;
            ret->basic_durinticks = orig->basic_durinticks;

          }
          break;
        default:
          g_warning ("Unknown object type %x", orig->type);
          ret = lily_directive_new ("%unknown object\n");
          break;
        }
    }
  if(ret){
    ret->lilypond = NULL;
    ret->midi_events = NULL;
  }
  return ret;
}

/**
 *  Create a new stem directive 
 *  @param type the stem directive type
 *  @return the stem directive
 */
DenemoObject *
dnm_stem_directive_new (enum stemdirections type)
{
  DenemoObject *ret;
  stemdirective *newstemdir = (stemdirective *) g_malloc (sizeof (stemdirective));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = STEMDIRECTIVE;
  ret->isinvisible = FALSE;
  newstemdir->type = type;
  ret->object = newstemdir;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new lilypond directive 
 *
 * @param type the lilypond directive body
 * @return the lilypond directive
 *
*/
DenemoObject *
lily_directive_new (gchar * type)
{
  DenemoObject *ret;
  lilydirective *newlily = (lilydirective *) g_malloc0 (sizeof (lilydirective));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LILYDIRECTIVE;
  newlily->postfix = g_string_new (type);
  ret->object = newlily;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

DenemoObject *
directive_object_new (DenemoDirective * directive)
{
  DenemoObject *ret;
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LILYDIRECTIVE;
  ret->object = directive;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new dynamic object
 * @param type the dynamic to create
 * @return the dynamic
  */
DenemoObject *
dynamic_new (gchar * type)
{
  DenemoObject *ret;
  dynamic *newdyn = (dynamic *) g_malloc0 (sizeof (dynamic));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = DYNAMIC;
  ret->isinvisible = FALSE;
  newdyn->type = g_string_new (type);
  ret->object = newdyn;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Create a new lyric object
 * @param type the lyric to create
 * @param position whether it shoul be centered or not
 * @param syllable whether it is a syllable
 * @return the dynamic
  */
DenemoObject *
dnm_lyric_new (gchar * type, gint position, gboolean syllable)
{
  DenemoObject *ret;
  lyric *newlyric = (lyric *) g_malloc0 (sizeof (lyric));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = LYRIC;
  ret->isinvisible = FALSE;


  newlyric->lyrics = g_string_new (type);
  newlyric->position = position;
  newlyric->is_syllable = syllable;
  ret->object = newlyric;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 *  Create a DenemoObject
 * @param type DenemoObject type 
 * @return the DenemoObject
 */

DenemoObject *
dnm_newobj (DenemoObjType type)
{
  DenemoObject *ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));;
  ret->type = type;
  set_basic_numticks (ret);
  setpixelmin (ret);            /* these do nothing at present - but if we introduce
                                   a show markers option then we will want to allot 
                                   some space
                                 */
  return ret;
}

/**
 * Set the key signature into the score
 * @param curstaff the current staff
 * @param tokey the key sig to insert
 * @param type  major/minor/mode
 */
void
dnm_setinitialkeysig (DenemoStaff * curstaff, gint tokey, gint type)
{
  take_snapshot ();
  curstaff->keysig.number = tokey;
  curstaff->keysig.isminor = type;

  initkeyaccs (curstaff->keysig.accs, tokey);
  //memcpy (curstaff->keysig.keyaccs, curstaff->leftmost_keyaccs, SEVENGINTS);
  curstaff->leftmost_keysig = &curstaff->keysig;
  showwhichaccidentalswholestaff (curstaff);
  adjust_tonal_center (curstaff->keysig.accs);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}
