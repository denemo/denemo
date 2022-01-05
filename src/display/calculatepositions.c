/* calculatepositions.c
 * functions that calculate the positions at which score objects are
 * drawn
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include "display/calculatepositions.h"
#include "command/chord.h"
#include "command/staff.h"
#include "core/utils.h"
#include "command/measure.h"

#define denobj(x) ((DenemoObject *) x->data)
#define NONDURATION(node) (denobj(node)->durinticks==0)

/**
 *  Check to see if there is any more music in
 *  the datastructures
 *  Used by find_xes_in_measure
 *  @param cur_obj_nodes pointer to the current objnodes
 *  @param num_staffs number of staffs in the score
 */
gboolean
music_remains_p (objnode ** cur_obj_nodes, gint num_staffs)
{
  gint i;

  for (i = 0; i < num_staffs; i++)
    {
      if (cur_obj_nodes[i])
        return TRUE;
    }
  return FALSE;
}

/**
 * Check to see if the starttick of all objnodes across
 * staves is equal
 * Used by find_xes_in_measure
 *
 * @param cur_obj_nodes pointer to the current objnodes
 * @param num_staffs number of staffs in the score
 */
gboolean
all_tickcounts_equal_p (objnode ** cur_obj_nodes, gint num_staffs)
{
  gint i, compare = -1;
  DenemoObject *curobj;

  for (i = 0; i < num_staffs; i++)
    {
      if (cur_obj_nodes[i])
        {
          curobj = (DenemoObject *) cur_obj_nodes[i]->data;
          if (compare == -1)
            compare = curobj->starttickofnextnote;
          else if (curobj->starttickofnextnote != compare)
            return FALSE;
          else if (NONDURATION(cur_obj_nodes[i]))
            return FALSE;
        }
    }
  return TRUE;
}

/**
 * structure and GCompareFunc used by the mechanism for dealing with
 * zero-duration DenemoObjects (clefs, tupmarks, etc.)
 */

typedef struct list_info
{
  gint start_position;
  gint pixels;
}
list_info;

list_info *
new_list_info (gint start_position, gint pixels)
{
  list_info *ret = (list_info *) g_malloc (sizeof (list_info));

  ret->start_position = start_position;
  ret->pixels = pixels;

  return ret;
}

G_GNUC_UNUSED
static void print_nonchords (GList *nonchords)
{
    GList *g;
    for (g=nonchords;g;g=g->next)
    {
        g_print ("(p = %d, t = %d)\n", ((list_info *) g->data)->pixels, ((list_info *) g->data)->start_position);
    }

}
/**
 * g_list_foreach function to compare the start postion of the
 * object
 * This is used on the non-chord infos, the non-chords being taken from across all staffs for a single block
 * a "block" being chosen so that the start and end ticks are respectively equal
 * the sorting is by tick and then within the same tick by the pixels value - here described as "the longer gap"
 */
static gint
list_compare_func (gpointer a, gpointer b)
{
  list_info *ali = (list_info *) a;
  list_info *bli = (list_info *) b;

  if (ali->start_position != bli->start_position)
    /* Something than starts sooner gets sorted first */
    return ali->start_position - bli->start_position;
  /* Otherwise put the longer gap first */
  return bli->pixels - ali->pixels;
}

/**
 * Used to remove zero duration DenemoObjects
 *
 * @param source list of DenemoObjects to prune
 */
static GList *
prune_list (GList * source)
{
  GList *previous;
  GList *current;
  GList *sink = NULL;

  source = g_list_sort (source, (GCompareFunc) list_compare_func);
  previous = source;
  if (previous)
    {
      sink = g_list_append (sink, previous->data);
      current = previous->next;
    }
  else
    current = NULL;

  while (current)
    {
       if ((((list_info *) current->data)->start_position != ((list_info *) previous->data)->start_position))
                        sink = g_list_append (sink, current->data),  previous = current       ;
      else
        g_free (current->data);

      current = current->next;
    }
  /* Okay. The stuff we care about has been copied to sink.  All that
   * remains to do is free source and return sink */
  g_list_free (source);//g_print("Keeping \n"), print_nonchords(sink), g_print("... ok???");
  return sink;
}



/**
 * Allocate_xes - allocate the x position for all chord elements
 * in the score(??? JUl 2011, it appears to be just a single "block" it is called for each "block")
 *
 * @param block_start_obj_nodes pointer to the starting object node
 * @param block_end_obj_nodes pointer to the last object node
 * @param num_staffs the number of staffs in the score
 * @param furthest_tick_advance
 * @param base_x
 * @param base_tick
 * @param shortest_chord_duration
 * @param shortest_chord_pixels
 * @param whole_note_width
 * @param non_chords
 *
 */
static void
allocate_xes (objnode ** block_start_obj_nodes, objnode ** block_end_obj_nodes, gint num_staffs, gint furthest_tick_advance, gint * base_x, gint * base_tick, gint shortest_chord_duration, gint shortest_chord_pixels, gint whole_note_width, GList * non_chords)
{
  gint ticks_in_block, shortest_chords_in_block, block_width, starts_at_tick;
  gint extra_advance = 0, non_chord_pixels, i;
  objnode *this_staff_obj_node;
  DenemoObject *curobj = 0;
  GList *non_chords_node = 0;

  /* Initializey stuff */
  non_chords = prune_list (non_chords);

  /* Set the block width */

  ticks_in_block = furthest_tick_advance - *base_tick;
  shortest_chords_in_block = (ticks_in_block % shortest_chord_duration)
    ? (ticks_in_block / shortest_chord_duration + 1)
    : (ticks_in_block / shortest_chord_duration);
  block_width = MAX (shortest_chords_in_block * shortest_chord_pixels, ticks_in_block * whole_note_width / WHOLE_NUMTICKS);

  /* Now go through staff-by-staff and set the xes within the block
   * only.  This code would be simpler if each mudela object stored its
   * own starttick rather that only that of its possibly hypothetical
   * successor */
  for (i = 0; i < num_staffs; i++)
    {
      this_staff_obj_node = block_start_obj_nodes[i];
      if (this_staff_obj_node)
        {
          starts_at_tick = *base_tick;
          non_chords_node = non_chords;
          extra_advance = 0;
          non_chord_pixels = 0; // if there are two non-chords in succession the minpixelsalloted is used
          while (this_staff_obj_node)
            {
              curobj = (DenemoObject *) this_staff_obj_node->data;

              while (non_chords_node && !NONDURATION (this_staff_obj_node) && (starts_at_tick >= ((list_info *) non_chords_node->data)->start_position))
                {
                  extra_advance += ((list_info *) non_chords_node->data)->pixels;
                  non_chords_node = non_chords_node->next;
                }
              if (NONDURATION (this_staff_obj_node))
                {
                  curobj->x = *base_x + extra_advance + non_chord_pixels + ((starts_at_tick - *base_tick) * block_width / (ticks_in_block ? ticks_in_block : 1));
                  non_chord_pixels += curobj->minpixelsalloted;
                }
              else
                {
                  curobj->x = *base_x + extra_advance + ((starts_at_tick - *base_tick) * block_width / (ticks_in_block ? ticks_in_block : 1));
                  non_chord_pixels = 0;
                }
              starts_at_tick = curobj->starttickofnextnote;

              if (this_staff_obj_node == block_end_obj_nodes[i])
                break;
              else
                this_staff_obj_node = this_staff_obj_node->next;
            }
        }
    }

  /* This while loop takes care of any more additions needed to
   * extra_advance still outstanding */

  while (non_chords_node)
    {
      extra_advance += ((list_info *) non_chords_node->data)->pixels;
      non_chords_node = non_chords_node->next;
    }

  /* Now increase the values of *base_x and *base_tick as a side-effect. */

  *base_x += block_width + extra_advance;
  *base_tick = furthest_tick_advance;   //this is growing....
  //g_debug("furthest %d\n", furthest_tick_advance);
  /* Free non_chords and we're done */

  g_list_foreach (non_chords, freeit, NULL);
  g_list_free (non_chords);
}




/**
 * This function calculates the horizontal position of every chord in
 * the measure.  I'm foreseeing only some minor complications
 * extending it to work with multiple simultaneous time signatures in
 * different staffs (though it takes some serious mudela tweaking to
 * get Lily to do that.)  The function works by looking for "blocks"
 * within the measure where the music starts and ends in all staffs on
 * the same tick.  When it finds one such group it calls allocate_xes,
 * which actually allocates space to the block and divides it
 * proportionally among all the notes in the block according to their
 * duration.  It also does stuff with a linked list for dealing with
 * non-chord mudela objects.  First, a utility #define: */
//fxim appends to the non-chords list for all the non-chord objects up to the next chord. The list is a list of accumulated x-offset and tick.
//If a chord follows the space before is also added to the accumulated x-offset.
// there is also tick related stuff done.
//cur_obj_nodes is moved to the first chord
#define fxim_utility \
  accumulator = 0;  \
  start_tick = 0;\
  if (cur_obj_nodes[i])  \
    start_tick = denobj (cur_obj_nodes[i])->starttick;  \
  while (cur_obj_nodes[i] && NONDURATION(cur_obj_nodes[i])/*denobj (cur_obj_nodes[i])->type != CHORD*/) \
    {  \
      curobj = (DenemoObject *)cur_obj_nodes[i]->data;  \
      accumulator += curobj->minpixelsalloted;  \
      non_chords = g_list_append (non_chords,  \
                                  new_list_info (start_tick, accumulator));  \
      cur_obj_nodes[i] = cur_obj_nodes[i]->next;  \
    }  \
  if (cur_obj_nodes[i])  \
    {  \
      if (denobj (cur_obj_nodes[i])->space_before)  \
        {  \
          accumulator += denobj (cur_obj_nodes[i])->space_before;  \
          non_chords = g_list_append (non_chords,  \
                                      new_list_info (start_tick, accumulator));  \
        }  \
      if (denobj (cur_obj_nodes[i])->durinticks < shortest_chord_duration)  \
        {  \
          shortest_chord_duration = denobj (cur_obj_nodes[i])->durinticks;  \
          shortest_chord_pixels =  \
            denobj (cur_obj_nodes[i])->minpixelsalloted;  \
        }  \
      max_advance_ticks =  \
        MAX (max_advance_ticks,  \
         denobj (cur_obj_nodes[i])->starttickofnextnote);  \
    }


static gboolean single_duration_bar (GList *objects)
	{ GList *g = objects;
		gint num = 0;
		for (;g;g=g->next)
			{
				if (NONDURATION (g) && (denobj (g)->type != LILYDIRECTIVE))
					continue;//some DenemoDirectives need to align - e.g. barlines so even though they have no duration space them out.
				num++;
				if (num > 1)
					break;
			}
	if (num <= 1)
		{//only at most one object with a duration in this bar, so position everything according to their minpixels
			gint x = 0;
			for (;objects;objects = objects->next)
				{
					x += denobj (objects)->space_before;
					denobj (objects)->x = x;
					x += denobj(objects)->minpixelsalloted;
				}
			return TRUE;
		}
	return FALSE;	
	}
/* Note that a lot more nodes get added to non_chords than is necessary,
 * but that's okay - prune_list will compensate for that nicely. */

/**
 * Iterate through the measure ready to set the x value for
 * each object
 *
 * @param si the scoreinfo structure
 * @param measurenum the measure to set the x values for
 * @param time1 the nominator of the timesig
 * @param time2 the denominator of the timesig
 * @return nothing
 */
void
find_xes_in_measure (DenemoMovement * si, gint measurenum)
{

  gint time1, time2;
  //timesig *thetime = g_list_
  staffnode *cur_staff = si->currentstaff;
  measurenode *mnode = g_list_nth (((DenemoStaff*)cur_staff->data)->themeasures, measurenum-1);
  if (mnode == NULL) 
	{ g_critical ("Call to find_xes_in_measure for bad measure number %d", measurenum);return;}
  DenemoMeasure *meas = (DenemoMeasure*)mnode->data;
  if (meas == NULL) 
	{ g_critical ("Call to find_xes_in_measure for bad measure number %d", measurenum);return;}
  time1 = meas->timesig->time1;
  time2 = meas->timesig->time2;


  gint num_staffs = g_list_length (si->thescore);
  gint base_x = 0;
  gint base_tick = 0;
  gint max_advance_ticks = 0;
  objnode **block_start_obj_nodes;
  objnode **cur_obj_nodes;

  gint shortest_chord_duration = G_MAXINT;
  gint shortest_chord_pixels = 0;

  gint i;
  gint accumulator, start_tick;
  GList *non_chords = NULL;
  gint whole_note_width = si->measurewidth * time2 / time1;
  DenemoObject *curobj;


  block_start_obj_nodes = (objnode **) g_malloc (sizeof (objnode *) * num_staffs);
  cur_obj_nodes = (objnode **) g_malloc (sizeof (objnode *) * num_staffs);

  for (i = 0, cur_staff = si->thescore; cur_staff; i++, cur_staff = cur_staff->next)
    {

// Point cur_obj_nodes[i] to the list of objects in the measure for the i'th staff  (if no measure NULL)
      if (((DenemoStaff *) cur_staff->data)->nummeasures >= measurenum)
        {
		if (single_duration_bar (((DenemoMeasure*)g_list_nth (((DenemoStaff*)cur_staff->data)->themeasures, measurenum - 1)->data)->objects))
			block_start_obj_nodes[i] = NULL;
		else
          block_start_obj_nodes[i] = cur_obj_nodes[i] = /*measure_first_obj_node*/ (((DenemoMeasure*)g_list_nth (((DenemoStaff*)cur_staff->data)->themeasures, measurenum - 1)->data)->objects); //FIXME DANGER
        }
      else
        {
          block_start_obj_nodes[i] = cur_obj_nodes[i] = NULL;
        }
// run the fxim thing on these objects

      fxim_utility; //creates the non_chords list up to the first chord, moving cur_obj_nodes to the first chord in each staff
    }

  while (non_chords != NULL || music_remains_p (cur_obj_nodes, num_staffs))
    {
      if (all_tickcounts_equal_p (cur_obj_nodes, num_staffs))
        {
          /* A-ha!  We've found a block.  Now go set the x positions
           * of all the objects within it appropriately */
          //g_debug("*******Max advance ticks %d\n", max_advance_ticks);
          allocate_xes (block_start_obj_nodes, cur_obj_nodes, num_staffs, max_advance_ticks, &base_x, &base_tick, shortest_chord_duration ? shortest_chord_duration : 1, shortest_chord_pixels, whole_note_width, non_chords);

          /* And do setup work for the next block */

          non_chords = NULL;
          shortest_chord_duration = G_MAXINT;
          shortest_chord_pixels = 0;
          for (i = 0; i < num_staffs; i++)
            {
              block_start_obj_nodes[i] = cur_obj_nodes[i] = (cur_obj_nodes[i] ? cur_obj_nodes[i]->next : NULL);
              fxim_utility;
            }
        }                       /* end if */
      else
        {
          /* We haven't found a block yet; move a single element of
           * cur_obj_nodes "ahead" */
          for (i = 0; i < num_staffs; i++)
            {
              if (cur_obj_nodes[i] && ((denobj (cur_obj_nodes[i])->starttickofnextnote < max_advance_ticks) || NONDURATION (cur_obj_nodes[i])
                                       //denobj (cur_obj_nodes[i])->type != CHORD
                  ))
                {
                  cur_obj_nodes[i] = cur_obj_nodes[i]->next;
                  fxim_utility;
                  break;
                }
            }
        }                       /* End else */
    }                           /* End while */

  g_list_nth (si->measurewidths, measurenum - 1)->data = GINT_TO_POINTER (MAX (base_x, si->measurewidth));

  g_free (block_start_obj_nodes);
  g_free (cur_obj_nodes);

}

/**
 * Iterate through entire score ready to
 * set x values for all objects in the score
 *
 * @param si the scoreinfo structure
 * @return none
 */
void
find_xes_in_all_measures (DenemoMovement * si)
{
  gint i, n = g_list_length (si->measurewidths);
  //g_debug ("Number of measures in score %d\n", n);
  for (i = 1; i <= n; i++)
    find_xes_in_measure (si, i);
  /* obviously inefficient; should fix this */
}
