
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */

#include <errno.h>
#include <math.h>
#include "export/print.h"
#include "core/view.h"
#include "command/scorelayout.h"
#include "command/lilydirectives.h"
#include "export/exportlilypond.h"
#include "export/exportmidi.h"
#include "scripting/scheme-callbacks.h"
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <librsvg/rsvg.h>

static gint changecount = -1;   //changecount when the playback typeset was last created 
static gboolean RightButtonPressed = FALSE;
static gboolean LeftButtonPressed = FALSE;
static gboolean Dragging = FALSE;
static gint RightButtonX, LeftButtonX, DragX;
static gint RightButtonY, LeftButtonY, DragY, LastY;
static gdouble IntroTime = 10.0, ScrollRate = 10.0;
static gboolean AllPartsTypeset = FALSE;
static gboolean PartOnly = FALSE;
static GtkAdjustment *VAdj = NULL;
static gdouble ScrollTime = -1.0;
typedef struct Timing {
    gdouble time;
    gdouble duration;
    gdouble x;
    gdouble y;
    gint line;
    gint col;
    DenemoObject *object;//the denemo object that corresponds to line, col
} Timing;

GList *TheTimings = NULL, *LastTiming=NULL, *NextTiming=NULL, *ScrollPoints = NULL;
gdouble TheScale = 1.0; //Scale of score font size relative to 18pt
/* Defines for making traversing XML trees easier */

#define FOREACH_CHILD_ELEM(childElem, parentElem) \
for ((childElem) = (parentElem)->xmlChildrenNode; \
     (childElem) != NULL; \
     (childElem) = (childElem)->next)

#define ELEM_NAME_EQ(childElem, childElemName) \
(strcmp ((gchar *)(childElem)->name, (childElemName)) == 0)

#define ILLEGAL_ELEM(parentElemName, childElem) \
do \
  { \
    g_warning ("Illegal element inside <%s>: <%s>", parentElemName, \
               (childElem)->name); \
  } while (0)

#define RETURN_IF_ELEM_NOT_FOUND(parentElemName, childElem, childElemName) \
do \
  { \
    if (childElem == NULL) \
      { \
        g_warning ("Element <%s> not found inside <%s>", childElemName, \
                   parentElemName); \
        return -1; \
      } \
  } while (0)

/**
 * Get the text from the child node list of elem, convert it to an integer,
 * and return it.  If unsuccessful, return G_MAXINT.
 */
static gint
getXMLIntChild (xmlNodePtr elem)
{
  gchar *text = (gchar *) xmlNodeListGetString (elem->doc, elem->xmlChildrenNode, 1);
  gint num = G_MAXINT;
  if (text == NULL)
    {
      g_warning ("No child text found %s", elem->name);
    }
  else
    {
      if (sscanf (text, " %d", &num) != 1)
        {
          g_warning ("Could not convert child text \"%s\" of <%s> to number", text, elem->name);
          num = G_MAXINT;
        }
      g_free (text);
    }
  return num;
}





//Ensures the playback view window is visible.
static void
show_playback_view (void)
{
    GtkWidget *w = gtk_widget_get_toplevel (Denemo.playbackview);
    if (!gtk_widget_get_visible (w))
        activate_action ("/MainMenu/ViewMenu/" TogglePlaybackView_STRING);
    else
        gtk_window_present (GTK_WINDOW (w));
}
        

//draw a circle 
static void
place_spot (cairo_t * cr, gint x, gint y)
{
  cairo_move_to (cr, x, y);
  cairo_arc (cr, x, y, PRINTMARKER / 4, 0.0, 2 * M_PI);
  cairo_fill (cr);
}

static void
get_window_size (gint * w, gint * h)
{
  GdkWindow *window;
  if (!GTK_IS_LAYOUT (Denemo.playbackview))
    window = gtk_widget_get_window (GTK_WIDGET (Denemo.playbackview));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.playbackview));
  if (window)
    {
     
#if GTK_MAJOR_VERSION==2
      gdk_drawable_get_size (window, w, h);
#else
      *w = gdk_window_get_width (window);
      *h = gdk_window_get_height (window);
#endif
    }
}

static void
get_window_position (gint * x, gint * y)
{
    
#if 0
  GtkAdjustment *adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printhscrollbar));
  *x = (gint) gtk_adjustment_get_value (adjust);
  adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printvscrollbar));
  *y = gtk_adjustment_get_value (adjust);
#else
  *x = *y = 0; //g_warning("Not calculating window scroll effects");
#endif
}

gboolean attach_timings (void)
{
  if (TheTimings == NULL)
        return FALSE;
  GList *g;
  for (g=TheTimings;g;g=g->next)      
    {
        Timing *this = (Timing *)g->data;
        DenemoObject *obj = get_object_at_lilypond (this->line, this->col);
      //g_print ("at %d %d\n", this->line, this->col);
            if (obj)
               {
                  obj->earliest_time = this->time;
                  obj->latest_time = this->time + this->duration; //g_print ("Set %.2f %.2f\n", obj->earliest_time, obj->latest_time);
                  this->object = obj;
                }
            else
               return FALSE;
    }
    
  return TRUE;
}
DenemoObject *get_object_for_time (gdouble time, gboolean start)
{
    GList *g;
    for (g=TheTimings;g;g=g->next)
        {
         Timing *this = (Timing *)g->data;
         //g_print ("Seeking %.2f Timing %.2f to %.2f\n", time, this->object->earliest_time, this->object->latest_time);
         if ((start? this->object->earliest_time:this->object->latest_time) > time)
            return this->object;
            
        }
    return NULL;
}
//over-draw the evince widget with padding etc ...
static gboolean
overdraw_print (cairo_t * cr)
{
  gint x, y;
  gdouble this, duration;
  gboolean drew_rectangle = FALSE;
  if (Dragging)
    {   //g_print ("Dragging from %d %d to %d %d \n", RightButtonX, RightButtonY, DragX, DragY);
        if (RightButtonPressed)
            cairo_set_source_rgba (cr, 0.2, 0.8, 0.8, 0.5);
        else
            cairo_set_source_rgba (cr, 0.8, 0.2, 0.4, 0.5);
        cairo_set_line_width (cr, 5.0);
        cairo_move_to (cr, RightButtonPressed?(double)RightButtonX:(double)LeftButtonX, RightButtonPressed?(double)RightButtonY:(double)LeftButtonY);
        cairo_line_to (cr, (double)DragX, (double)DragY);
        cairo_stroke (cr);
        
       return TRUE; 
    }
  cairo_scale (cr, 5.61*TheScale, 5.61*TheScale);
  if(!audio_is_playing())
    return TRUE;
 // if (!Denemo.project->movement->playingnow)
 //   return TRUE;
  if (TheTimings == NULL)
        return TRUE;

  if (LastTiming == NULL)
        {
            LastTiming = TheTimings;
        }        
  
    cairo_set_source_rgba (cr, 0x6e/255.0, 0xb9/255.0, 0xd5/255.0, 0.3);//6eb9d5

    gdouble time = Denemo.project->movement->playhead;
    GList *g;
    this = ((Timing *)LastTiming->data)->time;
    duration = ((Timing *)LastTiming->data)->duration;
    if (time < (this-0.01))
        {// g_print ("\n\n\nResetting LastTiming at %.2f for %.2f\n", time, this);
            LastTiming = TheTimings;
        }
        
    for(g=LastTiming;g && g->next;g=g->next)
        {
           this = ((Timing *)g->data)->time;
           duration = ((Timing *)g->data)->duration;
           //g_print (" %f this = %f test time>this %d and this-end < time %d Durations is %f\n ",  time,  this, (time > (this - 0.01)), (this + duration < time), duration);
           if (this + duration < time)
                       continue;
           if (time > (this - 0.1))
                    {  //g_print ("draw note at %.2f\n", this );
                        cairo_rectangle (cr, ((Timing *)((g)->data))->x  - (PRINTMARKER/5)/4, ((Timing *)((g)->data))->y - (PRINTMARKER/5)/2, PRINTMARKER/5, PRINTMARKER/5);
                        if(!drew_rectangle)
                            LastTiming = g;
                        drew_rectangle = TRUE;
                    }
            else
               break;
        }
  if (drew_rectangle)
     cairo_fill (cr);
  return TRUE;
}
static gboolean
predraw_print (cairo_t * cr)
{
  gint x, y, width, height;
  get_window_size (&width, &height);
  get_window_position (&x, &y);
  cairo_translate (cr, -x, -y);
  cairo_set_source_rgba (cr, 0xf1/255.0, 0xf4/255.0, 0x9d/255.0, 1.0);//cfdd36 f1f49d
  cairo_rectangle (cr, 0, 0.0, (gdouble)width, (gdouble)height);
  cairo_fill (cr);
  
    cairo_set_source_rgba (cr, 0x50/255.0, 0x0/255.0, 0x60/255.0, 1.0);//this should have no effect ...

  
  return FALSE;//propagate further
}
#if GTK_MAJOR_VERSION==3
static gint
playbackview_draw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return overdraw_print (cr);
}
static gint
playbackview_predraw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return predraw_print (cr);
}
#else
static gint
playbackview_draw_event (GtkWidget * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw_print (cr);
  cairo_destroy (cr);
  return TRUE;
}
static gint
playbackview_predraw_event (GtkWidget * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  predraw_print (cr);
  cairo_destroy (cr);
  return FALSE; //propagate further
}
#endif

static Timing *get_svg_position(gchar *id, GList *ids)
{
  for(;ids;ids=ids->next)
        {//g_print ("Testing %s with %s\n", ids->data, id);
            if (g_str_has_prefix ((gchar*)ids->data, id))
                {
                    
                  Timing *timing = (Timing *)g_malloc (sizeof(Timing));
                  if (2==sscanf ((gchar*)ids->data, "Note-%*d-%*d translate(%lf,%lf)%*s%*s", &timing->x, &timing->y))
                    {
                        //g_print ("Found Position %.2f %.2f\n", timing->x, timing->y);
                        return timing;
                    } else if (2==sscanf ((gchar*)ids->data, "Rest-%*d-%*d translate(%lf,%lf)%*s%*s", &timing->x, &timing->y))
                    {
                       //g_print ("Found Position %.2f %.2f\n", timing->x, timing->y);
                        return timing;
                    }
                    
                }
            
            
        }
    g_warning ("Failed to find a position in events.txt for %s\n", id);
    return NULL;
}

static void add_note (Timing *t)
{
    TheTimings = g_list_append (TheTimings, (gpointer)t);
    //g_print ("Added %.2f seconds (%.2f,%.2f)\n", t->time, t->x, t->y);
}
static void free_timings (void)
{
    GList *g;
    for (g = TheTimings; g;g=g->next)
        {
            g_free(g->data);
        }
    g_list_free (TheTimings);
    TheTimings = NULL;
    LastTiming = NextTiming = NULL;
}

static void compute_timings (gchar *base, GList *ids)
{
    free_timings();
    gchar *events = g_build_filename (base, "events.txt", NULL);
    FILE *fp = fopen (events, "r");  
    //g_print ("Collected %d ids\n", g_list_length (ids));
    if(fp)
        {
            gdouble moment, duration;
            gchar type [10];
            gint duration_type, col, line, midi;
            gdouble tempo = 60;
            gdouble timeCoef =  4;
            gdouble latestMoment = 0;
            gdouble adjustedElapsedTime = 0;
            gdouble nextTempo = 0;
            gdouble nextTempoMoment = 0;
            gboolean incomingTempo = FALSE;
            while (2 == fscanf (fp, "%lf%10s", &moment, type)) 
                {
                // g_print ("moment %.2f %s latestMoment %.2f\n", moment, type, latestMoment);
                  if (!strcmp (type, "tempo"))  
                        {
                            if (1 == fscanf (fp, "%lf", &nextTempo))
                                {
                                nextTempoMoment = moment;//g_print ("Next %s %.2f\n", type, nextTempo);
                                incomingTempo = TRUE;
                                } else g_warning ("Malformed events file");
                        }
                 else
                    {
                        if (!strcmp (type, "note"))
                            {
                            if (4 == fscanf (fp, "%*s%lf%*s%d%d%d", &duration, &col, &line, &midi))
                                    {
                                       // g_print ("moment ... %.2f %s %d %.2f %d %d %d\n", moment, type, duration_type, duration, col, line, midi); 
                                       if (incomingTempo)
                                        {
                                            if (moment > nextTempoMoment)
                                                {
                                                    tempo = nextTempo;//g_print (" tempo %.2f\n", tempo);
                                                    timeCoef = (60 / tempo);//g_print (" timeCoef %.2f\n", timeCoef);
                                                    incomingTempo = FALSE;
                                                }
                                        }
                                        gdouble elapsedTime = moment - latestMoment;
                                        adjustedElapsedTime += elapsedTime * timeCoef;//g_print ("adjustedElapsedtime %f\n", adjustedElapsedTime);
                                        gchar *idStr;
                                        Timing *timing;

                                                idStr = g_strdup_printf ("Note-%d-%d" , line, col);
                                                timing = get_svg_position (idStr, ids);
                                                
                                                if(timing)
                                                    {
                                                    timing->line = line;
                                                    timing->col = col;
                                                    timing->time = adjustedElapsedTime;
                                                    timing->duration = duration;
                                                    add_note (timing);//g_print ("AdjustedElapsed time %.2f note %d\n", adjustedElapsedTime, midi);
                                                    }
                                    }
                                    else
                                    g_warning ("Could not parse type %s\n", type);
                            }            
                                    
                        else if(!strcmp (type, "rest"))
                            {
                                if (3 == fscanf (fp, "%*s%lf%*s%d%d",  &duration, &col, &line))
                                    {
                                       // g_print ("moment ... %.2f %s %d %.2f %d %d %d\n", moment, type, duration_type, duration, col, line, midi); 
                                       if (incomingTempo)
                                        {
                                            if (moment > nextTempoMoment)
                                                {
                                                    tempo = nextTempo;//g_print (" tempo %.2f\n", tempo);
                                                    timeCoef = (60 / tempo);//g_print (" timeCoef %.2f\n", timeCoef);
                                                    incomingTempo = FALSE;
                                                }
                                        }
                                        gdouble elapsedTime = moment - latestMoment;
                                        adjustedElapsedTime += elapsedTime * timeCoef;//g_print ("adjustedElapsedtime %f\n", adjustedElapsedTime);
                                        gchar *idStr;
                                        Timing *timing;                                        
                                            
                                            
                                            
                                            idStr = g_strdup_printf ("Rest-%d-%d" , line, col);
                                            timing = get_svg_position (idStr, ids);
                                            if(timing)
                                                {
                                                timing->line = line;
                                                timing->col = col;
                                                timing->time = adjustedElapsedTime;
                                                add_note (timing);//g_print ("AdjustedElapsed time %.2f rest \n", adjustedElapsedTime);
                                                }
                                            
                                } //rest
                            else g_warning ("Don't know how to handle %s\n", type);
                            }
                            latestMoment = moment;
                        }// not tempo
                    } //while events
                fclose (fp);
            } //if events file
                    
    g_free (events); 
}

static GList * create_positions (gchar *filename)
{
  GList *ret = NULL;
  GError *err = NULL;
  xmlDocPtr doc = NULL;
  xmlNsPtr ns;
  xmlNodePtr rootElem;
  /* ignore blanks between nodes that appear as "text" */
  xmlKeepBlanksDefault (0);
  /* Try to parse the file(s). */
  filename = g_strdup (filename); //we may modify it
  while (g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      doc = xmlParseFile (filename);
      if (doc == NULL)
        {
          g_warning ("Could not read svg file %s", filename);
         break;
        }
        else
        {//g_print ("Parsing %s\n", filename);
          rootElem = xmlDocGetRootElement (doc);
          xmlNodePtr childElem;
          FOREACH_CHILD_ELEM (childElem, rootElem)
          {
              if (ELEM_NAME_EQ (childElem, "g"))
                { xmlNodePtr grandChildElem;
                  gchar *id = xmlGetProp (childElem, (xmlChar *) "id");
                     FOREACH_CHILD_ELEM (grandChildElem, childElem)
                       {
                         if (ELEM_NAME_EQ (grandChildElem, "g"))   //grouping to set color to black
                          { xmlNodePtr greatgrandChildElem;
                            FOREACH_CHILD_ELEM (greatgrandChildElem, grandChildElem)
                                {
                                    if (ELEM_NAME_EQ (greatgrandChildElem, "path"))
                                        {
                                            gchar *coords = xmlGetProp (greatgrandChildElem, (xmlChar *) "transform");
                                            //g_print ("ID %s has Coords %s\n", id, coords);
                                            if (id && coords)
                                                {
                                                gchar *data = g_strconcat (id, coords, NULL);
                                                ret = g_list_append (ret, data);
                                                xmlFree (id);
                                                xmlFree (coords);
                                                }
                                        } else g_warning ("Found %s", greatgrandChildElem->name);
                                    }
                            }
                        }
                    }
            }
              if (doc != NULL)
            xmlFreeDoc (doc);

        }
        //It may have spilt over into several svg files denemoprintA-page-1.svg etc
       gint num_pos = strlen (filename)-5;//"<n>.svg"
       *(filename+num_pos) = *(filename+num_pos) + 1; //no attempt beyond 9 pages!
       //FIXME check that mtime of this file is later than the last, or delete old svg's before starting.
    }
    g_free (filename);
    //g_print ("Read %d ids from file %s\n", g_list_length (ret), filename);
  return ret;  
}
static gint get_number_of_pages (gchar *base)
{
    gint i;
    for (i=1;i<10;i++)
        {
            gchar *filename = g_strdup_printf ("%s%s%d%s", get_print_status()->printbasename[get_print_status()->cycle], "-page-", i, ".svg");
            if (!(g_file_test (filename, G_FILE_TEST_EXISTS)))
                {
                    g_free(filename);
                    break;
                }
            //g_print ("Found %s\n", filename);
            g_free (filename);
        }
   return i-1;         
}
static gboolean
set_playback_view (void)
{
  static gint num_pages = 0;
  GFile *file;
  gchar *filename = g_strdup (get_print_status()->printname_svg[get_print_status()->cycle]);
  gboolean multipage = FALSE;
  //g_print("Output to %s num_pages is %d\n", filename, num_pages);
  if (get_print_status()->invalid)
    g_warning ("We got print status invalid %d\nTypeset may not be good.", get_print_status()->invalid);
  if (!(g_file_test (filename, G_FILE_TEST_EXISTS)))
      {
          g_free (filename);
          if (num_pages>0) //recursion failed, give up
            {
                num_pages = 0;
                g_warning ("Unable to get the right page length\n");
                return FALSE;
            }
          filename = g_strconcat (get_print_status()->printbasename[get_print_status()->cycle], "-page-1.svg", NULL);
          if (g_file_test (filename, G_FILE_TEST_EXISTS))
                {
                    g_free (filename); 
                    num_pages = get_number_of_pages (get_print_status()->printbasename[get_print_status()->cycle]);
                    if (num_pages<2)
                        {
                        g_warning ("Unable to determine number of pages\n");
                        return FALSE;  
                        }
                    gchar *scheme = g_strdup_printf ("%s%s%s%d%s", "(d-PlaybackView \"(list ", PartOnly?"#t":"#f", " \\\"20\\\" \\\"" , 100 * num_pages, "\\\")\")");
                    g_print ("Scheme created: %s for %d pages\n", scheme, num_pages);
                    call_out_to_guile (scheme);
                    g_free (scheme);
                    return FALSE;
                }
         g_free (filename);
         return FALSE; // no svg at all
      }
    if (num_pages == 0)
        num_pages = 1; //no recursion, so one page

    //if (get_print_status()->invalid == 0) ignore errors as it may have typeset anyway.
  get_print_status()->invalid = (g_file_test (filename, G_FILE_TEST_EXISTS)) ? 0 : 3;

 if (get_print_status()->invalid == 0)
    {
 
    compute_timings (g_path_get_dirname(filename), create_positions (filename)); 

#if 1 //def G_OS_WIN32
    GError *err = NULL;
    err = NULL;
    if (Denemo.prefs.dynamic_compression == 88)
           filename = string_dialog_entry (Denemo.project, "Back Door SVG Load", "Give SVG full path:", locateprintdir());
    GdkPixbuf *pb = //rsvg_pixbuf_from_file (filename, &err);
    rsvg_pixbuf_from_file_at_size (filename, 709, 3543 * num_pages, &err);
                               
    if(pb)
        {
            g_print ("Width %d\nHeight %d\n", gdk_pixbuf_get_width (pb), gdk_pixbuf_get_height (pb));//709, 7087 for two page, 709 by 3543 for single
            if(Denemo.playbackview)
                gtk_image_set_from_pixbuf (GTK_IMAGE (Denemo.playbackview), pb);
            else
                Denemo.playbackview = gtk_image_new_from_pixbuf (pb);
                g_print ("Loaded %s via rsvg pixbuf loader", filename);
        } else
        g_print ("\n\nThe rsvg pixbuf load of %s gave error: %s\n\n", filename, err?err->message: "no error return");

#else   
      if(Denemo.playbackview)
        gtk_image_set_from_file (GTK_IMAGE (Denemo.playbackview), filename);
      else
        Denemo.playbackview = gtk_image_new_from_file (filename);   
#endif

      static gboolean shown_once = FALSE;   //Make sure the user knows that the printarea is on screen
     // if (!shown_once)
        {
          shown_once = TRUE;
          show_playback_view ();
        }
    }
    g_free (filename);
  num_pages = 0;
  return TRUE;
}

static void
playbackview_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, gboolean print)
{
  progressbar_stop ();

  g_spawn_close_pid (get_print_status()->printpid);
  //g_print ("background %d\n", get_print_status()->background);
  if (get_print_status()->background == STATE_NONE)
    {
      call_out_to_guile ("(FinalizeTypesetting)");
      process_lilypond_errors ((gchar *) get_printfile_pathbasename ());
    }
  else
    {
      if (LilyPond_stderr != -1)
        close (LilyPond_stderr);
      LilyPond_stderr = -1;
    }
  get_print_status()->printpid = GPID_NONE;
  if(set_playback_view ())
    {
      gdouble total_time;
      changecount = Denemo.project->changecount;
      total_time = load_lilypond_midi (NULL, AllPartsTypeset);//g_print ("MIDI file total time = %.2f\n", total_time);
      AllPartsTypeset = FALSE;
  }
}



static gboolean
initialize_typesetting (void)
{
  return call_out_to_guile ("(InitializeTypesetting)");
}

//A button could be placed in the playback view to create an svg file from the view...
static void
copy_svg (void)
{
  gchar *filename;
  gchar *outuri = get_output_uri_from_scoreblock ();
  gchar *outpath;
  gchar *outname;
  outuri += strlen ("file://"); //skip the uri bit of it
  outpath = g_path_get_dirname (outuri);
  outname = g_path_get_basename (outuri);
  GtkWidget *chooser = gtk_file_chooser_dialog_new (_("SVG creation"),
                                                    GTK_WINDOW (Denemo.window),
                                                    GTK_FILE_CHOOSER_ACTION_SAVE,
                                                    GTK_STOCK_CANCEL,
                                                    GTK_RESPONSE_REJECT,
                                                    GTK_STOCK_SAVE,
                                                    GTK_RESPONSE_ACCEPT, NULL);
  GtkFileFilter *filter = gtk_file_filter_new();
  gtk_file_filter_set_name (filter, _("SVG files"));
  gtk_file_filter_add_pattern (filter, "*.svg");
  gtk_file_filter_add_pattern (filter, "*.SVG");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER(chooser), filter);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), outpath);
  gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (chooser), outname);
  gtk_widget_show_all (chooser);
  if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT)
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
  else
    filename = NULL;
  gtk_widget_destroy (chooser);

  if (filename)
    {
      gchar *contents;
      gsize length;
    
        
      if (g_file_get_contents (get_print_status()->printname_svg[get_print_status()->cycle], &contents, &length, NULL))
        {
            
            if ((!g_file_test (filename, G_FILE_TEST_EXISTS)) || confirm (_( "SVG creation"), _( "File Exists, overwrite?")))  
                {          
                  if (!g_file_set_contents (filename, contents, length, NULL))
                    {
                      gchar *msg = g_strdup_printf (_("Errno %d:\nCould not copy %s to %s. Perhaps because some other process is using the destination file. Try again with a new location\n"),
                                                    errno,
                                                    get_print_status()->printname_svg[get_print_status()->cycle],
                                                    filename);
                      warningdialog (msg);
                      g_free (msg);
                    }
                  else
                    {
                      gchar *uri = g_strconcat ("file://", filename, NULL);
                      if (strcmp(uri, get_output_uri_from_scoreblock ()))
                        score_status (Denemo.project, TRUE);
                      set_current_scoreblock_uri (uri);
                     
                      //g_print ("I have copied %s to %s (default was %s) uri %s\n", get_print_status()->printname_svg[get_print_status()->cycle], filename, outname, uri);
                    }
                  g_free (contents);
                }
        }
      g_free (outpath);
      g_free (outname);
      g_free (filename);
    }
}

void delete_svgs (void) {
    gint cycle = get_print_status()->cycle;
    cycle = !cycle;
    if (!get_print_status()->printname_svg[cycle])
        return;//not yet initialized
    g_unlink ( get_print_status()->printname_svg[cycle]);
    gint i;
    for (i=1;i<10;i++)
        {
            gchar *filename = g_strdup_printf ("%s%s%d%s", get_print_status()->printbasename[cycle], "-page-", i, ".svg");
            if (!g_file_test (filename, G_FILE_TEST_EXISTS))
                {
                 //g_print ("No file %s\n", filename);
                 g_free (filename);
                 break;
             }
            //g_print ("deleting %s\n", filename);
            g_unlink (filename);
#ifdef G_OS_WIN32
    if (g_file_test (filename, G_FILE_TEST_EXISTS)) g_warning ("File %s deletion failed\n\n", filename);    
#endif            
            
            g_free (filename);
        }
}

//re-creates the svg image and displays it
static void remake_playback_view (gboolean part)
{
    if (Denemo.project->movement->markstaffnum)
        Denemo.project->movement->markstaffnum = 0;//It can (and would otherwise) typeset just the selection - would that be useful?
    delete_svgs ();
    set_continuous_typesetting (FALSE);
    create_svg (part, FALSE);//there is a typeset() function defined which does initialize_typesetting() ...
    g_print ("Denemo.playbackview is at %p, Denemo at %p", Denemo.playbackview, &Denemo);
    g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) playbackview_finished, (gpointer) (FALSE));
}

//returns TRUE if a re-build has been kicked off.
static gboolean update_playback_view (void)
{
    //g_print ("Testing %d not equal %d \n", changecount, Denemo.project->changecount);
 if (changecount != Denemo.project->changecount)
        {
        call_out_to_guile (PartOnly?"(d-PlaybackView 'part)":"(d-PlaybackView)");//this installs the temporary directives to typeset svg and then
        return TRUE;
        }
return FALSE;
}
//Typeset and svg and display in playbackview window. Scale is the font size relative to 18.0 pt.
void
display_svg (gdouble scale, gboolean part)
{
    TheScale = scale; 
    (void)remake_playback_view (part);
      //bring print view back to show cursor
    if (Denemo.textview)
        gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (Denemo.textview),
                                      gtk_text_buffer_get_insert (Denemo.textbuffer),
                                      0.0,
                               TRUE, 0.5, 0.5);
}


static gint Locationx, Locationy;
static void button_press (GtkWidget *event_box, GdkEventButton *event)
{
    static gboolean seen = FALSE;
    if (audio_is_playing ())
        return;
    if (event->button == 3 && !seen) {
        infodialog (_("The is the Playback View Window. Click on a note to play from that note to the end. Click again to stop play. Drag between two notes to play from the first to the last, shift drag to create a loop. Right click on a note to move the Denemo cursor to that note in the Denemo Display."));
        seen = TRUE;
    }
    
    if (get_wysiwyg_info()->stage != TypesetForPlaybackView)
       {
            warningdialog (_("Use the Print View or re-typeset"));
            return;
       }
    
    gint x = event->x;
    gint y = event->y;
    //g_print ("At %d %d\n", x, y);
    GList *g;
    if (event->button == 3)
        {
            RightButtonPressed = TRUE;
            RightButtonX = x;
            RightButtonY = y;
            
        }
    else
        {
            LeftButtonPressed = TRUE;
            LeftButtonX = x;
            LeftButtonY = y;
        }

    for (g = TheTimings; g;g=g->next)
        {
            Timing *timing = g->data;
            if((x-timing->x*5.61*TheScale < PRINTMARKER/(2)) && (y-timing->y*5.61*TheScale < PRINTMARKER/(2)))
                {
                    //g_print ("Found line %d column %d\n", timing->line, timing->col);
                    Locationx = timing->col;
                    Locationy = timing->line;
                    gboolean found = goto_lilypond_position (timing->line, timing->col);
                    ScrollTime = timing->time;
                    if (found)
                        {
                            Dragging = TRUE;
                            DragX = x;
                            DragY = y;
                            LastY = event->y_root;
                        }
                    if (event->button != 3)
                        call_out_to_guile ("(DenemoSetPlaybackStart)");
                     else
                        {
                            if (found)
                                call_out_to_guile ("(d-PlayMidiNote 72 255 9 100)");
                        }
                    //g_print ("Set Playback Start %d column %d\n", timing->line, timing->col);
                    return;
                    
                }
            //g_print ("compare %d %d with %.2f, %.2f\n", x, y, timing->x*5.61*TheScale, timing->y*5.61*TheScale);
        }
        
    call_out_to_guile ("(d-PlayMidiNote 30 255 9 100)");                    
}
static void scroll_by (gdouble amount)
{
 
    gdouble value =  gtk_adjustment_get_value  (VAdj);
   g_print ("set to %.2f from %.2f\n", value+amount, value);
    gtk_adjustment_set_value (VAdj, value+amount);
}
static void scroll_to (gdouble amount)
{
    gtk_adjustment_set_value (VAdj, amount);
}
static gpointer encode (gdouble adjust, gdouble time)
{
    guint adj = (guint)adjust & 0xFFFF;
    guint t = ((guint)(time*10))<<16;
    return GINT_TO_POINTER (adj+t);
}
static void decode (guint val, gdouble *adjust, gdouble *time)
{
     *adjust = (gdouble)(val & 0xFFFF);
    *time = (((val>>16) & 0xFFFF)/10.0);
}
static gboolean playback_redraw (void)
{
    static gdouble last_time;
    if (audio_is_playing ())
            {
                gdouble time = Denemo.project->movement->playhead;
                if (ScrollPoints)
                    {
                       GList *g, *start=NULL, *end=NULL;
                       for (g=ScrollPoints;g;g=g->next)
                            {
                                gdouble adj, tm;
                                decode (GPOINTER_TO_INT(g->data), &adj, &tm);
                                //g_print ("%.2f %.2f\n", adj, tm);
                                if(g->next)
                                    {
                                        if (time < tm)
                                            {
                                               scroll_to (adj * time/tm);//g_print ("case 1");
                                               break;
                                            } else
                                            {
                                                gdouble nextadj, nexttm;
                                                decode (GPOINTER_TO_INT(g->next->data), &nextadj, &nexttm);
                                                if (time > nexttm)
                                                    continue;
                                                scroll_to (nextadj + (adj - nextadj)*((nexttm-time)/(nexttm - tm)));//g_print ("case 2");
                                                break;
                                            }
                                    }
                                else
                                    {
                                        if (time >= tm)
                                            {
                                               break;   
                                            }
                                        if (g->prev)
                                            { gdouble prevadj, prevtm;
                                               decode (GPOINTER_TO_INT(g->prev->data), &prevadj, &prevtm); 
                                               scroll_to (prevadj + (adj - prevadj)*((time-prevtm)/(tm - prevtm)));//g_print ("case 3");
                                               break;
                                            }
                                        if (tm>0) 
                                            {
                                            scroll_to (adj * (time/tm));//g_print ("case 4");
                                            }
                                        break;
                                    }
                        }
                    }
                else 
                    {
                    static gdouble waiting_time;
                    if (last_time < 0.0)
                        waiting_time = time + IntroTime;
                    if (last_time > waiting_time)
                        scroll_by (((time -last_time)*ScrollRate));
                    }
                last_time = time;
                gtk_widget_queue_draw (Denemo.playbackview);
            }
    else
        last_time = -1.0;
    return TRUE;
}

static void list_scroll_points (void) //debug only
{
   GList *g;
   for (g=ScrollPoints;g;g=g->next)
        {gdouble adj, tm;
         decode (g->data, &adj, &tm);
         g_print ("Scroll Point: %0.2f at time %0.2f\n", adj, tm);
     }  
    
}

static void button_release (GtkWidget *event_box, GdkEventButton *event)
{
    gint x = event->x;
    gint y = event->y;
    RightButtonPressed = FALSE;
    LeftButtonPressed = FALSE;
    
    if (Dragging &&   (event->button == 3))
        {
            g_print ("Store %.2f %.2f\n", gtk_adjustment_get_value (VAdj), ScrollTime);
            ScrollPoints = g_list_append (ScrollPoints, encode (gtk_adjustment_get_value (VAdj), ScrollTime));
            list_scroll_points();
        }

    Dragging = FALSE;
     if (audio_is_playing ())
        {
            call_out_to_guile ("(DenemoStop)");
            return;
        }

    gtk_widget_queue_draw (Denemo.playbackview);
    //g_print ("At %d %d\n", x, y);
    if (event->button == 3)
        return;    
    
    
    if (update_playback_view ())
        {
            if (continuous_typesetting ())
               ;//warningdialog (_("1Please turn continuous typsetting off first"));
            else
                warningdialog (_("Please wait while the Playback View is re-typeset then re-try"));
            return;
        }
    GList *g;
    for (g = TheTimings; g;g=g->next)
        {
            Timing *timing = g->data;
            if((x-timing->x*5.61*TheScale < PRINTMARKER/(2)) && (y-timing->y*5.61*TheScale < PRINTMARKER/(2)))
                {
                    
                    if ((timing->col == Locationx) && (timing->line == Locationy))
                        {
                            call_out_to_guile ("(d-DenemoPlayCursorToEnd)");
                            //g_print ("Found same line %d column %d\n", timing->line, timing->col);
                        }
                    else
                        {
                            goto_lilypond_position (timing->line, timing->col);
                            call_out_to_guile ("(if (not (d-NextChord)) (d-MoveCursorRight))(DenemoSetPlaybackEnd)");
                            //g_print ("Set playback end to %d column %d\n", timing->line, timing->col);
                            Denemo.project->movement->smfsync = Denemo.project->movement->changecount;
                            if (shift_held_down())
                                call_out_to_guile ("(d-OneShotTimer 500 \"(DenemoLoop)\")");
                            else
                                call_out_to_guile ("(d-OneShotTimer 500 \"(d-Play)\")");
                        }
                    
                    break;
                    
                }
           // g_print ("compare %d %d with %.2f, %.2f\n", x, y, timing->x*5.61*TheScale, timing->y*5.61*TheScale);
        }
       


}


static gint
hide_playback_on_delete (void)
{
  activate_action ("/MainMenu/ViewMenu/" TogglePlaybackView_STRING);
  return TRUE;
}


static void play_button (void)
{
   if (update_playback_view ())
        {
             if (continuous_typesetting ())
                ;//warningdialog (_("2Please turn continuous typsetting off first"));
            else
                warningdialog (_("Please wait while the Playback View is re-typeset then re-try"));
            return;
        }
    Denemo.project->movement->smfsync = Denemo.project->movement->changecount;
    call_out_to_guile ("(d-Performance)");
}
static void part_button (void)
{
    PartOnly = TRUE;
    if (Denemo.project->movement->smf)
        AllPartsTypeset = confirm ( _("MIDI Already Present"), _("Keep this music while typesetting current part?"));
    call_out_to_guile ("(d-PlaybackView 'part)");//this installs the temporary directives to typeset svg and then

}
static void movement_button (void)
{ 
    PartOnly = FALSE;
    call_out_to_guile ("(d-PlaybackView #f)");//this installs the temporary directives to typeset svg and then
}


static gboolean
motion_notify (GtkWidget * window, GdkEventMotion * event)
{//g_print ("Passed %.2f, %.2f\n", event->x, event->y);
  if (Dragging && RightButtonPressed)
    {
        scroll_by (LastY - event->y_root);g_print ("\tLast %d %d\t", LastY,  (gint)event->y_root);
        LastY = event->y_root;
    }
  if (RightButtonPressed || LeftButtonPressed)
    {
        DragX = event->x;
        DragY = event->y;
        gtk_widget_queue_draw (Denemo.playbackview);
    }
  return TRUE;

}


static void clear_scroll_points (void)
{
     g_list_free (ScrollPoints);
     ScrollPoints = NULL;
}
static void scroll_dialog (void)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;

  GtkWidget *intro;
  GtkWidget *rate;
  
  dialog = gtk_dialog_new_with_buttons (_("Automatic Scrolling"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  //TODO calculate hightest number in seconds
  gdouble max_end_time = 7200.0;
  //g_list_length (((DenemoStaff *) (gui->movement->thescore->data))->measures);

  label = gtk_label_new (_("Introduction Time"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  intro = gtk_spin_button_new_with_range (0.0, 30.0, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), intro, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (intro), (gdouble) IntroTime);

  label = gtk_label_new (_("Scroll Rate"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  rate = gtk_spin_button_new_with_range (0.0, 100, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), rate, TRUE, TRUE, 0);

  gtk_spin_button_set_value (GTK_SPIN_BUTTON (rate), (gdouble) ScrollRate);

  GtkWidget *button = gtk_button_new_with_label (_("Clear Scroll Points"));
  g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (clear_scroll_points), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);



  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      IntroTime = gtk_spin_button_get_value (GTK_SPIN_BUTTON (intro));
      ScrollRate = gtk_spin_button_get_value (GTK_SPIN_BUTTON (rate));
    }
  gtk_widget_destroy (dialog);
}


static gint
keypress_event (GtkWidget * widget, GdkEventKey * event)
{
    g_print ("Keypress event\n");
}
void
install_svgview (GtkWidget * top_vbox)
{
  if (Denemo.playbackview)
        return;
        
        
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
  GtkWidget *hbox;
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, FALSE, 0);

  GtkWidget *button = (GtkWidget*)gtk_button_new_with_label (_("Play/Stop"));
  g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (play_button), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  button = (GtkWidget*)gtk_button_new_with_label (_("All Parts"));
  g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (movement_button), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
   button = (GtkWidget*)gtk_button_new_with_label (_("Current Part"));
  g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (part_button), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
    button = (GtkWidget*)gtk_button_new_with_label (_("Set Scrolling"));
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect_swapped (G_OBJECT (button), "clicked", G_CALLBACK (scroll_dialog), NULL);  
  if (top_vbox == NULL)
    {
    top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    // if(!Denemo.prefs.manualtypeset)
    //      gtk_window_set_urgency_hint (GTK_WINDOW(Denemo.window), TRUE);//gtk_window_set_transient_for (GTK_WINDOW(top_vbox), GTK_WINDOW(Denemo.window));
    gtk_window_set_title (GTK_WINDOW (top_vbox), _("Denemo Playback View"));
    gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);
    //g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
    g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (hide_playback_on_delete), NULL);
    }
  
  
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);
 
  GtkWidget *score_and_scroll_win = gtk_scrolled_window_new (NULL, NULL);
  Denemo.playbackview = (GtkWidget *) gtk_image_new ();
    VAdj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW(score_and_scroll_win));
    // gtk_container_add (GTK_CONTAINER (score_and_scroll_win), Denemo.playbackview);
    //instead use an hbox to prevent the GtkImage widget expanding beyond the image size, which then causes positioning errors.
    hbox = gtk_hbox_new (FALSE, 1);
    GtkWidget *event_box = gtk_event_box_new (); 
    gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_win, TRUE, TRUE, 0);
#if ((GTK_MAJOR_VERSION>=3)  && (GTK_MINOR_VERSION>8))  
    gtk_container_add (GTK_CONTAINER (score_and_scroll_win), hbox);
#else   
    gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW(score_and_scroll_win), hbox);
#endif    
    gtk_box_pack_start (GTK_BOX (hbox), event_box, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (event_box), Denemo.playbackview);
    
    g_signal_connect (G_OBJECT (event_box), "button_press_event", G_CALLBACK (button_press), NULL);
    g_signal_connect (G_OBJECT (event_box), "button_release_event", G_CALLBACK (button_release), NULL);
    g_signal_connect (G_OBJECT (event_box), "motion-notify-event", G_CALLBACK (motion_notify), NULL);
   
//these don't fire off  
//gtk_widget_add_events (event_box, GDK_KEY_PRESS_MASK);
//g_signal_connect (G_OBJECT (event_box), "key_press_event", G_CALLBACK (keypress_event), NULL);

  
  
  
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_markup (score_and_scroll_win,
                                   _("This window shows the typeset score as one long page. During playback the notes playing are highlighted"));
#if GTK_MAJOR_VERSION != 2
  g_signal_connect_after (G_OBJECT (Denemo.playbackview), "draw", G_CALLBACK (playbackview_draw_event), NULL);
  g_signal_connect (G_OBJECT (Denemo.playbackview), "draw", G_CALLBACK (playbackview_predraw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (Denemo.playbackview), "expose_event", G_CALLBACK (playbackview_draw_event), NULL);
  g_signal_connect (G_OBJECT (Denemo.playbackview), "expose_event", G_CALLBACK (playbackview_predraw_event), NULL);
#endif
  gtk_widget_show_all (main_vbox);
  gtk_widget_hide (top_vbox);
  static gint id; 
  if (!id)
    id = g_timeout_add  (50, (GSourceFunc)playback_redraw, NULL);
}
