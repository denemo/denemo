/* lyparserfuncs.h
 * header file for utility functions invoked by the
 * mudela parser */

/* For Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */
#ifndef LYPARSERFUNCSH
#define LYPARSERFUNCSH

#include <denemo/denemo.h>
#include "twoints.h"
#include "file.h"

void set_initial_staffcontexts (DenemoScore *si);

void setstaffname (DenemoScore *si, gchar * str);

enum clefs cleftypefromname (gchar * str);

void set_clef (DenemoScore *si, gchar * str);

void set_key (DenemoScore *si, struct twoints t);

void set_time (DenemoScore *si, struct twoints t);

struct twoints twointer (gint a, gint b);

gint keynametonumber (gchar * string);

gint mutointernalduration (gint muduration);

void
addtonewrapper (DenemoObject * DenemoObject, gchar tonetype,
                gint enshift, gint octind, struct twoints duration,
                gint dclef);
void set_tone_option (DenemoObject * curmudelaobj, gchar * option);

gint pitchtomid_c_offset (gchar name, gint octave);
 /* reset the lexer's initial state (after failed lex/parse) */
 void reset_initial_lexer_state (void);
 /* sets the current lily node to the current node and displays the user_string of that
    node for editing */
 void set_text_node (GtkWidget * button, DenemoGUI * si);
 
 gint lookup_keyword (gchar *);

typedef struct crescendo_state
{
	gboolean active_cres;
	gboolean active_decres;
} crescendo_state;

void init_crescendo_state();

 /* node types returned by the lily lexer */
 /* nodegeneric is large enough to overlay all the other node types */
 typedef struct 
   {
     int type;
     gchar *user_string;
     union
       {
         char c;
         int i;
         gboolean b;
         GList *branch;
         double r;
         GString *gstr;
         DenemoObject m;		/*cheating - too big really */
       }
     u;
     char *post_user_string;
   }
 nodegeneric;
 
 typedef struct nodec
   {
     int type;
     gchar *user_string;
     char c;
   }
 nodec;
 typedef struct nodei
   {
     int type;
     gchar *user_string;
     int i;
   }
 nodei;
 typedef struct node2i
   {
     int type;
     gchar *user_string;
     struct twoints t;
   }
 node2i;
 typedef struct node4i
   {
     int type;
     gchar *user_string;
     struct twoints t1;
     struct twoints t2;
   }
 node4i;
 typedef struct nodeb
   {
     int type;
     gchar *user_string;
     gboolean boolean;
   }
 nodeb;
 typedef struct nodeglist
   {
     int type;
     gchar *user_string;
     GList *branch;
     char *post_user_string;
   }
 nodeglist;
 typedef struct nodeid
   {
     int type;
     gchar *user_string;
     GList *id;
   }
 nodeid;
 typedef struct nodemus
   {
     int type;
     gchar *user_string;
     GList *music;
   }
 nodemus;
 typedef struct noder
   {
     int type;
     gchar *user_string;
     double r;
   }
 noder;
 typedef struct nodegstr
   {
     int type;
     gchar *user_string;
     GString *gstr;
   }
 nodegstr;
 
 /* only in non-terminals */
 typedef struct noden
   {
     int type;
     gchar *user_string;
     note n;
   }
 noden;
 
 typedef struct nodemin
   {
     int type;
     gchar *user_string;
   }
 nodemin;
 
 /* a set of convenience macros for accessing objects held in GLists */
 #define ntype(g) (((nodemin*)(g)->data)->type)
 #define u_str(g) (((nodemin*)(g)->data)->user_string)
 #define u_post_str(g) (((nodeglist*)(g)->data)->post_user_string)
 #define br(g) (((nodeglist*)(g)->data)->branch)
 #define gstr(g) (((nodegstr*)(g)->data)->gstr)
 #define gi(g) (((nodei*)(g)->data)->i)
 #define gobj(g) (((DenemoObject *)(g)->data)->object)
 
 
 #define staffstruct(g) ((DenemoStaff*)(g->data))
 
 
 #define BAD_ENSHIFT (0xFFFF)	/* not an enshift */
 
void set_post_events (DenemoObject *mud, gchar *usr_str, GList *g);
 
 /* these lexer states are manipulated by the parser  */
 void push_notes_state (void);
 void push_figuredbass_state (void);
 void push_chord_state (void);
 void push_lyric_state (void);
 void pop_state (void);
 /* inform parser about whitespace after last rule but before EOF */
 void set_trailing_white_space (gchar *trailing);
 
 
 /* creates a denemo scoreinfo from the lily parse tree TOP */
 int create_score_from_lily (DenemoScore * si, GList * top);
 /* sets parser state to error, creates a dialog with text and offers to exit
  * if the user doesn't exit the parser takes the whole file for editing */
 void parser_error (gchar * text, int linenum);
 /* sets up a text display for the lily text */
 void create_text_display (DenemoGUI * gui);
 /* toggles the display to show the topmost lily node in si
    and enable text editing or switch back to scorearea editing */
 void toggle_top_node (GtkWidget * button, DenemoGUI *gui);
 /* show text associated with curlilynode of si */
 void lily_text_change (DenemoGUI *gui);
 /* traverse G a GList of object nodes returning the first of type T or
    NULL if none */
    GList *findtok (GList * g, int t);
   /* move to the next score in the list of scores */
   void next_score (GtkWidget * button, DenemoGUI* gui);

#endif /*LYPARSERFUNCSH */
