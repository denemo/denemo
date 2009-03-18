/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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

#ifndef DENEMOTYPES_H
#define DENEMOTYPES_H

#include "denemo_objects.h"
#define EXT_MIDI 0
#define EXT_CSOUND 1
typedef void (*GActionCallback) (GtkAction *action, gpointer data);
#define G_ACTIONCALLBACK(f) ((GActionCallback)(f)) 
 /* and the following typedefs are basically here for so that it's
 * possible to understand what my code is doing -- just as much for
 * my sake as yours!
 *
 * What can I say; I've done a lot of programming in Java and
 * SML/NJ; 
 * I like my type names to convey information. */

/* The ->data part of each objnode presently points to a DenemoObject */

typedef GList objnode;


typedef enum 
  {
    UNDO,
    REDO
  }unre_mode;

/* The idea here is to make everything recursive.  The dominant
   paradigm is a linked list.  Handy that there's such a nice
   precooked implementation of them in glib, eh?  Note, however, that
   the whole score isn't treated as a linked list of notes and other
   objects as it is in, say Rosegarden; instead, the program has a
   linked list of musical objects for each measure, and only then are
   measures linked into staffs.  That's my main beef with Rosegarden
   -- I don't tend to compose my stuff in the order in which it will
   eventually be played. As such, I like being able to start entering
   music in the middle of my score, then the beginning, then the end,
   or whatever, as appropriate.  */

typedef enum DenemoObjType
{
  CHORD=0,
  TUPOPEN,
  TUPCLOSE,
  CLEF,
  TIMESIG,
  KEYSIG,
  BARLINE,
  STEMDIRECTIVE,
  MEASUREBREAK,
  STAFFBREAK,
  DYNAMIC,
  GRACE_START,
  GRACE_END,
  LYRIC,
  FIGURE,
  LILYDIRECTIVE,
  FAKECHORD,
  PARTIAL /* WARNING when adding to this list, add also to the type names that follow
	  *  keep the numeration ordered to allow access ny array index. */
}DenemoObjType;
static gchar *DenemoObjTypeNames[] =
{
  "CHORD",
  "TUPOPEN",
  "TUPCLOSE",
  "CLEF",
  "TIMESIG",
  "KEYSIG",
  "BARLINE",
  "STEMDIRECTIVE",
  "MEASUREBREAK",
  "STAFFBREAK",
  "DYNAMIC",
  "GRACE_START",
  "GRACE_END",
  "LYRIC",
  "FIGURE",
  "LILYDIRECTIVE",
  "FAKECHORD",
  "PARTIAL"
};
#define DENEMO_OBJECT_TYPE_NAME(obj) (obj?((obj->type<G_N_ELEMENTS(DenemoObjTypeNames))?DenemoObjTypeNames[obj->type]:NULL):NULL)


/**
 * Enumeration for Tuplets type
 * 
 */
typedef enum tuplet_type{
	DUPLET,
	TRIPLET,
	QUADTUPLET,
	QUINTUPLET,
	SEXTUPLET,
	SEPTUPLET	
}tuplet_type;

/**
 * Enumeration for Denemo's input mode
 */
typedef enum input_mode {
#define MODE_MASK (~(INPUTCLASSIC|INPUTEDIT|INPUTINSERT))
  INPUTCLASSIC = 1<<0, /* classic mode */
  INPUTEDIT = 1<<1, /* edit mode */
  INPUTINSERT = 1<<2, /* insert mode */
#define ENTRY_TYPE_MASK (~(INPUTNORMAL|INPUTREST|INPUTBLANK|INPUTRHYTHM))
  INPUTNORMAL = 1<<3, /* entry type notes */
  INPUTREST = 1<<4, /* entry type rests */
  INPUTBLANK = 1<<5,/* entry type non-printing rests */
  TRAVERSE = 1<<6, /* read-only */
  INPUTRHYTHM = 1<<7, /*Input rhythms without pitches*/
}input_mode;

/**
 * Denemo Action type currently used for undo/redo 
 * 
 */
typedef enum  action_type {
  ACTION_INSERT,
  ACTION_DELETE,
  ACTION_CHANGE
}action_type;

/**
 * Contains all the top-level information of an musical object
 * the object pointer contains the actual object
 */
typedef struct 
{
  DenemoObjType type; /**< The type of object pointed to by the gpointer object field below */
  gchar *user_string;/**< Holds user's original text parsed to generated this 
			object */
  gint basic_durinticks;
  gint durinticks; /**< Duration of object */
  gint starttick; /**< When the object occurs */ 
  gint starttickofnextnote; /**< When the next object occurs */
  /**< Allots extra space for accidentals or reverse-aligned notes if
   * the stem is down */
  gint space_before; /**< Used to specify how much space is needed before the object */
  gint minpixelsalloted; 
  gint x; /**< Holds x co-ordinate relative to the beginning of the measure. used in mousing.c */
  gboolean isstart_beamgroup; /**< TRUE if object is the start of a beam group */
  gboolean isend_beamgroup; /**< TRUE if this is the end of a beam group */
  /* Mudelaobjunion u; has become a pointer*/
  gpointer object; /**< the structures pointed to are given in denemo_objects.h */
  gboolean isinvisible; /**< If  TRUE it will be rendered white and will not be seen. If it is set FALSE then it will be drawn normally. */
} DenemoObject;


/**
 * Control of LilyPond context
 * Allows for e.g.  Piano context within staff group context by using bit fields
 */
typedef enum 
{
  DENEMO_NONE = 0,
  DENEMO_PIANO_START =  1<<0,
  DENEMO_PIANO_END = 1<<1,
  DENEMO_GROUP_START = 1<<2,
  DENEMO_GROUP_END = 1<<3,
  DENEMO_CHOIR_START = 1<<4,
  DENEMO_CHOIR_END = 1<<5
} DenemoContext;


/**
 * The ->data part of each measurenode points to an objlist, which is
 * a list of the musical objects in that measure. 
 */
typedef GList measurenode;


/**
 * DenemoStaff contains all the information relating to a musical staff
 * 
 */
typedef struct 
{
  measurenode *measures; /**< This is a pointer to each measure in the staff */
  gint sclef; /**< The initial clef see denemo_objects.h clefs */
  gint skey; /**< The inital keysig. >0=sharps <0 = flats */  
  gint skey_isminor;  /**< Values 0 = major, 1 = minor, 2 = mode */
  gint skeyaccs[7]; /**< The initial keysignature. Values -7=Cb... -1=F, 0=C, 1=G...7=C# in a chain of fifths */
  gint stime1; /**< This is the initial timesig numerator */
  gint stime2; /**< This is the initial timesig denominator */
  gint leftmost_clefcontext; /**< The clef for the leftmost measure visible in the window*/
  gint leftmost_keysigcontext; /**< The keysig at thestart of the leftmost measure visible in the window*/
  gint leftmost_keywidth; /**< Space allowed for leftmost keysig  */
  gint leftmost_time1context; /**< The leftmost visible timesig numerator */
  gint leftmost_time2context; /**< The leftmost visible timesig denominator */

  gint leftmost_keyaccs[7]; /**<  The keysig at thestart of the leftmost measure visible in the window */
  gint leftmost_stem_directive; /**< Stem directive at start of leftmost visible measure */
  DenemoContext context;   /**< The Lilypond context in which this staff appears */
  /*
   * Staff Parameters
   * Added Adam Tee 27/1/2000, 2001 
   */
  gint no_of_lines; /**< Number of lines on the staff */
  gint transposition; /**< Determines if the notes are to be played back at pitch or not */
  gint pos_in_half_lines; /**< I believe this maybe broken. It does not seem to do much*/ 
  gint volume;	/**< Volume used for midi/csound playback */
  gboolean mute_volume; /**< mute Volume of voices playback */
  /* Back to Hiller stuff */
  GString *staff_name;
  /* RTS: I've introduced the staff name here, the other two are versions
     of the voice name; however I'm still sticking to the unwritten convention
     that each staff's voices are contiguous in si->thescore. Without this you
     can't have same named voices in different staffs. */
  GString *denemo_name; /**< denemo_name gets copied into lily_name */
  GString *lily_name; /**< this is the name of the staff that is export to lilypond */
  GString *midi_instrument; /**< midi instrument name used for the staff when exported via midi */
  gboolean midi_prognum_override; /**< override to allow manually setting prognum + channel */
  guint8 midi_prognum; /**< midi prognum assigned to the staff voice */
  guint8 midi_channel; /**< midi channel assigned to the staff voice */
  gint jack_midi_out_port; /**< the port number that jackmidi has assigned */ 
  gint space_above; /**< space above the staff used in the denemo gui */
  gint space_below; /**< space below the staff used in the denemo gui */
  gboolean haslyrics; /**< TRUE if the staff has had lyrics attached */
  gboolean hasfigures; /**<TRUE if the staff has had figures attached. Only one staff should have this set */
  gboolean hasfakechords; /**<TRUE if the staff has had chord symbols attached. Only one staff should have this set */
  gint voicenumber; /**< presently set to 2 for any non-primary voices; we might want to
   * change that, though */
  measurenode ** is_parasite; /**< points to address of host staff's measures 
				 field if measures are owned by another 
				 staff */

  gint nummeasures; /**< Number of measures in the staff*/
  GList *tone_store; /**< list of pitches and durations used a source for
			the notes in this staff
			the data are tone* */
  GString *staff_prolog;/**< Customised version of the LilyPond prolog defining the music of this staff */
  GList *staff_directives;/**< List of DenemoDirective for the staff context, (only relevant for primary staff)*/
  GList *voice_directives;/**< List of DenemoDirective for the voice context */
  GString *lyrics_prolog;/**< (Unused)Customised version of the LilyPond prolog defining the lyrics of this staff */
  GString *figures_prolog;/**<  (Unused)Customised version of the LilyPond prolog defining the figured bass of this staff */
  GString *fakechords_prolog;/**<  (Unused)Customised version of the LilyPond prolog defining the chord symbols of this staff */
}DenemoStaff;

/* The ->data part of each staffnode points to a staff structure */

typedef GList staffnode;/**< The ->data part of each staffnode points to a DenemoStaff structure */
//typedef staffnode *score;

/* a pair of staffs, used to relate two staffs together */
typedef struct staff_info
{
  DenemoStaff *main_staff; /**< eg the bass line or the vocal part */
  DenemoStaff *related_staff; /**< eg the figures for the bass or the lyrics*/
}
staff_info;

typedef enum
{
	KeymapEntry,
	KeymapToggleEntry,
	KeymapRadioEntry
}KeymapCommandType;

typedef struct DenemoKeymap
{
  //command information store
  GtkListStore *commands; // ListStore for the commands

  //reference for easy access
  GHashTable *idx_from_name; //hashtable linking the name of a command to
							 //its index in the ListStore (values are guint *)

  GHashTable *idx_from_keystring; //hashtable linking the string representing
                                  //a keypress to the index of its command
                                  //The keystring is the output of
  				  //dnm_accelerator_name()
  GHashTable *cursors;//hashtable linking GdkEvent state to a cursor that should be used in that state
}keymap;


/**
 * DenemoPrefs holds information on user preferences. 
 */
typedef struct DenemoPrefs
{
  GString *lilypath; /**< This is the executable or full path to the lilypond executable */
  GString *midiplayer; /**< This is the external midifile player */ 
  GString *audioplayer; /**< This is used for playing audio files created from csound or other */
  gboolean playbackoutput; /**< This is a switch to turn on playing the csound output 
			     wav file after rendering or not */
  gboolean immediateplayback; /**< This options sends audio directly to synth as notes 
				are being entered */
  gboolean overlays; /*< whether overlays or insert should be used with pitch entry */
  gboolean continuous; /*< whether pitch entry overlays should cross barlines */
  gboolean lilyentrystyle;  
  gboolean createclones; 
  gboolean articulation_palette; /**< This switch makes the articulation pallete visable */
  gboolean notation_palette; /**< This switch makes the duration toolbar visable */
  gboolean rhythm_palette; /**< This option makes the rhythm toolbar visable */
  gboolean saveparts; /**< Automatically save parts*/
  gboolean autosave; /**< Auto save data */
  gint autosave_timeout;
  gint maxhistory;/**< how long a history of used files to retain */
  GString *browser; /**< Default browser string */
  GString *csoundcommand; /**< command used to execute csound */
  GString *csoundorcfile; /**< Path to .orc file used for csound playback */
  gboolean rtcs; /**< Real time csound */
  GString *sequencer;  /**< path to sequencer device */
  GString *midi_in;  /**< path to midi_in device */
  gboolean *jacktransport; /**< toggle on and off jack transport */
  GString *pdfviewer; /**< PDF viewer */
  GString *imageviewer; /**< Image Viewer */
  GString *texteditor; /**< texteditor for editing scripts and lilypond files */
  GString *denemopath; /**< path were denemo files are saved */
  GQueue *history; /**< Queue to contain recently opened files */

  GString *lilyversion; /**< Lilypoind Version */
  GString *temperament; /**< Preferred temperament for tuning to */
  gboolean strictshortcuts; /**< Whether shortcuts require CaspLock & NumLock to be correct */
  gint resolution; /**< Resolution of exported selection in dpi */
}DenemoPrefs;

/* DenemoDirectives are attached to chords and to the individual notes of a chord. They attach LilyPond and MIDI directivees that add to the note information & describe how to display themselves in the Denemo display */
typedef struct DenemoDirective
{
  GString *tag; /**< tag identifying the owner of this directive, usually the denemo command that created it */
  GString *prefix; /**< LilyPond text to be inserted before the chord */
  GString *postfix;/**< LilyPond text to be inserted after the chord */
  GString *display; /**< some text to display to describe the LilyPond attached to the chord */
  gint tx,ty; /**< x and y offsets in pixels for the display text */
  gint minpixels;/**< horizontal space needed by the display */
  gint x, y; /**< x and y offsets passed to LilyPond to control printed position */
  GdkBitmap *graphic; /**< bitmap to draw for this directive */
  gint gx, gy; /**< x and y offsets in pixels for the graphic */
  GString *graphic_name; /**< name of the graphic to be drawn */
  gint width, height; /**< width and height of the bitmap */

  gboolean locked;/**< If true the directive cannot be deleted easily */
  /* MIDI attributes not done yet */
} DenemoDirective;

/**
 * Contains the lilypond header information for the movements, plus markup between movements.
 *
 */

typedef struct LilypondHeaderFields
{
/* LilyPond movement header and markup information */
  GString *title;
  GString *subtitle;
  GString *poet;
  GString *composer;
  GString *meter;
  GString *opus;
  GString *arranger;
  GString *instrument;
  GString *dedication;
  GString *piece;
  GString *head;
  GString *copyright;
  GString *footer;
  GString *tagline;
  GString *extra;
  /* lilypond before and after each \score block     */
  GString *lilypond_before;
  GString *lilypond_after;
  /* preferences to go into \layout block */
  GString *layout;
}LilypondHeaderFields;



typedef enum 
{
  REPLACE_SCORE,
  ADD_STAFFS,
  ADD_MOVEMENTS
} ImportType;

typedef enum 
{
  WOODWIND,
  BRASS,
  STRINGS,
  VOCALS,
  PITCHEDPERCUSSION,
  PLUCKEDSTRINGS,
  KEYBOARDS,
  NONE
}InstrumentType;


typedef enum InputSource {
  INPUTKEYBOARD,
  INPUTAUDIO,
  INPUTMIDI
} InputSource;


/**
 * Structure to contain the list of Instruments for the score
 * configuration wizard
 *
 */
typedef struct 
{
	GString *name;
	GString *midiinstrument;
	gint sclef;
	gint transposition;
	gint numstaffs;

}InstrumentConfig;

/**
 * Stores global instrument type and a list of InstrumentConfig structures
 */
typedef struct 
{
	InstrumentType type;
	GList *instruments;  // List to contain a list of Instruments of given type
}InstrumentList;

/**
 * Contains data required for undo/redo operation 
 * Borrowed idea from GScore
 */
typedef struct unre_data
{
  gpointer object;    /* pointer to object to be undone/redone */
  gint staffnum;      /* staff number */     
  gint measurenum;    /* measure number */
  gint position;      /* position in bar */
  enum action_type action; /*action type */

}unre_data;
 

/**
 * Structure to hold bookmark information
 * Id - gint
 * Bar - gint
 * Staff - gint
 */
typedef struct Bookmark
{
  gint id;
  gint bar;
  gint staff;
}Bookmark;

/** 
 * Control of the LilyPond output for the whole musical score DenemoGUI
 *
 */
typedef struct DenemoLilyControl
{
  GString *papersize;
  GString *staffsize;
  GString *lilyversion;
  gboolean orientation;
  gboolean excerpt;
  GList *directives; /**< list of DenemoDirective for all music in the movements */
	
} DenemoLilyControl;


typedef struct DenemoScriptParam { /**< commands called by scripts use one of these to pass in a string and return a boolean */
  GString *string;/**< input string to command */
  gboolean status;/**< return value - TRUE = normal case execution of command/FALSE = exceptional case*/
} DenemoScriptParam;



typedef struct DenemoScoreblock {
  GString *scoreblock;/**< text of the scoreblock */
  gboolean visible;/**< Whether the scoreblock should be used by default */
} DenemoScoreblock;


/*
 *  DenemoScore structure representing a single movement of a piece of music.
 *  A movement corresponds with a single \score{} block in the LilyPond language
 *  that is,  uninterrupted music on a set of staffs, preceded by a title.
 */
 
typedef struct DenemoScore
{
  gboolean readonly; /**< Indicates if the file is readonly or not */
  GList *curlilynode; /**< the node of the lily parse tree on display 
			 in textwindow */
  GList *lily_file; /**< root of lily file parse, see lilyparser.y etc  */

 
  gint leftmeasurenum; /**< start at 1 */
  gint rightmeasurenum;/**< start at 1 */
  gint top_staff;
  gint bottom_staff;
  gint measurewidth; /**< List of all minimum measure widths */
  GList *measurewidths;
  gint widthtoworkwith;
  gint staffspace;

  /* Fields that have more to do with the data model and its manipulation,
   * though they may be modified by side-effects of the drawing routines */
  // score thescore;
  staffnode *thescore;
  
  staffnode *currentprimarystaff;
  staffnode *currentstaff;
  gint currentstaffnum;
  measurenode *currentmeasure;
  gint currentmeasurenum;/**< start at 1 */
  objnode *currentobject; /**< currentobject points to the note preceding the cursor when the
   * cursor is appending. == NULL only when currentmeasure is empty. */
  gint highesty; /**< max value of highesty of chord in the staff */
  gint cursor_x;
  gint cursor_y;
  gint staffletter_y;
  gint maxkeywidth;
  gboolean cursor_appending;
  
  gboolean cursoroffend;
  gint cursorclef;
  gint cursoraccs[7];
  gint cursortime1;
  gint cursortime2;
  gint curmeasureclef;
  gint curmeasurekey;
  gint curmeasureaccs[7];
  gint nextmeasureaccs[7];
  /* These are used for determining what accidentals should be there
   * if the cursor advances to the next measure from the next "insert chord"
   * operation */
  gint curmeasure_stem_directive;



  /* Is there a figured bass present, is so this relates the bass
   * with its figures staff, if one is present */
  staff_info * has_figures;
  staff_info *has_fakechords;
  /* Now stuff that's used for marking areas */
  gint markstaffnum;
  gint markmeasurenum;
  gint markcursor_x;
  gint firststaffmarked;
  gint laststaffmarked;
  gint firstmeasuremarked;
  gint lastmeasuremarked;
  gint firstobjmarked;
  gint lastobjmarked;
  LilypondHeaderFields headerinfo;/**< Lilypond header fields and markup for this movement */


  /* Fields used for MIDI playback */
  gint tempo;
  gint start;
  gint end;
  gint stafftoplay;


  
  GList *savebuffer;
  /* Holds List of Plugin Names */
  



  /*list of undo data */
  GQueue *undodata;
  GQueue *redodata;
  gint undo_redo_mode;

  

 
  GList *bookmarks;
  gint currentbookmark;
  GList *Instruments;
  
}DenemoScore;

/**
 * DenemoGUI representing a musical score, with associated top level
 * GUI and a list of movements (DenemoScore) and a pointer to the current
 * movement. 
 */
typedef struct DenemoGUI
{
  /* Fields used fairly directly for drawing */
  GtkWidget *page;
  GtkWidget *scorearea;
  GdkPixmap *pixmap;
  GtkObject *vadjustment;
  GtkWidget *vscrollbar;
  GtkObject *hadjustment;
  GtkWidget *hscrollbar;

  GtkWidget *printarea;/**< area holding a print preview */
  GtkWidget *printvscrollbar;/**< scrollbar widget for printarea */
  GtkWidget *printhscrollbar;/**< scrollbar widget for printarea */
  GdkPixbuf *pixbuf;/**< print preview pixbuf */


  gchar *xbm; /**< xbm representation of graphic bitmap from selected rectangle in print preview area*/
  gint xbm_width, xbm_height;/**< width and height of the xbm data */
  GtkWidget *textwindow; /**< LilyPond output window */
  GtkTextBuffer *textbuffer;   /**< buffer for LilyPond text */
  GtkTextView *textview; /**< LilyPond output text view */
  gchar *namespec;/**< A spec of which parts/movements to print */
  

  GList *plugins;
  gint plugincounter;

  GtkWidget* articulation_palette; /**< Articulation palette window */
  InputSource input_source;/**< Where pitches are coming into Denemo (keyboard, audio, midi) */
  input_mode mode; /**< Input mode for Score */
  GtkWidget *progressbar;



  GList *movements;   /**< a list of DenemoScore, NULL if just one movement */
  DenemoScore *si;  /**< the (current)  movement in the musical score controlled by this gui */
  DenemoLilyControl lilycontrol; /**< Control of the LilyPond output for whole musical score */
  GList *custom_scoreblocks; /**< List of customized texts for LilyPond output, replaces standard score blocks, elements are DenemoScoreblock * */
  GString *custom_prolog; /**< Customized text for LilyPond output, replaces standard prolog */
  gpointer lilystart, lilyend; /**<range of lilytext  */
  GString **target; /**< pointer to target string for modification in lilytext  */
  GList *anchors;/**< anchors in the LilyPond text at points where that can be edited */

  GString *filename;/**< the filename to save to */
  GString *autosavename;/**< the filename to autosave to, full path */

 
  guint changecount;/**< number of edits since last save */
  guint lilysync;/**< value of changecount when the Lily text was last refreshed */

 
  /* support for rhythm patterns */
  GList *rhythms;/**< list of RhythmPattern s */
  GList *currhythm; /**< currently in use element of rhythms */
  GList *rstep; /**< step within RhythmPattern->rsteps, the current element of the current rhythm pattern */

  struct RhythmPattern *prevailing_rhythm; /**< one of singleton_rhythms used for entering notes */
}DenemoGUI;


/**
 * RhythmPattern: a list of RhythmElements with a button to invoke it;
 */

typedef struct RhythmPattern
{
  GList *rsteps; /**< the data are RhythmElements */
  GtkToolButton *button; /**< the button on the rhythm toolbar which invokes this rhythm */
} RhythmPattern;


/**
 * RhythmElement: information about one element of a RhythmPattern, 
 * e.g. one RhythmElement could contain the actions "quarter-note,dotted,begin slur";
*/

typedef struct RhythmElement
{
  GList* functions; /**< data in list are functions to be called including modifiers 
		      eg insert_chord_3key, add dot, slur ...  */
  gpointer icon; /**< a string, but displayed in music font, which labels the button when this RhythmElement is
		  the current one*/
  RhythmPattern *rhythm_pattern;/**< the rhythm pattern which this element belongs to */
} RhythmElement;


struct cs_callback
{
	GtkWidget *entry;
	GtkWidget *dialog;
	DenemoGUI *gui;
	
};

static gchar* ext_pidfiles[] = {"midiplayer.pid", "csoundplayer.pid", NULL};

/** 
 * The (singleton) root object for the program
 *
 */
struct DenemoRoot
{
  /* window state */
  gint width;
  gint height;
  gboolean maximized;
  keymap *map; /**< pointer to data describing each of the Denemo commands and their keyboard shortcuts */
  gchar *last_merged_command;/**<filename of last command merged into the menu system */
  gint last_keyval, last_keystate;/**< most recent keypress which successfully invoked a command */
  GList *guis; /**< the list of DenemoGUI objects, representing pieces of music
		  simultaneously open */
  DenemoPrefs prefs;  /**< Preferences stored on exit and re-loaded on startup */
  gint autosaveid;/**< autosave timer id: only one musical score is being autosaved at present */
  gint accelerator_status; /**< if the accelerators have been saved, or extra ones for special keys defined  */
  GtkUIManager *ui_manager;  /**< UI manager */
  GtkWidget *window;
  GtkActionGroup *action_group;/*< The action group for the actions that are Denemo commands */
  DenemoGUI *gui; /**< The current gui */
  GtkWidget *notebook;/**< contains the gui.page widgets */
  GtkWidget *statusbar;
  gint status_context_id;
  GtkWidget *input_source; /**< A label widget advising of source of external input */
  GString *input_filters; /**< Description of any filters operating on external input */
  GtkWidget *menubar;/**< Main menubar to giving load/save play etc functionality */
  GtkWidget *ClassicModeMenu;/**< Menu to give the note editing facilities in Classic mode */
  GtkWidget *InsertModeMenu;/**< Menu to give the note editing facilities in Insert mode */
  GtkWidget *EditModeMenu;/**< Menu to give the note editing facilities in Edit mode */
  GtkWidget *ModelessMenu;/**< Menu to give the note editing facilities when used without modes */
  gboolean QuickShortcutEdits;/**< TRUE if pressing a key while hovering over a menu item sets a shortcut */

  struct RhythmPattern *singleton_rhythms[256]; /**< rhythm patterns for the EntryToolbar */
  gboolean ScriptRecording;/**< TRUE when menuitems presses are being recorded as scheme script*/
  GtkWidget *ScriptView; /**< a GtkTextView containing a scheme script */
}  Denemo; /**< The root object. */

#endif
