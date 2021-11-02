/* pitchentry.c
 *  responses to pitchrecognition from audio in
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c)2007 Richard Shann
 */
#include <math.h>
#include <string.h>             /* for strcmp() */
#include <stdlib.h>             /* for abs() */
#include "audio/audio.h"
#include "audio/pitchentry.h"
#include "core/view.h"
#include "audio/audiointerface.h"
#include "command/measure.h"
#include "audio/pitchrecog.h"
#include "audio/audiocapture.h"
#include "audio/midirecord.h"

#define DEFAULT_HIGH (1400.0)
#define DEFAULT_LOW (60.0)
#define DEFAULT_TIMER_RATE (50)
#define QUARTER_COMMA_MEAN_TONE "Quarter comma meantone"

static GtkWidget *PR_window = NULL;     /* a top level window for controlling pitch-recognition entry.
                                           We do not create one of these for each view (ie each DenemoProject object, ie each score) because there is only one audio input source being used, so we would have to cope with resource contention issues, there is just no point. */
static DenemoProject *PR_gui;       /* the gui for which the pitch recognition has been set up */


gint PR_click;                  /*volume of a audible warning of next measure or extra tones in measure */
static gboolean PR_tuning;      /* whether the notes should be analyzed to refine the frequency determination
                                   for tuning instruments etc */
static GtkWidget *PR_notelabel = NULL;
static GtkWidget *PR_deviation = NULL;
static GtkWidget *PR_indicator = NULL;
static GtkWidget *PR_label = NULL;
static gdouble PR_target_pitch = 440.0;
static gdouble PR_accurate_pitch = 0.0;

static guint PR_timer;          // timer id
static guint PR_enter;          // signal id
static guint PR_leave;          // signal id
//keymap *PR_oldkeymap;// the keymap of PR_gui when PR was started
//keymap *PR_rhythmkeymap;// special keymap for rhythm work.

static guint PR_time = DEFAULT_TIMER_RATE;      //the period of timer for checking for a new pitch in ms 10 gives quicker response,100 more reliability...
gboolean PR_enable = TRUE;
guint greatest_interval;
static gdouble lowest_pitch = DEFAULT_LOW;
static gdouble highest_pitch = DEFAULT_HIGH;
static gboolean repeated_notes_allowed;
static gdouble transposition_required = 1.0;

static int enharmonic_position = 0;

typedef struct notespec
{
  guint step;                   /* 0=C 1=D ... 6=B */
  gint alteration;              /* -2 = bb, -1=b, 0=natural, 1=#, 2=x */
  gint cents;                   //deviation from Equal
} notespec;

typedef struct notepitch
{

  double pitch;                 /* pitch of note of given notespec in range C=260 to B=493 Hz
                                   actual pitch is *=2^octave   */
  notespec spec;
} notepitch;

typedef struct temperament
{
  gchar *name;
  gint unusedsharp;             /* which notepitch is the sharpest in circle of 5ths - initially G# */
  gint unusedflat;              /* which notepitch is the flattest in circle of 5ths - initially Eb */
  notepitch notepitches[12];    /* pitches of C-natural, C#, ... A#, B-natural */
} temperament;

static gint flat_degree = 3, sharp_degree = 8;  /* which notepitch is the sharpest in circle of 5ths - initially G#
                                                   flattest in circle of 5ths - initially Eb */
static gint temperament_offset = 0;     /* shift in temperament around circle of 5ths */
#if 0
Pythagorean
  261.6 279.4 294.3 310.1 331.1 348.8 372.5 392.4 419.1 441.5 465.1 496.7
  Van Zwolle
  261.6 279.4 294.3 314.3 331.1 353.6 372.5 392.4 419.1 441.5 471.5 496.7
  Meantone
  261.6 272.8 292.3 313.2 326.7 350.0 365.0 391.1 407.9 437.0 468.3 488.3
  Silbermann I
  261.6 275.0 293.0 312.2 328.1 349.6 367.5 391.6 411.6 438.5 467.2 491.1
  Silbermann II
  261.6 276.2 293.0 312.2 329.6 349.6 369.2 391.6 413.4 438.5 467.2 493.3
  Rameau
  261.6 276.9 293.3 309.7 330.4 348.8 369.2 391.1 413.9 438.5 463.0 493.9
  Werckmeister III
  261.6 277.5 293.3 311.1 328.9 350.0 370.0 391.1 414.8 438.5 466.7 493.3
  Werckmeister IV
  261.6 275.6 293.0 311.5 329.6 348.8 369.2 390.7 413.4 437.5 467.2 492.2
  Werckmeister V
  261.6 275.6 293.3 311.1 328.9 348.8 368.7 392.4 413.4 438.5 465.1 493.3
  Werckmeister VI
  261.6 278.3 293.8 311.3 331.1 349.5 372.5 391.7 414.2 441.5 466.9 496.7
  Kirnberger II 261.6 277.5 294.3 312.2 328.9 348.8 370.0 392.4 416.2 438.5 465.1 493.3 Kirnberger III 261.6 277.5 293.3 311.1 328.9 348.8 370.0 391.1 414.8 438.5 465.1 493.3 Italian 18 th Century 261.6 277.2 293.0 311.1 328.9 349.2 370.0 391.1 414.4 438.0 465.1 492.8 Equal Temperament 261.6 277.2 293.7 311.1 329.6 349.2 370.0 392.0 415.3 440.0 466.2 493.9 HARPSICHORD TUNING - A COURSE OUTLINE, by G.C.Klop, distributed by The Sunbury Press, P.O.Box 1778, Raleigh, NC 27602.
#endif




static temperament Pythagorean = {
  "Pythagorean", 8, 3,
  {
   {261.6, {0, 0, 0}},
   {279.4, {0, 1, 0}},
   {294.3, {1, 0, 0}},
   {310.1, {2, -1,0}},
   {331.1, {2, 0, 0}},
   {348.8, {3, 0, 0}},
   {372.5, {3, 1, 0}},
   {392.4, {4, 0, 0}},
   {419.1, {4, 1, 0}},
   {441.5, {5, 0, 0}},
   {465.1, {6, -1,0}},
   {496.7, {6, 0, 0}}
   }
};


static temperament Rameau = {
  "Rameau", 8, 3,
  {
   {261.6, {0, 0, 0}},
   {276.9, {0, 1, 0}},
   {293.3, {1, 0, 0}},
   {309.7, {2, -1,0}},
   {330.4, {2, 0, 0}},
   {348.8, {3, 0, 0}},
   {369.2, {3, 1, 0}},
   {391.1, {4, 0, 0}},
   {413.9, {4, 1, 0}},
   {438.5, {5, 0, 0}},
   {463.0, {6, -1,0}},
   {493.9, {6, 0, 0}}
   }
};


static temperament Equal = {
  "Equal", 8, 3,
  {
   {261.6, {0, 0, 0}},
   {277.2, {0, 1, 0}},
   {293.7, {1, 0, 0}},
   {311.1, {2, -1, 0}},
   {329.6, {2, 0, 0}},
   {349.2, {3, 0, 0}},
   {370.0, {3, 1, 0}},
   {392.0, {4, 0, 0}},
   {415.3, {4, 1, 0}},
   {440.0, {5, 0, 0}},
   {466.2, {6, -1, 0}},
   {493.9, {6, 0, 0}}
   }
};

static temperament Lehman = {
  "Lehman", 8, 3,
  {
   {262.37 * 1, {0, 0, 0}},
   {262.37 * 1.0590, {0, 1, 0}},
   {262.37 * 1.1203, {1, 0, 0}},
   {262.37 * 1.1889, {2, -1, 0}},
   {262.37 * 1.2551, {2, 0, 0}},
   {262.37 * 1.3360, {3, 0, 0}},
   {262.37 * 1.4120, {3, 1, 0}},
   {262.37 * 1.4968, {4, 0, 0}},
   {262.37 * 1.5869, {4, 1, 0}},
   {262.37 * 1.6770, {5, 0, 0}},
   {262.37 * 1.7816, {6, -1, 0}},
   {262.37 * 1.8827, {6, 0, 0}}
   }
};

static temperament Meantone = {
  QUARTER_COMMA_MEAN_TONE, 8, 3,
  {
   {261.6, {0, 0, 0}},
   {272.8, {0, 1, 0}},
   {292.3, {1, 0, 0}},
   {313.2, {2, -1, 0}},
   {326.7, {2, 0, 0}},
   {350.0, {3, 0, 0}},
   {365.0, {3, 1, 0}},
   {391.1, {4, 0, 0}},
   {407.9, {4, 1, 0}},
   {437.0, {5, 0, 0}},
   {468.3, {6, -1, 0}},
   {488.3, {6, 0, 0}}
   }
};

//  261.6 279.4 294.3 314.3 331.1 353.6 372.5 392.4 419.1 441.5 471.5 496.7
  static temperament VanZwolle = {
  "Van Zwolle", 8, 3,
  {
   {261.6, {0, 0, 0}},            /* c */
   {279.4, {0, 1, 0}},
   {294.3, {1, 0, 0}},            /* d */
   {314.3, {2, -1, 0}},           /* Eb */
   {331.1, {2, 0, 0}},            /* e */
   {353.6, {3, 0, 0}},            /* f */
   {372.5, {3, 1, 0}},
   {392.4, {4, 0, 0}},            /* g */
   {419.1, {4, 1, 0}},            /* g# */
   {441.5, {5, 0, 0}},            /* a */
   {471.5, {6, -1, 0}},           /* Bb */
   {496.7, {6, 0, 0}}             /* b */
   }
};

//261.6 275.0 293.0 312.2 328.1 349.6 367.5 391.6 411.6 438.5 467.2 491.1
static temperament SilbermannI = {
  "Silbermann I", 8, 3,
  {
   {261.6, {0, 0, 0}},            /* c */
   {275.0, {0, 1, 0}},
   {293.0, {1, 0, 0}},            /* d */
   {312.2, {2, -1, 0}},           /* Eb */
   {328.1, {2, 0, 0}},            /* e */
   {349.6, {3, 0, 0}},            /* f */
   {367.5, {3, 1, 0}},
   {391.6, {4, 0, 0}},            /* g */
   {411.6, {4, 1, 0}},            /* g# */
   {438.5, {5, 0, 0}},            /* a */
   {467.2, {6, -1, 0}},           /* Bb */
   {491.1, {6, 0, 0}}             /* b */
   }
};

// 261.6 276.2 293.0 312.2 329.6 349.6 369.2 391.6 413.4 438.5 467.2 493.3
static temperament SilbermannII = {
  "Silbermann II", 8, 3,
  {
   {261.6, {0, 0, 0}},            /* c */
   {276.2, {0, 1, 0}},
   {293.0, {1, 0, 0}},            /* d */
   {312.2, {2, -1, 0}},           /* Eb */
   {329.6, {2, 0, 0}},            /* e */
   {349.6, {3, 0, 0}},            /* f */
   {369.2, {3, 1, 0}},
   {391.6, {4, 0, 0}},            /* g */
   {413.4, {4, 1, 0}},            /* g# */
   {438.5, {5, 0, 0}},            /* a */
   {467.2, {6, -1, 0}},           /* Bb */
   {493.3, {6, 0, 0}}             /* b */
   }
};

//261.6 277.5 293.3 311.1 328.9 350.0 370.0 391.1 414.8 438.5 466.7 493.3
static temperament WerckmeisterIII = {
  "Werckmeister III", 8, 3,
  {
   {261.6, {0, 0, 0}},            /* c */
   {277.5, {0, 1, 0}},
   {293.3, {1, 0, 0}},            /* d */
   {311.1, {2, -1, 0}},           /* Eb */
   {328.9, {2, 0, 0}},            /* e */
   {350.0, {3, 0, 0}},            /* f */
   {370.0, {3, 1, 0}},
   {391.1, {4, 0, 0}},            /* g */
   {414.8, {4, 1, 0}},            /* g# */
   {438.5, {5, 0, 0}},            /* a */
   {466.7, {6, -1, 0}},           /* Bb */
   {493.3, {6, 0, 0}}             /* b */
   }
};

static temperament WerckmeisterIV = {
  "Werckmeister IV", 8, 3,
  {
   {263.11, {0, 0, 0}},            /* c */
   {275.93, {0, 1, 0}},
   {294.66, {1, 0, 0}},            /* d */
   {311.83, {2, -1, 0}},           /* Eb */
   {330.00, {2, 0, 0}},            /* e */
   {350.81, {3, 0, 0}},            /* f */
   {369.58, {3, 1, 0}},
   {392.88, {4, 0, 0}},            /* g */
   {413.90, {4, 1, 0}},            /* g# */
   {440.00, {5, 0, 0}},            /* a */
   {469.86, {6, -1, 0}},           /* Bb */
   {492.77, {6, 0, 0}}             /* b */
   }
};


static temperament *PR_temperament = &Equal;    /* the currently used temperament */

static temperament *temperaments[] = { &Equal, &Meantone, &WerckmeisterIII,  &WerckmeisterIV, &Lehman, &Rameau, &Pythagorean, &SilbermannII, &SilbermannI, &VanZwolle};




static void
pr_display_note (gchar * notename)
{
  gchar *labelstr = g_strdup_printf ("<span foreground=\"black\" font_desc=\"48\">%s</span>", notename);
  //printf("string is %s\n", labelstr);
  gtk_label_set_markup (GTK_LABEL (PR_notelabel), labelstr);
  g_free (labelstr);
}

static void
pr_display_pitch_deviation (double deviation)
{
  gchar *labelstr = g_strdup_printf ("<span foreground=\"%s\" font_desc=\"48\">%2.1f</span>", deviation > 0.0 ? "blue" : deviation < 0.0 ? "red" : "black", deviation);
  gtk_label_set_markup (GTK_LABEL (PR_deviation), labelstr);
  g_free (labelstr);
}



/* return c,d,e,f,g,a,b depending on the step. German translation will
   be difficult I'm afraid. */
static gchar
step_name (guint step)
{
  if (step > 4)
    step -= 7;
  return 'C' + step;


}

/* return "##", "#" ..."  "..... "bb" for double sharp to double flat accidentals
 a const string is returned*/
static gchar *
alteration_name (gint alteration)
{
  switch (alteration)
    {
    case -2:
      return "𝄫";
    case -1:
      return "♭";
    case 0:
      return "";
    case 1:
      return "#";

    case 2:
      return "𝄪";
    default:
      return "ER";
    }
}

static gint fifths[7] = { 0, 2, 4, -1, 1, 3, 5 };

/* check_interval() checks the interval passed notes
 * returns TRUE if the interval is unusual */
gboolean
check_interval (gint step1, gint enshift1, gint step2, gint enshift2)
{
  gint distance = fifths[step1] + 7 * enshift1 - fifths[step2] - 7 * enshift2;
  if (distance > 6 || distance < -6)
    return TRUE;
  return FALSE;
}

gboolean
check_midi_intervals (GList * midichord)
{
  GList *g = midichord;
  gint most = 0, least = G_MAXINT;
  for (; g; g = g->next)
    {
      gint offset, enshift, octave, value;
      notenum2enharmonic (GPOINTER_TO_INT (g->data), &offset, &enshift, &octave);
      value = fifths[offset] + 7 * enshift;
      most = MAX (most, value);
      least = MIN (least, value);
      //g_debug("note %d value %d\noffset %d enshift %d least=%d most=%d\n", g->data, value, offset, least, least, most);
    }
  return abs (most - least) < 6;
}

/* returns the note names currently set for the given temperament
 caller must free the string */
static gchar *
notenames (gpointer p)
{
  temperament *t = (temperament *) p;
  gchar *str, *oldstr;
  gint i;
  oldstr = g_strdup ("");
  for (i = 0; i < 12; i++)
    {
      if (i == flat_degree)
        str = g_strdup_printf ("%s<span size=\"large\" foreground=\"red\">%c%s</span> ", oldstr, step_name (t->notepitches[i].spec.step), alteration_name (t->notepitches[i].spec.alteration));
      else if (i == sharp_degree)
        str = g_strdup_printf ("%s<span size=\"large\" foreground=\"blue\">%c%s</span> ", oldstr, step_name (t->notepitches[i].spec.step), alteration_name (t->notepitches[i].spec.alteration));
      else
        str = g_strdup_printf ("%s%c%s ", oldstr, step_name (t->notepitches[i].spec.step), alteration_name (t->notepitches[i].spec.alteration));
      g_free (oldstr);
      oldstr = str;
    }
  return str;
}

gchar *
determine_interval (gint bass, gint harmony, gboolean * status)
{
  gint semitones = harmony - bass;
  gint *accs = ((DenemoStaff *) Denemo.project->movement->currentstaff->data)->keysig.accs;
  notepitch bassnote = PR_temperament->notepitches[bass % 12];
  notepitch harmonynote = PR_temperament->notepitches[harmony % 12];
  gint interval = harmonynote.spec.step - bassnote.spec.step + 1;
  if (interval < 2)
    interval += 7;
  if (interval == 2 && semitones > 12)
    interval = 9;
  gint inflection = harmonynote.spec.alteration - accs[harmonynote.spec.step];

  *status = check_interval (bassnote.spec.step, bassnote.spec.alteration, harmonynote.spec.step, harmonynote.spec.alteration);

//g_debug("have %d %d\n", bassnote.spec.step, harmonynote.spec.step);
// g_debug("Bass %d harmony %d\nInterval is %d, semitones is %d cf (%d, %d)  \n keyaccs of bass note %d of harmony %d\ninflection %d\n", bass, harmony, interval, semitones, bassnote.spec.alteration, harmonynote.spec.alteration, accs[bassnote.spec.step], accs[harmonynote.spec.step], inflection);
  gchar *modifier = "";
  if (interval == 5 && semitones == 6)
    modifier = "/";
  else if (inflection < 0)
    modifier = "-";
  else if (inflection > 0)
    modifier = "+";
  if (harmony < bass)
    return g_strdup_printf ("%s", "~"); //an extender - was A non printing figure
  if (interval == 3 && inflection)
    return g_strdup_printf ("%c%s", '_', modifier);
  else
    return g_strdup_printf ("%d%s", interval, modifier);
}


static gchar *
nameof (gint notenumber)
{
  return g_strdup_printf ("%c%s", step_name (PR_temperament->notepitches[notenumber].spec.step), alteration_name (PR_temperament->notepitches[notenumber].spec.alteration));
}

//Caller must free
gchar *
sharpest (void)
{
  return nameof (sharp_degree);
}

gchar *
flattest (void)
{
  return nameof (flat_degree);
}

/* returns an opaque id for the user's default temperament
 FIXME user prefs */
static gpointer
default_temperament ()
{
  gint i;
  for (i = 0; i < G_N_ELEMENTS (temperaments); i++)
    {
      if (!strcmp (Denemo.prefs.temperament->str, temperaments[i]->name))
        return (gpointer) temperaments[i];
    }
  return (gpointer) & Equal;
}


static void
sharpen (GtkWidget * label)
{
  enharmonic_position++;
#define f  (PR_temperament->notepitches[flat_degree].spec)
  gint next = (flat_degree + 11) % 12;
#define g (PR_temperament->notepitches[next].spec)
  if (g.alteration + 1 > 2)
    return;
  else
    {
      f.step = g.step;
      f.alteration = g.alteration + 1;
    }
#undef f
#undef g
  sharp_degree = flat_degree;
  flat_degree = (flat_degree + 7) % 12;
  temperament_offset = (temperament_offset + 5) % 12;
  if (!Denemo.non_interactive)
    {
      gchar *names = notenames (PR_temperament);
      gtk_label_set_markup ((GtkLabel *) label, names);
      g_free (names);
      switch_back_to_main_window ();
    }
  reset_temperament ();
  return;
}

static void
flatten (GtkWidget * label)
{
  enharmonic_position--;
#define s (PR_temperament->notepitches[sharp_degree].spec)
  gint next = (sharp_degree + 1) % 12;
#define t (PR_temperament->notepitches[next].spec)
  if (t.alteration - 1 < -2)
    return;
  else
    {
      s.step = t.step;
      s.alteration = t.alteration - 1;
    }
#undef s
#undef t
  flat_degree = sharp_degree;
  sharp_degree = (sharp_degree + 5) % 12;
  temperament_offset = (temperament_offset + 7) % 12;
  if (!Denemo.non_interactive)
    {
      gchar *names = notenames (PR_temperament);
      gtk_label_set_markup ((GtkLabel *) label, names);
      g_free (names);
      switch_back_to_main_window ();
    }
  reset_temperament ();
  return;
}

void
adjust_tonal_center (gint * accs)
{
  gint i;
  gint center;
  for (center = 0, i = 0; i < 7; i++)
    center += accs[i];
  set_enharmonic_position (center);
}

void
set_enharmonic_position (gint position)
{
  while (position < enharmonic_position)
    {
      set_flatter (NULL, NULL);
    }
  while (position > enharmonic_position)
    {
      set_sharper (NULL, NULL);
    }
}

gint
get_enharmonic_position (void)
{
  return enharmonic_position;
}

static void
enharmonic_step (gboolean sharp)
{
  gchar *sharpestname, *flattestname;
  if (sharp)
    sharpen (PR_label);
  else
    flatten (PR_label);
}

void
set_sharper (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{
  enharmonic_step (TRUE);
}

void
set_flatter (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{
  enharmonic_step (FALSE);
}

void
signal_measure_end (void)
{
  if (Denemo.prefs.immediateplayback)
    {
      play_note (DEFAULT_BACKEND, 0 /*port */ , 9, Denemo.prefs.measureswitchsound, 300, 127);//(gint) (100 * Denemo.project->movement->master_volume));
    }
  if (Denemo.project->movement->recording && (Denemo.project->movement->recording->click_track_created))
	synchronize_recording ();
}


//Computes the mid_c_offset and enshift for the MIDI notenum passed in
void
notenum2enharmonic (gint notenum, gint * poffset, gint * penshift, gint * poctave)
{

  *penshift = notenum % 12;
  temperament *t = PR_temperament;
  *poctave = (notenum) / 12 - 5;
  *poffset = t->notepitches[*penshift].spec.step;
  *penshift = t->notepitches[*penshift].spec.alteration;
  if ((notenum % 12) == 0 && *penshift == 1)
    *poctave = *poctave - 1;
  if ((notenum % 12) == 11 && *penshift == -1)
    *poctave = *poctave + 1;
  return;
}



#ifdef _HAVE_PORTAUDIO_
static void
sound_click (void)
{
  if (PR_click)
    signal_measure_end ();
}

static GList *
get_tones (GList * tone_store, gint measurenum)
{
  GList *g = g_list_nth (tone_store, measurenum);
  if (g)
    return g->data;
  return NULL;
}

/* apply the tones in the currentmeasure to the notes of the currentmeasure
* return TRUE if measure has enough tones for all its notes
*/

gboolean
apply_tones (DenemoMovement * si)
{
  gboolean ret = FALSE;
  DenemoStaff *curstaff = ((DenemoStaff *) si->currentstaff->data);
  GList *store;
  gint measurenum;
  store = (curstaff->tone_store);
  measurenode *curmeasure = curstaff->themeasures;
  GList *store_el = NULL;
  // move cursor to start of current measure
  si->currentobject = (objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects;
  si->cursor_x = 0;
  si->cursor_appending = !GPOINTER_TO_INT (si->currentobject);
  //calcmarkboundaries (si);
  measurenum = si->currentmeasurenum - 1;
  curmeasure = si->currentmeasure;
  if (curmeasure)
    {
      store_el = get_tones (store, measurenum);
      objnode *curobj = ((DenemoMeasure*)curmeasure->data)->objects;
      while (curobj)
        {
          tone *thetone = NULL;
          //skip over invalid tones
          while (store_el && (thetone = (tone *) store_el->data) && !thetone->valid)
            store_el = store_el->next;
          gboolean tone_stored = FALSE;
          DenemoObject *theobj = (DenemoObject *) curobj->data;
          if (theobj->type == CHORD && ((chord *) theobj->object)->notes /* not a rest */ )
            {
              if (thetone == NULL || store_el == NULL)
                ((chord *) theobj->object)->tone_node = NULL;
              else
                {
                  gint dclef = theobj->clef->type;
                  gint mid_c_offset = thetone->step;
                  ((chord *) theobj->object)->tone_node = store_el;
                  modify_note ((chord *) theobj->object, mid_c_offset, thetone->enshift, dclef);
                  tone_stored = TRUE;
                  if (!si->cursor_appending)
                    cursorright (NULL, NULL);
                  store_el = store_el->next;

                }               // tone available
            }                   // note available
          /* skip over non notes */
          do
            {
              curobj = curobj->next;
              if (curobj)
                theobj = (DenemoObject *) curobj->data;
            }
          while (curobj && (theobj->type != CHORD || ((chord *) theobj->object)->notes == NULL));
          if (tone_stored && curobj == NULL && curmeasure->next)
            ret = TRUE;
        }                       // while objects in measure
      if (store_el && !Denemo.prefs.continuous)
        sound_click ();         //extra tones in measure
      showwhichaccidentals ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects);
    }
  return ret;
}


/*
 * enter_note_in_score
 * enters the note FOUND in the score gui->movement at octave OCTAVE steps above/below mid-c
 */
static void
enter_note_in_score (DenemoProject * gui, notepitch * found, gint octave)
{
  //printf("Cursor_y %d and staffletter = %d\n", gui->movement->cursor_y, gui->movement->staffletter_y);
  gui->last_source = INPUTAUDIO;
  gui->movement->cursor_y = gui->movement->staffletter_y = found->spec.step;
  gui->movement->cursor_y += 7 * octave;
  //edit_or_append_pitch (found->spec.step, found->spec.alteration);
  Denemo.project->movement->pending_enshift = found->spec.alteration;
  edit_or_append_pitch (found->spec.step, TRUE);
  Denemo.project->movement->pending_enshift = 0;
  displayhelper (gui);
}





static GList *
put_tone (GList * store, gint measurenum, tone * thetone)
{
  // extend store if too small
  gint i = measurenum + 1 - g_list_length (store);
  for (; i > 0; i--)
    {
      store = g_list_append (store, NULL);
    }
  GList *g = g_list_nth (store, measurenum);
  if (g)
    g->data = g_list_append (g->data, thetone);
  return store;
}


/*
 * enter_tone_in_store
 * enters the note FOUND as a tone in the tone store
 */
static void
enter_tone_in_store (DenemoProject * gui, notepitch * found, gint octave)
{
  gboolean nextmeasure;
  tone *thetone = (tone *) g_malloc0 (sizeof (tone));
  //g_debug("tone %p\n", thetone);
  thetone->enshift = found->spec.alteration;
  thetone->step = found->spec.step + 7 * octave;
  thetone->octave = octave;
  thetone->valid = TRUE;
#define store  (((DenemoStaff*)gui->movement->currentstaff->data)->tone_store)
  store = put_tone (store, gui->movement->currentmeasurenum - 1, thetone);
  nextmeasure = apply_tones (gui->movement);
  displayhelper (gui);
  if (Denemo.prefs.continuous && nextmeasure)
    {
      sound_click ();
      measureright (NULL, NULL);
    }
#undef store
}

/*
 * clear the references to tones (ie any overlay) in the currentstaff
 */
static void
clear_tone_nodes (DenemoProject * gui)
{
  DenemoMovement *si = gui->movement;
  DenemoStaff *curstaff = ((DenemoStaff *) si->currentstaff->data);
  measurenode *curmeasure;
  for (curmeasure = curstaff->themeasures; curmeasure; curmeasure = curmeasure->next)
    {
      objnode *curobj = ((DenemoMeasure*)curmeasure->data)->objects;
      for (; curobj; curobj = curobj->next)
        {
          DenemoObject *theobj = (DenemoObject *) curobj->data;
          if (theobj->type == CHORD)
            {
              ((chord *) theobj->object)->tone_node = NULL;
            }
        }
    }
}

static void
free_tones (GList * tones)
{
  if (tones)
    {
      g_list_foreach (tones, freeit, NULL);
      g_list_free (tones);
    }
}

  // clear gui->movement->currentstaff->data->tone_store and the references to it
static void
clear_tone_store (G_GNUC_UNUSED GtkButton * button, DenemoProject * gui)
{
#define store  (((DenemoStaff*)gui->movement->currentstaff->data)->tone_store)
  g_list_foreach (store, (GFunc) free_tones, NULL);
  clear_tone_nodes (gui);
  g_list_free (store);
  store = NULL;
#undef store
  displayhelper (gui);
  if (PR_gui)
    switch_back_to_main_window ();
}

void
clear_overlay (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam* param)
{
  DenemoProject *gui = Denemo.project;
  clear_tone_store (NULL, gui);
}

/*
 * clear the references to tones (ie any overlay) in the currentmeasure
 */
/* UNUSED
static void
clear_tone_nodes_currentmeasure (DenemoMovement * si)
{
  measurenode *curmeasure = si->currentmeasure;
  objnode *curobj = ((DenemoMeasure*)curmeasure->data)->objects;
  for (; curobj; curobj = curobj->next)
    {
      DenemoObject *theobj = (DenemoObject *) curobj->data;
      if (theobj->type == CHORD)
        {
          ((chord *) theobj->object)->tone_node = NULL;
        }
    }
}
*/


gboolean
delete_tone (DenemoMovement * si, chord * thechord)
{
  GList *tone_node = thechord->tone_node;
  if (tone_node)
    {
      ((tone *) tone_node->data)->valid = FALSE;
      objnode *keep = si->currentobject;
      gint keepx = si->cursor_x;
      gboolean keepa = si->cursor_appending;
      apply_tones (si);
      /*restore the position of the cursor before the delete of the tone */
      si->currentobject = keep;
      si->cursor_x = keepx;
      si->cursor_appending = keepa;
      calcmarkboundaries (si);

      displayhelper (Denemo.project);
      score_status(Denemo.project, TRUE);
      return TRUE;
    }
  else
    {
      return FALSE;
    }

}

/* return note for the passed pitch, or NULL if not a good note;
 * @param pitch, the pitch being enquired about
 * @temperament, the temperament to use to determine which pitch.
 * @param which_octave returns the number of the octave above/below mid_c
FIXME there is a bug when the enharmonic changes take b# into the wrong octave
*/
static notepitch *
determine_note (gdouble pitch, temperament * t, gint * which_octave, double *deviation)
{
  gint i;
  gint octave = 0;
  while (pitch > t->notepitches[11].pitch * (1.0231) /* quartertone */ )
    {
      //printf("pitch going down %f pitch %f\n", pitch, t->notepitches[11].pitch*(1.0231) );
      pitch /= 2;
      octave++;
    }
  while (pitch < t->notepitches[0].pitch * (0.977) /* quartertone */ )
    {
      //printf("pitch going up %f pitch\n", pitch);
      pitch *= 2;
      octave--;
    }
  for (i = 0; i < 12; i++)
    {
      //printf("considering %d %f\n", pitch, t->notepitches[i].pitch);
      if ((pitch > t->notepitches[i].pitch * (0.977)) && (pitch <= t->notepitches[i].pitch * (1.0231)))
        {
          *which_octave = octave;
          //printf("found %d octave \n", octave);
          *deviation = (pitch - t->notepitches[i].pitch) / (t->notepitches[i].pitch * (pow (2, 1.0 / 12.0) - 1.0) / 100.0);
          return &t->notepitches[i];
        }
    }

  return NULL;
}




static void
display_pitch (double note)
{
  gint octave;
  double deviation;
  temperament *t = (temperament *) PR_temperament;
  if (!GTK_IS_WINDOW (PR_window))
    return;
  notepitch *found = determine_note (note, t, &octave, &deviation);
  if (found)
    {
      int i;
      gchar *octstr = g_strdup ("");
      for (i = 0; i < octave + 1; i++)
        {
          gchar *str = g_strdup_printf ("%s%c", octstr, '\'');
          g_free (octstr);
          octstr = str;
        }
      for (i = 0; i > octave + 1; i--)
        {
          gchar *str = g_strdup_printf ("%s%c", octstr, ',');
          g_free (octstr);
          octstr = str;
        }
      pr_display_note (g_strdup_printf ("%c%s%s", step_name (found->spec.step), alteration_name (found->spec.alteration), octstr));
      g_free (octstr);
      pr_display_pitch_deviation (deviation);
      // FIXME if tuning display a graphic for how far note is from target_note
      if (PR_tuning)
        {
          PR_accurate_pitch = note;
          //gtk_widget_draw(PR_indicator, NULL);
          gtk_widget_queue_draw (PR_indicator);
          gtk_widget_queue_draw (PR_indicator);
        }
    }
  //fprintf(stderr, "Pitch is %0.2f\t", note);
}

static gint
measure_pitch_accurately (G_GNUC_UNUSED gpointer data)
{
  double note = determine_frequency ();
  //g_debug("returned %f\n", note);
  if (note > 0.0)
    display_pitch (note);

  return TRUE;
}


static float
Freq2Pitch (float freq)
{
  return 0.5 + (69.0 + 12.0 * (log (freq / 440.0) / log (2.0)));
}


#ifdef DISABLE_AUBIO
#else
/* look for a new note played into audio input, if
   present insert it into the score/store */
gint
pitchentry (DenemoProject * gui)
{
  static gint last_step = -1, last_alteration, last_octave;
  if (PR_window == NULL)
    return FALSE;               /* stops the timer */
  double deviation;
  temperament *t = (temperament *) PR_temperament;
  gint octave;
  gdouble note;
  note = get_pitch ();
  note *= transposition_required;
  if ((note < highest_pitch) && (note > lowest_pitch))
    {
      //printf("Got a note %2.1f\n", note);
      notepitch *found = determine_note (note, t, &octave, &deviation);
      if (found)
        {
          /* if tuning */
          if (PR_tuning)
            {
              setTuningTarget (note);
              PR_target_pitch = found->pitch * (pow (2, (octave)));
            }
          if (!repeated_notes_allowed)
            if (found->spec.step == last_step && found->spec.alteration == last_alteration && (octave == last_octave || octave == last_octave - 1) /* sometimes it jumps down an octave */ )
              {
                last_step = -1;
                printf ("Ignoring repeated note\n");
                return TRUE;
              }

          last_step = found->spec.step;
          last_alteration = found->spec.alteration;
          last_octave = octave;

          if (!PR_enable)
            {
              //printf("Enter the score area to insert notes!");
              return TRUE;
            }

          // Enter the note in the score
          if (!PR_tuning)
            {
              display_pitch (note);
              if (gui->input_source == INPUTMIDI)
                {
                  gint key = (gint) (Freq2Pitch (found->pitch * (pow (2, (octave)))));
                  //g_debug("pitch %f key number %d\n",found->pitch, key);

                  DenemoStaff *curstaffstruct = ((DenemoStaff *) Denemo.project->movement->currentstaff->data);
                  play_note (DEFAULT_BACKEND, curstaffstruct->midi_port, curstaffstruct->midi_channel, key, 300 /*duration */ , 0);
                }
              if (gui->input_source == INPUTMIDI || !Denemo.prefs.overlays)
                {
                  enter_note_in_score (gui, found, octave);
                  if (gui->mode & INPUTRHYTHM)
                    {
                      static gboolean beep = FALSE;
                      gint measure = gui->movement->currentmeasurenum;
                      scheme_next_note (NULL);
                      if (measure != gui->movement->currentmeasurenum)
                        beep = TRUE;
                      else if (beep)
                        signal_measure_end (), beep = FALSE;
                    }
                }
              else
                enter_tone_in_store (gui, found, octave);
            }

        }                       //note found
    }                           // acceptable range
  return TRUE;
}




  // toggle continuous advance to next measure or not
static void
toggle_continuous (G_GNUC_UNUSED GtkButton * button, G_GNUC_UNUSED gpointer data)
{
  Denemo.prefs.continuous = !Denemo.prefs.continuous;
  switch_back_to_main_window ();
}

static void
change_silence (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  double silence = gtk_spin_button_get_value (widget);
  set_silence (silence);
  switch_back_to_main_window ();
}

static void
change_threshold (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  double t = gtk_spin_button_get_value (widget);
  set_threshold (t);
  switch_back_to_main_window ();
}

static void
change_smoothing (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  double m = gtk_spin_button_get_value (widget);
  set_smoothing (m);
  switch_back_to_main_window ();
}

static void
change_onset_detection (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  guint m = gtk_spin_button_get_value (widget);
  set_onset_type (m);
  switch_back_to_main_window ();
}


static void
change_lowest_pitch (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  lowest_pitch = gtk_spin_button_get_value (widget);
  switch_back_to_main_window ();
}

static void
change_highest_pitch (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  highest_pitch = gtk_spin_button_get_value (widget);
  switch_back_to_main_window ();
}

static void
change_greatest_interval (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  greatest_interval = gtk_spin_button_get_value_as_int (widget);
  switch_back_to_main_window ();
}

static void
change_transposition (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  gdouble power = gtk_spin_button_get_value_as_int (widget);
  transposition_required = pow (2.0, power);
  switch_back_to_main_window ();
//printf("transposing = %f 2^1/12=%f\n", transposition_required, pow(2,1.0/12.0));
}


static void
frequency_smoothing (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  double m = gtk_spin_button_get_value (widget);
  set_frequency_smoothing (m);
}

#endif

/* stop_pitch_input
   if not midi stop audio and aubio
 */
int
stop_pitch_input (void)
{
#ifdef DISABLE_AUBIO
return -1;
#else
  DenemoProject *gui = Denemo.project;
  if (PR_timer)
    g_source_remove (PR_timer);

  if (PR_enter)
    g_signal_handler_disconnect (Denemo.scorearea, PR_enter);
  if (PR_leave)
    g_signal_handler_disconnect (Denemo.scorearea, PR_leave);
  PR_timer = PR_enter = PR_leave = 0;

  if (gui->input_source == INPUTAUDIO)
    terminate_pitch_recognition ();
//   else
//     stop_midi_input();
  clear_tone_store (NULL, gui);
  if (GTK_IS_WINDOW (PR_window))
    {
      GtkWidget *temp = PR_window;
      PR_window = NULL, gtk_widget_destroy (temp);
    }
  PR_gui = NULL;

  return 0;
#endif
}



static gboolean
window_destroy_callback (void)
{
  if (PR_gui == NULL)
    return FALSE;
  activate_action ("KeyboardOnly");//FIXME will not set radio buttons
  PR_window = NULL;
  clear_tone_store (NULL, Denemo.project);
  return FALSE;
}

static gboolean
stop_tuning_callback ()
{
  PR_tuning = FALSE;
  collect_data_for_tuning (PR_tuning);
  PR_indicator = NULL;
  return FALSE;
}




static void
toggle_repeated_notes_allowed ()
{
  repeated_notes_allowed = !repeated_notes_allowed;
  switch_back_to_main_window ();
}



//eventually make this value control the loudness of a click
static void
change_click_volume (GtkSpinButton * widget)
{
  PR_click = (guint) gtk_spin_button_get_value (widget);
  switch_back_to_main_window ();
}


static void
change_timer_rate (GtkSpinButton * widget, G_GNUC_UNUSED gpointer data)
{
  PR_time = (guint) gtk_spin_button_get_value (widget);
  start_pitch_input ();         //FIXME do not call the whole of start_pitch_recognition, just the timer setting bit???
  switch_back_to_main_window ();
}

  // toggle Denemo.prefs.overlays to show where the notes detected should go
static void
toggle_insert (G_GNUC_UNUSED GtkButton * button)
{
  Denemo.prefs.overlays = !Denemo.prefs.overlays;
  clear_tone_store (NULL, Denemo.project);
  switch_back_to_main_window ();
}

static gint
draw_indicator (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  int barwidth = 20;
  int centre = 400;
  double cent = log10f (PR_accurate_pitch / PR_target_pitch) * 1200 / log10f (2);
  int iCent = lround ((2 * cent + 100) * 4);    // causes warning if not stdc=-c99, whose problem is this?
  //value 400 is perfect, 410 = 5/2 cents sharp
  if (iCent < 0)
    iCent = 0;
  if (iCent > 800)
    iCent = 800;

  //cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

  if (iCent < 380)
    cairo_set_source_rgb (cr, 1, 0, 0);
  else if (iCent > 420)
    cairo_set_source_rgb (cr, 0, 0, 1);
  else
    cairo_set_source_rgb (cr, 0, 1, 0);
  cairo_rectangle (cr, iCent - barwidth / 2, 0, barwidth, 320);
  cairo_fill (cr);
  cairo_set_source_rgb (cr, 0, 0, 0);
  cairo_rectangle (cr, centre - barwidth / 8, 0, barwidth / 4, 320);
  cairo_fill (cr);

  return TRUE;
}

static void
toggle_tuning (GtkToggleButton * button, DenemoProject * gui)
{
  static int id;
  static GtkWidget *widget;
  PR_tuning = gtk_toggle_button_get_active (button);
  collect_data_for_tuning (PR_tuning);
  if (PR_tuning)
    {

      if (PR_indicator == NULL)
        {
          widget = gtk_window_new (GTK_WINDOW_TOPLEVEL);
          gtk_window_set_title (GTK_WINDOW (widget), "Tuning Indicator");
          gtk_window_set_default_size (GTK_WINDOW (widget), 800, 100);
          g_signal_connect (G_OBJECT (widget), "destroy", G_CALLBACK (stop_tuning_callback), NULL);
          GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
          gtk_container_add (GTK_CONTAINER (widget), hbox);
          PR_indicator = gtk_drawing_area_new ();
          gtk_box_pack_start (GTK_BOX (hbox), PR_indicator, TRUE, TRUE, 0);
#if GTK_MAJOR_VERSION==3
            g_signal_connect (G_OBJECT (PR_indicator), "draw", G_CALLBACK (draw_indicator), NULL);
#else
            g_signal_connect (G_OBJECT (PR_indicator), "expose_event", G_CALLBACK (draw_indicator), NULL);
#endif

          gtk_widget_show_all (widget);
        }
      gtk_window_present (GTK_WINDOW (widget));
      id = g_timeout_add (200, (GSourceFunc) measure_pitch_accurately, NULL);
    }
  else if (id)
    {
      if (PR_indicator)
        gtk_widget_hide (widget);
      g_source_remove (id);
      id = 0;
    }
}
#endif

/* return an array of values representing deviations from equal temperament for 12 notes from C for the passed temperament. Returned value is read only */
static gdouble *
get_cents (temperament * t)
{
  static gdouble array[12];
  int i, j;
  for (i = 0; i < 12; i++)
    {
      j = (i + temperament_offset) % 12;
      //g_debug("cents tempered %d to %d unshifted %f shifted %f\n", i, j, 1200 * log2(t->notepitches[i].pitch/Equal.notepitches[i].pitch), 1200 * log2(t->notepitches[j].pitch/Equal.notepitches[j].pitch));
      array[i] = 1200 * log2 (t->notepitches[j].pitch / Equal.notepitches[j].pitch);
    }
  return array;
}

/* return a string off offsets from 64 representing cents deviation for current temperment
 caller must free*/
gchar *
get_cents_string (void)
{
  gdouble *values = get_cents (PR_temperament);
  return g_strdup_printf (" %d %d %d %d %d %d %d %d %d %d %d %d ", (gint) (64 + values[0]) & 0x7f, (gint) (64 + values[1]) & 0x7f, (gint) (64 + values[2]) & 0x7f, (gint) (64 + values[3]) & 0x7f, (gint) (64 + values[4]) & 0x7f, (gint) (64 + values[5]) & 0x7f, (gint) (64 + values[6]) & 0x7f, (gint) (64 + values[7]) & 0x7f, (gint) (64 + values[8]) & 0x7f, (gint) (64 + values[9]) & 0x7f, (gint) (64 + values[10]) & 0x7f, (gint) (64 + values[11]) & 0x7f);
}

gchar *
get_sharpest (void)
{
  return nameof (sharp_degree);
}

gchar *
get_flattest (void)
{
  return nameof (flat_degree);
}

gchar *
get_temperament_name (void)
{
  return g_strdup (PR_temperament->name);
}

void
set_tuning (void)
{
  if (strcmp (PR_temperament->name, QUARTER_COMMA_MEAN_TONE))
    change_tuning (get_cents (PR_temperament));
  else
    set_meantone_tuning (enharmonic_position);  //Really other temperaments could do this too...
}

#define COLUMN_NAME (0)
#define COLUMN_PTR (1)

static void
temperament_changed_callback (GtkComboBox * combobox, GtkListStore * list_store)
{
  GtkTreeIter iter;
  gtk_combo_box_get_active_iter (GTK_COMBO_BOX (combobox), &iter);
  gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter, COLUMN_PTR, &PR_temperament, -1);
  set_tuning ();                //note synth may not be attached...
  g_string_assign (Denemo.prefs.temperament, PR_temperament->name);
  switch_back_to_main_window ();
}

void
reset_temperament (void)
{
  change_tuning (get_cents (PR_temperament));
}

GtkWidget *
get_enharmonic_frame (void)
{
  static GtkWidget *frame;
  if (frame == NULL)
    {
      frame = gtk_frame_new ("Enharmonic selection");
      g_object_ref (frame);
      //gtk_container_add (GTK_CONTAINER (main_vbox), frame);
      GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), hbox);
      GtkWidget *label = gtk_label_new ("");
      PR_label = label;
      
      label = gtk_label_new ("");
      gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
      gchar *button_label = g_strdup_printf ("<span font_desc=\"%d\" foreground=\"red\">♭ ◄</span>", Denemo.prefs.fontsize); 
      gtk_label_set_markup (GTK_LABEL (label), button_label); 
      GtkWidget *button = gtk_button_new ();
      gtk_container_add (GTK_CONTAINER(button), label); 
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK(set_flatter), NULL);
      
      gchar *names = notenames (PR_temperament);
      //label = gtk_label_new ("");
      gtk_label_set_markup (GTK_LABEL (PR_label), names);
      g_free (names);
      gtk_box_pack_start (GTK_BOX (hbox), PR_label, FALSE, TRUE, 0);

      label = gtk_label_new ("");
      gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
      g_free (button_label);
      button_label = g_strdup_printf ("<span font_desc=\"%d\" foreground=\"blue\">► ♯</span>", Denemo.prefs.fontsize); 
      gtk_label_set_markup (GTK_LABEL (label), button_label);
      g_free (button_label);
      button = gtk_button_new ();
      gtk_container_add (GTK_CONTAINER(button), label); 
      gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
      g_signal_connect (button, "clicked", G_CALLBACK(set_sharper), NULL);
    }
  GtkWidget *cont = gtk_widget_get_parent (frame);
  if (cont)
    gtk_container_remove (GTK_CONTAINER (cont), frame);
  return frame;
}

GtkWidget *
get_temperament_combo (void)
{
  static GtkWidget *combobox;
  if (combobox == NULL)
    {


      GtkListStore *list_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_POINTER);
      GtkCellRenderer *renderer;
      combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (list_store));
      gtk_widget_set_tooltip_text (combobox, _("Set the musical temperament (tuning) to be used for playback."));
      g_object_ref (combobox);
      PR_temperament = &Equal;
      int i;
      for (i = 0; i < (gint) G_N_ELEMENTS (temperaments); i++)
        {
          GtkTreeIter iter;
          gtk_list_store_append (list_store, &iter);
          gtk_list_store_set (list_store, &iter, COLUMN_NAME, temperaments[i]->name, COLUMN_PTR, temperaments[i], -1);

          if ((i == 0) || (Denemo.prefs.temperament && !strcmp (Denemo.prefs.temperament->str, temperaments[i]->name)))
            {
              gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combobox), &iter);
              PR_temperament = temperaments[i];
            }
        }
      renderer = gtk_cell_renderer_text_new ();
      gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox), renderer, TRUE);
      gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox), renderer, "text", COLUMN_NAME);
      g_signal_connect (G_OBJECT (combobox), "changed", G_CALLBACK (temperament_changed_callback), list_store);
      //set_tuning();
    }
//  GtkWidget *cont = gtk_widget_get_parent(combobox);
  // if(cont)
  //   gtk_container_remove(GTK_CONTAINER(cont), combobox);
  return combobox;
}

#ifdef _HAVE_PORTAUDIO_

static void
create_pitch_recognition_window (DenemoProject * gui)
{
#ifdef DISABLE_AUBIO
return;
#else

  GtkWidget *hbox, *hbox2;
  GtkWidget *button;
  GtkWidget *frame;
  GtkWidget *label;
  GtkAdjustment *spinner_adj;
  GtkWidget *spinner;
  if (GTK_IS_WINDOW (PR_window))
    {
      g_warning ("unexpected call");
      return;
    }
  PR_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (GTK_WINDOW (PR_window), "Pitch Input Control");
  g_signal_connect (G_OBJECT (PR_window), "destroy", G_CALLBACK (window_destroy_callback), NULL);
  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 1);
  gtk_container_add (GTK_CONTAINER (PR_window), main_vbox);
  if (gui->input_source == INPUTAUDIO)
    {
      frame = gtk_frame_new ("Mode");
      gtk_container_add (GTK_CONTAINER (main_vbox), frame);
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), hbox);

      GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);
      GtkWidget *radio_button = gtk_radio_button_new_with_label (NULL, "Overlay Pitches");

      g_signal_connect (G_OBJECT (radio_button), "toggled", G_CALLBACK (toggle_insert), NULL);
      button = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (radio_button), "Insert Notes");

      //g_debug("Overlays %d\n", Denemo.prefs.overlays);
      if (Denemo.prefs.overlays)
        {
          //gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(button), FALSE);
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio_button), TRUE);
        }
      else
        {
          Denemo.prefs.overlays = !Denemo.prefs.overlays;
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);

          //gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(radio_button), FALSE);
        }
      //g_debug("Overlays after button setting %d\n", Denemo.prefs.overlays);
      gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);       /* no need for callback */

      gtk_box_pack_start (GTK_BOX (vbox), radio_button, TRUE, TRUE, 0);


      button = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (radio_button), "Tuning");
      g_signal_connect (G_OBJECT (button), "toggled", G_CALLBACK (toggle_tuning), gui);
      gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);


      frame = gtk_frame_new (_("Overlay Pitches"));
      gtk_container_add (GTK_CONTAINER (hbox), frame);

      GtkWidget *vbox2 = gtk_vbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), vbox2);

      hbox2 = gtk_hbox_new (FALSE, 1);

      gtk_box_pack_start (GTK_BOX (vbox2), hbox2, TRUE, TRUE, 0);

      button = gtk_button_new_with_label (_("Clear Overlay"));     //FIXME make this a proxy for the ClearOverlay action ??
      gtk_box_pack_start (GTK_BOX (hbox2), button, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (clear_tone_store), gui);
      hbox2 = gtk_hbox_new (FALSE, 1);

      gtk_box_pack_start (GTK_BOX (vbox2), hbox2, TRUE, TRUE, 0);
      button = gtk_check_button_new_with_label (_("Continuous"));
      gtk_box_pack_start (GTK_BOX (hbox2), button, TRUE, TRUE, 0);

      g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (toggle_continuous), NULL);
      if (Denemo.prefs.continuous)
        {
          Denemo.prefs.continuous = !Denemo.prefs.continuous;
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), !Denemo.prefs.continuous);
        }
      label = gtk_label_new (_("Click Volume"));
      gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
      PR_click = 1;
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new ((double) PR_click, 0.0, 1.0, 1.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 1.0, 1);
      gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_click_volume), NULL);

    }                           // if audio input allow use of overlay mechanism, MIDI can use MIDIAdvanceOnEdit filter.


  frame = get_enharmonic_frame ();
  if (!gtk_widget_get_parent (frame))
    gtk_container_add (GTK_CONTAINER (main_vbox), frame);






  frame = gtk_frame_new ("Detected note");
  gtk_container_add (GTK_CONTAINER (main_vbox), frame);
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (frame), hbox);

  PR_notelabel = gtk_label_new ("---");
  gtk_box_pack_start (GTK_BOX (hbox), PR_notelabel, TRUE, TRUE, 0);
  PR_deviation = gtk_label_new ("0.0");
  gtk_box_pack_start (GTK_BOX (hbox), PR_deviation, TRUE, TRUE, 0);



  if (gui->input_source == INPUTAUDIO)
    {
      /* spinners to select silence, threshold, smoothing */

      frame = gtk_frame_new (_("Pitch Recognition Parameters"));
      gtk_container_add (GTK_CONTAINER (main_vbox), frame);
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), hbox);

      hbox2 = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

      label = gtk_label_new (_("Silence"));
      gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (-90.0, -1000.0, 100.0, 10.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 100.0, 0);
      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_silence), NULL);

      hbox2 = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

      label = gtk_label_new (_("Threshold"));
      gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 0.01, 100.0, 0.1, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 0.5, 2);
      gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_threshold), NULL);

      hbox2 = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

      label = gtk_label_new (_("Smoothing"));
      gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (6.0, 0.0, 100.0, 1.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 0.5, 2);
      gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_smoothing), NULL);

      label = gtk_label_new (_("Onset"));
      gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (7.0, 0.0, 7.0, 1.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 0.5, 2);
      gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_onset_detection), NULL);


    }
  /* spinners to constrain the note values */


  frame = gtk_frame_new (_("Note validation criteria"));
  gtk_container_add (GTK_CONTAINER (main_vbox), frame);
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (frame), hbox);

  hbox2 = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

  label = gtk_label_new (_("Lowest Pitch"));
  gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
  spinner_adj = (GtkAdjustment *) gtk_adjustment_new (DEFAULT_LOW, 10.0, 2080.0, 10.0, 1.0, 1.0);
  spinner = gtk_spin_button_new (spinner_adj, 100.0, 1);
  gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_lowest_pitch), NULL);

  hbox2 = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

  label = gtk_label_new (_("Highest Pitch"));
  gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
  spinner_adj = (GtkAdjustment *) gtk_adjustment_new (DEFAULT_HIGH, 120.0, 9600.0, 10.0, 1.0, 1.0);
  spinner = gtk_spin_button_new (spinner_adj, 100.0, 1);
  gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_highest_pitch), NULL);

  hbox2 = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (hbox), hbox2, TRUE, TRUE, 0);

  label = gtk_label_new (_("Greatest Interval"));
  gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
  spinner_adj = (GtkAdjustment *) gtk_adjustment_new (8.0, 1.0, 15.0, 1.0, 1.0, 1.0);
  spinner = gtk_spin_button_new (spinner_adj, 1.0, 0);
  gtk_box_pack_start (GTK_BOX (hbox2), spinner, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_greatest_interval), NULL);



  /* options */
  if (gui->input_source == INPUTAUDIO)
    {
      frame = gtk_frame_new (_("Input handling"));
      gtk_container_add (GTK_CONTAINER (main_vbox), frame);
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), hbox);



      label = gtk_check_button_new_with_label (_("Disable repeated notes"));
      gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (label), "clicked", G_CALLBACK (toggle_repeated_notes_allowed), NULL);
      label = gtk_label_new ("Transpose Input");
      gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (0.0, -3.0, 3.0, 1.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 1.0, 0);
      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_transposition), NULL);

      label = gtk_label_new (_("Delay"));
      gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (DEFAULT_TIMER_RATE, 1.0, 500.0, 10.0, 1.0, 1.0);
      spinner = gtk_spin_button_new (spinner_adj, 10.0, 0);
      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (change_timer_rate), NULL);

      frame = gtk_frame_new (_("Frequency Measurement"));
      gtk_container_add (GTK_CONTAINER (main_vbox), frame);
      hbox = gtk_hbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (frame), hbox);

      label = gtk_label_new (_("Frequency smoothing"));
      gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
      spinner_adj = (GtkAdjustment *) gtk_adjustment_new (0.25, 0.1, 1.0, 0.05, 0.05, 0.05);
      spinner = gtk_spin_button_new (spinner_adj, 0.5, 2);
      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);
      g_signal_connect (G_OBJECT (spinner), "value-changed", G_CALLBACK (frequency_smoothing), NULL);


      frame = get_temperament_combo ();
      if (!gtk_widget_get_parent (frame))
        gtk_container_add (GTK_CONTAINER (hbox), frame);
    }
/* now show the window, but leave the main window with the focus */
  gtk_window_set_focus_on_map ((GtkWindow *) PR_window, FALSE);
  gtk_widget_show_all (PR_window);
  gtk_window_set_focus ((GtkWindow *) PR_window, NULL);
  gtk_widget_grab_focus (Denemo.scorearea);

  // FIXME make the visibility of the Frequency Measurement frame depend of the tuning toggle
  // sort out the size of the drawing area
  // sort out how to pass the deviation from in-tune to the draw_indicator function

#endif
}


gint
setup_pitch_input (void)
{
#ifdef DISABLE_AUBIO
return -1;
#else

  DenemoProject *gui = Denemo.project;
  if (GTK_IS_WINDOW (PR_window))
    {
      gtk_window_present (GTK_WINDOW (PR_window));
      return 0;
    }
  if (PR_temperament == NULL)
    PR_temperament = default_temperament ();
  if (gui->input_source == INPUTAUDIO ? (initialize_pitch_recognition () == 0) : /*(init_midi_input()==0) */ TRUE)
    {
      if (gui->input_source == INPUTAUDIO)
        {                       //FIXME these should be done at initialize_pitch_recognition time
          set_silence (-90.0);
          set_threshold (0.3);
          set_smoothing (6.0);
        }
      if (gui->input_source == INPUTMIDI)
        {
          PR_time = 5;
        }
    }
  else
    return -1;
  transposition_required = 1.0;
  lowest_pitch = DEFAULT_LOW;
  highest_pitch = DEFAULT_HIGH;
  repeated_notes_allowed = TRUE;
  create_pitch_recognition_window (gui);
  //read_PRkeymap(gui);
  return 0;

#endif
}


static void
scorearea_set_active (G_GNUC_UNUSED GtkWidget * widget, G_GNUC_UNUSED GdkEventCrossing * event, G_GNUC_UNUSED gpointer data)
{
  PR_enable = TRUE;
  //gtk_widget_draw(Denemo.scorearea, NULL);
  draw_score_area();
}

static void
scorearea_set_inactive (G_GNUC_UNUSED GtkWidget * widget, G_GNUC_UNUSED GdkEventCrossing * event, G_GNUC_UNUSED gpointer data)
{
  PR_enable = FALSE;
  //gtk_widget_draw(Denemo.scorearea, NULL);
  draw_score_area();
}

void
start_pitch_input (void)
{
#ifdef DISABLE_AUBIO
return;
#else
  DenemoProject *gui = Denemo.project;
  if (PR_timer)
    g_source_remove (PR_timer);

  PR_timer = g_timeout_add (PR_time, (GSourceFunc) pitchentry, Denemo.project);

  if (PR_timer == 0)
    g_error ("Timer id 0 - if valid the code needs re-writing (documentation not clear)");
  if (gui->input_source == INPUTAUDIO)
    {                           /* for input from microphone avoid accidental activation by insisting on pointer being in the score drawing area */
      gtk_widget_add_events (Denemo.scorearea, GDK_LEAVE_NOTIFY_MASK | GDK_ENTER_NOTIFY_MASK);
      PR_enter = g_signal_connect (G_OBJECT (Denemo.scorearea), "enter-notify-event", G_CALLBACK (scorearea_set_active), NULL);
      PR_leave = g_signal_connect (G_OBJECT (Denemo.scorearea), "leave-notify-event", G_CALLBACK (scorearea_set_inactive), NULL);
    }
  else
    PR_enable = TRUE;           /* for midi input you are unlikely to enter notes by accident */
  PR_gui = gui;
#endif
}

gboolean
pitch_recognition_system_active (void)
{
  return PR_window != NULL;
}

gboolean
pitch_entry_active (DenemoProject * gui)
{
  return (PR_gui == gui) && PR_enable;
}

#else
gboolean
pitch_entry_active (DenemoProject * gui)
{
  return 0;
}

gint
setup_pitch_input (void)
{
  return -1;
}

void
start_pitch_input (void)
{
}

int
stop_pitch_input (void)
{
  return 0;
}

void
clear_overlay (DenemoAction * action, DenemoScriptParam* param)
{
}
#endif
