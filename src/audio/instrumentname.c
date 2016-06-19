/****************************************************************
 *
 * instrumentname.cpp
 *
 * Functions for identifying instrument names
 *
 * (c) 2001-2005 Per Andersson
 *
 * License: GPL version 3 or later
 *
 */

#define INSTRUMENT_DB_VERSION	1.1

/*
 * What? The function select_program() tries to find an instrument name
 * that matches the input string as closely as possible.
 *
 * How? this time with a pre processor to make it even more flexible.
 *
 * Why? I can't remember if the good guitar sound is called "Acoustic guitar
 *  (nylon)" or "Nylon acoustic" or "Nylon guitar" or ...
 *
 * See www.wotsit.org for some info on midi and the standard file format
 *
 *
 * 	Per
 *
 ****************************************************************
 *
 *  Per Andersson
 *  Artifex consulting
 *
 *  email to: artifex@europe.com
 *
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <glib/gprintf.h>
#include "ctype.h"

/****************************************************************/

/**
 * synonyms data base
 */

char *synonym_table[] = {
  "piano", "pno", "pi", "",
  "guitar", "guit", "gui", "gtr", "gt", "",
  "electric", "electr", "elect", "ele", "el", "",
  "distorted", "distort", "disted", "dist", "dis", "" "synthetic", "synth", "synt", "syn", "sy", "",
  "overdriven", "overdrive", "ovrdrv", "ovrdr", "over", "",
  "oohs", "oos", "ohs", "oo", "o", "",
  "aahs", "aas", "ahs", "aa", "a", "",
  NULL,
};

/**
 * instrument name recognition data base
 *
 * handles some common instruments (not yet complete)
 */

typedef struct instrname
{
  char *text;                   /* word to match */
  int gsprogram;                /* timidity program number */
  int matchpoints;              /* score to compare */
}
instrname_t;

/**
 * Midi instruments
 *
 */
instrname_t keywords[] = {
  {"piano", 1, 10},
  {"grand", 1, 8},
  {"acoustic", 1, 5},

  {"piano", 2, 10},
  {"bright", 2, 5},
  {"brite", 2, 5},
  {"acoustic", 2, 5},

  {"piano", 3, 10},
  {"grand", 3, 8},
  {"electric", 3, 5},

  {"piano", 4, 10},
  {"honkytonk", 4, 8},

  {"honky", 4, 4},
  {"tonk", 4, 4},

  {"piano", 5, 10},
  {"electric", 5, 5},

  {"piano", 6, 10},
  {"electric", 6, 5},

  {"harpsichord", 7, 10},

  {"clav", 8, 10},

  {"celesta", 9, 10},

  {"glockenspiel", 10, 10},

  {"music box", 11, 10},

  {"vibraphone", 12, 10},

  {"marimba", 13, 10},

  {"xylophone", 14, 10},

  {"tubular bells", 15, 10},

  {"dulcimer", 16, 10},

  {"organ", 17, 10},
  {"drawbar", 17, 5},

  {"organ", 18, 10},
  {"percussive", 18, 5},

  {"organ", 19, 10},
  {"rock", 19, 5},

  {"organ", 20, 10},
  {"church", 20, 5},
  {"pipe", 20, 5},

  {"organ", 21, 10},
  {"reed", 21, 5},

  {"accordion", 22, 10},

  {"concertina", 23, 10},

  {"guitar", 25, 11},
  {"acoustic", 25, 8},
  {"nylon", 25, 8},

  {"guitar", 26, 10},
  {"acoustic", 26, 8},
  {"steel", 26, 8},

  {"guitar", 27, 10},
  {"electric", 27, 8},
  {"jazz", 27, 10},

  {"guitar", 28, 10},
  {"electric", 28, 8},
  {"clean", 28, 10},

  {"guitar", 29, 10},
  {"electric", 29, 8},
  {"muted", 29, 8},

  {"guitar", 30, 10},
  {"electric", 30, 5},
  {"overdriven", 30, 8},

  {"guitar", 31, 10},
  {"electric", 31, 5},
  {"distorted", 31, 8},

  {"guitar", 32, 10},
  {"electric", 32, 5},
  {"harmonics", 32, 8},

  {"bass", 33, 10},
  {"acoustic", 33, 8},

  {"bass", 34, 10},
  {"electric", 34, 8},
  {"finger", 34, 8},

  {"bass", 35, 10},
  {"electric", 35, 8},
  {"pick", 36, 8},

  {"bass", 36, 10},
  {"electric", 36, 8},
  {"fretless", 36, 8},

  {"bass", 37, 10},
  {"slap", 37, 8},

  {"bass", 39, 10},
  {"synth", 39, 5},

  {"violin", 41, 10},
  {"fiddle", 41, 10},

  {"viola", 42, 10},
  {"bratsch", 42, 10},

  {"cello", 43, 10},
  {"violoncello", 43, 10},

  {"contrabass", 44, 10},
  {"contra", 44, 8},
  {"bass", 44, 5},
  {"bowed", 44, 8},

  {"strings", 45, 10},
  {"tremolo", 45, 8},

  {"strings", 46, 10},
  {"pizzicato", 46, 10},

  {"strings", 47, 10},
  {"orchestral", 47, 10},

  {"timpani", 48, 10},

  {"string", 49, 10},
  {"ensemble", 49, 8},

  {"synthstrings", 51, 10},
  {"synth", 51, 5},
  {"strings", 51, 10},

  {"choir", 53, 10},
  {"aahs", 53, 10},

  {"voice", 54, 10},
  {"oohs", 54, 10},

  {"synth", 55, 5},
  {"voice", 55, 10},

  {"orchestra", 56, 10},
  {"hit", 56, 5},

  {"trumpet", 57, 10},

  {"trombone", 58, 10},

  {"tuba", 59, 10},

  {"trumpet", 60, 9},
  {"muted", 60, 5},

  {"french", 61, 8},
  {"horn", 61, 8},

  {"brass", 62, 8},
  {"section", 62, 8},

  {"brass", 63, 10},
  {"synth", 63, 5},

  {"sax", 65, 10},
  {"soprano", 65, 8},

  {"sax", 66, 10},
  {"alto", 66, 8},

  {"sax", 67, 10},
  {"tenor", 67, 8},

  {"sax", 68, 10},
  {"baritone", 68, 8},

  {"oboe", 69, 10},

  {"english", 70, 9},
  {"horn", 70, 9},

  {"bassoon", 71, 10},

  {"clarinet", 72, 10},

  {"piccolo", 73, 10},
  {"flute", 73, 3},

  {"flute", 74, 10},
  {"traverso", 74, 10},

  {"recorder", 75, 10},

  {"pan", 76, 8},
  {"flute", 76, 8},

  {"blown", 77, 5},
  {"bottle", 77, 8},

  {"skakuhachi", 78, 10},

  {"whistle", 79, 10},

  {"ocarina", 80, 10},

  {"", 0, 0},

};

void
clear_score_table (int *score_table)
{
  int i;

  for (i = 0; i < 128; i++)
    {
      score_table[i] = 0;
    }
}

/**
 * scans the data base and count points for matching words
 */

void
match (instrname_t * keywords, char *name, int *score_table)
{
  int i;
  int instr;

  /* scan all entries */
  for (i = 0; keywords[i].text[0]; i++)
    {
      if (!strcmp (keywords[i].text, name))
        {
          instr = keywords[i].gsprogram;
          score_table[instr] += keywords[i].matchpoints;
        }
    }
}

/**
 * replace a word by it's canonical form
 */

char *
synonym (char **tab, char *txt)
{
  char *saved;

  saved = NULL;

  for (; *tab; tab++)
    {
      if (!saved)
        {
          saved = *tab;
        }
      if (!strcmp (txt, *tab))
        {
          return saved;
        }
      if (**tab == 0)
        {
          saved = NULL;
        }
    }

  return txt;
}

/**
 * convert a (potentially multi-word) instrument name to a
 * midi program number 0 .. 127
 *
 * I hope this is fairly GS compatible!
 */

int
select_program (char *instr)
{
  char name[100];
  char *namepointer;
  int score[128];
  int i;
  int winner = 1;
  char *input_instr = instr;
  /* get a fresh start */
  clear_score_table (score);

  /* scan the input for words */
  while (*instr)
    {
      namepointer = name;
      while (isalpha (*instr))
        {
          /* copy a word */
          *namepointer++ = tolower (*instr++);
        }
      *namepointer = 0;
      if (name[0])
        {
          /* it was a word: match it */
          match (keywords, synonym (synonym_table, name), score);
        }
      if (*instr)
        {
          instr++;
        }
    }

  /* then select the best match */
  winner = 0;

  for (i = 0; i < 128; i++)
    {
      if (score[i] > score[winner])
        {
          winner = i;
        }
    }

  g_debug ("For %s choose prog %d\n", input_instr, winner & 127);
  return winner & 127;
}

/****************************************************************/
