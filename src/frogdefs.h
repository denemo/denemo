/* frogdefs.h */

/* Definitions pertaining to the frog i/o language */
/* (c) 1999, 2000, 2001 Adam Tee, Matthew Hiller */

#ifndef FROGDEFS_H
#define FROGDEFS_H
#include <stdio.h>
#include <ctype.h>
#include <denemo/denemo.h> 
#define FROG_MAX_TOKEN_STRING  20

struct p_position
{
  float xoff;
  float yoff;
};

struct p_beam_type
{
  char btype[10];
  char direction[5];
};

struct p_beam
{
  struct p_beam_type type;
  char direction[5];
};

struct p_pitch
{
  char notename[3];
  int octave;
  int number_of_lines;
};

struct p_gracenoteoption
{
  char type[FROG_MAX_TOKEN_STRING];
  int dots;
  struct p_position t_position;
};

struct p_grace
{
  char stemdir[5];
  struct p_gracenoteoption option;
  struct p_beam t_beamd;
  struct p_pitch t_pitch;
  struct p_position t_position;
  float duration;
};

struct p_modifier
{
  char type[FROG_MAX_TOKEN_STRING];
  struct p_grace gracenote;
  int dots;
  struct p_position t_position;
};

typedef struct p_noteoption
{
  char type[FROG_MAX_TOKEN_STRING];
  struct p_modifier t_modifier;
  struct p_noteoption *next;
}optionlist;

struct p_note
{
  char stemdir[5];
  struct p_noteoption *t_option;
  struct p_beam t_beamd;
  struct p_pitch t_pitch;
  struct p_position t_position;
  float duration;
};


struct p_staff
{
  int numoflines;
  int positioninhalflines;
  int transposition;
  char name[FROG_MAX_TOKEN_STRING];
};

struct p_rest
{
  int dots;
  char rest;
};


struct p_tupops
{
  int numerator;
  int denominator;
};

struct p_slur 
{
  char string[15];
  struct p_position t_position;
  char curve[15];
};

struct p_hairpin
{
  char string[15];
};


int froginput (char *filename, DenemoGUI *gui);


#endif
