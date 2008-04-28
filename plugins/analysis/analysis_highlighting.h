#ifndef ANALYSIS_HIGHTLIGHING_H
#define ANALYSIS_HIGHTLIGHING_H
#include <denemo/denemo.h> 
#include <denemo/denemo_version.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct AnalysisRes {
  GList *pm;
  GList *harmony;
}AnalysisRes, *AnalysisResPtr;

typedef struct Results {
  int staff;
  int start_bar;
  float start_beat;
  int end_bar;
  float end_beat;
  float similarity;
}Results , *resultsPtr;

typedef struct Chords {
  int bar;
  float length;
  GList *list;
  gchar *numeral;
  int digit;
  gchar *modtonality;
  gchar *modnumeral;
  int moddigit;
}Chords, *chordsPtr;

typedef struct chorddesc {
  gchar *root;
  gchar *type;
}ChordDesc, *chorddescPtr;

typedef struct rmnnumeral {
  gchar* numeral;
  int digit;
}rmnNumeral, *rmnNumeralPtr;

#endif
