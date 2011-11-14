//
// C++ Interface: scorewizard
//
// Description: 
//
//
// Author: Adam Tee <adam@ajtee.plus.com>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef SCOREWIZARD_H
#define SCOREWIZARD_H

#include <denemo/denemo.h>
#include "keysigdialog.h"

/**
 * Paper setup callback data
 * 
 */
typedef struct headersetupcb
{
	GtkWidget *title;
	GtkWidget *subtitle;
	GtkWidget *poet;
	GtkWidget *composer;
	GtkWidget *meter;
	GtkWidget *opus;
	GtkWidget *arranger;
	GtkWidget *instrument;
	GtkWidget *dedication;
	GtkWidget *piece;
	GtkWidget *head;
	GtkWidget *copyright;
	GtkWidget *footer;
	GtkWidget *tagline;

}headersetupcb;


typedef struct papersetupcb
{
	GtkWidget *papersize;
	GtkWidget *lilyversion;
	GtkWidget *portrait;
	GtkWidget *fontsize;
	GtkWidget *lilypond;
}papersetupcb;


typedef struct timekeysigcb
{
	GtkWidget *tempo;
	GtkWidget *numerator;
	GtkWidget *denominator;
	GtkWidget *keysig;
	keysig_data *keysig_widgets;	
}timekeysigcb;



/**
 * Instrument setup callbackdata
 *
 */
typedef struct instsdata
{
	GList *instruments;
	GtkListStore *list_store;
}instsdata;


/**
 * Score setup callback data
 */
typedef struct scoresetup
{
  DenemoScore *si;
  papersetupcb *paper;
  instsdata *insts;
}scoresetup;

typedef struct scoredata
{
	GtkListStore **list_store;
	GtkTreeSelection *selection;
	GtkListStore *score_list;
}scoredata;

typedef struct wizarddata
{
	DenemoGUI *gui;
	GtkListStore *list_store;
	GList *instruments;
	GtkTreeIter iter;
	GtkWidget *notebook;
	GList *score_instruments;
	scoredata *sdata;
	papersetupcb *paper;
	timekeysigcb *tsetup;
	instsdata *cbdata;
	instsdata *icbdata;
	headersetupcb *hsetup;
	gint staffnumber;
	gint currentstaffnumber;
	/* gui */
	GtkWidget *score_list;
	GtkWidget *instrument_list;
	GtkWidget *nextbutton;
	GtkWidget *backbutton;	
	GtkWidget *finishbutton;
}wizarddata;




void scorewizard(GtkAction *action, gpointer param);
void setpaperconfig(papersetupcb *cbdata, DenemoGUI *gui);
void applyheader_settings(headersetupcb *hsetup, DenemoGUI *gui);
papersetupcb *papersetup(GtkWidget *notebook, DenemoGUI *gui, gboolean isnotebook); /* isnotebook should defaults to TRUE */
headersetupcb *headersetup (GtkWidget *notebook, DenemoGUI *gui, gboolean isnotebook);
#endif
