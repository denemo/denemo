#include "timesig.h"

/**
 * Create a new timesignature object
 * @param time1 nominator of time signature
 * @param time2 denominator of the time signature
 * @return the timesignature
 */
DenemoObject *
dnm_newtimesigobj (gint time1, gint time2)
{
  DenemoObject *ret;
  timesig *newtimesig = (timesig *) g_malloc0 (sizeof (timesig));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = TIMESIG;
  newtimesig->time1 = time1;
  newtimesig->time2 = time2;
  ret->object = newtimesig;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Wrapper function to create new 4/4 time sig and insert into the score
 */
void
newtimesig44 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (4, 4));
}

/**
 * Wrapper function to create new 2/4 time sig and insert into the score
 */
void
newtimesig24 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (2, 4));
}

/**
 * Wrapper function to create new 3/4 time sig and insert into the score
 */
void
newtimesig34 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (3, 4));
}

/**
 * Wrapper function to create new 6/4 time sig and insert into the score
 */
void
newtimesig64 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (6, 4));
}

/**
 * Wrapper function to create new 5/4 time sig and insert into the score
 */
void
newtimesig54 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (5, 4));
}

/**
 * Wrapper function to create new 3/8 time sig and insert into the score
 */
void
newtimesig38 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (3, 8));
}

/**
 * Wrapper function to create new 6/8 time sig and insert into the score
 */
void
newtimesig68 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (6, 8));
}

/**
 * Wrapper function to create new 9/8 time sig and insert into the score
 */
void
newtimesig98 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (9, 8));
}

/**
 * Wrapper function to create new 12/8 time sig and insert into the score
 */
void
newtimesig128 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (12, 8));
}

/**
 * Wrapper function to create new 2/2 time sig and insert into the score
 */
void
newtimesig22 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (2, 2));
}

/**
 * Wrapper function to create new 3/2 time sig and insert into the score
 */
void
newtimesig32 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (3, 2));
}

/**
 * Wrapper function to create new 4/2 time sig and insert into the score
 */
void
newtimesig42 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newtimesigobj (4, 2));
}


void
settimesig22 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 2, 2, TRUE);
}

void
settimesig42 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 4, 2, TRUE);
}

void
settimesig32 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 3, 2, TRUE);
}

void
settimesig44 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 4, 4, TRUE);
}

void
settimesig54 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 5, 4, TRUE);
}

void
settimesig24 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 2, 4, TRUE);
}

void
settimesig34 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 3, 4, TRUE);
}

void
settimesig68 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 6, 8, TRUE);
}

void
settimesig128 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 12, 8, TRUE);
}

void
settimesig38 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 3, 8, TRUE);
}

void
settimesig98 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 9, 8, TRUE);
}

void
settimesig64 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.project->movement, curstaff, 6, 4, TRUE);
}
