#ifndef __PRINT_VIEW_H__
#define __PRINT_VIEW_H__

#include <denemo/denemo.h>
#include "export/print.h"
gboolean get_offset (gdouble * x, gdouble * y);
gboolean get_positions (gdouble * neary, gdouble * fary, WwGrob grob);
gboolean get_new_target (void);
gboolean get_reference_point (void);
gboolean get_control_point (gint which);
gboolean get_curve (gdouble * x1, gdouble * y1, gdouble * x2, gdouble * y2, gdouble * x3, gdouble * y3, gdouble * x4, gdouble * y4);

void present_print_view_window ();
void typeset_part (void);
gboolean continuous_typesetting (void);
void set_continuous_typesetting (gboolean setting);
gboolean get_new_point (void);
gboolean typeset_for_script (gchar * script);
gboolean print_typeset_pdf (void);
void typeset_current_layout (void);
void typeset_current_movement (void);
void implement_show_print_view (gboolean refresh_if_needed);
void install_printpreview (void);
void refresh_print_view (gboolean interactive);
void printview_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, gboolean print);
void print_from_print_view (gboolean all_movements);
gboolean printview_is_stale (void);
void unpause_continuous_typesetting (void);
void pause_continuous_typesetting (void);
void display_printhistory (void);
void delete_conditions (DenemoProject *gui);
void  set_condition (DenemoInclusionCriterion *condition);
void create_new_inclusion_criterion (gchar *name);
#endif
