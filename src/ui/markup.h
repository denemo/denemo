/* markup.h
 * dialog for getting markup from user
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2015  Richard Shann

 */
#ifndef MARKUP_H
#define MARKUP_H
//returns user's text and formatted lilypond markuo for that text
gboolean get_user_markup (GString *text, GString *lilypond, gchar* title, char *instruction, gchar *initial_value, gboolean not_modal, gboolean format_only);
gboolean run_preview (GtkWidget *textbuffer);
#endif
