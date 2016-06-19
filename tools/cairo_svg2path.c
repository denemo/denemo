/*
 * cairo_svg2path.c
 *
 * Copyright (C) 2012 - Juan pablo Ugarte
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#define GETTEXT_PACKAGE "cairo-svg2path"

#include <glib.h>
#include <math.h>
#include <string.h>

typedef struct
{
  const gchar *id;
  gchar *path;
  gdouble width, height;
} ParseData;

static void
start_element (GMarkupParseContext * context, const gchar * element_name, const gchar ** attribute_names, const gchar ** attribute_values, gpointer user_data, GError ** error)
{
  ParseData *state = user_data;

  if (strcmp (element_name, "svg") == 0)
    {
      gint i;

      for (i = 0; attribute_names[i]; i++)
        {
          if (!g_strcmp0 (attribute_names[i], "width"))
            state->width = g_strtod (attribute_values[i], NULL);
          else if (!g_strcmp0 (attribute_names[i], "height"))
            state->height = g_strtod (attribute_values[i], NULL);
        }
    }
  else if (strcmp (element_name, "path") == 0)
    {
      const gchar *id, *path;
      gint i;

      for (i = 0; attribute_names[i]; i++)
        {
          if (!g_strcmp0 (attribute_names[i], "d"))
            path = attribute_values[i];
          else if (!g_strcmp0 (attribute_names[i], "id"))
            id = attribute_values[i];
        }

      if ((state->id && !g_strcmp0 (id, state->id)) || state->path == NULL)
        state->path = g_strdup (path);

      return;
    }
}

gchar *
parse_svg_file (const gchar * filename, const gchar * path, gdouble * width, gdouble * height, GError ** error)
{
  GMarkupParser parser = { start_element };
  ParseData state = { path, NULL };
  GMarkupParseContext *context;
  gchar *contents, *retval;
  gsize size;

  if (!g_file_get_contents (filename, &contents, &size, error))
    return NULL;

  context = g_markup_parse_context_new (&parser, G_MARKUP_TREAT_CDATA_AS_TEXT | G_MARKUP_PREFIX_ERROR_POSITION, &state, NULL);

  if (g_markup_parse_context_parse (context, contents, size, error) && g_markup_parse_context_end_parse (context, error))
    {
      retval = state.path;
      if (width)
        *width = state.width;
      if (height)
        *height = state.height;
    }
  else
    {
      g_free (state.path);
      retval = NULL;
    }

  g_markup_parse_context_free (context);
  g_free (contents);

  return retval;
}

/*
 * These two data structures are taken from cairo
 */
typedef enum
{
  CAIRO_PATH_MOVE_TO,
  CAIRO_PATH_LINE_TO,
  CAIRO_PATH_CURVE_TO,
  CAIRO_PATH_CLOSE_PATH
} cairo_path_data_type_t;

typedef union
{
  struct
  {
    cairo_path_data_type_t type;
    int length;
  } header;
  struct
  {
    double x, y;
  } point;
} cairo_path_data_t;

#define N_ELEMENTS 1024

/**********************************************************/
/*  Below is the code that parses the actual path data.   */
/*                                                        */
/*  This code is taken from librsvg and was originally    */
/*  written by Raph Levien <raph@artofcode.com> for Gill. */
/**********************************************************/

typedef struct
{
  cairo_path_data_t *path;
  gint size;
  gint alloc_size;
  gdouble cpx, cpy;             /* current point                               */
  gdouble rpx, rpy;             /* reflection point (for 's' and 't' commands) */
  gchar cmd;                    /* current command (lowercase)                 */
  gint param;                   /* number of parameters                        */
  gboolean rel;                 /* true if relative coords                     */
  gdouble params[7];            /* parameters that have been parsed            */
} ParsePathContext;

static inline void
ctx_realloc_path (ParsePathContext * ctx)
{
  if ((ctx->size + 1) >= ctx->alloc_size)
    {
      ctx->alloc_size += N_ELEMENTS;
      ctx->path = g_realloc_n (ctx->path, ctx->alloc_size, sizeof (cairo_path_data_t));
    }
}

static inline void
ctx_point_append (ParsePathContext * ctx, double x, double y)
{
  cairo_path_data_t *data = &ctx->path[ctx->size];

  data->point.x = x;
  data->point.y = y;
  ctx->size++;
  ctx_realloc_path (ctx);
}

static inline void
ctx_header_append (ParsePathContext * ctx, cairo_path_data_type_t type, int length, double x, double y)
{
  cairo_path_data_t *data = &ctx->path[ctx->size];

  data->header.type = type;
  data->header.length = length;
  ctx->size++;
  ctx_realloc_path (ctx);

  if (length > 1)
    ctx_point_append (ctx, x, y);
}

/* supply defaults for missing parameters, assuming relative coordinates
   are to be interpreted as x,y */
static void
parse_path_default_xy (ParsePathContext * ctx, gint n_params)
{
  gint i;

  if (ctx->rel)
    {
      for (i = ctx->param; i < n_params; i++)
        {
          if (i > 2)
            ctx->params[i] = ctx->params[i - 2];
          else if (i == 1)
            ctx->params[i] = ctx->cpy;
          else if (i == 0)
            /* we shouldn't get here (ctx->param > 0 as precondition) */
            ctx->params[i] = ctx->cpx;
        }
    }
  else
    {
      for (i = ctx->param; i < n_params; i++)
        ctx->params[i] = 0.0;
    }
}

static void
parse_path_do_cmd (ParsePathContext * ctx, gboolean final)
{
  switch (ctx->cmd)
    {
    case 'm':
      /* moveto */
      if (ctx->param == 2 || final)
        {
          parse_path_default_xy (ctx, 2);

          ctx->cpx = ctx->rpx = ctx->params[0];
          ctx->cpy = ctx->rpy = ctx->params[1];

          ctx_header_append (ctx, CAIRO_PATH_MOVE_TO, 2, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 'l':
      /* lineto */
      if (ctx->param == 2 || final)
        {
          parse_path_default_xy (ctx, 2);

          ctx->cpx = ctx->rpx = ctx->params[0];
          ctx->cpy = ctx->rpy = ctx->params[1];

          ctx_header_append (ctx, CAIRO_PATH_LINE_TO, 2, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 'c':
      /* curveto */
      if (ctx->param == 6 || final)
        {
          gdouble x, y;

          parse_path_default_xy (ctx, 6);

          x = ctx->params[0];
          y = ctx->params[1];
          ctx->rpx = ctx->params[2];
          ctx->rpy = ctx->params[3];
          ctx->cpx = ctx->params[4];
          ctx->cpy = ctx->params[5];

          ctx_header_append (ctx, CAIRO_PATH_CURVE_TO, 4, x, y);
          ctx_point_append (ctx, ctx->rpx, ctx->rpy);
          ctx_point_append (ctx, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 's':
      /* smooth curveto */
      if (ctx->param == 4 || final)
        {
          gdouble x, y;

          parse_path_default_xy (ctx, 4);

          x = 2 * ctx->cpx - ctx->rpx;
          y = 2 * ctx->cpy - ctx->rpy;
          ctx->rpx = ctx->params[0];
          ctx->rpy = ctx->params[1];
          ctx->cpx = ctx->params[2];
          ctx->cpy = ctx->params[3];

          ctx_header_append (ctx, CAIRO_PATH_CURVE_TO, 4, x, y);
          ctx_point_append (ctx, ctx->rpx, ctx->rpy);
          ctx_point_append (ctx, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 'h':
      /* horizontal lineto */
      if (ctx->param == 1)
        {
          ctx->cpx = ctx->rpx = ctx->params[0];

          ctx_header_append (ctx, CAIRO_PATH_LINE_TO, 2, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 'v':
      /* vertical lineto */
      if (ctx->param == 1)
        {
          ctx->cpy = ctx->rpy = ctx->params[0];

          ctx_header_append (ctx, CAIRO_PATH_LINE_TO, 2, ctx->cpx, ctx->cpy);

          ctx->param = 0;
        }
      break;

    case 'q':
      /* quadratic bezier curveto */
      if (ctx->param == 4 || final)
        {
          parse_path_default_xy (ctx, 4);

          ctx->rpx = ctx->params[0];
          ctx->rpy = ctx->params[1];
          ctx->cpx = ctx->params[2];
          ctx->cpy = ctx->params[3];

          g_warning ("quadratic bezier curveto not implemented");

          ctx->param = 0;
        }
      break;

    case 't':
      /* truetype quadratic bezier curveto */
      if (ctx->param == 2 || final)
        {
          parse_path_default_xy (ctx, 2);

          ctx->rpx = 2 * ctx->cpx - ctx->rpx;
          ctx->rpy = 2 * ctx->cpy - ctx->rpy;
          ctx->cpx = ctx->params[0];
          ctx->cpy = ctx->params[1];

          g_warning ("truetype quadratic bezier curveto not implemented");

          ctx->param = 0;
        }
      else if (final)
        {
          if (ctx->param > 2)
            {
              parse_path_default_xy (ctx, 4);

              ctx->rpx = ctx->params[0];
              ctx->rpy = ctx->params[1];
              ctx->cpx = ctx->params[2];
              ctx->cpy = ctx->params[3];

              g_warning ("conicto not implemented");
            }
          else
            {
              parse_path_default_xy (ctx, 2);

              ctx->cpx = ctx->rpx = ctx->params[0];
              ctx->cpy = ctx->rpy = ctx->params[1];

              ctx_header_append (ctx, CAIRO_PATH_LINE_TO, 2, ctx->cpx, ctx->cpy);
            }

          ctx->param = 0;
        }
      break;

    case 'a':
      if (ctx->param == 7 || final)
        {
          ctx->cpx = ctx->rpx = ctx->params[5];
          ctx->cpy = ctx->rpy = ctx->params[6];

          g_warning ("arcto not implemented");

          ctx->param = 0;
        }
      break;

    default:
      ctx->param = 0;
      break;
    }
}

static cairo_path_data_t *
parse_path_data (const gchar * data, gint * n_elements)
{
  ParsePathContext ctx = { 0, };

  gboolean in_num = FALSE;
  gboolean in_frac = FALSE;
  gboolean in_exp = FALSE;
  gboolean exp_wait_sign = FALSE;
  gdouble val = 0.0;
  gchar c = 0;
  gint sign = 0;
  gint exp = 0;
  gint exp_sign = 0;
  gdouble frac = 0.0;
  gint i;

  ctx.alloc_size = N_ELEMENTS;
  ctx.size = 0;
  ctx.path = g_try_malloc0_n (ctx.alloc_size, sizeof (cairo_path_data_t));

  for (i = 0;; i++)
    {
      c = data[i];
      if (c >= '0' && c <= '9')
        {
          /* digit */
          if (in_num)
            {
              if (in_exp)
                {
                  exp = (exp * 10) + c - '0';
                  exp_wait_sign = FALSE;
                }
              else if (in_frac)
                val += (frac *= 0.1) * (c - '0');
              else
                val = (val * 10) + c - '0';
            }
          else
            {
              in_num = TRUE;
              in_frac = FALSE;
              in_exp = FALSE;
              exp = 0;
              exp_sign = 1;
              exp_wait_sign = FALSE;
              val = c - '0';
              sign = 1;
            }
        }
      else if (c == '.')
        {
          if (!in_num)
            {
              in_num = TRUE;
              val = 0;
            }
          in_frac = TRUE;
          frac = 1;
        }
      else if ((c == 'E' || c == 'e') && in_num)
        {
          in_exp = TRUE;
          exp_wait_sign = TRUE;
          exp = 0;
          exp_sign = 1;
        }
      else if ((c == '+' || c == '-') && in_exp)
        {
          exp_sign = c == '+' ? 1 : -1;
        }
      else if (in_num)
        {
          /* end of number */

          val *= sign * pow (10, exp_sign * exp);
          if (ctx.rel)
            {
              /* Handle relative coordinates. This switch statement attempts
                 to determine _what_ the coords are relative to. This is
                 underspecified in the 12 Apr working draft. */
              switch (ctx.cmd)
                {
                case 'l':
                case 'm':
                case 'c':
                case 's':
                case 'q':
                case 't':
                  /* rule: even-numbered params are x-relative, odd-numbered
                     are y-relative */
                  if ((ctx.param & 1) == 0)
                    val += ctx.cpx;
                  else if ((ctx.param & 1) == 1)
                    val += ctx.cpy;
                  break;

                case 'a':
                  /* rule: sixth and seventh are x and y, rest are not
                     relative */
                  if (ctx.param == 5)
                    val += ctx.cpx;
                  else if (ctx.param == 6)
                    val += ctx.cpy;
                  break;
                case 'h':
                  /* rule: x-relative */
                  val += ctx.cpx;
                  break;
                case 'v':
                  /* rule: y-relative */
                  val += ctx.cpy;
                  break;
                }
            }

          ctx.params[ctx.param++] = val;
          parse_path_do_cmd (&ctx, FALSE);
          in_num = FALSE;
        }

      if (c == '\0')
        break;
      else if ((c == '+' || c == '-') && !exp_wait_sign)
        {
          sign = c == '+' ? 1 : -1;
          val = 0;
          in_num = TRUE;
          in_frac = FALSE;
          in_exp = FALSE;
          exp = 0;
          exp_sign = 1;
          exp_wait_sign = FALSE;
        }
      else if (c == 'z' || c == 'Z')
        {
          if (ctx.param)
            parse_path_do_cmd (&ctx, TRUE);

          ctx_header_append (&ctx, CAIRO_PATH_CLOSE_PATH, 1, 0, 0);
        }
      else if (c >= 'A' && c <= 'Z' && c != 'E')
        {
          if (ctx.param)
            parse_path_do_cmd (&ctx, TRUE);
          ctx.cmd = c + 'a' - 'A';
          ctx.rel = FALSE;
        }
      else if (c >= 'a' && c <= 'z' && c != 'e')
        {
          if (ctx.param)
            parse_path_do_cmd (&ctx, TRUE);
          ctx.cmd = c;
          ctx.rel = TRUE;
        }
      /* else c _should_ be whitespace or , */
    }

  if (n_elements)
    *n_elements = ctx.size;

  return ctx.path;
}

static void
path_data_append_csource (GString * string, gchar * name, cairo_path_data_t * path_data, gint size, gdouble width, gdouble height)
{
  gint i;

  g_string_append_printf (string, "static cairo_path_data_t %s_data[] = {\n", name);

  for (i = 0; i < size; i += path_data[i].header.length)
    {
      cairo_path_data_t *data = &path_data[i];
      gboolean not_last = i + 1 < size;

      g_string_append_printf (string, "\t{.header.type = %d, .header.length = %d}%s", data->header.type, data->header.length, (not_last) ? ",\n" : "");

      switch (data->header.type)
        {
        case CAIRO_PATH_MOVE_TO:
        case CAIRO_PATH_LINE_TO:
          g_string_append_printf (string, "\t{.point.x = %lf, .point.y = %lf}%s", data[1].point.x, data[1].point.y, (not_last) ? ",\n" : "");
          break;
        case CAIRO_PATH_CURVE_TO:
          g_string_append_printf (string, "\t{.point.x = %lf, .point.y = %lf},\n" "\t{.point.x = %lf, .point.y = %lf},\n" "\t{.point.x = %lf, .point.y = %lf}%s", data[1].point.x, data[1].point.y, data[2].point.x, data[2].point.y, data[3].point.x, data[3].point.y, (not_last) ? ",\n" : "");
          break;
        case CAIRO_PATH_CLOSE_PATH:
          break;
        }
    }

  g_string_append_printf (string, "\n};\n\n" "cairo_path_t %s_path = {0, %s_data, %d};\n\n", name, name, size);
}

static void
path_data_append_cheader_start (GString * string, gchar * name)
{
  gchar *NAME = g_utf8_strup (name, -1);
  g_string_printf (string, "#ifndef __%s_H__\n#define __%s_H__\n\n", NAME, NAME);
  g_free (NAME);
}

static void
path_data_append_cheader_end (GString * string, gchar * name)
{
  gchar *NAME = g_utf8_strup (name, -1);
  g_string_append_printf (string, "\n#endif /* __%s_H__ */\n", NAME);
  g_free (NAME);
}

static void
path_data_append_cheader (GString * string, gchar * name, gdouble width, gdouble height)
{
  gchar *NAME = g_utf8_strup (name, -1);

  g_string_append_printf (string, "#define %s_WIDTH %lf\n" "#define %s_HEIGHT %lf\n" "extern cairo_path_t %s_path;\n\n", NAME, width, NAME, height, name);

  g_free (NAME);
}

/* gcc cairo_svg2path.c -o cairo_svg2path `pkg-config --libs --cflags glib-2.0` -lm */

int
main (int argc, char **argv)
{
  static gchar **filenames, *target;
  static GOptionEntry entries[] = {
    {"target", 't', 0, G_OPTION_ARG_FILENAME, &target, "name of the output file", NULL},
    {G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, "A SVG file to convert", NULL},
    {NULL}
  };
  GOptionContext *context;
  GError *error = NULL;
  gchar *basename;
  GString *string;
  gboolean dump_header;
  gint i;

  context = g_option_context_new ("FILES - Convert path data in a SVG to a cairo_path_t");
  g_option_context_add_main_entries (context, entries, GETTEXT_PACKAGE);
  if (!g_option_context_parse (context, &argc, &argv, &error))
    {
      g_warning ("option parsing failed: %s\n", error->message);
      return 1;
    }

  if (!target || !filenames)
    {
      g_print ("%s", g_option_context_get_help (context, TRUE, NULL));
      return 1;
    }

  basename = g_path_get_basename (target);
  g_strdelimit (basename, ".", '\0');
  dump_header = g_str_has_suffix (target, ".h");

  if (dump_header)
    {
      string = g_string_new ("");
      path_data_append_cheader_start (string, basename);
    }
  else
    string = g_string_new ("#include <cairo.h>\n\n");

  for (i = 0; filenames[i]; i++)
    {
      gdouble width, height;
      gchar *path;

      path = parse_svg_file (filenames[i], NULL, &width, &height, &error);

      if (path)
        {
          cairo_path_data_t *path_data;
          gint size;

          path_data = parse_path_data (path, &size);

          if (dump_header)
            path_data_append_cheader (string, basename, width, height);
          else
            path_data_append_csource (string, basename, path_data, size, width, height);
        }
      else
        g_printerr ("Could not parse SVG file %s", filenames[i]);
    }

  if (dump_header)
    path_data_append_cheader_end (string, basename);

  g_file_set_contents (target, string->str, -1, NULL);

  g_free (basename);
  g_string_free (string, TRUE);

  return 0;
}
