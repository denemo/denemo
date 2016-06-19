/* processstaffname.cpp
 * for Denemo, the GNU graphical frontend to GNU Lilypond
 *
 * (c) 2000-2005 Matthew Hiller */

/* This function accepts a Denemo staff's canonicalized name
 * denemo_name, and sets lily_name to be what the corresponding voice
 * will be called in the mudela representation. Both GString *s should
 * be initialized before this function is called */
/* e.g., "bass voice"->"BassVoice" */

#include <glib.h>
#include <string.h>
#include <stdio.h>

#define ROWS     4
#define COLS     4

static guint pows[ROWS][COLS] = { {1000, 1000, 1000, 1000},
{900, 500, 400, 100},
{90, 50, 40, 10},
{9, 5, 4, 1}
};

static char *roms[ROWS][COLS] = { {"M", "M", "M", "M"},
{"CM", "D", "CD", "C"},
{"XC", "L", "XL", "X"},
{"IX", "V", "IV", "I"}
};

static void
to_roman (guint decimal, GString * lily_name)
{
  guint power;                  /* current power of 10 */
  guint indx;                   /* indexes through values to subtract */

  for (power = 0; power < ROWS; power++)
    for (indx = 0; indx < COLS; indx++)
      while (decimal >= pows[power][indx])
        {
          g_string_append (lily_name, roms[power][indx]);
          decimal -= pows[power][indx];
        }
}

/* make an acceptable lily name from denemo_name: FIXME - check for anything other than alphaword */
void
set_lily_name (GString * denemo_name, GString * lily_name)
{
  guint i;
  gchar c;
  gboolean last_char_was_space = TRUE;

  g_string_assign (lily_name, "");
  for (i = 0; i < denemo_name->len; i++)
    {
      c = denemo_name->str[i];
      if (c == ' ')
        last_char_was_space = TRUE;
      else
        {
          guint decimal;
          int numchars;
          if (c == '0')         /* character 0 that is */
            g_string_append (lily_name, "O");   /* replace with upper case o */
          else if ((sscanf (&denemo_name->str[i], "%u%n", &decimal, &numchars)) == 1)
            {
              i += numchars, i--;    /* move over digits that have been converted by sscanf */
              to_roman (decimal, lily_name);
            }
          else
            {
              if (last_char_was_space && 'a' <= c && c <= 'z')
                {
                  /* Make the character uppercase */
                  c -= ('a' - 'A');
                }
              g_string_append_c (lily_name, c);
              last_char_was_space = FALSE;
            }

        }
    }
}

void
dnm_set_lily_name (GString * denemo_name, GString * lily_name)
{
  set_lily_name (denemo_name, lily_name);
}

/* This function accepts a Lilypond voice name lily_name and calculates
 * from it the canonicalized denemo_name. Both Gstring *s should be
 * initialized before this function is called */
/* e.g., "BassVoice"->"bass voice" */

void
set_denemo_name (GString * lily_name, GString * denemo_name)
{
  guint i;
  gchar c;

  g_string_assign (denemo_name, "");
  for (i = 0; i < lily_name->len; i++)
    {
      c = lily_name->str[i];
      if ('A' <= c && c <= 'Z')
        {
          /* Need to make the character lowercase and insert a space */
          c += ('a' - 'A');
          if (i)
            /* insert a space before it as well */
            g_string_append_c (denemo_name, ' ');
        }
      g_string_append_c (denemo_name, c);
    }
}

/* This function accepts a gchar *proposal as an argument. If
 * this proposal can be made into a canonical Denemo staff name,
 * it is stored in denemo_name. If not, denemo_name is left
 * unchanged. It returns -1 if the name could not be canonicalized. */

gint
canonicalize_denemo_name (gchar * proposal, GString * denemo_name)
{
  gchar *accept = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890 ";
  guint i;
  gchar c;
  gboolean last_char_was_space = FALSE;
  if ((proposal==NULL) || (denemo_name == NULL))
    {
        g_critical ("Bad call to canonicalize_denemo_name");
        return -1;
    }
  //g_strdown (proposal);

  if (strspn (proposal, accept) == strlen (proposal))
    {
      /* Okay; we have only acceptable characters. Let's boogie */
      g_string_assign (denemo_name, proposal);
      for (i = 0; i < denemo_name->len;)
        {
          c = denemo_name->str[i];
          if (c == ' ')
            {
              if (last_char_was_space)
                g_string_erase (denemo_name, i, 1);
              else
                {
                  last_char_was_space = TRUE;
                  i++;
                }
            }
          else
            {
              last_char_was_space = FALSE;
              i++;
            }
        }
      return 0;
    }
  return -1;
}
