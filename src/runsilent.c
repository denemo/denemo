/* runsilent.cpp
/
/  (c) 2002-2005 AJ Anderson
/
*/

#include "runsilent.h"
#include "importxml.h"
#include "exportlilypond.h"
#include <string.h>

void
silentconversion (char *file_in, DenemoGUI * gui)
{
  char *file_out;
  if (strcmp (file_in + strlen (file_in) - 7, ".denemo") == 0)
    {
      printf ("\nConverting file %s", file_in);
      importXML (file_in, gui, REPLACE_SCORE);
    }

  file_out = "silentconversion.ly";
  exportlilypond ((gchar *) file_out, gui, TRUE);
  printf ("\nWritten to %s\n\n", file_out);
};
