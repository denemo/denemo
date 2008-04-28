//
// C++ Interface: bookmarks
//
// Description: 
//
//
// Author: Adam Tee <adam@ajtee.plus.com>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef BOOKMARKS_H
#define BOOKMARKS_H

#include <denemo/denemo.h>

void addbookmark(GtkAction *action,DenemoGUI *gui);
void gotobookmark(GtkAction *action, DenemoGUI *gui);
void nextbookmark(GtkAction *action, DenemoGUI *gui);
void prevbookmark(GtkAction *action, DenemoGUI *gui);
void deletebookmarks(GtkAction *action, DenemoGUI *gui);


#endif
