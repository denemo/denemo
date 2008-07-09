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

void addbookmark(GtkAction *action);
void gotobookmark(GtkAction *action);
void nextbookmark(GtkAction *action);
void prevbookmark(GtkAction *action);
void deletebookmarks(GtkAction *action);


#endif
