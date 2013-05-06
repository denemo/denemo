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

void addbookmark (GtkAction * action, gpointer param);
void gotobookmark (GtkAction * action, gpointer param);
void nextbookmark (GtkAction * action, gpointer param);
void prevbookmark (GtkAction * action, gpointer param);
void deletebookmarks (GtkAction * action, gpointer param);


#endif
