//
// parseinstruments.h
//
//
//
//
// Author: Adam Tee <adam@ajtee.plus.com>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef PARSEINSTRUMENTS_H
#define PARSEINSTRUMENTS_H


#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlmemory.h>
#include <denemo/denemo.h>



GList *parseInstruments (GList * instruments);
InstrumentType lookuptype (gchar * string);
#endif
