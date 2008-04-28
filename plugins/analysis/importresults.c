#include "importresults.h"
#include "analysis_highlighting.h"

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

static chorddescPtr
parseChord(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur)
{
  printf("Parse Chord\n");
  chorddescPtr ret = NULL;
  ret = (chorddescPtr) malloc(sizeof(ChordDesc));
  if(ret == NULL) {
    fprintf(stderr, "out of memory\n");
    return (NULL);
  }

  memset(ret, 0, sizeof(ChordDesc));
  cur = cur->xmlChildrenNode;

  while (cur != NULL) {
    if((!xmlStrcmp(cur->name, (const xmlChar*)"root")) &&
       (cur->ns == ns)) {
      ret->root = (gchar *)xmlNodeListGetString(doc,cur->xmlChildrenNode,1);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar*)"type")) &&
       (cur->ns == ns)) {
      
      ret->type = (gchar *)xmlNodeListGetString(doc,cur->xmlChildrenNode,1);
    }
    cur = cur->next;
  }

  return(ret);
}
    
  
static rmnNumeralPtr
parseRomanNumeral(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur)
{
  printf("Parse Numeral\n"); 
  rmnNumeralPtr ret = NULL;
 
  ret = ( rmnNumeralPtr) malloc(sizeof(rmnNumeral));
  if(ret == NULL) {
    fprintf(stderr, "out of memory\n");
    return (NULL);
  }

  memset(ret, 0, sizeof(rmnNumeral));
  cur = cur->xmlChildrenNode;

  while (cur != NULL) {
    if((!xmlStrcmp(cur->name, (const xmlChar*)"numeral")) &&
       (cur->ns == ns)) {
      ret->numeral = (gchar *)xmlNodeListGetString(doc,cur->xmlChildrenNode,1);
      if(ret->numeral == NULL) {
	ret->numeral = "??";
      }
    }
    if((!xmlStrcmp(cur->name, (const xmlChar*)"digit")) &&
       (cur->ns == ns)) {
      gchar *text = (gchar *)xmlNodeListGetString(doc,cur->xmlChildrenNode,1);
      sscanf(text, "%d", &ret->digit);
      g_free(text);
    }
    cur = cur->next;
  }

  return(ret);
}

static resultsPtr 
parseResult(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur) {
  printf("Parse Results\n");
  resultsPtr ret = NULL;
  gchar*text;

  ret = (resultsPtr) malloc(sizeof(Results));
  if(ret == NULL) {
    fprintf(stderr, "out of memory\n");
    return (NULL);
  }

  memset(ret, 0, sizeof(Results));
  
  cur = cur->xmlChildrenNode;
  while(cur != NULL) {
    if((!xmlStrcmp(cur->name, (const xmlChar *)"staff")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%d", &ret->staff);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"sbar")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%d", &ret->start_bar);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"sbeat")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%f", &ret->start_beat);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"ebar")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%d", &ret->end_bar);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"ebeat")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%f", &ret->end_beat);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"similarity")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text, "%f", &ret->similarity);
      g_free(text);
    }
    cur = cur->next;
  }

  return (ret);
}

static chordsPtr 
parseChordResults(xmlDocPtr doc, xmlNsPtr ns, xmlNodePtr cur)
{
  printf("Parse Harmony\n");
  chordsPtr ret = NULL;
  gchar*text;
  rmnNumeralPtr tmp;
  int modtonality = FALSE;
  chorddescPtr chorddesc;
  
  ret = (chordsPtr) malloc(sizeof(Chords));
  if(ret == NULL) {
    fprintf(stderr, "out of memory\n");
    return (NULL);
  }

  memset(ret, 0, sizeof(Chords));
  cur = cur->xmlChildrenNode;

  while (cur != NULL) {
    if((!xmlStrcmp(cur->name, (const xmlChar *)"bar")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%d", &ret->bar);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"length")) &&
       (cur->ns == ns)) {
      text = (gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);
      if(text != NULL)
	sscanf(text , "%f", &ret->length);
      g_free(text);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"chord")) &&
       (cur->ns == ns)) {
      chorddesc = parseChord(doc,ns,cur);
      ret->list = g_list_append(ret->list, chorddesc);
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"romannumeral")) &&
       (cur->ns == ns)) {
      if(modtonality) {
	tmp = parseRomanNumeral(doc,ns,cur);
	ret->modnumeral = tmp->numeral;
	ret->moddigit = tmp->digit;
      }else {
	tmp = parseRomanNumeral(doc,ns, cur);
	ret->numeral = tmp->numeral;
	ret->digit = tmp->digit;
      }
      modtonality = FALSE;
    }
    if((!xmlStrcmp(cur->name, (const xmlChar *)"modtonality")) &&
       (cur->ns == ns)) {
      ret->modtonality = 
	(gchar *)xmlNodeListGetString(doc, cur->xmlChildrenNode, 1);;
      modtonality = TRUE;
    }
    cur = cur->next;
  }
  return(ret);
}


AnalysisResPtr parseAnalysisResFile(char *filename) 
{
  xmlDocPtr doc;
  AnalysisResPtr ret;
  xmlNsPtr ns;
  xmlNodePtr cur;
  resultsPtr tmpres;
  chordsPtr tmpchord;
//  GList *tmp;
 // chordsPtr chord;

  doc = xmlParseFile(filename);
  if(doc == NULL)
    return NULL;

  cur = xmlDocGetRootElement(doc);
  if(cur == NULL) {
    fprintf(stderr, "empty document\n");
    xmlFreeDoc(doc);
    return NULL;
  }
  ns = xmlSearchNsByHref(doc, cur,
			 (const xmlChar *) 
			 "http://denemo.sourceforge.net/xmlns/Analysis");
  if (ns == NULL) {
    fprintf(stderr,
	    "document of the wrong type, GJob Namespace not found\n");
    xmlFreeDoc(doc);
    return(NULL);
  }
  if (xmlStrcmp(cur->name, (const xmlChar *) "analysisdata")) {
    fprintf(stderr,"document of the wrong type, root node != analysisdata");
    xmlFreeDoc(doc);
    return(NULL);
  }
  
  

  ret = (AnalysisResPtr) malloc(sizeof(AnalysisRes));
  if (ret == NULL) {
    fprintf(stderr,"out of memory\n");
    xmlFreeDoc(doc);
    return(NULL);
  }
  ret->harmony = NULL;
  ret->pm = NULL;
  while ( cur && xmlIsBlankNode ( cur ) )
    {
      cur = cur -> next;
    }
  if ( cur == 0 )
    return ( NULL );

  cur = cur->xmlChildrenNode;
  while(cur != NULL) {
    if((!xmlStrcmp(cur->name, (const xmlChar *)"PatternMatch")) &&
       (cur->ns == ns)) {
      tmpres = parseResult(doc, ns, cur);
      ret->pm = g_list_append(ret->pm, tmpres);
    }
    
    if((!xmlStrcmp(cur->name, (const xmlChar *)"Harmony")) &&
       (cur->ns == ns)) {
      tmpchord = parseChordResults(doc, ns, cur);
      g_print("Bar %d Length %f\n", tmpchord->bar, tmpchord->length);
      ret->harmony = g_list_append(ret->harmony, tmpchord);
    }
    cur = cur->next;
  }
  printf("Length of List %d\n", g_list_length(ret->harmony)); 
  xmlCleanupParser();

  
  return ret;
}
