#ifndef lint
/*static char rcsid[] =
"$Id: tcbtable.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/*
 * Public Domain 1995,1996 Timothy Butler
 *
 * THIS DOCUMENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 */
/*
 * NAME 
 * ====
 * tcbtable - tag callback table routines
 * 
 * SYNOPSIS
 * ========
 *
 * - NIFFIOPTCBTableNew()
 * - NIFFIOPTCBTableDelete()
 * - NIFFIOPTCBTableLookup()
 * - NIFFIOPTCBTableMakeEntry()
 * 
 */

#include <assert.h>
#include <stdlib.h>

#include <niffio.h>
#include <niffiop.h>

/*
 * Tag callback tables are implemented as a static array of dynamic
 * lists of callback entries.
 * 
 * Each array index corresponds to a tag id [0,255].
 *
 * Each list entry corresponds to a callback record associated with a 
 * particular FOURCC.
 * This is so we can store different tag callbacks depending on what
 * type of chunk a tag is found in.
 */

typedef struct NIFFIOPTCBListItem *NIFFIOPTCBList;

typedef struct NIFFIOPTCBListItem
{

    NIFFIOPTagEntry tagentry;  /* The callback record   */
    NIFFIOPTCBList  next;      /* Next item in the list */

} NIFFIOPTCBListItem;

#define TCBTABLE_LIM 256
struct NIFFIOPTCBTable
{
    NIFFIOPTCBList lists[TCBTABLE_LIM]; /* a list for each tag id */
};

/*
 * NIFFIOPTCBTableNew
 * ==================
 * Allocate and return a pointer to a new TCBTable, 
 * return null on failure.
 */
NIFFIOPTCBTable *
NIFFIOPTCBTableNew(void)
{
    NIFFIOPTCBTable *pTCBTableNew;
    int i;   /* index into array of TCBlists */
    
    pTCBTableNew = (NIFFIOPTCBTable *) malloc (sizeof(NIFFIOPTCBTable));
    if (pTCBTableNew == 0)
        return (NIFFIOPTCBTable *) 0;
    
    for (i = 0; i < TCBTABLE_LIM; i++)
    {
        pTCBTableNew->lists[i] = (NIFFIOPTCBList) 0;
    }
    
    return pTCBTableNew;
}

/*
 * NIFFIOPTCBTableDelete
 * =====================
 * Free the memory allocated to a TCBTable,
 * including all its entries.
 */
void
NIFFIOPTCBTableDelete(NIFFIOPTCBTable *ptable)
{
    int i; /* index into array of lists */
    NIFFIOPTCBList listThis; /* Points to a list item to free */
    NIFFIOPTCBList listNext; /* Points to the list item after tcblistThis */
    
    assert (ptable != 0);
    
    /*
     * Free each list in the table
     */
    for (i = 0; i < TCBTABLE_LIM; i++)
    {
        /* Start of list */
        listThis = ptable->lists[i];
        
        /* Free each item in the list */
        while(listThis != 0)
        {
            listNext = listThis->next;  /* remember next before we free this */
            free (listThis);
            listThis = listNext;
        }
        
    }
    
    /*
     * Free the table array 
     */
    free(ptable);
}

/*
 * NIFFIOPTCBTableMakeEntry
 * ========================
 * Make a new entry in a Tag Callback Table given a tag id and
 * a tag callback entry.
 *
 * If an entry already exists for a tag/FOURCC combination then
 * the result is undefined. (for now)
 */
RIFFIOSuccess
NIFFIOPTCBTableMakeEntry(NIFFIOPTCBTable *ptable,
                                                 BYTE tagid,
                                                 const NIFFIOPTagEntry *pentry)
{
    char strModule[] = "NIFFIOPTCBTableMakeEntry";
    NIFFIOPTCBList      list;       /* Pointer to each list item */
    NIFFIOPTCBListItem *pitemNew;   /* Newly created list item to add */
    
    assert (ptable != 0);
    assert (pentry != 0);
    
    /*
     * Allocate a new list item
     */
    pitemNew = (NIFFIOPTCBListItem *) malloc(sizeof(NIFFIOPTCBListItem));
    if (! pitemNew)
    {
        RIFFIOError(strModule, "Couldn't allocate new list item");
        return RIFFIO_FAIL;
    }
    
    /*
     * Initialize the new list item
     */
    pitemNew->tagentry = *pentry; /* Copy values provided */
    pitemNew->next = 0;           /* Will always be appended to list */
    
    /*
     * Find the list corresponding to the tagid
     */
    list = ptable->lists[tagid];
    
    /*
     * If the list is empty, then append the item immediately
     * and we are done.
     */
    if (! list)
    {
        ptable->lists[tagid] = pitemNew;
        return RIFFIO_OK;
    }
    
    /*
     * The list is not empty, find the last item
     */
    while (list->next != 0)
        list = list->next;
    
    /*
     * Append the new item to the last list item
     */
    list->next = pitemNew;
    
    return RIFFIO_OK;
    
}               


/*
 * NIFFIOPTCBTableLookup
 * =====================
 * Find a tag callback entry given a tagid and a FOURCC (stored in *pentry)
 * We will match a NIFFIO_FOURCC_WILDCARD entry also.
 */
RIFFIOSuccess
NIFFIOPTCBTableLookup(NIFFIOPTCBTable *ptable,
                      BYTE tagid,
                      NIFFIOPTagEntry *pentry)
{
    NIFFIOPTCBList listThis; /* Points to each list item */
    NIFFIOPTCBList listNext; /* The item after *listThis */
    
    FOURCC fcc;              /* The FOURCC to match */
    
    assert (ptable != 0);
    assert (pentry != 0);

    fcc = pentry->fcc;
        
    /*
     * Find the list corresponding to the tag
     */
    listNext = ptable->lists[tagid];

    /*
     * Search the list for a matching FOURCC
     */
    while (listNext != 0)
    {
        listThis = listNext;
        if ((listThis->tagentry.fcc == fcc)
            || (listThis->tagentry.fcc == NIFFIO_FOURCC_WILDCARD))
        {
            /*
             * Found a match, copy the entry, done
             */
            *pentry = listThis->tagentry;
            return RIFFIO_OK;
        }
        listNext = listThis->next;
    }
        
    /* 
     * If we made it here then we reached the end of the list
     * without a match
     */
    return RIFFIO_FAIL;

}
