#ifndef lint
/*static char rcsid[] =
"$Id: table.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/*
 * Rewritten by A. Anderson 2002
 *
 */
/***************************************************************************/
/*
 * NAME
 * ====
 * table - Maintain a table of FOURCC/value pairs
 *
 * SYNOPSIS
 * ========
 * 
 * - RIFFIOFCCTableNew()
 * - RIFFIOFCCTableDelete()
 * - RIFFIOFCCTableMakeEntry()
 * - RIFFIOFCCTableLookup()
 * - RIFFIOFCCTableCount()
 * - RIFFIOFCCTableCreateArray()
 * - RIFFIOFCCTableFreeEntries()
 * - RIFFIOFCCTableForEachEntry()
 * - RIFFIOFCCTableDump()
 */
/***************************************************************************/

#include <assert.h>
#include <stdlib.h>

#include <niff/riffio.h>


/***************************************************************************/
/*
 * RIFFIOFCCTableNew
 * =================
 * Return a newly created table.
 */
RIFFIOFCCTable *
RIFFIOFCCTableNew(void)
/* OBLIGATIONS
 * -----------
 * Use RIFFIOFCCTableDelete to free the table.
 * 
 * RETURNS
 * -------
 * The new table or null on failure.
 */
/***************************************************************************/
{
    RIFFIOFCCTable *ptableNew;
printf("%d\n",sizeof(RIFFIOFCCTable));

    ptableNew = (RIFFIOFCCTable *) malloc (sizeof(RIFFIOFCCTable));

    if (ptableNew == 0)
        return (RIFFIOFCCTable *) 0;

	ptableNew	->items = (GList *) 0;

      return ptableNew;


}

/***************************************************************************/
/*
 * RIFFIOFCCTableDelete
 * ====================
 * Free all memory allocated for a table.
 */
void
RIFFIOFCCTableDelete(RIFFIOFCCTable *ptable)
/* 
 * WARNING
 * -------
 * Orphans all table entry values if they happen to be pointers.
 *
 * ENTRY
 * -----
 * <*ptable> must have been created by RIFFIOFCCTableCreate.
 */
/***************************************************************************/
{
       RIFFIOFCCTableFreeEntries(ptable);

       delete (ptable);
}



/***************************************************************************/
/*
 * RIFFIOFCCTableMakeEntry
 * =======================
 * Make a new entry in a table.
 */
RIFFIOSuccess
RIFFIOFCCTableMakeEntry(RIFFIOFCCTable *ptable, RIFFIOFCCTableEntry newEntry)
/*
 * Replaces any existing entry with the value of the new entry.
 * 
 * RETURN
 * ------
 * Return the success status of the operation.
 */
/***************************************************************************/
{

    	unsigned int i;
     RIFFIOFCCTableEntry *elementi;

    assert(ptable != 0);
    assert(ptable->items != 0);
	
        
    /*
     * Try to find an existing entry to replace
     * by scanning the list.

     * We will inevitably either
     *   - find a match, or
     *   - reach the end of the list
     * In either case we _will_ exit the while loop by
     * "return"ing from this function.
     */

	printf("\n%d\n",g_list_length(ptable->items));
	if(g_list_length(ptable->items) ==0)
	{
		
		ptable->items =g_list_append(ptable->items, &newEntry);
		return RIFFIO_OK;
	}
	else
	{
		for(i=0;g_list_nth_data(ptable->items,i);i++)
		{
			elementi = (RIFFIOFCCTableEntry *) g_list_nth_data(ptable->items,i);
			if(elementi->fcc == newEntry.fcc)
				break;
					
		};
        /*
         * Did we find a matching fcc?
         */
        if (elementi->fcc == newEntry.fcc)
        {
            /*
             * We have we found a matching fcc.
             * Replace its old value with our new one
             */
            ((RIFFIOFCCTableEntry *)(g_list_nth_data(ptable->items,i)))->l = newEntry.l;

            /*
             * We are done, we have replaced an existing entry
             */
            return RIFFIO_OK;
        };


        /*
         * Did we reach the end of the list?
         */
        if (i==g_list_length(ptable->items))
        {
            /*
             * Yes, Append new list item
             */
                //don't think this works
              ptable->items = g_list_append(ptable->items,(gpointer) &newEntry);

            /*
             * We are finished adding a new list item to the table
             */
            return RIFFIO_OK;

        };
   };
  return RIFFIO_FAIL;
}

/***************************************************************************/
/*
 * RIFFIOFCCTableLookup
 * ====================
 * Lookup a value in a RIFFIOFCCTable given a FOURCC.
 */
RIFFIOSuccess
RIFFIOFCCTableLookup(RIFFIOFCCTable *ptable, RIFFIOFCCTableEntry *entryp)
/***************************************************************************/
{

      unsigned int i = 0;
	RIFFIOFCCTableEntry *elementi;
    assert(ptable != 0);
    assert(ptable->items != 0);
    assert(entryp != 0);
    
   for(i=0;i!=g_list_length(ptable->items);i++)
		{
			elementi = (RIFFIOFCCTableEntry *) g_list_nth_data(ptable->items,i);
			if(elementi->fcc == entryp->fcc)
				 entryp->l = elementi->l;
				return RIFFIO_OK;					
		};

    /*
     * We are done, DID NOT find matching FOURCC
     */
    return RIFFIO_FAIL;
    
}


/***************************************************************************/
/*
 * RIFFIOFCCTableCount
 * ===================
 * Return the number of entries in a table
 */
unsigned
RIFFIOFCCTableCount(RIFFIOFCCTable *ptable)
/***************************************************************************/
{

       return unsigned(g_list_length(ptable->items));
}

/*
 * Treat all the entries in a table like pointers and 
 * free them all.
 */
void
RIFFIOFCCTableFreeEntries(RIFFIOFCCTable *ptable)
{
     assert (ptable != 0);
	if (ptable->items !=NULL)
    {
		assert (ptable->items != 0);
	     g_list_free(ptable->items);
	};
}

/***************************************************************************/
/*
 * RIFFIOFCCTableCreateArray
 * =========================
 * Return a newly allocated array of all the entries in a table.
 */
RIFFIOFCCTable *
RIFFIOFCCTableCreateArray(RIFFIOFCCTable *ptable)
/*
 * EXIT
 * ----
 * The new array is NOT sorted in any particular order.
 *
 * OBLIGATIONS
 * -----------
 * The new array must be freed by the caller. 
 */
/***************************************************************************/
{

    RIFFIOFCCTable *array; /* array to return */

    /*
     * Allocate memory for the new array
     */
    array = (RIFFIOFCCTable*) malloc (sizeof(RIFFIOFCCTable));

	array = ptable;

    return array;

}

/***************************************************************************/
/*
 * RIFFIOFCCTableForEachEntry
 * ===========================
 * Apply a function over every entry in a table.
 */
void
RIFFIOFCCTableForEachEntry(RIFFIOFCCTable *ptable, void f(RIFFIOFCCTableEntry))
/*
 * ENTRY
 * -----
 * <f> must not modify, add or delete any entries
 */
/***************************************************************************/
{
    assert(ptable != 0);
    assert(ptable->items != 0);
    
   /*dunno*/
}

#ifndef NDEBUG
/***************************************************************************/
/*
 * RIFFIOFCCTableDump
 * ==================
 * Print the contents of a RIFFIOFCCTable for debugging
 */
void
RIFFIOFCCTableDump(FILE *fp, RIFFIOFCCTable *ptable)
/***************************************************************************/
{

    unsigned int i;                           /* index */

    
    assert(fp != 0);
    assert(ptable != 0);
    assert(ptable->items != 0);
    
    for (i = 0; i != g_list_length(ptable->items); i++)
    {
        fprintf(fp, "item[%d]:\n", i);

            char fccString[5];
            RIFFIOFOURCCToString(((RIFFIOFCCTableEntry *)g_list_nth_data(ptable->items,i))->fcc,fccString);
            fprintf(fp, "\t%s\n", fccString);
      }

}
#endif
