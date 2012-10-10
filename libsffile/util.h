/*================================================================
 * utility routines
 *================================================================*/

#ifndef UTIL_H_DEF
#define UTIL_H_DEF

#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define numberof(ary)	(sizeof(ary)/sizeof(ary[0]))
#ifndef offsetof
#define offsetof(s_type,field) ((int)(((char*)(&(((s_type*)NULL)->field)))-((char*)NULL)))
#endif

#define BITON(var,flag)	((var) |= (flag))
#define BITOFF(var,flag) ((var) &= ~(flag))
#define BITSWT(var,flag) ((var) ^= (flag))

extern int verbose, debug;
#define DEBUG(LVL,XXX)	{if (verbose > LVL) { XXX; }}

/* cmpopen.c */
int CmpSearchFile(char *name);
char *CmpExtension(int type);
FILE *CmpOpenFile(char *name, int *flag);
void CmpCloseFile(FILE *fp, int flag);

/* malloc.c */
void *safe_malloc(int size);
void safe_free(void *ptr);

/* signal.c */
void add_signal(int sig, void (*handler)(), int exit_after);

/* fskip.c */
void fskip(int size, FILE *fd, int seekable);

#endif
