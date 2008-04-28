typedef union {
  gint a;
  gchar c;
  gchar *strval;
  struct twoints t;
} YYSTYPE;
#define	STAFFBEGIN	257
#define	STAFFEND	258
#define	TIMESIGTOK	259
#define	KEYSIGTOK	260
#define	CLEFTOK	261
#define	INSTRUMENT	262
#define	BEGINCHORD	263
#define	ENDCHORD	264
#define	TONETYPE	265
#define	ENSHIFT	266
#define	OCTIND	267
#define	BASEDURATION	268
#define	NUMDOTS	269
#define	MINOR	270
#define	MAJOR	271
#define	BLINE	272
#define	BEGINTUPLET	273
#define	ENDTUPLET	274
#define	STEMDIRECTIVETOK	275
#define	RELOADDIRECTIVE	276
#define	DYNAMICTOK	277
#define	SLUR_START	278
#define	TIE	279
#define	STARTGRACE	280
#define	ENDGRACE	281
#define	LYTONEOPTION	282
#define	SLUR_END	283


extern YYSTYPE lylval;
