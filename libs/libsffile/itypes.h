/*================================================================
 * integer types
 *================================================================*/

#ifndef ITYPES_H_DEF
#define ITYPES_H_DEF

/* 8bit bytes */
typedef unsigned char byte;

/* 16bit integers */
typedef unsigned short uint16;
typedef short int16;

/* 32bit long integers */
typedef unsigned int uint32;
typedef int int32;

/**/
typedef union uint32rec {
	byte b8[4];
	uint16 b16[4];
	uint32 b32;
} uint32rec;

/* vp=uint32rec, cp=char* */
#define get32rec(vp,cp) memcpy(vp, cp, 4)

#if 1
/* little endian */
#define swapi(x) x
#define swapl(x) x

#else
/* big endian */
#define swapi(x) ((((x)&0xFF)<<8) | (((x)>>8)&0xFF))
#define swapl(x) ((((x)&0xFF)<<24) | \
		      (((x)&0xFF00)<<8) | \
		      (((x)&0xFF0000)>>8) | \
		      (((x)>>24)&0xFF))
#endif

#endif
