/*================================================================
 * sffile.c
 *	read SoundFont file (SBK/SF2) and store the layer lists
 *
 * Copyright (C) 1996,1997 Takashi Iwai
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *================================================================*/
//Interface to Denemo License:  FSF GPL version 3 or later

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sffile.h"
#include "sf_util.h"


/**
 * Convert illegal characters in soundfont
 *
 */

static void ConvertIllegalChar(char *name){
  char *p;
  for (p = name; *p; p++) {
    if (!isprint(*p) || *p == '{' || *p == '}')
      *p = ' ';
    else if (*p == '[')
      *p = '(';
    else if (*p == ']')
      *p = ')';
  }
}

/**
 * parse soundfont file "soundfont" and return number of presets. If "soundfont" is NULL use previously loaded soundfont
 * if name or preset are Non-null, fill in the values for the given index (counting from 0).
 */
int  ParseSoundfont(char *soundfont, int index, char **name, int *preset, int *bank) {
  FILE *fp;
  static SFInfo sf;
  static initialized = FALSE;
  int i;
  int number = 0;
  if(soundfont) {
		if(initialized)
			free_soundfont(&sf);
		if ((fp = fopen(soundfont, "rb")) == NULL) {
			printf("\ncan't open soundfont file\n");
			return 0;
		} else if (load_soundfont(&sf, fp, TRUE)) {
			return 0;
		} else {
			initialized = TRUE;
			fclose(fp);
		}
		for (i = 0; i < sf.npresets-1; i++) {
			ConvertIllegalChar(sf.preset[i].hdr.name);
		}
	}
	if(initialized) {
		number = sf.npresets;
		if(index<number) {
			if(name)
				*name = sf.preset[index].hdr.name;
			if(preset)
				*preset = sf.preset[index].preset;
			if(bank)
				*bank = sf.preset[index].bank;
		}
	}
	return number;
}

/*================================================================
 * preset / instrument bag record
 *================================================================*/

typedef struct _SFBags {
	int nbags;
	uint16 *bag;
	int ngens;
	SFGenRec *gen;
} SFBags;

static SFBags prbags, inbags;


/*----------------------------------------------------------------
 * function prototypes
 *----------------------------------------------------------------*/

#define NEW(type,nums)	(type*)safe_malloc(sizeof(type) * (nums))

#define READCHUNK(var,fd)	fread(&var, 8, 1, fd)
#define READID(var,fd)	fread(var, 4, 1, fd)
#define READSTR(var,fd)	fread(var, 20, 1, fd)
#define READDW(var,fd)	fread(&var, 4, 1, fd)
#define READW(var,fd)	fread(&var, 2, 1, fd)
#define READB(var,fd)	fread(&var, 1, 1, fd)
#define SKIPB(fd)	{char dummy; fread(&dummy, 1, 1, fd);}
#define SKIPW(fd)	{short dummy; fread(&dummy, 2, 1, fd);}
#define SKIPDW(fd)	{int dummy; fread(&dummy, 4, 1, fd);}

static int seekable;
#define FSKIP(size,fd)	fskip(size, fd, seekable)


/*----------------------------------------------------------------*/

static int chunkid(char *id);
static int process_list(int size, SFInfo *sf, FILE *fd);
static int process_info(int size, SFInfo *sf, FILE *fd);
static int process_sdta(int size, SFInfo *sf, FILE *fd);
static int process_pdta(int size, SFInfo *sf, FILE *fd);
static void load_sample_names(int size, SFInfo *sf, FILE *fd);
static void load_preset_header(int size, SFInfo *sf, FILE *fd);
static void load_inst_header(int size, SFInfo *sf, FILE *fd);
static void load_bag(int size, SFBags *bagp, FILE *fd);
static void load_gen(int size, SFBags *bagp, FILE *fd);
static void load_sample_info(int size, SFInfo *sf, FILE *fd);
static void convert_layers(SFInfo *sf);
static void generate_layers(SFHeader *hdr, SFHeader *next, SFBags *bags);
static void free_layer(SFHeader *hdr);


/*----------------------------------------------------------------
 * id numbers
 *----------------------------------------------------------------*/

enum {
	/* level 0; chunk */
	UNKN_ID, RIFF_ID, LIST_ID, SFBK_ID,
	/* level 1; id only */
	INFO_ID, SDTA_ID, PDTA_ID,
	/* info stuff; chunk */
	IFIL_ID, ISNG_ID, IROM_ID, INAM_ID, IVER_ID, IPRD_ID, ICOP_ID,
	/* sample data stuff; chunk */
	SNAM_ID, SMPL_ID,
	/* preset stuff; chunk */
	PHDR_ID, PBAG_ID, PMOD_ID, PGEN_ID,
	/* inst stuff; chunk */
	INST_ID, IBAG_ID, IMOD_ID, IGEN_ID,
	/* sample header; chunk */
	SHDR_ID,
};


/*================================================================
 * load a soundfont file
 *================================================================*/

int load_soundfont(SFInfo *sf, FILE *fd, int is_seekable)
{
	SFChunk chunk;

	seekable = is_seekable;

	sf->preset = NULL;
	sf->sample = NULL;
	sf->inst = NULL;
	sf->sf_name = NULL;

	prbags.bag = inbags.bag = NULL;
	prbags.gen = inbags.gen = NULL;

	/* check RIFF file header */
	READCHUNK(chunk, fd);
	if (chunkid(chunk.id) != RIFF_ID) {
		fprintf(stderr, "*** not a RIFF file\n");
		return -1;
	}
	/* check file id */
	READID(chunk.id, fd);
	if (chunkid(chunk.id) != SFBK_ID) {
		fprintf(stderr, "*** not a SoundFont file\n");
		return -1;
	}

	for (;;) {
		READCHUNK(chunk, fd);
		if (feof(fd))
			break;
		else if (chunkid(chunk.id) == LIST_ID) {
			if (process_list(chunk.size, sf, fd))
				break;
		} else {
			fprintf(stderr, "*** illegal id in level 0: %4.4s %4d\n",
				chunk.id, chunk.size);
			FSKIP(chunk.size, fd);
		}
	}

	/* parse layer structure */
	convert_layers(sf);

	/* free private tables */
	if (prbags.bag) free(prbags.bag);
	if (prbags.gen) free(prbags.gen);
	if (inbags.bag) free(inbags.bag);
	if (inbags.gen) free(inbags.gen);

	return 0;
}


/*================================================================
 * free buffer
 *================================================================*/

void free_soundfont(SFInfo *sf)
{
	int i;
	if (sf->preset) {
		for (i = 0; i < sf->npresets; i++)
			free_layer(&sf->preset[i].hdr);
		free(sf->preset);
	}
	if (sf->inst) {
		for (i = 0; i < sf->ninsts; i++)
			free_layer(&sf->inst[i].hdr);
		free(sf->inst);
	}
	if (sf->sample) free(sf->sample);
	if (sf->sf_name) free(sf->sf_name);
}


/*----------------------------------------------------------------
 * get id value from 4bytes ID string
 *----------------------------------------------------------------*/

static int chunkid(char *id)
{
	static struct idstring {
		char *str;
		int id;
	} idlist[] = {
		{"RIFF", RIFF_ID},
		{"LIST", LIST_ID},
		{"sfbk", SFBK_ID},
		{"INFO", INFO_ID},
		{"sdta", SDTA_ID},
		{"snam", SNAM_ID},
		{"smpl", SMPL_ID},
		{"pdta", PDTA_ID},
		{"phdr", PHDR_ID},
		{"pbag", PBAG_ID},
		{"pmod", PMOD_ID},
		{"pgen", PGEN_ID},
		{"inst", INST_ID},
		{"ibag", IBAG_ID},
		{"imod", IMOD_ID},
		{"igen", IGEN_ID},
		{"shdr", SHDR_ID},
		{"ifil", IFIL_ID},
		{"isng", ISNG_ID},
		{"irom", IROM_ID},
		{"iver", IVER_ID},
		{"INAM", INAM_ID},
		{"IPRD", IPRD_ID},
		{"ICOP", ICOP_ID},
	};

	int i;

	for (i = 0; i < sizeof(idlist)/sizeof(idlist[0]); i++) {
		if (strncmp(id, idlist[i].str, 4) == 0)
			return idlist[i].id;
	}

	return UNKN_ID;
}


/*================================================================
 * process a list chunk
 *================================================================*/

static int process_list(int size, SFInfo *sf, FILE *fd)
{
	SFChunk chunk;

	/* read the following id string */
	READID(chunk.id, fd); size -= 4;

	switch (chunkid(chunk.id)) {
	case INFO_ID:
		return process_info(size, sf, fd);
	case SDTA_ID:
		return process_sdta(size, sf, fd);
	case PDTA_ID:
		return process_pdta(size, sf, fd);
	default:
		fprintf(stderr, "*** illegal id in level 1: %4.4s\n", chunk.id);
		FSKIP(size, fd); /* skip it */
		return 0;
	}
}


/*================================================================
 * process info list
 *================================================================*/

static int process_info(int size, SFInfo *sf, FILE *fd)
{
	sf->infopos = ftell(fd);
	sf->infosize = size;

	/* parse the buffer */
	while (size > 0) {
		SFChunk chunk;

		/* read a sub chunk */
		READCHUNK(chunk, fd); size -= 8;
		if (feof(fd))
			return -1;

		switch (chunkid(chunk.id)) {
		case IFIL_ID:
			/* soundfont file version */
			READW(sf->version, fd);
			READW(sf->minorversion, fd);
			break;
		case INAM_ID:
			/* name of the font */
			sf->sf_name = (char*)safe_malloc(chunk.size + 1);
			fread(sf->sf_name, 1, chunk.size, fd);
			sf->sf_name[chunk.size] = 0;
			break;
		default:
			FSKIP(chunk.size, fd);
			break;
		}
		size -= chunk.size;
	}
	return 0;
}


/*================================================================
 * process sample data list
 *================================================================*/

static int process_sdta(int size, SFInfo *sf, FILE *fd)
{
	while (size > 0) {
		SFChunk chunk;

		/* read a sub chunk */
		READCHUNK(chunk, fd); size -= 8;
		if (feof(fd))
			return -1;

		switch (chunkid(chunk.id)) {
		case SNAM_ID:
			/* sample name list */
			load_sample_names(chunk.size, sf, fd);
			break;
		case SMPL_ID:
			/* sample data starts from here */
			sf->samplepos = ftell(fd);
			sf->samplesize = chunk.size;
			FSKIP(chunk.size, fd);
			break;
		default:
			FSKIP(chunk.size, fd);
			break;
		}
		size -= chunk.size;
	}
	return 0;
}


/*================================================================
 * process preset data list
 *================================================================*/

static int process_pdta(int size, SFInfo *sf, FILE *fd)
{
	while (size > 0) {
		SFChunk chunk;

		/* read a subchunk */
		READCHUNK(chunk, fd); size -= 8;
		if (feof(fd))
			return -1;

		switch (chunkid(chunk.id)) {
		case PHDR_ID:
			load_preset_header(chunk.size, sf, fd);
			break;
		case PBAG_ID:
			load_bag(chunk.size, &prbags, fd);
			break;
		case PGEN_ID:
			load_gen(chunk.size, &prbags, fd);
			break;
		case INST_ID:
			load_inst_header(chunk.size, sf, fd);
			break;
		case IBAG_ID:
			load_bag(chunk.size, &inbags, fd);
			break;
		case IGEN_ID:
			load_gen(chunk.size, &inbags, fd);
			break;
		case SHDR_ID:
			load_sample_info(chunk.size, sf, fd);
			break;
		case PMOD_ID: /* ignored */
		case IMOD_ID: /* ingored */
		default:
			FSKIP(chunk.size, fd);
			break;
		}
		size -= chunk.size;
	}
	return 0;
}


/*----------------------------------------------------------------
 * store sample name list; sf1 only
 *----------------------------------------------------------------*/

static void load_sample_names(int size, SFInfo *sf, FILE *fd)
{
	int i, nsamples;
	if (sf->version > 1) {
		fprintf(stderr, "*** version 2 has obsolete format??\n");
		FSKIP(size, fd);
		return;
	}

	/* each sample name has a fixed lentgh (20 bytes) */
	nsamples = size / 20;
	if (sf->sample == NULL) {
		sf->nsamples = nsamples;
		sf->sample = NEW(SFSampleInfo, sf->nsamples);
	} else if (sf->nsamples != nsamples) {
		fprintf(stderr, "*** different # of samples ?? (%d : %d)\n",
		       sf->nsamples, nsamples);
		FSKIP(size, fd);
		return;
	}

	/* read each name from file */
	for (i = 0; i < sf->nsamples; i++) {
		READSTR(sf->sample[i].name, fd);
	}
}


/*----------------------------------------------------------------
 * preset header list
 *----------------------------------------------------------------*/

static void load_preset_header(int size, SFInfo *sf, FILE *fd)
{
	int i;

	sf->npresets = size / 38;
	sf->preset = NEW(SFPresetHdr, sf->npresets);
	for (i = 0; i < sf->npresets; i++) {
		READSTR(sf->preset[i].hdr.name, fd);
		READW(sf->preset[i].preset, fd);
		READW(sf->preset[i].bank, fd);
		READW(sf->preset[i].hdr.bagNdx, fd);
		SKIPDW(fd); /* lib; ignored*/
		SKIPDW(fd); /* genre; ignored */
		SKIPDW(fd); /* morph; ignored */
		/* initialize layer table; it'll be parsed later */
		sf->preset[i].hdr.nlayers = 0;
		sf->preset[i].hdr.layer = NULL;
	}
}


/*----------------------------------------------------------------
 * instrument header list
 *----------------------------------------------------------------*/

static void load_inst_header(int size, SFInfo *sf, FILE *fd)
{
	int i;

	sf->ninsts = size / 22;
	sf->inst = NEW(SFInstHdr, sf->ninsts);
	for (i = 0; i < sf->ninsts; i++) {
		READSTR(sf->inst[i].hdr.name, fd);
		READW(sf->inst[i].hdr.bagNdx, fd);
		/* iniitialize layer table; it'll be parsed later */
		sf->inst[i].hdr.nlayers = 0;
		sf->inst[i].hdr.layer = NULL;
	}
}


/*----------------------------------------------------------------
 * load preset/instrument bag list on the private table
 *----------------------------------------------------------------*/

static void load_bag(int size, SFBags *bagp, FILE *fd)
{
	int i;

	size /= 4;
	bagp->bag = NEW(uint16, size);
	for (i = 0; i < size; i++) {
		READW(bagp->bag[i], fd);
		SKIPW(fd); /* mod; ignored */
	}
	bagp->nbags = size;
}


/*----------------------------------------------------------------
 * load preset/instrument generator list on the private table
 *----------------------------------------------------------------*/

static void load_gen(int size, SFBags *bagp, FILE *fd)
{
	int i;

	size /= 4;
	bagp->gen = NEW(SFGenRec, size);
	for (i = 0; i < size; i++) {
		READW(bagp->gen[i].oper, fd);
		READW(bagp->gen[i].amount, fd);
	}
	bagp->ngens = size;
}


/*----------------------------------------------------------------
 * load sample info list
 *----------------------------------------------------------------*/

static void load_sample_info(int size, SFInfo *sf, FILE *fd)
{
	int i;
	int in_rom;

	/* the record size depends on the soundfont version */
	if (sf->version > 1) {
		/* SF2 includes sample name and other infos */
		sf->nsamples = size / 46;
		sf->sample = NEW(SFSampleInfo, sf->nsamples);
	} else  {
		/* SBK; sample name may be read already */
		int nsamples = size / 16;
		if (sf->sample == NULL) {
			sf->nsamples = nsamples;
			sf->sample = NEW(SFSampleInfo, sf->nsamples);
		} else if (sf->nsamples != nsamples) {
#if 0
			fprintf(stderr, "*** different # of infos ?? (%d : %d)\n",
			       sf->nsamples, nsamples);
			FSKIP(size, fd);
			return;
#endif
			/* overwrite it */
			sf->nsamples = nsamples;
		}
	}

	in_rom = 1;  /* data may start from ROM samples */
	for (i = 0; i < sf->nsamples; i++) {
		if (sf->version > 1) /* SF2 only */
			READSTR(sf->sample[i].name, fd);
		READDW(sf->sample[i].startsample, fd);
		READDW(sf->sample[i].endsample, fd);
		READDW(sf->sample[i].startloop, fd);
		READDW(sf->sample[i].endloop, fd);
		if (sf->version > 1) { /* SF2 only */
			READDW(sf->sample[i].samplerate, fd);
			READB(sf->sample[i].originalPitch, fd);
			READB(sf->sample[i].pitchCorrection, fd);
			READW(sf->sample[i].samplelink, fd);
			READW(sf->sample[i].sampletype, fd);
		} else { /* for SBK; set missing infos */
			sf->sample[i].samplerate = 44100;
			sf->sample[i].originalPitch = 60;
			sf->sample[i].pitchCorrection = 0;
			sf->sample[i].samplelink = 0;
			/* the first RAM data starts from address 0 */
			if (sf->sample[i].startsample == 0)
				in_rom = 0;
			if (in_rom)
				sf->sample[i].sampletype = 0x8001;
			else
				sf->sample[i].sampletype = 1;
		}
	}
}


/*================================================================
 * convert from bags to layers
 *================================================================*/

static void convert_layers(SFInfo *sf)
{
	int i;

	if (prbags.bag == NULL || prbags.gen == NULL ||
	    inbags.bag == NULL || inbags.gen == NULL) {
		fprintf(stderr, "*** illegal bags / gens\n");
		return;
	}

	for (i = 0; i < sf->npresets - 1; i++) {
		generate_layers(&sf->preset[i].hdr,
				&sf->preset[i+1].hdr,
				&prbags);
	}
	for (i = 0; i < sf->ninsts - 1; i++) {
		generate_layers(&sf->inst[i].hdr,
				&sf->inst[i+1].hdr,
				&inbags);
	}
}


/*----------------------------------------------------------------
 * generate layer lists from stored bags
 *----------------------------------------------------------------*/

static void generate_layers(SFHeader *hdr, SFHeader *next, SFBags *bags)
{
	int i;
	SFGenLayer *layp;

	hdr->nlayers = next->bagNdx - hdr->bagNdx;
	if (hdr->nlayers < 0) {
		fprintf(stderr, "illegal layer numbers %d\n", hdr->nlayers);
		return;
	}
	if (hdr->nlayers == 0)
		return;
	hdr->layer = (SFGenLayer*)safe_malloc(sizeof(SFGenLayer) * hdr->nlayers);
	layp = hdr->layer;
	for (layp = hdr->layer, i = hdr->bagNdx; i < next->bagNdx; layp++, i++) {
		int genNdx = bags->bag[i];
		layp->nlists = bags->bag[i+1] - genNdx;
		if (layp->nlists < 0) {
			fprintf(stderr, "illegal list numbers %d\n", layp->nlists);
			return;
		}
		layp->list = (SFGenRec*)safe_malloc(sizeof(SFGenRec) * layp->nlists);
		memcpy(layp->list, &bags->gen[genNdx],
		       sizeof(SFGenRec) * layp->nlists);
	}
}

/*----------------------------------------------------------------
 * free a layer
 *----------------------------------------------------------------*/

static void free_layer(SFHeader *hdr)
{
	int i;
	for (i = 0; i < hdr->nlayers; i++) {
		SFGenLayer *layp = &hdr->layer[i];
		if (layp->nlists > 0)
			free(layp->list);
	}
	if (hdr->nlayers > 0)
		free(hdr->layer);
}
