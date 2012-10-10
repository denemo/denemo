/*
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void *safe_malloc(int size)
{
	void *p;
	p = (void*)calloc(size, 1);
	if (p == NULL) {
		fprintf(stderr, "can't malloc buffer for size %d!!\n", size);
		exit(1);
	}
	return p;
}

void safe_free(void *buf)
{
	if (buf)
		free(buf);
}
