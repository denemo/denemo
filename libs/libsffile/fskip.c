/*----------------------------------------------------------------
 * skip file position
 *----------------------------------------------------------------*/

#include <stdio.h>

void fskip(int size, FILE *fd, int seekable)
{
	if (seekable)
		fseek(fd, size, SEEK_CUR);
	else {
		char tmp[1024];
		while (size >= (int)sizeof(tmp)) {
			size -= fread(tmp, 1, sizeof(tmp), fd);

		}
		while (size > 0)
			size -= fread(tmp, 1, size, fd);
	}
}


