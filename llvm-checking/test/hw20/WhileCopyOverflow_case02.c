#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "securec.h"


void *cwe120_c_86(void *dest, const void *src, size_t count)
{
	char *tmp = (char *)dest;
	char *s = (char *)src;
	int i = 0;
	while (i < count)
	{
		/* POTENTIAL FLAW:循环上线来自外部，缓冲区长度未知，可能造成缓冲区溢出 */
		*tmp++ = *s++;
		i++;
	}
	return dest;
}
int main(int argc, char **argv)
{
	char dest1[10];
	char *src1 = "test1";
	if (argc > 1) {
		cwe120_c_86(dest1, src1, atoi(argv[1]));
	}

	return 0;
}

