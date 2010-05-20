/*
questo programmino mi è servito il 2006/03/20 alle 23.01
e deve generare 100 numeri randoms compresi nel range di uno
short int (16 bits dai), e produrre un cosino in assembly LC-2 */

#include <stdio.h>
#include <time.h>
#include <math.h>

#define KNUM_NUM	100
#define KNUM_MIN	-1000
#define KNUM_MAX	+1000

short int kRand(void);

int main(int argc, char *argv[])
{
	short int i, num;
	FILE *fp;
	srand(time(NULL));
	if (fp=fopen("wwww.asm","wt")) {
		for (i = 0; i < KNUM_NUM; i++)
			fprintf(fp, "\t.fill\tx%04X\n", kRand() & 0xFFFF);
		fclose(fp);
	}
	else
		return(1);

	return(0);
}
short int kRand()
{
	short int N = abs(KNUM_MIN) + abs(KNUM_MAX);
	N = rand() % N + KNUM_MIN;
	return N;
}
