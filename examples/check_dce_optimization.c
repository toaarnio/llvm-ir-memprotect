/**
 * Usage: 
 * prog <first int> <second int>
 * 
 * if the first parameter > 128 program should fail because index overflow
 * if the second parameter > 256 program should fail because index underflow
 * returns <first int> + <second int> on success
 */

#include <stdio.h>
#include <stdlib.h>

#define LEN_A 128
#define LEN_B 256

// If you like to get more compact end program that does not have any extra
// printf code..
#define ENABLE_STDIO 1

int main(int argc, char* argv[])
{
	int a[ LEN_A ];
	int b[ LEN_B ];

	if (argc < 3) {
#if ENABLE_STDIO
		printf("Missing arguments. Usage: prog <first int> <second int>\n");
		printf("* if the first parameter > 128 program should fail because index overflow\n");
		printf("* if the second parameter > 256 program should fail because index underflow\n");
		printf("* returns <first int> + <second int> on success\n");
#endif
		return 1;
	}

	int loop_a = atoi(argv[1]);
	int loop_b = atoi(argv[2]);

	// --------- initialization loops where checks can be eliminated by dce -------

	for (int i = 0; i < LEN_A; i++) a[i] = atoi(argv[i%(argc-1)+1]);
	for (int i = 0; i < LEN_B; i++) b[i] = a[i%LEN_A];


	// -------- loops where checks cannot be eliminated --------------

	// loop until first parameter value on table a 
	for (int i = 0; i < loop_a; i++) {
		a[i] = 0;
	}

	// loop backward over b as many steps as second commandline parameter
	int index = LEN_B;
	for (int i = 0; i < loop_b; i++) {
		index--;
		b[index] = 0;
	}	

#if ENABLE_STDIO
	printf("Finally got until the end and returning %i\n", loop_a + loop_b);
#endif
	return loop_a + loop_b;
}

