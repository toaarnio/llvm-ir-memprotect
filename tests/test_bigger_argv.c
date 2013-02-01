/**
 * Usage: 
 * prog <first int> <second int>
 * 
 * if the first parameter > 128 program should fail because index overflow
 * if the second parameter > 256 program should fail because index underflow
 * returns 0 on success, traps to boundary checks if fail 
 */

// test case without clamp-pointers pass....

// RUN: clang -c $TEST_SRC -O3 -emit-llvm -o $OUT_FILE.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: ( lli $OUT_FILE.clamped.ll 128 256;
// RUN:   ( [ $? = 11 ] && echo "OK: returned 11 as expected") ||
// RUN:   ( echo "FAIL: invalid return value" && false )
// RUN: ) &&
// RUN: ( lli $OUT_FILE.clamped.ll 128 257;
// RUN:   ( [ $? = 1 ] && echo "OK: returned 1 as expected") ||
// RUN:   ( echo "FAIL: invalid return value" && false )
// RUN: ) &&
// RUN: ( lli $OUT_FILE.clamped.ll 129 256;
// RUN:   ( [ $? = 10 ] && echo "OK: returned 10 as expected") ||
// RUN:   ( echo "FAIL: invalid return value" && false )
// RUN: ) &&
// RUN: ( lli $OUT_FILE.clamped.ll 99999999 99999999;
// RUN:   ( [ $? = 0 ] && echo "OK: returned 0 as expected and didn't crash") ||
// RUN:   ( echo "FAIL: invalid return value" && false )
// RUN: )

#include <stdlib.h>

#define LEN_A 128
#define LEN_B 256

// If you like to get more compact end program that does not have any extra
// printf code.. also currently clamp pointers pass crash to printfs
#ifndef ENABLE_STDIO
#define ENABLE_STDIO 1
#endif

#if ENABLE_STDIO
#include <stdio.h>
#endif

// TODO: fix this: clamping pass breaks passing values from commandline parameters
int main(int argc, char* argv[])
{
	int a[ LEN_A ];
	int b[ LEN_B ];

	if (argc < 3) {
#if ENABLE_STDIO
		printf("Missing arguments. Usage: prog <first int> <second int>\n");
		printf("* if the first parameter <= 128 program should return 10 \n");
		printf("* if the second parameter <= 256 program should return 1 \n");
		printf("* if both are in range returns 11 \n");
		printf("* if both are out of range returns 0 (out of bound reads returns 0) \n");
		printf("* program should never fail if parameters are less than max_int \n");
#endif
		return 1;
	}

    volatile int loop_a = atoi(argv[1]);
    volatile int loop_b = atoi(argv[2]);

	// --------- initialization loops where checks could be eliminated by sophisticated dce -------
	for (int i = 0; i < LEN_A; i++) a[i] = argc+i;
	for (int i = 0; i < LEN_B; i++) b[i] = a[i%LEN_A];

	// -------- loops where checks cannot be eliminated --------------
	// loop until first parameter value on table a 
	for (int i = 0; i < loop_a; i++) {
		a[i] = 1;
	}

	// loop backward over b as many steps as second commandline parameter
	int index = LEN_B;
	for (int i = 0; i < loop_b; i++) {
		index--;
		b[index] = 10;
	}

#if ENABLE_STDIO
	printf("Finally got until the end and returning %i\n", b[LEN_B - loop_b] + a[loop_a-1]);
#endif

	// prevent dce to touch loops above and return something we can use to check functionality
	return b[LEN_B - loop_b] + a[loop_a - 1];
}

