/**
 * Usage: 
 * prog <first int> <second int>
 * 
 * if the first parameter > 128 program should fail because index overflow
 * if the second parameter > 256 program should fail because index underflow
 * returns 0 on success...
 */

// test case without clamp-pointers pass....
// clang -c $TEST_SRC -O3 -emit-llvm -o $OUT_FILE.bc &&
// lli $OUT_FILE.bc 128 256 && 
// if lli $OUT_FILE.bc 129 256; then echo "Command should have failed"; false; else echo "failed as expected"; true; fi &&
// if lli $OUT_FILE.bc 128 257; then echo "Command should have failed"; false; else echo "failed as expected"; true; fi

// RUN: clang -c $TEST_SRC -O3 -emit-llvm -o $OUT_FILE.bc &&
// RUN: llvm-dis < $OUT_FILE.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll 128 256 && 
// RUN: if lli $OUT_FILE.clamped.ll 129 256; then echo "Command should have failed"; false; else echo "failed as expected"; true; fi &&
// RUN: if lli $OUT_FILE.clamped.ll 128 257; then echo "Command should have failed"; false; else echo "failed as expected"; true; fi

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

#define ATOI( x, ret_var )                                      \
  do {                                                          \
    char* cursor = x;                                           \
    int val = 0;                                                \
    while (cursor) {                                            \
      int next = *cursor -'0';                                  \
      val = val * 10;                                           \
      val = val + next;                                         \
      cursor++;                                                 \
    }                                                           \
    ret_var = val;                                              \
  } while (0)                                     


// TODO: fix this: clamping pass breaks passing values from commandline parameters
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
    //	int loop_a;
    //  ATOI(argv[1], loop_a);
	int loop_b = atoi(argv[2]);
    //	int loop_b;
    //    ATOI(argv[2], loop_b);

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

	// prevent dce to touch loops above
	return b[LEN_B - loop_b] + a[loop_a-1];
}

