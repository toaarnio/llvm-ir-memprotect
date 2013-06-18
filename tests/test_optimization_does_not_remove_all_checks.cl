/**
 * Test program to see if checks stays correctly in code after pass.
 */

// RUN: $OCLANG -c $TEST_SRC -O0 -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -O3 -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Checking that at least 2 tests did stay (b[index]=0; and return b[loop_b];)" &&
// RUN: [ $(grep "boundary.check.ok.[^:]*:" $OUT_FILE.clamped.optimized.ll | wc -l) > 2 ] || 
// RUN: ( cat $OUT_FILE.clamped.optimized.ll && echo "" &&
// RUN:   echo "Could not find enough memory access tests..." && echo "" && false )
#define LEN_B 256

__kernel void test_kernel(int argc, int loop_b)
{
	int b[ LEN_B ];

	// --------- initialization loops where checks can be eliminated by dce -------
	for (int i = 0; i < LEN_B; i++) b[i] = argc;

	// ------ checks should be there -------
	// loop backward over b as many steps as second commandline parameter
	int index = LEN_B;
	for (int i = 0; i < loop_b; i++) {
		index--;
		b[index] = 0;
	}	
	// prevent dce to touch loops above and reduce the whole program to return 0;
	printf("%i", b[loop_b]);
}

