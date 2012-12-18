/**
 * Test program to see if checks stays correctly in code after pass.
 */

// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.optimized.bc &&
// RUN: llvm-dis $OUT_FILE.optimized.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.optimized.bc -o $OUT_FILE.clamped.ll &&
// RUN: opt -O3 -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// DONT RUN: echo "Checking that one 2 tests did stay" &&
// DONT RUN: [ $(grep "if\.end[^:]*:" $OUT_FILE.clamped.optimized.ll | wc -l) == 2 ] || 
// DONT RUN: ( cat $OUT_FILE.clamped.optimized.ll && echo "" &&
// DONT RUN:   echo "Optimizations did not remove unnecessary checks from first loop" && echo "" && false )
// RUN: echo "Test is disabled, until feature is implemented."
#define LEN_B 256

// this should prevent any optimizations
extern int loop_b;

int main(int argc, char *argv[])
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
	return b[loop_b];
}

