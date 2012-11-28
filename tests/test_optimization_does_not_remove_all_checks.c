/**
 * Test program to see if checks stays correctly in code after pass.
 */

// RUN: clang -c $TEST_SRC -Os -emit-llvm -o $OUT_FILE.optimized.bc &&
// RUN: llvm-dis $OUT_FILE.optimized.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.optimized.bc -o $OUT_FILE.clamped.ll &&
// RUN: opt -Os -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "-- extracting main functions from .ll code" &&
// RUN: sed -e ':a' -e 'N' -e '$!ba' -Ee 's/.*(define i32 @main[^}]*).*/\1/g' $OUT_FILE.clamped.optimized.ll > $OUT_FILE.clamped.main.ll &&
// RUN: sed -e ':a' -e 'N' -e '$!ba' -Ee 's/.*(define i32 @main[^}]*).*/\1/g' $OUT_FILE.optimized.ll > $OUT_FILE.main.ll &&
// RUN: echo "Line count of code should not be the same" &&
// RUN: [ $(wc -l < $OUT_FILE.clamped.main.ll) !=  $(wc -l < $OUT_FILE.main.ll) ]

#define LEN_B 256

// this should prevent any optimizations
extern int loop_b;

int main()
{
	int b[ LEN_B ];

	// --------- initialization loops where checks can be eliminated by dce -------
	for (int i = 0; i < LEN_B; i++) b[i] = loop_b;

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

