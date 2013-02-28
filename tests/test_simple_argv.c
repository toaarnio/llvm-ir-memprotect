// RUN: clang -c $TEST_SRC -O3 -emit-llvm -o $OUT_FILE.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "Compiled fine." &&
// RUN: echo "Running with commandline param 0, should return 0" &&
// RUN: lli $OUT_FILE.clamped.ll 0 &&
// RUN: ( (lli $OUT_FILE.clamped.ll 1 && echo "FAIL: expected return val 1" && false) || 
// RUN:   (echo "OK: running with param 1 returned non zero as expected") ) 

#include <stdint.h>

int32_t main(int32_t argc, char* argv[])
{
	int32_t val = (*argv[1]) - '0';
	return val;
}

