// RUN: clang -c $TEST_SRC -O3 -emit-llvm -o $OUT_FILE.bc &&
// RUN: llvm-dis < $OUT_FILE.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "Compiled fine."

#include <stdint.h>

int32_t main(int32_t argc, char* argv[])
{
	int32_t val = (*argv[1]) - '0';
	return val;
}

