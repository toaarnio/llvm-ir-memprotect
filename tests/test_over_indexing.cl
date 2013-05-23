// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: echo "Running indices to 10000 with original kernel." &&
// RUN: ( $RUN_KERNEL $OUT_FILE.ll square 10000 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,10):(float,{0,0,0,0,0})";
// RUN:   ( [ ! $? -eq 0 ] && echo "OK: heavily overindexing original kernel failed as expected") ||
// RUN:   ( echo "FAIL: maybe failed, expected overindexing to crash " && false )
// RUN: ) &&
// RUN: ../inject-builtin-declarations.pl $OUT_FILE.ll ../../pocl/builtin-declarations.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -Oz -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Running optimized webcl kernel with over indexing parameters" &&
// RUN: $RUN_KERNEL $OUT_FILE.clamped.optimized.ll square 10000 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,5):(int,10):(float,{0,0,0,0,0}):(int,5)" &&
// RUN: echo "Test OK: Didn't crash, good!"

__kernel void square(__global float* input, int dummy_inthe_middle,  __global float* output) {
  int i = get_global_id(0);
  output[i] = input[i]*input[i];
}
