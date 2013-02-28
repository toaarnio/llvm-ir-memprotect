// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: echo "Running original kernel with correct parameters" &&
// RUN: ($RUN_KERNEL $OUT_FILE.ll square 5 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,10):(float,{0,0,0,0,0})" | 
// RUN:  grep "1.000000,4.000000,9.000000,16.000000,25.000000,") &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -Oz -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Running optimized webcl kernel with correct parameters" &&
// RUN: ($RUN_KERNEL $OUT_FILE.clamped.optimized.ll square 5 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,5):(int,10):(float,{0,0,0,0,0}):(int,5)" | 
// RUN:  grep "1.000000,4.000000,9.000000,16.000000,25.000000,") &&
// RUN: echo "Running optimized webcl kernel with over indexing parameters access to last table element should return 0.0f or 4.0f depending on boundary check method" &&
// RUN: ( ($RUN_KERNEL $OUT_FILE.clamped.optimized.ll square 5 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,4):(int,10):(float,{0,0,0,0,0}):(int,4)" | grep "1.000000,4.000000,9.000000,16.000000,0.000000,") || (echo "Unexpected output from execution." && false) ) &&
// RUN: echo "Trying to call webcl kernel with original bad parameters" &&
// RUN: ( $RUN_KERNEL $OUT_FILE.clamped.optimized.ll square 5 "(float,{1.0f,2.0f,3.0f,4.0f,5.0f}):(int,10):(float,{0,0,0,0,0})";
// RUN:   ( [ ! $? -eq 0 ] && echo "OK: running clamped kernel without count parameters failed as expected") ||
// RUN:   ( echo "FAIL: calling webcl kernel should have failed" && false )
// RUN: )

__kernel void square(__global float* input, int dummy_inthe_middle,  __global float* output) {
  int i = get_global_id(0);
  output[i] = input[i]*input[i];
  printf("%f,", output[i]);
}

