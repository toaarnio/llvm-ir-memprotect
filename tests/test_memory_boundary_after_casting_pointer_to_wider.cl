// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -Oz -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Running kernel should not be able to read last 2 value completely" &&
// RUN: ( ($RUN_KERNEL $OUT_FILE.clamped.optimized.ll test_kernel 5 "(ushort,{0,1,2,3,4,5}):(int,4)" | 
// RUN:    grep "1,262144,1048576,3145728,0,") || (echo "Unexpected output from execution." && false) )

__kernel void test_kernel(__global ushort* input) {
  int i = get_global_id(0);
  __global uint* int_input = (__global uint*)&input[i];
  printf("%ui,", *int_input);
}

