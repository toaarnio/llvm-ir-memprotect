// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -Oz -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.optimized.ll &&
// RUN: echo "Running kernel should not be able to read last 2 value completely" &&
// RUN: ( ($RUN_KERNEL $OUT_FILE.clamped.optimized.ll test_kernel 5 "(uchar,{0,1,2,3,4,5}):(int,4)" | 
// RUN:    grep "256,513,770,0,0,") || (echo "Unexpected output from execution." && false) )

__kernel void test_kernel(__global uchar* input) {
  int i = get_global_id(0);
  __global ushort* ushort_input = (__global ushort*)(&input[i]);
  printf("%u,", *ushort_input);
}

