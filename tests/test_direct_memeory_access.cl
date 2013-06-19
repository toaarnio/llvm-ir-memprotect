// RUN: $OCLANG -c $TEST_SRC -O0 -o $OUT_FILE.bc &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: echo "Check that some checks are added to case. Would be really strange if in some program this really would produce any reasonable code." &&
// RUN: [ $( $RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(int,{1}):(int,1)" ) == "0" ] ||
// RUN: (echo "Accessing directly memory should return 0.." && false)

__kernel void test_kernel(__global int *in) {
  // should compile and print 0
  printf("%i", *((int*)0x00ff00ff));
}
