// RUN: $OCLANG -c $TEST_SRC -O0 -S -o $OUT_FILE.ll &&
// RUN: opt  -load $CLAMP_PLUGIN -clamp-pointers $OUT_FILE.ll -S -o $OUT_FILE.clamped.ll &&
// RUN: echo "Check taking reference from int and returning next value and one before next, should print 0,1 ." &&
// RUN: [ $( $RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(int,1)" ) == "0,1" ] || 
// RUN: ( echo "Didn't get expected result 0,1" && false ) 

__kernel void test_kernel(int ext_a) {
  int a = ext_a;
  int *b = &a+1;
  printf("%i,%i", *(&a+1), *(b-1));
}
