// RUN: $OCLANG $TEST_SRC -S -O3 -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Testing that cannot read more from kernel argument that is allowed in count argument." &&
// RUN: RESULT=`$RUN_KERNEL $OUT_FILE.clamped.ll test 7 "(float,{1,2,3,4,20,30}):(int,4):(float,{1,2,3,4,30,40}):(int,4)"` &&
// RUN: echo $RESULT | grep "1.000000 0.000000 2.000000 4.000000 3.000000 3.000000 4.000000 2.000000 0.000000 1.000000 0.000000 0.000000 0.000000 0.000000" || 
// RUN: (echo "Failed result: $RESULT" && false)

__kernel void test(__global float* in, __global float* out) {
  int i = get_global_id(0);
  printf("%f %f ", in[i], *(out+4-i) );
}
