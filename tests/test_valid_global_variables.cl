// RUN: $OCLANG $TEST_SRC -S -O0 -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Testing that when run with global work group size 5 only 1,2,3 prints non-zero" &&
// RUN: RESULT=`$RUN_KERNEL $OUT_FILE.clamped.ll test 5 "(float,{1,2,3,4}):(int,4):(float,{1,2,3,4}):(int,4)"` &&
// RUN: echo $RESULT | grep "1.000000 1, 2.000000 2, 3.000000 3, 0.000000 0, 0.000000 0," || 
// RUN: (echo "Failed result: $RESULT" && false)

struct test_struct {
  int i;
  long l;
  float f;
};

__kernel void test(__global float* in, __global float* out) {
  int i = get_global_id(0);
  __local float test[3];
  __local struct test_struct struct_table[3];  
  test[i] = in[i];
  out[i] = in[i];
  struct_table[i].f = test[i];
  struct_table[i].l = struct_table[i].f;
  printf("%f %li, ", test[i] ,struct_table[i].l);
}
