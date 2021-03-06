// RUN: $OCLANG $TEST_SRC -S -O3 -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Testing that when run with global work group size 6 only 1,2,3 and maybe 4 should print non-zero" &&
// RUN: RESULT=`$RUN_KERNEL $OUT_FILE.clamped.ll test 6 "(float,{1,2,3,4,10,20}):(int,4):(float,{1,2,3,4,10,20}):(int,4)"` &&
// ORIG KERNEL RUN: RESULT=`$RUN_KERNEL $OUT_FILE.ll test 6 "(float,{1,2,3,4,10,20}):(float,{1,2,3,4,10,20})"` &&
// RUN: ( echo $RESULT | grep "1.000000 1, 2.000000 2, 3.000000 3, 0.000000 0, 0.000000 0," || 
// RUN:   echo $RESULT | grep "1.000000 1, 2.000000 2, 3.000000 3, 4.000000 4, 0.000000 0," ) ||
// RUN: (echo "Failed result: $RESULT" && false)

// NOTE: test depends on feature, that with different optimizations some allocations might be 
//       copied to static allocation struct, which causes test[i] = in[i] to actually work 
//       also when i == 3, for i == 5 limits of kernel input causes 0-load.

struct test_struct {
  int i;
  long l;
  uint ui;
  float f;
};

__kernel void test(__global float* in, __global float* out) {
  int i = get_global_id(0);  
  __local float test[3];
  __local struct test_struct struct_table[3];  
  test[i] = in[i]; // test[0] = in[0], test[1] = in[1], test[2] = in[2], test[3] ?= in[3], test[4] ?= 0, test[5] ?= 0  
  out[i] = in[i]; // out[0,1,2,3] = in[0,1,2,3]
  struct_table[i].f = test[i]; // struct_table[0,1,2].f = test[0,1,2] and maybe [3,4,5] too
  struct_table[i].l = struct_table[i].f;
  printf("%f %li, ", test[i] ,struct_table[i].l);
}
