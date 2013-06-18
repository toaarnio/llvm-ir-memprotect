// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "float4 _cl_overloadable vload4(long i, __global float* in) { return (float4)(in[i], in[i+1], in[i+2], in[i+3]); } IMPLEMENT_SAFE_VLOAD(float, __global, 4, (-42, -42, -42, -42) );" > vload4_builtins.cl &&
// RUN: $OCLANG vload4_builtins.cl -S -o vload4_builtins.ll &&
// RUN: llvm-link $OUT_FILE.clamped.ll vload4_builtins.ll -S -o $OUT_FILE.clamped.linked.ll &&
// RUN: echo "Checking that fully overflowing data is made safe" &&
// RUN: ($RUN_KERNEL $OUT_FILE.clamped.linked.ll test_kernel 2 "(float,{1.0f,2.0f,3.0f,4.0f}):(int,4)" | grep "1: -42, -42, -42, -42") &&
// RUN: echo "Checking that original data is preserved when overflowing fully" &&
// RUN: ($RUN_KERNEL $OUT_FILE.clamped.linked.ll test_kernel 2 "(float,{1.0f,2.0f,3.0f,4.0f}):(int,4)" | grep "0: 1, 2, 3, 4") &&
// RUN: echo "Checking that partially overflowing data is made safe" &&
// RUN: ($RUN_KERNEL $OUT_FILE.clamped.linked.ll test_kernel 2 "(float,{1.0f,2.0f,3.0f,4.0f,6.0f}):(int,5)" | grep "1: -42, -42, -42, -42") &&
// RUN: echo "Checking that original data is preserved overflowing partially" &&
// RUN: ($RUN_KERNEL $OUT_FILE.clamped.linked.ll test_kernel 2 "(float,{1.0f,2.0f,3.0f,4.0f,6.0f}):(int,5)" | grep "0: 1, 2, 3, 4") &&
// RUN: true

__kernel void test_kernel(__global float* in) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  printf("%d: %d, %d, %d, %d\n", i, (int) loaded.s0, (int) loaded.s1, (int) loaded.s2, (int) loaded.s3);
}
