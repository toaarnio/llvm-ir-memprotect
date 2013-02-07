// RUN: clang $TEST_SRC -x cl -include pocl_kernel.h -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll
// TODO: write runner, which actually executes the kernel code...

__kernel void square(__global float* input, __global float* output) {
  int i = get_global_id(0);
  output[i] = input[i]*input[i];
}
