// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  __local float test[10];
  event_t e = async_work_group_strided_copy(test, in, 10, 1);
  e = async_work_group_strided_copy(out, test, 10, 1);
}

