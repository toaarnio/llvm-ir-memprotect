// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Checking that call is made to safe signature" &&
// RUN: grep "call.*vload4l10float_safe"  $OUT_FILE.clamped.ll

struct float_safe {
  float *cur;
  float *begin;
  float *end;
};

float4 _cl_overloadable vload4(long i, struct float_safe safeptr) {
  float *ptr = safeptr.cur + i;
  if (ptr < safeptr.begin) return (float4)(0, 0, 0, 0);
  if (ptr + 3 >= safeptr.end) return (float4)(0, 0, 0, 0);
  return (float4)(ptr[0], ptr[1], ptr[2], ptr[3]);
}

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  vstore4(loaded, i, out);
}

