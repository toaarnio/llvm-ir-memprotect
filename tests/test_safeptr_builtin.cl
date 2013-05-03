// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Checking that call is made to safe signature" &&
// RUN: grep "call.*_Z6vload4lPfS_S_1"  $OUT_FILE.clamped.ll && grep "call.*_Z7vstore4Dv4_flPfS0_S0_2"  $OUT_FILE.clamped.ll

float4 _cl_overloadable vload4(long i, float* cur, float* begin, float* end) {
  float *ptr = cur + i * 4;
  if (ptr < begin) return (float4)(0, 0, 0, 0);
  if (ptr + 4 > end) return (float4)(0, 0, 0, 0);
  return vload4(i, cur);
}

void _cl_overloadable vstore4(float4 x, long i, float* cur, float* begin, float* end) {
  float *ptr = cur + i * 4;
  if (ptr < begin) return;
  if (ptr + 4 > end) return;
  return vstore4(x, i, cur);
}

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  vstore4(loaded, i, out);
}

