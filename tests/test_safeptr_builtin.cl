// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Checking that call is made to safe signature" &&
// RUN: grep "call.*_Z6vload4lPfS_S_"  $OUT_FILE.clamped.ll && grep "call.*_Z7vstore4Dv4_flPfS0_S0_"  $OUT_FILE.clamped.ll

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  vstore4(loaded, i, out);
}
