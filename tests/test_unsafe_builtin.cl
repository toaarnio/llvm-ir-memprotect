// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Checking that call is made to safe signature" &&
// RUN: grep "_Z7vstore4Dv4_flPU3AS1fS1_S1_396" $OUT_FILE.clamped.ll &&
// RUN: grep "_Z6vload4lPU3AS1fS0_S0_234" $OUT_FILE.clamped.ll

__kernel void test_kernel(__global float* in, __global float* out) {
  int i = get_global_id(0);
  float4 loaded = vload4(i, in);
  vstore4(loaded, i, out);
}

