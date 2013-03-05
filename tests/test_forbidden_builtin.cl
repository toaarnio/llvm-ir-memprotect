// RUN: $OCLANG $TEST_SRC -S -o $OUT_FILE.ll &&
// RUN: ( opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll;
// RUN:   ( [ ! $? = 0 ] && echo "OK: Compilation failed as expected") ||
// RUN:   ( echo "FAIL: pass should have asserted" && false )
// RUN: )

__kernel void test_kernel(__global half* in, __global half* out) {
  int i = get_global_id(0);
  float4 loaded = vload_half4(i, in);
  vstore_half4(loaded, i, out);
}

