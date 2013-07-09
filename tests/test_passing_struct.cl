// RUN: $OCLANG -c $TEST_SRC -O0 -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Check passing struct in function argument as value. Should count 3." &&
// RUN: [ $($RUN_KERNEL $OUT_FILE.ll test_kernel 1 "(int,{0}):(int,{0})") == $($RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(int,{0}):(int,1):(int,{0}):(int,1)") ]
struct OkStruct {
  int first;
  float second;
  int third;
  float fourth;
};

int extract_struct(struct OkStruct s_param) {
  return s_param.first + s_param.second + s_param.third + s_param.fourth;
}

__kernel void test_kernel(__global int *in, __global int *out) {
  struct OkStruct ok = { 2, 5.0, 3, 6 };
  ok.first = 2;
  ok.second = 0.1;
  ok.third = 1;
  ok.fourth = 0.01;
  out[0] = extract_struct(ok);
  printf("%i\n", out[0]);
}
