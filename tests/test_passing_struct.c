// RUN: clang -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: echo "Check passing struct in function argument as value." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 3 ]

struct OkStruct {
  int first;
  float second;
  int third;
  float fourth;
};

int extract_struct(struct OkStruct s_param) {
  return s_param.first + s_param.second + s_param.third + s_param.fourth;
}

int main(void) {
  struct OkStruct ok = { 2, 5.0, 3, 6 };
  ok.first = 2;
  ok.second = 0.1;
  ok.third = 1;
  ok.fourth = 0.01;
  return extract_struct(ok);
}
