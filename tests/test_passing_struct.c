// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check that unsupported code cannot be compiled TODO: this can be made to work" &&
// RUN: (
// RUN:   opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll > $OUT_FILE.stdout.txt 2>&1;
// RUN:     ([ $? != 0 ] && echo "Compilation aborted as expected" && grep "On line" $OUT_FILE.stdout.txt) || 
// RUN:     (echo "Clamping should have aborted unsupported case, however pass resulted:" && cat $OUT_FILE.clamped.ll && false) 
// RUN: ) 

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
