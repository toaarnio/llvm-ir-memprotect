// RUN: clang -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: echo "Check that valid indirect pointer returns correct value." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 1 ]

int val = 1;
int *ext_val = &val;

int main(void) {
  int **indirect_ptr1 = &ext_val;
  int **indirect_ptr2 = (indirect_ptr1+10);
  return **(indirect_ptr2-10);
}
