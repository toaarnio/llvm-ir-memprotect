// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check taking reference from int and returning next value" &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 0 ]

int main(void) {
  int a;
  return *(&a + 1);
}
