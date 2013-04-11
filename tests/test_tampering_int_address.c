// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check taking reference from int and returning next value" &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 0 ]

volatile int val = 5;

int main(void) {
  return *(&val + 1);
}
