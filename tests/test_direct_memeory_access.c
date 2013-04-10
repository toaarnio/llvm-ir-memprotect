// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check that some checks are added to case. Would be really strange if in some program this really would produce any reasonable code." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll

// should compile and return 0
int main(void) {
  return *((int*)0x00ff00ff);
}
