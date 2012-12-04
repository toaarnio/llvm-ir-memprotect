/**
 * We should either assert or handle this case.
 */

// RUN: clang -c $TEST_SRC -O1 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check that unsupported code cannot be compiled" &&
// RUN: (
// RUN:   opt -load $CLAMP_PLUGIN -clamp-pointers -internalize -internalize-public-api-list=main -globaldce -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll ; 
// RUN:     ([ $? != 0 ] && echo "Compilation aborted as expected") || 
// RUN:     (echo "Clamping should have aborted unsupported case, however pass resulted:" && cat $OUT_FILE.clamped.ll && false) 
// RUN: ) 

extern int ext_val;

int main(void) {
  return *(&ext_val + 1);
}
