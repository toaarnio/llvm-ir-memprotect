// RUN: clang -c $TEST_SRC -O0 -emit-llvm -o $OUT_FILE.bc &&
// RUN: echo "Check that unsupported code cannot be compiled" &&
// RUN: (
// RUN:   opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.bc -o $OUT_FILE.clamped.ll > $OUT_FILE.stdout.txt 2>&1;
// RUN:     ([ $? != 0 ] && echo "Compilation aborted as expected" && grep Assertion $OUT_FILE.stdout.txt) || 
// RUN:     (echo "Clamping should have aborted unsupported case, however pass resulted:" && cat $OUT_FILE.clamped.ll && false) 
// RUN: ) 

extern int ext_val;

int main(void) {
  int table[10];
  table[1] = ext_val;
  long table_address_as_int = (long)table;
  table_address_as_int += sizeof(int);
  int *pointer = (int*)table_address_as_int;
  return *pointer;
}
