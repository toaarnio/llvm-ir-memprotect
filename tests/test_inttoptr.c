// RUN: clang -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: echo "Check that this we can addres memory correctly even after pointer to int cast." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 1 ]

volatile int val = 1;

int main(void) {
  int table[10];
  table[1] = val;
  long table_address_as_int = (long)table;
  table_address_as_int += sizeof(int); 
  int *pointer = (int*)table_address_as_int;
  return *pointer;
}
