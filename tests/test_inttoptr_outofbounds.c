// RUN: clang -ffreestanding -fno-builtin -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: echo "Check that invalid address returns 0 even after pointer to int cast." &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: opt -O3 -S $OUT_FILE.clamped.ll -o $OUT_FILE.clamped.O3.ll &&
// RUN: lli $OUT_FILE.clamped.ll; [ $? = 0 ]

volatile int val = 1;

int main(void) {
  // if trying to do = { 5 } does memset 0 initialization, might be a bug?
  int table[10] = { 5, 5, 5, 5, 5, 5, 5, 5, 5 ,5 };
  table[1] = val;
  long table_address_as_int = (long)table;
  // index enough far away to get 0
  table_address_as_int += 200*sizeof(int); 
  int *pointer = (int*)table_address_as_int;
  return *pointer;
}
