// RUN: $OCLANG -c $TEST_SRC -O0 -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Check that this we can addres memory correctly even after pointer to int cast." &&
// RUN: $RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(int,1)" &&
// RUN: echo "Add check that running kernel outputs 1" && false

__kernel void test_kernel(int val) {
  int table[10];
  table[1] = val;
  long table_address_as_int = (long)table;
  table_address_as_int += sizeof(int); 
  int *pointer = (int*)table_address_as_int;
  printf("%i", *pointer);
}
