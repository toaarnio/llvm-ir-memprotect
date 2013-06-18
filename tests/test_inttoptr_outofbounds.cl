// RUN: $OCLANG -c $TEST_SRC -O0 -S -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: echo "Check that invalid address returns 0 even after pointer to int cast." &&
// RUN: $RUN_KERNEL $OUT_FILE.clamped.ll test_kernel 1 "(int,1)" &&
// RUN: echo "Add check that running kernel outputs 0" && false

__kernel void test_kernel(int val) {
  // if trying to do = { 5 } does memset 0 initialization, might be a bug?
  int table[10] = { 5, 5, 5, 5, 5, 5, 5, 5, 5 ,5 };
  table[1] = val;
  long table_address_as_int = (long)table;
  // index enough far away to get 0
  table_address_as_int += 200*sizeof(int); 
  int *pointer = (int*)table_address_as_int;
  printf("%i", *pointer);
}
