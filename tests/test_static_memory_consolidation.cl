// RUN: echo "Compiling with nvptx target to get IR with proper address spaces. With llvm 3.3 maybe could be changed to SPIR." &&
// RUN: TARGET_FLAGS="-target spir" $OCLANG $TEST_SRC -S -O0 -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: if ! grep "%PrivateAllocationsType = type" $OUT_FILE.clamped.ll > /dev/null; then echo "Private address space was not found." && false; fi &&
// RUN: if grep "@GlobalAllocationsType" $OUT_FILE.clamped.ll > /dev/null; then echo "No static allocations from global AS allowed." && false; fi &&
// RUN: if ! grep "@localAllocations" $OUT_FILE.clamped.ll > /dev/null; then echo "Local address space was not found." && false; fi && 
// RUN: if ! grep "@constantAllocations" $OUT_FILE.clamped.ll > /dev/null; then echo "Constant address space was not found." && false; fi

typedef struct {
  int i;
  long l;
  float f;
} test_struct;

// Actually this is not allowed in OpenCL ( kernel cannot allocate from global )
/* __global float test_global[3] = { 1, 2, 3 }; */

// TODO add function call and also pass some args that are not directly got from argument

__constant float test_constant[3] = { 1, 2, 3 };

float test_calling_foo(
  int value_arg, 
  float *test_private,
  __global float* helper_in, 
  __global float* helper_out, 
  __constant float* helper_factors, 
  __local float* helper_scratch,
  __local test_struct* helper_test) {

  return (test_private[value_arg] + *helper_in + *helper_factors + *helper_out + *helper_scratch + helper_test[0].f);
}

__kernel void test(
  __global float* in, __global float* out, 
  __constant float* factors, __local float* scratch) {

  unsigned int i = 1; 

  //get_global_id(0);
  // compiler does not allow this..
  /* __constant float constant_zero = 0; */

  __local float test_local[3];

  float test_private[3] = { 1, 2, 3 };
  __private float test_private2[3] = { 1, 2, 3 };
  __local test_struct struct_table[3];

  //  __global float* another_test_global[3];
  //  __global float* global_test_val_float;
  //  __private test_struct private_struct_table[3];
  //  __global test_struct* global_struct_table[3];

  __global float *test = in;
  test_local[i] = test[i];
  // test = out; // uncomment this to fail simple data dependency check 
  test_local[i] = test[i];

  test_local[i] = in[i];
  out[i] = in[i];
  struct_table[i].f = test_private[i];
  struct_table[i].l = struct_table[i].f;

  out[i] = test_calling_foo(i, test_private2, in, out, factors, scratch, struct_table);
}
