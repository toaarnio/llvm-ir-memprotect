// RUN: echo "Compiling with nvptx target to get IR with proper address spaces. With llvm 3.3 maybe could be changed to SPIR." &&
// RUN: TARGET_FLAGS="-target nvptx" $OCLANG $TEST_SRC -S -O0 -o $OUT_FILE.ll &&
// RUN: opt -load $CLAMP_PLUGIN -clamp-pointers -allow-unsafe-exceptions -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll &&
// RUN: if ! grep "@AddressSpace0StaticData" $OUT_FILE.clamped.ll > /dev/null; then echo "Private address space was not found." && false; fi &&
// RUN: if grep "@AddressSpace1StaticData" $OUT_FILE.clamped.ll > /dev/null; then echo "No static allocations from global AS allowed." && false; fi &&
// RUN: if ! grep "@AddressSpace3StaticData" $OUT_FILE.clamped.ll > /dev/null; then echo "Local address space was not found." && false; fi && 
// RUN: if ! grep "@AddressSpace4StaticData" $OUT_FILE.clamped.ll > /dev/null; then echo "Constant address space was not found." && false; fi

typedef struct {
  int i;
  long l;
  float f;
} test_struct;

// Actually this is not allowed in OpenCL ( kernel cannot allocate from global )
/* __global float test_global[3] = { 1, 2, 3 }; */

__constant float test_constant[3] = { 1, 2, 3 };
__private float private_float = 1;
__local float local_float;

__kernel void test(__global float* in, __global float* out) {
  unsigned int i = 1; //get_global_id(0);
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
  struct_table[i].f = test_local[i];
  struct_table[i].l = struct_table[i].f;
}
