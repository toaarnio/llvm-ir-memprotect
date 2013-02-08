// RUN: clang $TEST_SRC -x cl -include pocl_kernel.h -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -S -o $OUT_FILE.ll &&
// RUN: opt -debug -load $CLAMP_PLUGIN -clamp-pointers -S $OUT_FILE.ll -o $OUT_FILE.clamped.ll
// TODO: write runner, which actually executes the kernel code...
// TODO: check that opencl kernel is converted to webcl one, which takes one ulong after each pointer defining size

__kernel void square(__global float* input, __global float* output) {
  int i = get_global_id(0);
  output[i] = input[i]*input[i];
}

/*
#define BLOCK_SIZE 16

__kernel void matmul(__global float* A, __global float* B, __global float* C) {

  __local float scratchA[BLOCK_SIZE][BLOCK_SIZE];
  __local float scratchB[BLOCK_SIZE][BLOCK_SIZE];

  int globalX = get_global_id(0);
  int globalY = get_global_id(1);
  int size = get_global_size(0);
  int k;
  float sum = 0.0f;
  int numBlocks = size / BLOCK_SIZE;
  int b;

  int tidX = get_local_id(0);
  int tidY = get_local_id(1);

  for(b = 0; b < numBlocks; ++b) {
      // Populate a cache for A/B
      int x;
      int y;

      x = b * BLOCK_SIZE + tidX;
      y = globalY;

      scratchA[tidY][tidX] = A[y * size + x];

      x = globalX;
      y = b * BLOCK_SIZE + tidY;

      scratchB[tidY][tidX] = B[y * size + x];

      barrier(CLK_LOCAL_MEM_FENCE);

      for(k = 0; k < BLOCK_SIZE; ++k) {
          float myA;
          float myB;

          myA = scratchA[tidY][k];
          myB = scratchB[k][tidX];

          sum += myA * myB;
        }

      barrier(CLK_LOCAL_MEM_FENCE);
    }

  C[globalY * size + globalX] = sum;
}
*/
